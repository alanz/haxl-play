{-# LANGUAGE ExistentialQuantification,
             TypeFamilies,
             GADTs,
             RankNTypes,
             ScopedTypeVariables,
             DeriveDataTypeable,
             StandaloneDeriving,
             MultiParamTypeClasses,
             FlexibleInstances #-}

-- Based on "The Haxl Project at Facebook (slides from my talk at ZuriHac)"
-- https://github.com/meiersi/HaskellerZ/blob/master/meetups/20130829-FPAfternoon_The_Haxl_Project_at_Facebook/The%20Haxl%20Project%20at%20Facebook.pdf?raw=true

-- http://www.reddit.com/r/haskell/comments/1le4y5/the_haxl_project_at_facebook_slides_from_my_talk/

----------------------------------------------------------

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad hiding (mapM, filterM)
import Data.Maybe
import Data.Text (Text)
import Data.Traversable hiding (mapM)
import Data.Typeable
import Prelude hiding (mapM)
import Data.Hashable
import Data.Hashable.Extras
import Data.Monoid (Monoid(..))

main = putStrLn "hello"

newtype Haxl a = Haxl { unHaxl :: IO (Result a) }

data Result a = Done a | Blocked (Haxl a)

instance Monad Haxl where
  return a = Haxl (return (Done a))
  m >>= k = Haxl $ do
     a <- unHaxl m
     case a of
       Done a -> unHaxl (k a)
       Blocked r -> return (Blocked (r >>= k))

dataFetch :: Request a -> Haxl a
dataFetch r = do
  addRequest r
  Haxl (return (Blocked (Haxl (getResult r))))


instance Applicative Haxl where
  pure = return
  Haxl f <*> Haxl a = Haxl $ do
    r <- f
    case r of
      Done f' -> do
        ra <- a
        case ra of
          Done a' -> return (Done (f' a'))
          Blocked a' -> return (Blocked (f' <$> a'))
      Blocked f' -> do
        ra <- a
        case ra of
          Done a' -> return (Blocked (f' <*> return a'))
          Blocked a' -> return (Blocked (f' <*> a'))


-- mapM :: Monad m => (a -> m b) -> [a] -> m [b]
-- class (Functor t, Data.Foldable.Foldable t) => Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-- mapM :: (a -> Haxl b) ->  [a] -> Haxl [b]
-- mapM :: (a -> Haxl b) ->  [a] -> Haxl [b]
mapM = traverse

-- Prelude.sequence :: Monad m => [m a] -> m [a]
-- Data.Traversable.sequence :: (Monad m, Traversable t) => t (m a) -> m (t a)

-- class (Functor t, Data.Foldable.Foldable t) => Traversable t where
--    Data.Traversable.sequence :: Monad m => t (m a) -> m (t a)

-- class (Functor t, Data.Foldable.Foldable t) => Traversable t where
--    sequenceA :: Applicative f => t (f a) -> f (t a)

-- sequence = sequenceA


-- ---------------------------------------------------------------------

-- filterM that batches the data fetches correctly
-- filterM :: (Applicative f, Monad f) => (a -> f Bool) -> [a] -> f [a]
filterM :: (a -> Haxl Bool) -> [a] -> Haxl [a]
filterM pred xs = do
  bools <- mapM pred xs
  return [ x | (x,True) <- zip xs bools ]


-- ---------------------------------------------------------------------
-- Examples in use

--
-- Return the names of all the friends of the user that are
-- members of the Functional Programming group.
--
fpFriends :: Id -> Haxl [Text]
fpFriends id = do
  fs <- friendsOf id
  fp <- filterM (memberOfGroup functionalProgramming) fs
  mapM getName fp

-- ---------------------------------------------------------------------
-- Written another way

nameOfFPFriend :: Id -> Haxl (Maybe Text)
nameOfFPFriend id = do
  b <- memberOfGroup functionalProgramming id
  if b then Just <$> getName id
       else return Nothing

fpFriends2 :: Id -> Haxl [Text]
fpFriends2 id = do
  fs <- friendsOf id
  name_maybes <- mapM nameOfFPFriend fs
  return (catMaybes name_maybes)

------------------------------------------------------------------------

newtype HaxlQuery a = HaxlQuery { query :: Haxl [a] }
-- basically ListT Haxl, but ListT doesn’t work!
-- instances of Monad, MonadPlus

selectFrom :: Haxl [a] -> HaxlQuery a
selectFrom = HaxlQuery

liftH :: Haxl a -> HaxlQuery a
liftH m = HaxlQuery (return <$> m)

suchThat :: Haxl Bool -> HaxlQuery ()
suchThat m = liftH m >>= guard

-- ---------------------------------------------------------------------

fpFriends3 :: Id -> Haxl [Text]
fpFriends3 id =
  query $ head [ name | f <- [selectFrom (friendsOf id)],
                        _ <- [suchThat (memberOfFPGroup f)],
                        name <- [liftH (getName id)] ]

-- ---------------------------------------------------------------------
-- DSL tricks

instance Num a => Num (Haxl a) where
  (+)         = liftA2 (+)
  (-)         = liftA2 (-)
  (*)         = liftA2 (*)
  abs         = liftA abs
  signum      = liftA signum
  fromInteger = fromInteger

-- ---------------------------------------------------------------------

simonFriends :: Id -> Haxl [Id]
simonFriends id =
  filterM (isCalled "Simon") =<< friendsOf id

numCoolFriends id =
  (length <$> fpFriends id) + (length <$> simonFriends id)
-- NOTE: each side of the '+' above does duplicate data fetching


-- =====================================================================
-- Implementation

-- An example GADT
data ExampleReq a where
  CountAardvarks :: String -> ExampleReq Int
  ListWombats :: Id -> ExampleReq [Id]
  deriving Typeable

-- Data Source State
class (Typeable1 req, Hashable1 req, Eq1 req)
  => DataSource req where
  data DataState req
  fetch :: DataState req -> [BlockedFetch req] -> IO ()

class Eq1 req where
  eq1 :: req a -> req a -> Bool

data BlockedFetch req = forall a . BlockedFetch (req a) (MVar a)

-- Core has a single way to issue a request
--   (renamed from dataFetch to avoid clash with earlier defn)
coreDataFetch :: DataSource req => req a -> Haxl a
coreDataFetch r = do
  addRequest r
  Haxl (return (Blocked (Haxl (getResult r))))


------------------------------------------------------------------------
-- Simplistic core by AZ


-- Friends data source -------------------------------------

-- First define FriendsReq

data FriendsReq a where
  FriendsOf :: Id -> FriendsReq [Id]

instance Eq1 FriendsReq where
  eq1 (FriendsOf x) (FriendsOf y) = x == y

instance Hashable1 FriendsReq where
  hashWithSalt1 m (FriendsOf x) = hashWithSalt m x

-- instance Typeable1 FriendsReq where
--  typeOf1 (FriendsOf x) =
deriving instance Typeable1 FriendsReq

-- Then define a Friends DataSource

instance DataSource FriendsReq where
  data DataState FriendsReq = Ss

  -- fetch :: [BlockedFetch FriendsReq] -> IO ()
  fetch s bfs = do
    return ()

-- the core data structure

-- a needs to be Typeable
data CoreState a = CoreState
                  { pending :: [a]
                  }

-- coreDataFetch :: DataSource req => req a -> Haxl a

friendsOf :: Id -> Haxl [Id]
friendsOf = undefined



------------------------------------------------------------------------
-- ++AZ++ missing definitions ---------------------------

-- newtype HaxlQuery a = HaxlQuery { query :: Haxl [a] }

instance Monoid (HaxlQuery a) where
  mempty = HaxlQuery $ return []
  (HaxlQuery x) `mappend` (HaxlQuery y) = HaxlQuery ((++) <$> x <*> y)

instance MonadPlus HaxlQuery where
  mzero = mempty
  mplus = mappend

instance Monad HaxlQuery where
  return a = HaxlQuery $ return [a]
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- (>>=) :: HaxlQuery a -> (a -> HaxlQuery b) -> HaxlQuery b
  -- so a :: Haxl [t]
  --    b :: Haxl [u]
  m >>= k = do
     a <- m
     (k a)

-- ---------------------------------------------------------------------



memberOfGroup = undefined
functionalProgramming = undefined
memberOfFPGroup = undefined

getName :: Id -> Haxl Text
getName = undefined

isCalled = undefined

data Request a = Request a
addRequest = undefined
getResult = undefined

data Id = Id String
          deriving (Eq,Show)

instance Hashable Id where
  hashWithSalt m (Id a) = hashWithSalt m a 

instance Functor Haxl where
  fmap = undefined

{-
Need
fmap :: (a -> b) -> f a -> f b
fmap id = id
fmap (p . q) = (fmap p) . (fmap q)
-}

-- ------------------------------------------------------------------------

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

t = transpose [[1,2,3],[4,5,6]]

--  -----------------------------------------------------------------------

-- understanding the class definition for DataSource

class Foo a where
  data FooState a
  fooOp :: a -> Int

instance Foo String where
  data FooState String = Baz String | Buz | Biz
  fooOp str = length str

blorp:: FooState String -> Int
blorp Buz = 0
blorp Biz = 0
blorp (Baz str) = fooOp str


