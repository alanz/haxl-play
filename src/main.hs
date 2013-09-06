{-# LANGUAGE ExistentialQuantification,
             TypeFamilies,
             GADTs,
             RankNTypes,
             ScopedTypeVariables,
             DeriveDataTypeable,
             StandaloneDeriving,
             MultiParamTypeClasses,
             FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

-- Based on "The Haxl Project at Facebook (slides from my talk at ZuriHac)"
-- https://github.com/meiersi/HaskellerZ/blob/master/meetups/20130829-FPAfternoon_The_Haxl_Project_at_Facebook/The%20Haxl%20Project%20at%20Facebook.pdf?raw=true

-- http://www.reddit.com/r/haskell/comments/1le4y5/the_haxl_project_at_facebook_slides_from_my_talk/

----------------------------------------------------------

import Control.Applicative
import Control.Concurrent.MVar
import Control.Monad hiding (mapM, filterM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free  -- from the `free` package
import Data.Hashable
import Data.Hashable.Extras
import Data.Maybe
import Data.Monoid (Monoid(..))
import qualified Data.Sequence as S -- Queue with O(1) head and tail operations
import Data.Text (Text)
import Data.Traversable hiding (mapM)
import Data.Typeable
import Prelude hiding (mapM)

main = putStrLn "hello"

newtype Haxl a = Haxl { unHaxl :: IO (Result a) }
                 deriving (Functor)

data Result a = Done a
              | Blocked (Haxl a)
                 deriving (Functor)

instance Monad Haxl where
  return a = Haxl (return (Done a))
  m >>= k = Haxl $ do
     a <- unHaxl m
     case a of
       Done a' -> unHaxl (k a')
       Blocked r -> return (Blocked (r >>= k))

{-

-- ++AZ++ initial simple version, generalised to a DataSource req
-- later

-- The only side effects happen here
dataFetch :: Request a -> Haxl a
dataFetch r = do
  addRequest r
  Haxl (return (Blocked (Haxl (getResult r))))
-}

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

numCoolFriends :: Id -> Haxl Int
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

------------------------------------------------------------------------
-- Data Source / core


class (Typeable1 req, Hashable1 req, Eq1 req) => DataSource req where

  data DataState req -- ^ Internal state, stored by core on behalf of
                     -- the DataSource

  fetch :: DataState req -> [BlockedFetch req] -> IO ()


-- AZ: I think this stores the original request (hence Eq1) and the
-- MVar the result will eventually be written to when the IO
-- completes.
data BlockedFetch req = forall a . BlockedFetch (req a) (MVar a)
-- Question: how does a BlockedFetch come into existence? Can only be
-- through the dataFetch call interacting with the core.


-- It is Core’s job to keep track of requests submitted via dataFetch

-- When the computation is blocked, we have to fetch the batch of
-- requests from the data source

-- Core has a single way to issue a request
-- The only side effects happen here
-- Note: I think this should be putting stuff in the scheduler
dataFetch :: DataSource req => req a -> Haxl a
dataFetch r = do
  addRequest r
  Haxl (return (Blocked (Haxl (getResult r))))

-- Three ways that a data source interacts with core:
--   * issuing a data fetch request [addRequest?]
--   * persistent state             [via DataState req]
--   * fetching the data            [via fetch...getResult]



-- AZ new guess: addRequest/getResult belong in roundRobin, to manage
-- the state properly.
-- AZ Guess: Pass the request to the data source, store the state that
-- changes pertaining to the data source, as well as keep track of the
-- fact of the request in a BlockedFetch item.
addRequest :: DataSource req => req a -> Haxl a
addRequest = undefined

-- AZ guess: should end up calling fetch on the given datasource.
getResult :: DataSource req => req a -> IO (Result a)
getResult = undefined
  -- fetch s []

-- ---------------------------------------------------------------------

-- Friends data source -------------------------------------

-- First define FriendsReq
data FriendsReq a where
  FriendsOf :: Id -> FriendsReq [Id]

-- Then define a Friends DataSource
instance DataSource FriendsReq where
  data DataState FriendsReq = Ss

  -- fetch :: DataState req -> [BlockedFetch req] -> IO ()
  fetch = fetchFriendsDs

fetchFriendsDs :: DataState FriendsReq -> [BlockedFetch FriendsReq] -> IO ()
fetchFriendsDs s bfs = do
  mapM_ fetchOne bfs
  return ()

fetchOne :: BlockedFetch FriendsReq -> IO ()
fetchOne (BlockedFetch (FriendsOf id) mvar) = do
  -- NOTE: calculating v could make use of external IO
  let v = [id,Id "foo"] -- Friends with self :)
  putMVar mvar v
  return ()

-- ---------------------------------------------------------------------
-- Admin stuff

class Eq1 req where
  eq1 :: req a -> req a -> Bool

instance Eq1 FriendsReq where
  eq1 (FriendsOf x) (FriendsOf y) = x == y

instance Hashable1 FriendsReq where
  hashWithSalt1 m (FriendsOf x) = hashWithSalt m x

-- instance Typeable1 FriendsReq where
--  typeOf1 (FriendsOf x) =
deriving instance Typeable1 FriendsReq


------------------------------------------------------------------------
-- Simplistic core by AZ


-- testCore :: IO ()
testCore = do
  runCore $ friendsOf (Id "Alan")
  return ()

runCore :: Haxl a -> Haxl ()
runCore q = roundRobin (core q)

core :: Haxl a -> Thread Haxl a
core q = do
  lift q


-- --------------------------------------------------------------------

friendsOf :: Id -> Haxl [Id]
friendsOf id = do
  let req = FriendsOf id
  dataFetch req

-- ---------------------------------------------------------------------

thread1 :: Thread Haxl [Id]
thread1 = do
  r <- lift $ friendsOf (Id "Simon")
  return r


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


data Id = Id String
          deriving (Eq,Show)

instance Hashable Id where
  hashWithSalt m (Id a) = hashWithSalt m a


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


-- ---------------------------------------------------------------------
-- Threading model, based on
-- http://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html

data ThreadF next = Fork  next next
                  | Yield next
                  | ThreadDone
                  deriving (Functor)

type Thread = FreeT ThreadF

yield :: (Monad m) => Thread m ()
yield = liftF (Yield ())

done :: (Monad m) => Thread m r
done = liftF ThreadDone

cFork :: (Monad m) => Thread m Bool
cFork = liftF (Fork False True)

fork :: (Monad m) => Thread m a -> Thread m ()
fork thread = do
    child <- cFork
    when child $ do
        thread
        done

roundRobin :: (Monad m) => Thread m a -> m ()
roundRobin t = go (S.singleton t)  -- Begin with a single thread
  where
    go ts = case (S.viewl ts) of
        -- The queue is empty: we're done!
        S.EmptyL   -> return ()

        -- The queue is non-empty: Process the first thread
        t S.:< ts' -> do
            x <- runFreeT t  -- Run this thread's effects
            case x of
                -- New threads go to the back of the queue
                Free (Fork t1 t2) -> go (t1 S.<| (ts' S.|> t2))

                -- Yielding threads go to the back of the queue
                Free (Yield   t') -> go (ts' S.|> t')

                -- Thread done: Remove the thread from the queue
                Free  ThreadDone  -> go ts'
                Pure  _           -> go ts'

-- ---------------------------------------------------------------------

-- EOF

