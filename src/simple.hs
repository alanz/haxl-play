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

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Data.List
import Data.Typeable
import qualified Data.Map as Map

main = putStrLn "hello"

data Id = Id String
          deriving (Eq,Show,Ord)

friendsOf :: Id -> IO [Id]
friendsOf x = do
  return $ Map.findWithDefault [] x friendsMap


friendsMap = Map.fromList
   [(Id "a", [Id "b",Id "c"])
   ,(Id "b", [Id "a",Id "c"])
   ]


numFof :: Id -> Id -> IO Int
numFof x y = do
  fox <- friendsOf x
  foy <- friendsOf y
  return $ length (intersect fox foy)


concurrentNumFof x y = do
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  forkIO (friendsOf x >>= putMVar m1)
  forkIO (friendsOf y >>= putMVar m2)
  fx <- takeMVar m1
  fy <- takeMVar m2
  return (length (intersect fx fy))

asyncNumFof x y = do
  ax <- async (friendsOf x)
  ay <- async (friendsOf y)
  fx <- wait ax
  fy <- wait ay
  return (length (intersect fx fy))

concurrentlyNumFof x y = do
  (fx,fy) <- concurrently (friendsOf x) (friendsOf y)
  return (length (intersect fx fy))

-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------

hFriendsOf :: Id -> Haxl [Id]
hFriendsOf x = do
  a <- dataFetch (FindFriends x)
  return a

-- ---------------------------------------------------------------------

-- data Request a = R a
data Request a where
  FindFriends :: Id -> Request [Id]
  deriving Typeable


dataFetch :: Request a -> Haxl a
dataFetch r = do
  addRequest r
  Haxl (return (Blocked (Haxl (getResult r))))

addRequest :: Request a -> Haxl a
addRequest = undefined

getResult :: Request a -> IO (Result a)
getResult = undefined

-- ---------------------------------------------------------------------

-- numCommonFriends :: Id -> Id -> Haxl Int
numCommonFriends1 x y = do
  fx <- friendsOf x
  fy <- friendsOf y
  return (length (intersect fx fy))


-- ---------------------------------------------------------------------

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

-- ---------------------------------------------------------------------

numCommonFriends x y =
  length <$> (intersect <$> friendsOf x <*> friendsOf y)
