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
{- # LANGUAGE FlexibleContexts # -}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.List
import Data.Typeable
import qualified Data.Map as Map
import qualified Data.Sequence as S -- Queue with O(1) head and tail operations

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
  -- (>>=) :: Monad m => m a -> (a -> m b) -> m b
  -- (>>=) :: Haxl a -> (a -> Haxl b) -> Haxl b
  m >>= k = Haxl $ do
     a <- unHaxl m
     case a of
       Done a' -> unHaxl (k a')
       Blocked r -> return (Blocked (r >>= k))

-- ---------------------------------------------------------------------
-- We need a recursive data structure to thread through the
-- computation
-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html


-- This could come about as the interaction of Haxl and Result

-- Result (IO (Result a)) = Blocked (Haxl (IO Result a))

t1 :: Haxl Char
t1 = Haxl (return (Done 'a'))

t2 :: Haxl (Haxl Char)
t2 = Haxl (return (Blocked (return t1)))

t3 :: Haxl (Haxl Char)
t3 = Haxl (return (Blocked (return (Haxl (return (Done 'a'))))))


-- Do we need some way of forking, or does Applicative do that for us?

-- We need to go from hFriendsOf x to some kind of structure.

-- The essence of dataFetch is that we get back something like
t4 :: IO (Result a) -> Haxl a
t4 j = Haxl (return (Blocked (Haxl j)))
-- where j is 'getResult' of the original request.

-- In monadic computation, we have
commonFriends x y = do
  xf <- friendsOf x
  yf <- friendsOf y
  return $ intersect xf yf

-- Desugared
commonFriends' x y =
  friendsOf x >>=
     (\xf -> friendsOf y >>=
       (\yf -> return
         (intersect xf yf)
       )
     )

-- substituting (getResult (FindFriends x)) and (getResult (findFriends y)) as per t4
{-
commonFriends'' x y =
  (getResult (FindFriends x)) >>=
     (\xf -> (getResult (FindFriends y)) >>=
       (\yf -> return
         (intersect xf yf)
       )
     )
-}
-- ---------------------------------------------------------------------

hFriendsOf :: Id -> Haxl [Id]
hFriendsOf x = dataFetch (FindFriends x)

-- ---------------------------------------------------------------------

-- data Request a = R a
data Request a where
  FindFriends :: Id -> Request [Id]
  deriving (Typeable)

deriving instance Eq (Request a)
deriving instance Ord (Request a)
deriving instance Show (Request a)

-- Is dataFetch like build_trace in EventsThreads?
dataFetch :: Request a -> Haxl a
dataFetch r = do
  addRequest r
  Haxl (return (Blocked (Haxl (getResult r))))

-- ---------------------------------------------------------------------
-- Introducing the Free Monad (as used in EventsThreads)

data ThreadF a = AddRequest a
               | GetResult a
               deriving (Functor)

type Thread = FreeT ThreadF

-- or fAddRequest :: MonadFree ThreadF m => a -> m a
fAddRequest :: (Monad m) => a -> Thread m a
fAddRequest r = liftF (AddRequest r)

fGetResult :: (Monad m) => a -> Thread m a
fGetResult r = liftF (GetResult r)

-- State structure. Each request must be cached. Duplicate requests
-- must be coalesced. BUT, need to keep track of the individual
-- blocked calculation points.
--
-- cache : (req,Maybe res,mvar)
--

worker_main c ready_queue = do
  trace <- readChan ready_queue
  s' <- case trace of
    AddRequest req@(FindFriends x) ->
      do
        case Map.lookup req c of
          Nothing -> do
            mvar <- newEmptyMVar
            forkIO (friendsOf x >>= putMVar mvar)
            return $ Map.insert req (Nothing,mvar) c
          Just (Just r,_) -> do
            -- How to return the result?
            return c
          Just (Nothing,mvar) -> do
            mr <- tryTakeMVar mvar
            case mr of
              Nothing -> return c
              Just r -> return $ Map.insert req (Just r,mvar) c
    GetResult req@(FindFriends x) ->
      do
        return c
  -- recurse
  worker_main s' ready_queue

run calc = do
  rq <- newChan
  worker_main Map.empty rq

-- ---------------------------------------------------------------------


-- From EventsThreads
build_trace :: Haxl a -> Result a
-- build_trace (Haxl f) = f (\c -> SYS_RET)
build_trace (Haxl f) = undefined

{-
roundRobin h -> go Map.empty (S.singleton t)
  where
    go c ts = case (viewl ts) of
        -- The queue is empty: we're done!
        EmptyL   -> return ()

    t S.:< ts' -> do
      x <- runFreeT t
      case x of
        Free (Done x) -> go ts'
        Free (Blocked x)
-}

-- ---------------------------------------------------------------------

addRequest :: Request a -> Haxl (Request a)
addRequest req@(FindFriends x) = undefined

getResult :: Request a -> IO (Result a)
getResult req = undefined


-- Note: Needs to be generalised to all requests, by implementing a
-- class for Request
doAddRequest ::
  Map.Map (Request [Id]) (Maybe a, MVar [Id])
  -> Request t
  -> IO (Map.Map (Request [Id]) (Maybe a, MVar [Id]))
doAddRequest c req@(FindFriends x) = do
        case Map.lookup req c of
          Nothing -> do
            mvar <- newEmptyMVar
            forkIO (friendsOf x >>= putMVar mvar)
            return $ Map.insert req (Nothing,mvar) c
          Just _ -> return c

doGetResult c req@(FindFriends x) = do
        case Map.lookup req c of
          Nothing -> do
            -- This should never happen....
            mvar <- newEmptyMVar
            forkIO (friendsOf x >>= putMVar mvar)
            return (Map.insert req (Nothing,mvar) c, Nothing)
          Just (Just r,_) -> return (c,Just r)
          Just (Nothing,mvar) -> do
            mr <- tryTakeMVar mvar
            case mr of
              Nothing -> return (c,Nothing)
              Just r -> return (Map.insert req (Just r,mvar) c,Just r)



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

-- ---------------------------------------------------------------------

haxlFriendsOf :: Id -> Haxl [Id]
haxlFriendsOf x = dataFetch (FindFriends x)

haxlCommonFriends :: Id -> Id -> Haxl [Id]
haxlCommonFriends x y =
  (intersect <$> haxlFriendsOf x <*> haxlFriendsOf y)

