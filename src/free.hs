{-# LANGUAGE DeriveFunctor #-}

-- Experimenting with Free Monads

-- Based on the Concurrency section in
-- http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free  -- from the `free` package
import Data.Sequence -- Queue with O(1) head and tail operations

main = putStrLn "hello"

{-
-- type Thread m = [m ()]

data Thread m r = Atomic (m (Thread m r)) | Return r

atomic :: (Monad m) => m a -> Thread m a
atomic m = Atomic $ liftM Return m


instance (Monad m) => Monad (Thread m) where
    return = Return
    (Atomic m) >>= f = Atomic (liftM (>>= f) m)
    (Return r) >>= f = f r

-- ---------------------------------------------------------------------

thread1 :: Thread IO ()
thread1 = do
    atomic $ print 1
    atomic $ print 2

thread2 :: Thread IO ()
thread2 = do
    str <- atomic $ getLine
    atomic $ putStrLn str

-- ---------------------------------------------------------------------

interleave ::
    (Monad m) => Thread m r -> Thread m r -> Thread m r
interleave (Atomic m1) (Atomic m2) = do
    next1 <- atomic m1
    next2 <- atomic m2
    interleave next1 next2
interleave t1 (Return _) = t1
interleave (Return _) t2 = t2

-- ---------------------------------------------------------------------

runThread :: (Monad m) => Thread m r -> m r
runThread (Atomic m) = m >>= runThread
runThread (Return r) = return r

-- Run as
--
-- > runThread (interleave thread1 thread2)
--
-}

------------------------------------------------------------------------


-- http://www.haskellforall.com/2013/06/from-zero-to-cooperative-threads-in-33.html


data ThreadF next = Fork  next next
                  | Yield next
                  | Done
                  deriving (Functor)

type Thread = FreeT ThreadF

yield :: (Monad m) => Thread m ()
yield = liftF (Yield ())

done :: (Monad m) => Thread m r
done = liftF Done

cFork :: (Monad m) => Thread m Bool
cFork = liftF (Fork False True)

fork :: (Monad m) => Thread m a -> Thread m ()
fork thread = do
    child <- cFork
    when child $ do
        thread
        done

roundRobin :: (Monad m) => Thread m a -> m ()
roundRobin t = go (singleton t)  -- Begin with a single thread
  where
    go ts = case (viewl ts) of
        -- The queue is empty: we're done!
        EmptyL   -> return ()

        -- The queue is non-empty: Process the first thread
        t :< ts' -> do
            x <- runFreeT t  -- Run this thread's effects
            case x of
                -- New threads go to the back of the queue
                Free (Fork t1 t2) -> go (t1 <| (ts' |> t2))

                -- Yielding threads go to the back of the queue
                Free (Yield   t') -> go (ts' |> t')

                -- Thread done: Remove the thread from the queue
                Free  Done        -> go ts'
                Pure  _           -> go ts'

-- -------------------------------------------------------------
-- Try this with IO actions

thread1 :: Thread IO ()
thread1 = do
    lift $ print 1
    yield
    liftIO $ print 2

thread2 :: Thread IO ()
thread2 = do
    str <- liftIO getLine
    yield
    liftIO $ putStrLn str

comp :: Thread IO ()
comp = do
  fork thread2
  thread1

t = roundRobin comp

