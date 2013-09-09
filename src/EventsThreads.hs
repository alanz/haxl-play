-- Based on
--
-- 'Combining events and threads for scalable network
-- services implementation and evaluation of monadic,
-- application-level concurrency primitives'
--
-- by Peng Li, and Steve Zdancewic
--
-- http://portal.acm.org/citation.cfm?doid=1273442.1250756

import Control.Concurrent.Chan
import Network.Socket
-- import Control.Exception

main = putStrLn "hello"



-- ---------------------------------------------------------------------
-- The CPS Monad M

newtype M a = M ((a->Trace)->Trace)
instance Monad M where
  return x = M (\c -> c x)
  (M g) >>= f = M (\c -> g (\a -> let M h =f a in h c))

-- Converting monadic computation to a trace
build_trace ::M a-> Trace
build_trace (M f) = f (\c-> SYS_RET)

-- ---------------------------------------------------------------------
-- A list of system calls used in the multithreaded programming style:

-- sys_nbio f  -- Perform a nonblocking IO function f
sys_nbio f = M(\c->SYS_NBIO (do x<-f;return (c x)))

-- sys_fork c  -- Create a new thread running function c
sys_fork f = M (\c->SYS_FORK (build_trace f) (c ()) )

-- sys_yield   -- Switch to another thread
sys_yield = M (\c -> SYS_YIELD (c () ) )

-- sys_ret     -- Terminate the current thread
sys_ret = M (\c->SYS_RET)

-- sys_epoll_wait fd event -- Block and wait for an epoll event on a file descriptor
sys_epoll_wait fd ev = M (\c -> SYS_EPOLL_WAIT fd ev (c ()))

-- sys_throw e −−raise an exception e
sys_throw = undefined
-- sys_catch f g −−execute computation f using the exception handler g
sys_catch f g = undefined


-- ---------------------------------------------------------------------

-- alias for now
type FD = Int

data EPOLL_EVENT = EPOLL_READ

data Trace =
     SYS_NBIO (IO Trace)
   | SYS_FORK Trace Trace
   | SYS_YIELD Trace
   | SYS_RET
   | SYS_EPOLL_WAIT FD EPOLL_EVENT Trace
   | SYS_THROW Exception
   | SYS_CATCH Trace (Exception->Trace) Trace

execute trace =
  case trace of
    SYS_NBIO c ->
      do
         cont <- c
         execute cont
    SYS_FORK t1 t2 ->
      do
        return ()

-- ---------------------------------------------------------------------

-- example server - fig 10
sock_accept server_fd = do {
  new_fd <- sys_nbio (myaccept server_fd);
  if new_fd > 0
    then return new_fd
    else do { sys_epoll_wait new_fd EPOLL_READ;
              sock_accept server_fd;
            }
}

-- ---------------------------------------------------------------------
-- round robin scheduler - fig 11

worker_main :: Chan Trace -> IO b
worker_main ready_queue = do {
  -- fetch a trace from the queue
  trace <- readChan ready_queue;
  case trace of
    -- Nonblocking I/O operation: c has type 'IO Trace'
    SYS_NBIO c ->
      do { -- Perform the I/O operation in c
           -- The result is cont, which has type 'Trace'
           cont <- c;
           -- Add the continuation to the end of the ready queue
           writeChan ready_queue cont;
         }
    -- Fork: write both continuations to the end of the ready queue
    SYS_FORK c1 c2 ->
      do { writeChan ready_queue c1;
           writeChan ready_queue c2;
         }
    SYS_RET -> return (); -- thread terminated, forget it
  ;
  worker_main ready_queue; -- recursion
}

-- ---------------------------------------------------------------------
-- Figure 13 - Multithreaded code with exception handling

-- send a file over a socke
send_file :: Socket -> String -> M ()
send_file sock filename =
  do { fd <- file_open filename;
       buf <- alloc_aligned_memory buffer_size;
       sys_catch (
         copy_data fd sock buf 0
       ) (\exception -> do {
              file_close fd;
              sys_throw exception;
            }) -- so the caller can catch it again
       ;
       file_close fd;
       }

-- ---------------------------------------------------------------------
-- Satisfy the compiler, stubs

myaccept :: Num b => Socket -> IO b
myaccept fd = do
  (sock,_sockAddr) <- accept fd
  return $ fromIntegral $ fdSocket sock

data Exception = E

file_open = undefined
alloc_aligned_memory = undefined
buffer_size = undefined
copy_data = undefined

file_close :: FD -> M ()
file_close = undefined

