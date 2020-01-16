-----------------------------------------------------------------------------
-- |
-- Module      :  Network.FastCGI
-- Copyright   :  (c) Bjorn Bringert 2004-2005, (c) Lemmih 2006
-- License     :  BSD-style (see the file libraries/network/LICENSE)
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (uses FFI)
--
-- Interface for FastCGI <http://fastcgi.com/>, using the fcgiapp API.
--
-----------------------------------------------------------------------------
module Network.FastCGI
    (
    -- * Single-threaded interface
      runFastCGIorCGI
    , runOneFastCGIorCGI
    , runFastCGI
    , runOneFastCGI
    -- * Concurrent interface
    , runFastCGIConcurrent
    , runFastCGIConcurrent'
    -- * Re-export
    , module Network.CGI
    ) where

import Control.Concurrent ( forkOS )
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad    ( liftM )
import Data.Word (Word8)
import Foreign          ( Ptr, castPtr, nullPtr, peekArray0
                        , alloca, mallocBytes, free, throwIfNeg_)
import Foreign.C        ( CInt(..), CString, CStringLen
                         , peekCString )
import Foreign.Storable ( Storable (..) )
import System.IO.Unsafe (unsafeInterleaveIO,unsafePerformIO)

import Network.CGI
import Network.CGI.Monad (runCGIT)
import Network.CGI.Protocol (runCGIEnvFPS)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as Lazy
#if __GLASGOW_HASKELL__ >= 608
import qualified Data.ByteString.Internal as BSB
import qualified Data.ByteString.Unsafe   as BSB
#else
import qualified Data.ByteString.Base as BSB
#endif

-- For debugging
import Control.Concurrent ( myThreadId )
import Prelude hiding     ( log, catch )
import System.IO          ( hPutStrLn, stderr )

#include <fcgiapp.h>

------------------------------------------------------------------------

data FCGX_Stream
type StreamPtr = Ptr FCGX_Stream
type Environ = Ptr CString

------------------------------------------------------------------------

foreign import ccall unsafe "fcgiapp.h FCGX_IsCGI" fcgx_isCGI
    :: IO CInt

foreign import ccall unsafe "fcgiapp.h FCGX_GetStr" fcgx_getStr
    :: CString -> CInt -> StreamPtr -> IO CInt

foreign import ccall unsafe "fcgiapp.h FCGX_PutStr" fcgx_putStr
    :: CString -> CInt -> StreamPtr -> IO CInt

foreign import ccall safe "fcgiapp.h FCGX_Accept" fcgx_accept
    :: Ptr StreamPtr -> Ptr StreamPtr -> Ptr StreamPtr -> Ptr Environ -> IO CInt
foreign import ccall unsafe "fcgiapp.h FCGX_Finish" fcgx_finish
    :: IO ()

------------------------------------------------------------------------

-- | Handle a single CGI request, or FastCGI requests in an infinite loop.
--   This function only returns normally if it was a CGI request.
--   This lets you use the same program
--   as either a FastCGI or CGI program, depending on what the server
--   treats it as.
runFastCGIorCGI :: CGI CGIResult -> IO ()
runFastCGIorCGI f = do fcgi <- runOneFastCGIorCGI f
                       if fcgi then runFastCGIorCGI f
                               else return ()

-- | Handle a single FastCGI or CGI request. This lets you use the same program
--   as either a FastCGI or CGI program, depending on what the server
--   treats it as.
runOneFastCGIorCGI :: CGI CGIResult
                   -> IO Bool -- ^ True if it was a FastCGI request,
                              --   False if CGI.
runOneFastCGIorCGI f =
    do x <- fcgx_isCGI
       if x /= 0 then runCGI f >> return False
                 else runOneFastCGI f >> return True

-- | Handle FastCGI requests in an infinite loop.
runFastCGI :: CGI CGIResult -> IO ()
runFastCGI f = runOneFastCGI f >> runFastCGI f

-- | Handle a single FastCGI request.
runOneFastCGI :: CGI CGIResult -> IO ()
runOneFastCGI f = do
    alloca (\inp ->
            alloca (\outp ->
                    alloca (\errp ->
                            alloca (\envp ->
                                    oneRequest f inp outp errp envp))))

oneRequest :: CGI CGIResult
           -> Ptr StreamPtr
           -> Ptr StreamPtr
           -> Ptr StreamPtr
           -> Ptr Environ
           -> IO ()
oneRequest f inp outp errp envp =
    do
    testReturn "FCGX_Accept" $ fcgx_accept inp outp errp envp
    ins  <- peek inp
    outs <- peek outp
    errs <- peek errp
    env  <- peek envp
    handleRequest f ins outs errs env
    fcgx_finish

handleRequest :: CGI CGIResult
              -> StreamPtr
              -> StreamPtr
              -> StreamPtr
              -> Environ
              -> IO ()
handleRequest f ins outs _errs env =
    do
    vars <- environToTable env
    input <- sRead ins
    output' <- runCGIEnvFPS vars input (runCGIT f)
    sPutStr outs output'



data FCGX_Request

foreign import ccall unsafe "fcgiapp.h FCGX_Init" fcgx_init
    :: IO CInt

foreign import ccall unsafe "fcgiapp.h FCGX_InitRequest" fcgx_initrequest
    :: Ptr FCGX_Request -> CInt -> CInt -> IO CInt

foreign import ccall safe "fcgiapp.h FCGX_Accept_r" fcgx_accept_r
    :: Ptr FCGX_Request -> IO CInt

foreign import ccall unsafe "fcgiapp.h FCGX_Finish_r" fcgx_finish_r
    :: Ptr FCGX_Request -> IO ()

-- | Like 'Network.CGI.runCGI', but uses the FastCGI interface
--   and forks off a new thread (using 'forkOS') for every request.
runFastCGIConcurrent :: Int -- ^ Max number of concurrent threads.
                     -> CGI CGIResult -> IO ()
runFastCGIConcurrent = runFastCGIConcurrent' forkOS

runFastCGIConcurrent' :: (IO () -> IO a) -- ^ How to fork a request.
                      -> Int             -- ^ Max number of concurrent threads.
                      -> CGI CGIResult -> IO ()

runFastCGIConcurrent' fork m f
    = do qsem <- newQSem m
         testReturn "FCGX_Init" $ fcgx_init
         let loop = do waitQSem qsem
                       reqp <- acceptRequest
                       _ <- fork (oneRequestMT f reqp
                             `finally`
                            (finishRequest reqp >> signalQSem qsem))
                       loop
         loop `catch` \(e::IOException) -> log (show e)

oneRequestMT :: CGI CGIResult -> Ptr FCGX_Request -> IO ()
oneRequestMT f r = do
     env    <- peekEnvp r
     vars   <- environToTable env
     ins    <- peekIn r
     input  <- sRead ins
     output' <- runCGIEnvFPS vars input (runCGIT f)
     outs   <- peekOut r
     sPutStr outs output'
--
-- * FCGX_Reqest struct
--

acceptRequest :: IO (Ptr FCGX_Request)
acceptRequest = do
    reqp <- mallocBytes (#size FCGX_Request)
    initAndAccept reqp
    return reqp
  where initAndAccept reqp = do
          testReturn "FCGX_InitRequest" $ fcgx_initrequest reqp 0 0
          testReturn "FCGX_Accept_r" $ fcgx_accept_r reqp

finishRequest :: Ptr FCGX_Request -> IO ()
finishRequest reqp = do
                     fcgx_finish_r reqp
                     free reqp

peekIn, peekOut, _peekErr :: Ptr FCGX_Request -> IO (Ptr FCGX_Stream)
peekIn  = (#peek FCGX_Request, in)
peekOut = (#peek FCGX_Request, out)
_peekErr = (#peek FCGX_Request, err)

peekEnvp :: Ptr FCGX_Request -> IO Environ
peekEnvp = (#peek FCGX_Request, envp)


--
-- * Stream IO
--

sPutStr :: StreamPtr -> Lazy.ByteString -> IO ()
sPutStr h str =
  mapM_ (flip BSB.unsafeUseAsCStringLen (fcgxPutCStringLen h))
        (Lazy.toChunks str)
  `catch` \(_ :: IOException) -> return ()

fcgxPutCStringLen :: StreamPtr -> CStringLen -> IO ()
fcgxPutCStringLen h (cs,len) =
    testReturn "FCGX_PutStr" $ fcgx_putStr cs (fromIntegral len) h

sRead :: StreamPtr -> IO Lazy.ByteString
sRead h = buildByteString (fcgxGetBuf h) 4096

fcgxGetBuf :: StreamPtr -> Ptr a -> Int -> IO Int
fcgxGetBuf h p c =
    liftM fromIntegral $ fcgx_getStr (castPtr p) (fromIntegral c) h

--
-- * ByteString utilities
--

-- | Data.ByteString.Lazy.hGetContentsN generalized to arbitrary
--   reading functions.
buildByteString :: (Ptr Word8 -> Int -> IO Int) -> Int -> IO Lazy.ByteString
buildByteString f k = lazyRead >>= return . Lazy.fromChunks
  where
    lazyRead = unsafeInterleaveIO $ do
        ps <- BSB.createAndTrim k $ \p -> f p k
        case BS.length ps of
            0         -> return []
            n | n < k -> return [ps]
            _         -> do pss <- lazyRead
                            return (ps : pss)

--
-- * Utilities
--

testReturn :: String -> IO CInt -> IO ()
testReturn e = throwIfNeg_ (\n -> e ++ " failed with error code: "++ show n)

environToTable :: Environ -> IO [(String,String)]
environToTable arr =
    do css <- peekArray0 nullPtr arr
       ss <- mapM peekCString css
       return $ map (splitBy '=') ss

-- | Split a list at the first occurence of a marker.
--   Do not include the marker in any of the resulting lists.
--   If the marker does not occur in the list, the entire
--   input with be in the first list.
splitBy :: Eq a => a -> [a] -> ([a],[a])
splitBy x xs = (y, drop 1 z)
    where (y,z) = break (==x) xs

--
-- * Debugging
--

{-# NOINLINE logMutex #-}
logMutex :: MVar ()
logMutex = unsafePerformIO (newMVar ())

log :: String -> IO ()
log msg = do
          t <- myThreadId
          withMVar logMutex (const $ hPutStrLn stderr (show t ++ ": " ++ msg))
