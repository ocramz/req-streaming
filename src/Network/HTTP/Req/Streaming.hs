{-# language DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Network.HTTP.Req.Streaming where

{-
trying to provide similar functionality as req-conduit

https://www.stackage.org/haddock/nightly-2019-04-21/req-conduit-1.0.0/Network-HTTP-Req-Conduit.html

bracketP alloc free inside = ConduitT $ \rest -> do
  (key, seed) <- allocate alloc free
  unConduitT (inside seed) $ \res -> do
    release key
    rest res

instance MonadHttp (ConduitM i o (ResourceT IO)) where
  handleHttpException = liftIO . throwIO

main :: IO ()
main = runConduitRes $ do
  let size = 100000 :: Int
  req' GET (https "httpbin.org" /: "bytes" /~ size) NoReqBody mempty
    (\request manager ->
      bracketP (L.responseOpen request manager) L.responseClose
        responseBodySource)
    .| CB.sinkFile "my-file.bin"

-}

import Control.Monad (unless)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
-- import Control.Monad.Trans.Class (lift)
-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
-- resourcet
import Control.Monad.Trans.Resource -- (runResourceT, allocate, release)
-- req
import Network.HTTP.Req -- (req')
-- http-client
import qualified Network.HTTP.Client as L
-- bytestring
import qualified Data.ByteString     as B
import           Data.ByteString.Builder (byteString, Builder)
-- streaming
import Streaming.Prelude (effects, for, yield)
import Streaming
-- streaming-utils
import Streaming.Zip
-- streaming-bytestring
import qualified Data.ByteString.Streaming as SB




-- streamingResponseR stream status headers =
--     responseStream status headers streamingBody
--     where
--     streamingBody writeBuilder flush =
--         let writer a =
--                 do liftIO (writeBuilder (byteString a))
--                     -- flushes for every produced bytestring, perhaps not optimal
--                    liftIO flush
--          in runResourceT $ void $ effects $ for stream writer





newtype BStreamT m a =
  BStreamT { unBStream :: Stream (Of B.ByteString) (ResourceT m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadIO m => MonadResource (BStreamT m) where
  -- liftResourceT = lift

instance MonadIO m => MonadHttp (BStreamT m) where
  handleHttpException = liftIO . throwIO

  
  
-- responseBracket :: MonadIO m => L.Request -> L.Manager -> BStreamT m ()
responseBracket request mgr = 
  bracketR
    (L.responseOpen request mgr)
    L.responseClose
    (BStreamT . responseBodySource)
      

-- | Present a HTTP(S) response body as a 'Stream' of 'B.ByteString's
responseBodySource :: MonadIO m
  => L.Response L.BodyReader -- ^ Response with body reader
  -> Stream (Of B.ByteString) m () -- ^ Response body as a 'Stream'
responseBodySource = bodyReaderSource . L.responseBody

bodyReaderSource :: MonadIO m => L.BodyReader -> Stream (Of B.ByteString) m ()
bodyReaderSource br = go
  where
    go = do
      bs <- liftIO (L.brRead br)
      unless (B.null bs) $ do
        yield bs
        go

bracketR :: MonadResource m =>
            IO a         -- ^ allocate
         -> (a -> IO ()) -- ^ free
         -> (a -> m b)   -- ^ inside
         -> m b
bracketR alloc free inside = do
  (key, x) <- allocate alloc free
  y <- inside x
  release key
  pure y

-- bracketRes alloc free inside = do 
--   (key, x) <- allocate alloc free
--   y <- inside x
--   release key
--   -- free x
--   pure y


----------------------------------------------------------------------------
-- Helpers

-- | This is taken from "Network.HTTP.Client.Conduit" without modifications.

-- -- srcToPopperIO :: Source IO ByteString -> L.GivesPopper ()
-- srcToPopperIO src f = do
--   (rsrc0, ()) <- src $$+ return ()
--   irsrc <- newIORef rsrc0
--   let popper :: IO B.ByteString
--       popper = do
--         rsrc <- readIORef irsrc
--         (rsrc', mres) <- rsrc $$++ await
--         writeIORef irsrc rsrc'
--         case mres of
--           Nothing -> return B.empty
--           Just bs
--               | B.null bs -> popper
--               | otherwise -> return bs
--   f popper
