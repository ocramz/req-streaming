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
-- import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
-- unliftio-core
import Control.Monad.IO.Unlift (MonadUnliftIO(..))
-- resourcet
import Control.Monad.Trans.Resource (runResourceT, ResourceT, MonadResource, allocate, release)
-- req
import Network.HTTP.Req (req')
-- http-client
import qualified Network.HTTP.Client as L
-- bytestring
import qualified Data.ByteString     as B
-- streaming
import Streaming.Prelude 



  

responseBracket request mgr =
  bracketR
    (L.responseOpen request mgr)
    L.responseClose
    -- (lift . responseBodySource)
      


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

bracketR :: MonadUnliftIO m => IO t -> (t -> IO ()) -> (t -> m a) -> m a
bracketR alloc free inside = runResourceT $ do
  (key, x) <- allocate alloc free
  y <- lift $ inside x
  release key
  pure y
