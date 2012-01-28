module Main where

import Network.Server.Concurrent
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Enumerator ((=$=))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET
import Control.Monad
import Control.Monad.Trans (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

main = do
  print "started"
  run 3570 (app)


{-
 - Simple line based echo server
 -}
app s@(E.Continue k) = do
   m <- EB.takeWhile ((/=) 10)
   _ <- EB.take 1
   if (L.null m) 
       then return s
       else do
           newStep <- lift $ E.runIteratee $ k $ E.Chunks $ L.toChunks m 
           app newStep
app e = return e
