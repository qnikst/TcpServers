{-# LANGUAGE RankNTypes #-}
-------------------------------------------------------------------------------
--
-- Module: Network.Server.Concurrent
-- Licence: BSD3
--
-- Maintainer: 
-- Stability: experimental
-- Portability: NixOs
--
-- Skeleton for Concurrent TCP Server based on green threads.
-- This module based on Warp <url:http://hackage.haskell.org/package/warp-0.4.6.3>
-- package
--
-------------------------------------------------------------------------------

-- | light-weight TCP server
--
module Network.Server.Concurrent
    (
    -- * Datatypes
    Application,
    Request,
    Responce,
    -- * Run server
    run,
    runSettings,
    runSettingsSocket
    ) where

import Prelude hiding (catch)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Network (sClose, Socket)
import Network.Socket.Enumerator (enumSocket)
import Network.Socket 
    ( accept, Family (..)
    , SocketType (Stream), listen, bindSocket, setSocketOption, maxListenQueue
    , SockAddr, SocketOption (ReuseAddr)
    , AddrInfo(..), AddrInfoFlag(..), defaultHints, getAddrInfo
    )
import qualified Network.Socket
import qualified Network.Socket.ByteString as Sock
import Control.Exception 
    ( bracket, finally, Exception, SomeException, catch
    , fromException, bracketOnError
    )

import Data.Enumerator (($$),(>>==),($=))
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB

import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever,when)
import Control.Monad.Trans (MonadIO (..))

--import Debug.Trace

-- | Request type
type Request = ByteString

-- | Responce type
-- TODO: add Command to Server
-- smth like data Responce = Send L.ByteString | Stop | ...
type Responce = ByteString 

-- | Application type
type Application = MonadIO m => E.Enumeratee Request Responce m b

data Settings = Settings 
    { settingsHost   :: String -- ^ Host to bind to, or * for all. Default: *
    , settingsPort   :: Int    -- ^ port to listen on. Default: 3500    
    , settingsOnException :: SomeException -> IO () -- ^ Exception handler
    }

defaultSettings :: Settings
defaultSettings = Settings 
    { settingsPort = 3500
    , settingsHost = "*"
    , settingsOnException = \e -> return ()
{-        case fromException e of
            Just x -> go x
            Nothing -> 
                if go' $ fromException e
                    then hPutStrLn stderr $ show e
                    else return () -}
    }

-- | Run application on specified port
run :: Port -> Application -> IO ()
run p = runSettings defaultSettings  {settingsPort = p}

-- | Run application with specified settings
runSettings :: Settings -> Application -> IO ()
runSettings set = 
    bracket (bindPort (settingsPort set) (settingsHost set))
    sClose . 
    (flip (runSettingsSocket set))

-- | Run application with settings on created socket
runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket set socket app = do
    let onE  = settingsOnException set
        port = settingsPort set
    forever $ do
        (conn, sa) <- accept socket
        _ <- forkIO $ serveConnection set onE port app conn sa
        return ()

-- | Serve connection
serveConnection settings onException port app conn remoteHost' = do
    catch 
        (finally 
            (E.run_ $ enumSocket bytesPerRead conn $= app $$ sendResponces conn)
            (sClose conn))
        onException
    where
        sendResponces :: MonadIO m => Socket -> E.Iteratee ByteString m ()
        sendResponces socket = do
          resp <- EB.consume
          when (not $ L.null resp) $ liftIO (Sock.sendMany socket $ L.toChunks resp)
          sendResponces socket

type Port = Int

bindPort :: Int -> String -> IO Socket
--bindPort _ _ | trace ">bindPort" False = undefined 
bindPort p s = do
    let hints = defaultHints { addrFlags = [ AI_PASSIVE
                                           , AI_NUMERICSERV
                                           , AI_NUMERICHOST] 
                             , addrSocketType = Stream }
        host = if s=="*" then Nothing else Just s
        port = Just . show $ p
    addrs <- getAddrInfo (Just hints) host port
    let addrs' = filter (\x -> addrFamily x == AF_INET6) addrs
        addr = if null addrs' then head addrs else head addrs'
    bracketOnError
        (Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        (sClose)
        (\sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (addrAddress addr)
            listen sock maxListenQueue
            return sock
        )

bytesPerRead :: Integer
bytesPerRead = 4096
