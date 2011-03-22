module Server 
    (serveGame,
     plainHandler) where

import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO

type HandlerFunc = SockAddr -> String -> IO ()

serveGame :: String 
          -> HandlerFunc
          -> IO ()

serveGame port handlerfunc = withSocketsDo $ 
   do
      addrinfos <- getAddrInfo
                (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
		Nothing (Just port)
      let serveraddr = head addrinfos
 
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol

      bindSocket sock (addrAddress serveraddr) 

      -- Start listening for requests, max of 8 connections
      listen sock 8

      lock <- newMVar ()

      -- Loop Forever (Ctrl-C aborts)
      procRequests lock sock

   where
      -- Handle incoming connection requests
      procRequests :: MVar () -> Socket -> IO ()
      procRequests lock mastersock = 
          do (connsock, clientaddr) <- accept mastersock
             handle lock clientaddr
                "client connect"
             forkIO $ procMessages lock connsock clientaddr
             procRequests lock mastersock 

      -- Handle incoming connection messages
      procMessages :: MVar () -> Socket -> SockAddr -> IO ()
      procMessages lock connsock clientaddr =
          do connhandle <- socketToHandle connsock ReadMode
             hSetBuffering connhandle LineBuffering 
             messages <- hGetContents connhandle
             mapM_ (handle lock clientaddr) (lines messages)
             hClose connhandle
             handle lock clientaddr
                "client disconnected"

      -- Lock handler before passing data
      handle :: MVar () -> HandlerFunc
      handle lock clientaddr msg = 
          withMVar lock
             (\a -> handlerfunc clientaddr msg >> return a)

-- print incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = 
   putStrLn $ "From " ++ show addr ++ ": " ++ msg

