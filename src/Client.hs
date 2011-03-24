import Data.Bits
import Network.Socket
import Network.BSD
import Data.List
import System.IO

data GameClientHandle =
   GameClientHandle {gcHandle :: Handle,
                     gcProgram :: String}

opengame :: HostName               -- ^ Remote host
         -> String                 -- ^ Port number
         -> String                 -- ^ Client Name
         -> IO GameClientHandle    -- ^ Game handle to use
opengame hostname port progname =
   withSocketsDo $ do 
      addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
      let serveraddr = head addrinfos
 
      sock <- socket (addrFamily serveraddr) Stream defaultProtocol
      setSocketOption sock KeepAlive 1
      connect sock (addrAddress serveraddr)

      h <- socketToHandle sock WriteMode

      hSetBuffering h (BlockBuffering Nothing)
      
      return $ GameClientHandle h progname

sayPlayer :: GameClientHandle -> String -> IO ()
sayPlayer gameclienth msg = 
   do hPutStrLn (gcHandle gameclienth) msg
      hFlush (gcHandle gameclienth)
