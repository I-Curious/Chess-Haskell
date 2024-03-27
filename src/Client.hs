import Control.Concurrent        (forkIO, threadDelay)
import Control.Monad             (forever)
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)

runUDPServer :: IO ()
runUDPServer = do 
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  print "UDP server is waiting..."
  recv sock 4096 >>= \message -> print ("UDP server received: " ++ C.unpack message)
  print "UDP server socket is closing now."
  close sock

sendMessage :: String -> IO ()
sendMessage s = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  connect sock (addrAddress serveraddr)
  sendAll sock $ C.pack s
  close sock

runUDPServerForever :: IO ()
runUDPServerForever = do
  addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "7000")
  let serveraddr = head addrinfos 
  sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
  bind sock (addrAddress serveraddr)
  forever (do recv sock 4096 >>= print)