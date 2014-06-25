module Svc where

import Network			(PortID(PortNumber), withSocketsDo, listenOn, accept)
import Network.Socket
import Control.Concurrent	(forkIO)
import Control.Applicative	((<$>))
import Control.Exception	(bracket)

data Protocols = NFS | MNT | NLM | RQUOTA deriving (Eq, Ord, Enum, Show, Bounded)

data ServiceTransport = ServiceTransport { fd :: Socket } 
data Rendezvous = Rendezvous { sendSize :: Int, recvSize :: Int, maxrec :: Int }

initSocket :: a -> SocketType -> b -> IO Socket
initSocket param sock_type proto = do
	sock <- socket AF_INET sock_type 0
	setSocketOption sock ReuseAddr 1
	return (sock)

initSvc :: a -> IO ()
initSvc param = withSocketsDo $ do
	putStrLn "Initialize Service"
	let ps = [(minBound :: Protocols) ..]
	-- tcp_socks <- sequence $ fmap (\x-> socket AF_INET Stream 0) ps
	-- udp_socks <- sequence $ fmap (\x-> socket AF_INET Datagram 0) ps
	tcp_socks <- sequence $ fmap (initSocket param Stream) ps
	udp_socks <- sequence $ fmap (initSocket param Datagram) ps
	print $length tcp_socks
	return ()


createEventChain :: IO ()
createEventChain = do
	return ()

registerTransport :: ServiceTransport -> IO ()
registerTransport xprt = do
	createEventChain

createTCPSvc :: Socket -> IO ServiceTransport
createTCPSvc sock = do
	-- Find shared FD state
	let svc = ServiceTransport {fd = sock}
	return (svc)

lookupRequest :: Channel -> IO Request

rpcDispatcher :: Channel -> IO ()
rpcDispatcher chan = do
	req <- lookupRequest chan

