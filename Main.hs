module Main (main) where
import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import System.Posix.Daemonize
import System.Exit
import System.IO
import Control.Monad
import Control.Concurrent (threadDelay)

data Flag
	= Daemon
	| Help
	| Config String
	| Log String
	| Debug String
	deriving (Eq, Ord, Show)

data Options = Options {
		optDaemon	:: Bool,
		optHelp		:: Bool,
		optConfig	:: String,
		optLog		:: String,
		optDebug	:: Int 
	}

options :: [OptDescr Flag]
options = [
	Option ['d'] ["daemon"]	(NoArg Daemon)			"start in daemon mode",
	Option ['h'] ["help"]	(NoArg Help)			"display this help",
	Option ['f'] ["config"] (ReqArg Config "config_file")	"set the config file to be used",
	Option ['L'] ["log"]	(ReqArg Log "log_file")		"set the default logfile for the daemon",
	Option ['N'] ["debug"]	(ReqArg Debug "dbg_lvl")	"set the verbosity level"
	]

-- parseOpts :: [String] -> IO ([Flag], [String])
parseOpts :: String -> [String] -> IO ([Flag])
parseOpts me argv = case getOpt Permute options argv of
	(opts, _, []) -> do
		if Help `elem` opts then do
			hPutStrLn stderr (usageInfo header options)
			exitWith ExitSuccess
		else
			return (opts)
	(_, _, errs) -> do
		hPutStrLn stderr (concat errs ++ usageInfo header options)
		exitWith (ExitFailure 1)
	where header = "Usage: " ++ me ++ " [-hd][-L <logfile>][-N <dbg_lvl>][-f <config_file>]"

main :: IO ()
main = do
	me <- getProgName
	args <- getArgs
	opts <- parseOpts me args

	if Daemon `elem` opts then do
		serviced hacuService
		--daemonize nfsStart
	else
		nfsStart2

hacuService :: CreateDaemon ()
hacuService = simpleDaemon { program = nfsStart }

nfsStart :: a -> IO ()
nfsStart param = do
	threadDelay $ 1000000 * 1000

nfsStart2 :: IO ()
nfsStart2 = do
	threadDelay $ 1000000 * 1000
