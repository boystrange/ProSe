-- This file is part of ProSe
--
-- ProSe is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- ProSe is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with ProSe. If not, see <http://www.gnu.org/licenses/>.
--
-- Copyright 2024 Luca Padovani

-- |This module parses the command-line arguments and invokes the type checker.
module Main (main) where

import qualified Instrumenter
import qualified Resolver
import qualified Checker
import qualified Solver
import qualified InformationFlow
import Atoms
import Measure
import Strategy
import Render
import Exceptions (MyException)
import Parser (parseProcess)
import Process
import Type
import System.Console.GetOpt
import System.IO (stdout, stderr, hFlush, hPutStrLn)
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.Environment (getProgName, getArgs)
import Control.Monad (forM_, unless, when)
import Control.Exception (catch)
import qualified Data.Version
import Data.Time (getCurrentTime, diffUTCTime)
import System.FilePath.Posix (takeFileName)

import qualified Data.Set as Set
import qualified Data.Map as Map

-- |Version of the program.
version :: Data.Version.Version
version = Data.Version.makeVersion [1, 0]

-- |Entry point.
main :: IO ()
main = do
  progName <- getProgName
  (args, file) <- getArgs >>= parse progName
  source <- if file == "-" then getContents else readFile file
  case parseProcess file source of
    Left msg -> printWarning msg
    Right (tdefs, pdefs) ->
      let pdefs' = Resolver.resolve tdefs pdefs in
        catch (check file args pdefs') (handler args)
  where
    check :: FilePath -> [Flag] -> [ProcessDefS] -> IO ()
    check file args pdefs0 = do
      let verbose = Verbose `elem` args
      let logging = Logging `elem` args
      let manualI = ManualI `elem` args
      let strat = if OnlyCall `elem` args
                  then onlyCallStrategy
                  else if FreePut `elem` args
                       then freePutStrategy
                       else defaultStrategy
      when logging
        (do putStr $ takeFileName file ++ " ... "
            hFlush stdout)
      start <- getCurrentTime
      let pdefs = if manualI then pdefs0 else Instrumenter.instrument pdefs0
      let (cs, pdefs') = Checker.checkTypes strat pdefs
      forM_ pdefs' printProcessDec
      when verbose (forM_ cs (\c -> putStrLn $ "  " ++ show c))
      when True
        (do let μs = Set.toList (mv cs)
            case Solver.solve μs cs of
              Nothing -> printNO "fair termination checker"
              Just μmap -> do printSolution μmap
                              putStrLn ""
                              printOK (Just "fair termination checker")
        )
      unless (InformationFlow.mergeable pdefs') (printNO "information flow analysis")
      stop <- getCurrentTime
      when logging $ printOK (Just (show (diffUTCTime stop start)))
      -- case Solver.solve pdefs cs of
      --   Nothing -> printNO "fair termination checker failed"
      --   Just pdefs' -> forM_ pdefs' printProcessDec

    handler :: [Flag] -> MyException -> IO ()
    handler _ e = printNO (show e)

-- |Representation of supported flags.
data Flag = Verbose  -- -v --verbose
          | Version  -- -V --version
          | Logging  --    --log
          | FreePut  -- -p --free-put
          | OnlyCall -- -c --only-call
          | ManualI  -- -i --disable-instrumentation
          | Help     --    --help
            deriving (Eq, Ord)

-- |List of supported flags.
flags :: [OptDescr Flag]
flags =
   [ Option []  ["log"]                     (NoArg Logging)  "Log type checking time"
   , Option "v" ["verbose"]                 (NoArg Verbose)  "Print type checking and running activities"
   , Option "V" ["version"]                 (NoArg Version)  "Print version information"
   , Option "h" ["help"]                    (NoArg Help)     "Print this help message"
   , Option "p" ["free-put"]                (NoArg FreePut)  "Put operations cost nothing"
   , Option "c" ["only-call"]               (NoArg OnlyCall) "Only process invocations are measured"
   , Option "i" ["disable-instrumentation"] (NoArg ManualI)  "Disable automatic instrumentation"]

-- |The information displayed when the verbose option is specified.
versionInfo :: String -> String
versionInfo progName =
  "LInFA " ++ Data.Version.showVersion version ++ " Copyright © 2024 Luca Padovani\n"
  ++ "License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>.\n"
  ++ "This is free software: you are free to change and redistribute it.\n"
  ++ "There is NO WARRANTY, to the extent permitted by law."

-- |Parse command-line arguments.
parse :: String -> [String] -> IO ([Flag], String)
parse progName argv =
  case getOpt Permute flags argv of
    (args, files, []) -> do
      when (Version `elem` args)
        (do hPutStrLn stderr (versionInfo progName)
            exitWith ExitSuccess)
      when (null files || length files > 1 || Help `elem` args)
        (do hPutStrLn stderr (usageInfo header flags)
            exitWith ExitSuccess)
      return (args, head files)
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
  where
    header = "Usage: " ++ progName ++ " [options] [FILE]"
