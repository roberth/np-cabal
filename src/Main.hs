module Main where
import System.Environment
import System.Process
import System.Exit
import Options.Applicative
import Options.Applicative.Common
import Options.Applicative.Help.Core as C
import Options.Applicative.Help.Types as C
import Options.Applicative.Help.Chunk(extractChunk)
import Control.Monad
import System.IO(stdout,stderr,hPutStr,writeFile)
import Data.Monoid
import System.Directory
import System.FilePath
import Data.List

data Command
  = Hello
  | System String

opts :: Parser (IO ())
opts = subparser
       ( command "build"
                  (info (pure doBuild <* many (strArgument mempty))
                    (progDesc "Build the project artifacts")
                  )
      <> command "cabal2nix"
                  (info (pure doCabal2nix <* many (strArgument mempty))
                    (progDesc "Run cabal2nix")
                  )
      <> command "version"
                  (info (pure doPrintVersions)
                    (progDesc "Print version numbers of tools")
                  )
      <> command "run"
                  (info (pure doRun <*> many (strArgument mempty))
                    (progDesc "Build and run the only executable")
                  )
      <> command "help"
                  (info (pure doHelp <* many (strArgument mempty))
                    (progDesc "Show this help message")
                  )
       )

optparsePrefs = defaultPrefs
                { prefShowHelpOnError = True
                }

doPrintVersions :: IO ()
doPrintVersions = do
  -- TODO: version of np-cabal
  callProcess "nix-build" ["--version"]
  -- TODO: more versions

doBuild :: IO ()
doBuild = do
  outputsDir <- makeAbsolute ".nix-project/cache/outputs"
  derivationsDir <- makeAbsolute ".nix-project/cache/derivations"
  
  forM_ [ outputsDir, derivationsDir ] $ createDirectoryIfMissing True
  
  r <- spawnProcess "nix-build" [ "--out-link"
                           , outputsDir </> "result"
                           , "--drv-link"
                           , derivationsDir </> "result"
                           ] >>= waitForProcess
  case r of ExitFailure _ -> exitWith r
            _ -> mempty

-- TODO arguments
-- TODO select program
doRun :: [String] -> IO ()
doRun args = do
  doBuild
  outputsDir <- makeAbsolute ".nix-project/cache/outputs"
  let binDir = outputsDir </> "result/bin"
  binFiles <- map (binDir </>) <$> listDirectory binDir
  exeFiles <- filterM (\x -> executable <$> getPermissions x) binFiles
  executablePath <- case exeFiles of
    [] -> fail "Build did not produce an executable to run."
    [theOne] -> return theOne
    x -> fail "Build produced more than one executable. Can't choose."

  spawnProcess executablePath args >>= waitForProcess >>= exitWith

-- TODO arguments
-- TODO select program
doCabal2nix :: IO ()
doCabal2nix = do
  doBuild
  -- outputsDir <- makeAbsolute ".nix-project/cache/outputs"
  -- let binDir = outputsDir </> "result/bin"
  cabalFiles <- filter (".cabal" `isSuffixOf`) <$> listDirectory "."
  cabalFile <- case cabalFiles of
    [] -> fail "Could not find a cabal file."
    [theOne] -> return theOne
    x -> fail "Directory contains multiple cabal files. Can't choose."

  cabal2nix <- nixBuildSingle ["-A", "cabal2nix", "<nixpkgs>"] -- TODO which package set?

  let cabal2nixExe = cabal2nix </> "bin" </> "cabal2nix"
  putStrLn cabal2nixExe

  (exitCode, out, err) <- readProcessWithExitCode cabal2nixExe ["."] ""
  hPutStr stderr err
  if exitCode /= ExitSuccess
    then exitWith exitCode
    else return ()
  writeFile (dropExtension cabalFile <.> ".nix") out

nixBuildSingle :: [String] -> IO String
nixBuildSingle = nixBuild >=> makeSingle
  where makeSingle [x] = return x
        makeSingle [] = fail "No build result"
        makeSingle _ = fail "Expected only one build artifact"

nixBuild :: [String] -> IO [String]
nixBuild args = do
  (exitCode, out, err) <- readProcessWithExitCode "nix-build" ("--no-build-output" : args) ""
  hPutStr stderr err
  if exitCode /= ExitSuccess
    then exitWith exitCode
    else return ()
  return (words out)

doHelp :: IO ()
doHelp = do
  putStrLn $ C.renderHelp 80 $ C.parserHelp optparsePrefs opts

main :: IO ()
main = withProgName "np" $ do
  args <- getArgs
  case args of
    _ -> join $ customExecParser optparsePrefs (info opts mempty)
