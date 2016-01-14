#!/usr/bin/env runhaskell
-- Run this script from the root of the exercism checkout!
module Main where
import           Control.Exception  (bracket, finally)
import           Control.Monad      (filterM, (>=>))
import           Data.List          (find, intercalate, isPrefixOf, isSuffixOf)
import           Data.Maybe         (catMaybes, fromJust)
import           System.Directory   (copyFile, getCurrentDirectory,
                                     getDirectoryContents,
                                     getTemporaryDirectory,
                                     removeDirectoryRecursive, removeFile,
                                     setCurrentDirectory)
import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..), exitFailure)
import           System.FilePath    ((</>))
import           System.Posix.Files (getFileStatus, isDirectory)
import           System.Posix.Temp  (mkdtemp)
import           System.Process     (rawSystem)


getSolution :: [FilePath] -> IO FilePath
getSolution = return . fromJust . find isSolution
  where
    isSolution :: FilePath -> Bool
    isSolution p = ".hs" `isSuffixOf` p && (not . isSuffixOf "_test.hs") p

withTemporaryDirectory_ :: FilePath -> IO a -> IO a
withTemporaryDirectory_ fp f =
     do sysTmpDir <- getTemporaryDirectory
        curDir <- getCurrentDirectory
        bracket (mkdtemp (sysTmpDir </> fp))
                (\path -> setCurrentDirectory curDir >>
                          removeDirectoryRecursive path)
                (\path -> setCurrentDirectory path >> f)

assignmentsDir :: FilePath
assignmentsDir = "."

parseModule :: [String] -> String
parseModule = (!!1) . words . head . filter (isPrefixOf "module ")

testAssignment :: FilePath -> FilePath -> IO (Maybe String)
testAssignment dir fn = do
  let d = dir </> fn
      -- example = d </> "example.hs"
      testFile = d </> (fn ++ "_test.hs")
      opts = ["-Wall", {--"-Werror",--} testFile]
  solution <- getDirectoryContents d >>= (getSolution >=> pure . (d </>))
  putStrLn $ "-- " ++ fn
  modFile <- (++ ".hs") . parseModule . lines <$> readFile solution
  copyFile solution modFile
  exitCode <- finally (rawSystem "runhaskell" opts) (removeFile modFile)
  return $ case exitCode of
    ExitSuccess -> Nothing
    _           -> Just fn

getAssignments :: FilePath -> [FilePath] -> IO [FilePath]
getAssignments dir = filterM isAssignmentDir
  where
    isAssignmentDir path = case path of
      '.':_  -> return False
      '_':_  -> return False
      "bin"  -> return False
      "docs" -> return False
      _      -> isDirectory <$> getFileStatus (dir </> path)

main :: IO ()
main = do
  dir  <- (</> assignmentsDir) <$> getCurrentDirectory
  dirs <- getArgs >>= \args -> case args of
    [] -> getDirectoryContents dir
    _  -> return args
  withTemporaryDirectory_ "exercism-haskell" $ do
    failures <- catMaybes <$>
      (getAssignments dir dirs >>= mapM (testAssignment dir))
    case failures of
      [] -> putStrLn "SUCCESS!"
      xs -> putStrLn ("Failures: " ++ intercalate ", " xs) >> exitFailure
