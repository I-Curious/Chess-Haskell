module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Directory qualified as Dir
import System.Exit qualified as Exit
import System.IO qualified as IO
import System.Process qualified as Process
import Text.Printf (printf)
import Control.Monad
import Data.Algorithm.Diff qualified as Diff
import Data.Algorithm.DiffOutput qualified as DiffOutput
import ChessGame qualified

main :: IO ()
main = tests >>= defaultMain


tests :: IO TestTree
tests =
  testGroup "Haskhell"
    <$> sequence
    [ testGroup "ChessGame"
          <$> sequence
          [
            chessPlayTests :: IO TestTree
          ]
    ]

chessPlayTests :: IO TestTree
chessPlayTests = do
  let testData =
        [ "chessA",
          "chessB",
          "chessC"
        ]
  return $
    testGroup "play" $
      genTestCases (map mkPlayTest testData)
  where
    mkPlayTest ex = do
      let inFile = "examples/Chess/" ++ ex ++ ".stdin"
      inText <- readFile inFile
      let outFile = "examples/Chess/" ++ ex ++ ".stdout"
      exptOut <- readFile outFile
      (actExit, actOut, actErr) <- do
        Process.readProcessWithExitCode
          "stack"
          ["--silent", "run", "chessgame"]
          inText
      let msgExitDiff = diffMsg "exit code" (show actExit) (show Exit.ExitSuccess)
      let msgStdoutDiff = diffMsg "stdout" actOut exptOut
      let msgStderrDiff = diffMsg "stderr" actErr ""
      let msg = concat [msgExitDiff, msgStdoutDiff, msgStderrDiff]
      unless
        (msg == "")
        ( assertFailure $
            "cmd: stack run chessgame < " ++ inFile ++ msg
        )

genTestCases :: [Assertion] -> [TestTree]
genTestCases = labelCases "test"

labelCases :: String -> [Assertion] -> [TestTree]
labelCases prefix assertions = labelCasesAtWidth (log10 (length assertions)) prefix assertions
  where
    log10 n = if n < 10 then 1 else 1 + log10 (n `div` 10)

labelCasesAtWidth :: Int -> String -> [Assertion] -> [TestTree]
labelCasesAtWidth width prefix = zipWith (testCase . printf (prefix ++ "%0" ++ show width ++ "d")) [1 :: Integer ..]


diff :: String -> String -> Maybe String
diff act expt =
  if act == expt
    then Nothing
    else
      Just
        ( init $
            unlines $
              map ("  " ++) $
                lines $
                  DiffOutput.ppDiff $
                    Diff.getGroupedDiff (lines act) (lines expt)
        )

diffMsg :: String -> String -> String -> String
diffMsg lab act expt =
  case diff act expt of
    Nothing -> ""
    Just diffStr -> "\n" ++ lab ++ " diff:\n" ++ diffStr