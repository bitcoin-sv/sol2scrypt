import Transpiler
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 (pack)
import Json
import System.FilePath (replaceExtension, takeBaseName)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsString)
import Utils
import Data.Bifunctor
import Data.ByteString.Lazy.Char8 (singleton)

main :: IO ()
main = defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = testGroup "golden tests" <$> sequence [solTests]

solTests :: IO TestTree
solTests = testGroup "sol" <$> sequence [makeTests "pass" ".scrypt", makeTests "fail" ".json"]

makeTests :: String -> String -> IO TestTree
makeTests groupName extension = do
  srcFiles <- findByExtension [".sol"] $ "test/golden/" ++ groupName
  return $
    testGroup
      groupName
      $ [ goldenVsString testName goldenFile $ runOutputTranspileTest srcFile
          | srcFile <- srcFiles,
            let testName = takeBaseName srcFile,
            let goldenFile = replaceExtension srcFile extension
        ]


-- run transpile tests on file
runOutputTranspileTest :: FilePath -> IO BSL.ByteString
runOutputTranspileTest srcFile = do
  result <- transpileFile srcFile
  case transpileLogs result of
    [] -> return $ pack (scryptCode result)
    logs -> return $ pack $ jsonPrettyPrint logs