module Main where

import           Hisp.Prelude
import           Hisp
import           Test.QuickCheck.Gen
import           Test.Validity
main :: IO ()
main =
  do
    runOutputUnitTests
    runParserUnitTests
    putStrLn . fromMaybe "All tests passed." . prettyValidation =<< generate (validate <$> (genValid @LT))
