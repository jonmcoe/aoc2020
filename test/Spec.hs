{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (traverse_)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)


main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} specs

specs :: Spec
specs = describe "intcocde verififcation" $ traverse_ test cases
  where
    test Case{..} = it description' assertion
      where
        description' = unwords [input, outputA, outputB]
        assertion   = (outputA ++ outputB) `shouldBe` concatMap ($ input) [f1, f2]  -- TODO: proper `and`-like behavior?

data Case = Case { input   :: String
                 , outputA :: String
                 , outputB :: String
                 , f1      :: String -> String
                 , f2      :: String -> String
                 }
cases :: [Case]
cases = []