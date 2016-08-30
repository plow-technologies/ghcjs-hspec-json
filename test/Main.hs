module Main where

import           Test.Hspec
import qualified Test.Hspec.Json.GHCJSSpec as G

main :: IO ()
main = do
  hspec G.spec
