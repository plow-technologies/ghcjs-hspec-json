{-# LANGUAGE DeriveGeneric #-}

module GHCJS.JSVal.Aeson.TestsSpec where

import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics
import           GHCJS.Marshal
import           System.IO
import           System.IO.Temp
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.QuickCheck

import           GHCJS.JSVal.Aeson.Tests

hspecSilently :: Spec -> IO Summary
hspecSilently s = do
  withSystemTempFile "ghcjs-hspec-jsval-aeson" $ \ path handle -> do
    hClose handle
    let silentConfig :: Test.Hspec.Core.Runner.Config
        silentConfig = defaultConfig{
          configOutputFile = Right path
        }
    hspecWithResult silentConfig s

spec :: Spec
spec = do
  describe "genericToJSValTests" $ do
    it "detects faulty ToJSVal instances" $ do
      summary <- hspecSilently $ genericToJSValTests faultyProxy
      summary `shouldBe` Summary 1 1

data Faulty
  = Faulty {
    faultyFoo :: String,
    faultyBar :: String
  }
  deriving (Show, Generic)

faultyProxy :: Proxy Faulty
faultyProxy = Proxy

instance ToJSVal Faulty
instance ToJSON Faulty where
  toJSON = genericToJSON defaultOptions{
    fieldLabelModifier = drop (length "faulty")
  }

instance Arbitrary Faulty where
  arbitrary = Faulty <$> arbitrary <*> arbitrary

