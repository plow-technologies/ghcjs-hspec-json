{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Json.GHCJSSpec where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics
import           GHCJS.Marshal
import           System.IO
import           System.IO.Temp
import           Test.Hspec
import           Test.Hspec.Core.Runner
import           Test.QuickCheck

import           Test.Hspec.Json.GHCJS

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

    it "creates passing tests for correct serialization" $ do
      summary <- hspecSilently $ genericToJSValTests correctProxy
      summary `shouldBe` Summary 1 0

    it "creates passing tests for sum types" $ do
      summary <- hspecSilently $ genericToJSValTests correctSumProxy
      summary `shouldBe` Summary 1 0

  describe "genericFromJSValTests" $ do
    it "detects faulty FromJSVal instances" $ do
      summary <- hspecSilently $ genericFromJSValTests faultyProxy
      summary `shouldBe` Summary 1 1

    it "creates passing tests for correct serialization" $ do
      summary <- hspecSilently $ genericFromJSValTests correctProxy
      summary `shouldBe` Summary 1 0

    it "creates passing tests for sum types" $ do
      summary <- hspecSilently $ genericFromJSValTests correctSumProxy
      summary `shouldBe` Summary 1 0

data Faulty
  = Faulty {
    faultyFoo :: String,
    faultyBar :: String
  }
  deriving (Show, Eq, Generic)

faultyProxy :: Proxy Faulty
faultyProxy = Proxy

instance ToJSVal Faulty

instance ToJSON Faulty where
  toJSON = genericToJSON defaultOptions{
    fieldLabelModifier = drop (length ("faulty" :: String))
  }

instance FromJSVal Faulty

instance FromJSON Faulty where
  parseJSON = genericParseJSON defaultOptions{
    fieldLabelModifier = drop (length ("faulty" :: String))
  }

instance Arbitrary Faulty where
  arbitrary = Faulty <$> arbitrary <*> arbitrary

data Correct
  = Correct {
    correctFoo :: String,
    correctBar :: String
  }
  deriving (Show, Eq, Generic)

correctProxy :: Proxy Correct
correctProxy = Proxy

instance ToJSVal Correct

instance ToJSON Correct

instance FromJSVal Correct

instance FromJSON Correct

instance Arbitrary Correct where
  arbitrary = Correct <$> arbitrary <*> arbitrary

data CorrectSum
  = Foo {
    correctSumFoo :: String
  }
  | Bar {
    correctSumFoo :: String,
    correctSumBar :: String
  }
  deriving (Show, Eq, Generic)

correctSumProxy :: Proxy CorrectSum
correctSumProxy = Proxy

instance ToJSVal CorrectSum

instance ToJSON CorrectSum where
  toJSON = \ case
    Foo foo -> object ["Foo" .= foo]
    Bar foo bar -> object
      ["Bar" .= object ["correctSumFoo" .= foo, "correctSumBar" .= bar]]

instance FromJSVal CorrectSum

instance FromJSON CorrectSum where
  parseJSON = withObject "CorrectSum" $ \ o ->
    (Foo <$> o .: "Foo") <|>
    (o .: "Bar" >>= \ dict ->
      Bar <$> dict .: "correctSumFoo" <*> dict .: "correctSumBar")

instance Arbitrary CorrectSum where
  arbitrary = oneof $
    (Foo <$> arbitrary) :
    (Bar <$> arbitrary <*> arbitrary) :
    []
