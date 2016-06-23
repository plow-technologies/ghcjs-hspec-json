{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Hspec.Json.GHCJSSpec where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics
import           GHCJS.Marshal
import           GHCJS.Types
import           JavaScript.Object as JSO
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

shouldTestAs :: Spec -> Summary -> IO ()
shouldTestAs spec expected = do
  summary <- hspecSilently spec
  summary `shouldBe` expected

spec :: Spec
spec = do
  describe "genericToJSValTests" $ do
    it "detects faulty ToJSVal instances" $ do
      genericToJSValTests faultyProxy `shouldTestAs` Summary 1 1

    it "creates passing tests for correct serialization" $ do
      genericToJSValTests correctProxy `shouldTestAs` Summary 1 0

    it "creates passing tests for sum types" $ do
      genericToJSValTests correctSumProxy `shouldTestAs` Summary 1 0

  describe "genericFromJSValTests" $ do
    it "detects faulty FromJSVal instances" $ do
      genericFromJSValTests faultyProxy `shouldTestAs` Summary 1 1

    it "creates passing tests for correct serialization" $ do
      genericFromJSValTests correctProxy `shouldTestAs` Summary 1 0

    it "creates passing tests for sum types" $ do
      genericFromJSValTests correctSumProxy `shouldTestAs` Summary 1 0

  describe "genericAesonToJSVal" $ do
    it "detects incompatible json encodings" $ do
      genericAesonToJSVal faultyProxy `shouldTestAs` Summary 1 1

    context "when used with compatible encodings" $ do
      it "creates passing tests" $ do
        genericAesonToJSVal correctProxy `shouldTestAs` Summary 1 0

      it "creates passing tests for sum types" $ do
        genericAesonToJSVal correctSumProxy `shouldTestAs` Summary 1 0

  describe "genericJSValToAeson" $ do
    it "detects incompatible json encodings" $ do
      genericJSValToAeson faultyProxy `shouldTestAs` Summary 1 1

    context "when used with compatible encodings" $ do
      it "creates passing tests" $ do
        genericJSValToAeson correctProxy `shouldTestAs` Summary 1 0

      it "creates passing tests for sum types" $ do
        genericJSValToAeson correctSumProxy `shouldTestAs` Summary 1 0

  describe "genericJSValRoundtrip" $ do
    it "detects incompatible json encodings" $ do
      genericJSValRoundtrip faultyRoundtripProxy `shouldTestAs` Summary 1 1

    context "when used with compatible encodings" $ do
      it "creates passing tests" $ do
        genericJSValRoundtrip correctProxy `shouldTestAs` Summary 1 0

      it "creates passing tests for sum types" $ do
        genericJSValRoundtrip correctSumProxy `shouldTestAs` Summary 1 0

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

-- | Type where roundtrips don't work.
data FaultyRoundtrip
  = FaultyRoundtrip {
    faultyRoundtripFoo :: String,
    faultyRoundtripBar :: Int
  }
  deriving (Show, Eq, Generic)

faultyRoundtripProxy :: Proxy FaultyRoundtrip
faultyRoundtripProxy = Proxy

instance ToJSVal FaultyRoundtrip where
  toJSVal x = do
    new <- JSO.create
    foo <- toJSVal $ faultyRoundtripFoo x
    JSO.setProp "foo" foo new
    bar <- toJSVal $ faultyRoundtripBar x
    JSO.setProp "bar" bar new
    return $ jsval new

instance FromJSVal FaultyRoundtrip

instance Arbitrary FaultyRoundtrip where
  arbitrary = FaultyRoundtrip <$> arbitrary <*> arbitrary

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
