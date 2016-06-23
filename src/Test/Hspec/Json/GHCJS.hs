{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hspec.Json.GHCJS (
  genericToJSValTests,
  genericFromJSValTests,
  genericAesonToJSVal,
  genericJSValToAeson,
  genericJSValRoundtrip,
  genericAesonRoundtrip,

  -- re-export
  Proxy(..),
 ) where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import qualified Data.Aeson as Aeson
import           Data.Aeson as Aeson hiding (encode)
import           Data.ByteString.Lazy (ByteString)
import           Data.JSString
import           Data.Proxy
import           Data.String.Conversions
import           Data.Typeable
import           GHCJS.Marshal as JSVal
import           GHCJS.Types
import           Test.Hspec
import           Test.QuickCheck

-- | Generically create an hspec 'Spec' that tests whether the 'ToJSVal'
-- and the 'ToJSON' instances for the given datatype match.
genericToJSValTests :: forall a .
  (Typeable a, Show a, Arbitrary a, ToJSON a, ToJSVal a) =>
  Proxy a -> Spec
genericToJSValTests proxy = do
  describe ("ToJSVal " ++ show (typeRep proxy)) $ do
    it "converts to JSON compatible with aeson" $ do
      property $ \ (a :: a) -> do
        jsVal <- toJSVal a
        jsValAsByteString <- jsonStringify jsVal
        Aeson.encode a `shouldBeSameJson` jsValAsByteString

shouldBeSameJson :: ByteString -> ByteString -> IO ()
shouldBeSameJson a b = do
  let dec :: ByteString -> Value
      dec s = either (\ err -> error ("invalid json: " ++ err ++ "\n" ++ cs s)) id $
        eitherDecode s
  dec a `shouldBe` dec b

genericFromJSValTests :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a, FromJSVal a) =>
  Proxy a -> Spec
genericFromJSValTests proxy = do
  describe ("FromJSVal " ++ show (typeRep proxy)) $ do
    it "converts from JSON compatible with aeson" $ do
      property $ \ (a :: a) -> do
        let json :: ByteString = Aeson.encode a
            expected :: Maybe a = decode json
        expected `shouldBe` Just a
        jsVal :: JSVal <- jsonParse json
        result :: Maybe a <- fromJSVal jsVal
        result `shouldBe` expected

-- | Returns tests that make sure that values can be serialized to JSON using
-- `aeson` and then read back into Haskell values using `fromJSVal`.
--
-- This is a common case when transferring values from a server to a client.
genericAesonToJSVal :: forall a .
  (Typeable a, Show a, Eq a, Arbitrary a, ToJSON a, FromJSVal a) =>
  Proxy a -> Spec
genericAesonToJSVal proxy = do
  describe ("JSON encoding of " ++ show (typeRep proxy)) $ do
    it "allows to encode values with aeson and decode with FromJSVal" $ do
      shouldBeIdentity proxy $
        Aeson.encode >>> jsonParse >=> fromJSValIO

-- | Returns tests that make sure that values can be serialized to JSON using
-- `toJSVal` and then read back into Haskell values using `aeson`.
--
-- This is a common case when sending values from a client to a server.
genericJSValToAeson :: forall a .
  (Typeable a, Show a, Eq a, Arbitrary a, ToJSVal a, FromJSON a) =>
  Proxy a -> Spec
genericJSValToAeson proxy = do
  describe ("JSON encoding of " ++ show (typeRep proxy)) $ do
    it "allows to encode values with ToJSVal and decode with aeson" $ do
      shouldBeIdentity proxy $
        toJSVal >=> jsonStringify >=> aesonDecodeIO

genericJSValRoundtrip :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSVal a, FromJSVal a) =>
  Proxy a -> Spec
genericJSValRoundtrip proxy = do
  describe ("JSON encoding of " ++ show (typeRep proxy)) $ do
    it "allows to encode values with JSVal and read them back" $ do
      shouldBeIdentity proxy $
        toJSVal >=> fromJSValIO

genericAesonRoundtrip :: forall a .
  (Typeable a, Eq a, Show a, Arbitrary a, ToJSON a, FromJSON a) =>
  Proxy a -> Spec
genericAesonRoundtrip proxy = do
  describe ("JSON encoding of " ++ show (typeRep proxy)) $ do
    it "allows to encode values with aeson and read them back" $ do
      shouldBeIdentity proxy $
        Aeson.encode >>> aesonDecodeIO

-- * utils

shouldBeIdentity :: (Eq a, Show a, Arbitrary a) =>
  Proxy a -> (a -> IO a) -> Property
shouldBeIdentity Proxy function =
  property $ \ (a :: a) -> do
    function a `shouldReturn` a

fromJSValIO :: forall a . Typeable a => FromJSVal a => JSVal -> IO a
fromJSValIO jsVal = fromJSVal jsVal >>= \ case
  Just r -> return r
  Nothing -> throwIO $ ErrorCall
    ("fromJSVal couldn't convert to type " ++ show (typeRep (Proxy :: Proxy a)))

aesonDecodeIO :: FromJSON a => ByteString -> IO a
aesonDecodeIO bs = case eitherDecode bs of
  Right a -> return a
  Left msg -> throwIO $ ErrorCall
    ("aeson couldn't parse value: " ++ msg)

-- * ffi json stuff

jsonStringify :: JSVal -> IO ByteString
jsonStringify jsVal = cs <$> unpack <$> json_stringify jsVal
foreign import javascript unsafe
  "JSON.stringify($1)"
  json_stringify :: JSVal -> IO JSString

jsonParse :: ByteString -> IO JSVal
jsonParse = json_parse . pack . cs
foreign import javascript unsafe
  "JSON.parse($1)"
  json_parse :: JSString -> IO JSVal
