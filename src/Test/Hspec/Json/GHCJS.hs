{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Hspec.Json.GHCJS (
  genericToJSValTests,
  genericFromJSValTests,

  -- re-export
  Proxy(..),
 ) where

import           Data.Aeson as Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.JSString
import           Data.Proxy
import           Data.String.Conversions
import           Data.Typeable
import           GHCJS.Marshal
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
        jsValAsString <- unpack <$> jsonStringify jsVal
        Aeson.encode a `shouldBeSameJson` cs jsValAsString

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
        let json :: ByteString = encode a
            expected :: Maybe a = decode json
        expected `shouldBe` Just a
        jsVal :: JSVal <- jsonParse $ pack $ cs json
        result :: Maybe a <- fromJSVal jsVal
        result `shouldBe` expected

-- * ffi json stuff

foreign import javascript unsafe
  "JSON.stringify($1)"
  jsonStringify :: JSVal -> IO JSString

foreign import javascript unsafe
  "JSON.parse($1)"
  jsonParse :: JSString -> IO JSVal
