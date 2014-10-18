{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module ButterCoreBencodeSpec where

import Data.Attoparsec
import Data.ByteString
import Data.Map
import Test.Hspec

import Butter.Core.Bencode

spec :: Spec
spec =
    describe "parseBEValue" $ do
      it "parses `BEInt`s" $
        parseOnly parseBEValue "i123e" `shouldBe` Right (BEInt 123)

      it "parses `BEString`s" $
        parseOnly parseBEValue "5:hello" `shouldBe` Right (BEString "hello")

      it "parses `BEList`s" $
        parseOnly parseBEValue "l5:helloi123ee"
          `shouldBe` Right (BEList [BEString "hello", BEInt 123])

      it "parses `BEDict`s" $ do
          let Right (BEDict m) = parseOnly parseBEValue "d5:helloi132ee"
          (m ! ("hello" :: ByteString)) `shouldBe` (BEInt 132)

      it "fails to parse invalid input" $
        parseOnly parseBEValue "asdfkajsdfaksdjf"
          `shouldSatisfy` \case Left _ -> True ; _ -> False
