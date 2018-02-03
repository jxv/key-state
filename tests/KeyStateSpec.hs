{-# LANGUAGE LambdaCase #-}
module KeyStateSpec where

import Test.Hspec
import KeyState

spec :: Spec
spec = do
  describe "updateKeyState" $ do
    context "Not touching" $ do
      it "Untouched key state (init)" $ do
        let ks = initKeyState :: KeyState Int
        let expected = KeyState KeyStatus'Untouched (Just 1)
        updateKeyState 1 ks False `shouldBe` expected

      it "Untouched key state" $ do
        let ks = initKeyState { ksCounter = Just 10 } :: KeyState Int
        let expected = KeyState KeyStatus'Untouched (Just 11)
        updateKeyState 1 ks False `shouldBe` expected

      it "Pressed key state (init)" $ do
        let ks = initKeyState { ksStatus = KeyStatus'Pressed } :: KeyState Int
        let expected = KeyState KeyStatus'Released Nothing
        updateKeyState 1 ks False `shouldBe` expected

      it "Pressed key state" $ do
        let ks = KeyState KeyStatus'Pressed (Just 10) :: KeyState Int
        let expected = KeyState KeyStatus'Released Nothing
        updateKeyState 1 ks False `shouldBe` expected

      it "Held key state" $ do
        let ks = KeyState KeyStatus'Held (Just 10) :: KeyState Int
        let expected = KeyState KeyStatus'Released Nothing
        updateKeyState 1 ks False `shouldBe` expected

      it "Released key state" $ do
        let ks = KeyState KeyStatus'Released (Just 10) :: KeyState Int
        let expected = KeyState KeyStatus'Untouched Nothing
        updateKeyState 1 ks False `shouldBe` expected

    context "Touching" $ do
      it "Untouched key state (init)" $ do
        let ks = initKeyState :: KeyState Int
        let expected = KeyState KeyStatus'Pressed Nothing
        updateKeyState 1 ks True `shouldBe` expected

      it "Untouched key state" $ do
        let ks = initKeyState { ksCounter = Just 10 } :: KeyState Int
        let expected = KeyState KeyStatus'Pressed Nothing
        updateKeyState 1 ks True `shouldBe` expected

      it "Pressed key state (init)" $ do
        let ks = initKeyState { ksStatus = KeyStatus'Pressed } :: KeyState Int
        let expected = KeyState KeyStatus'Held Nothing
        updateKeyState 1 ks True `shouldBe` expected

      it "Pressed key state" $ do
        let ks = KeyState KeyStatus'Pressed (Just 10) :: KeyState Int
        let expected = KeyState KeyStatus'Held Nothing
        updateKeyState 1 ks True `shouldBe` expected

      it "Held key state" $ do
        let ks = KeyState KeyStatus'Held (Just 10) :: KeyState Int
        let expected = KeyState KeyStatus'Held (Just 11)
        updateKeyState 1 ks True `shouldBe` expected

      it "Released key state" $ do
        let ks = KeyState KeyStatus'Released (Just 10) :: KeyState Int
        let expected = KeyState KeyStatus'Pressed Nothing
        updateKeyState 1 ks True `shouldBe` expected
