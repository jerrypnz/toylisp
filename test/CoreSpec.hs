module CoreSpec (spec) where

import Core
import Test.Hspec

spec :: Spec
spec = do

  describe "Environment" $ do

    it "sets parent to Nothing for empty env" $ do
      parent emptyEnv `shouldBe` Nothing

    let currentEnv = emptyEnv
    let newEnv = pushEnv currentEnv [(Sym "foo", AtomValue $ String "123")]

    it "sets parent to current env when pushing new env" $ do
      parent newEnv `shouldBe` Just currentEnv

    it "gets back parent env when poping old env" $ do
      popEnv newEnv `shouldBe` currentEnv

  describe "Lookup Stuff in Environment" $ do

    let testVal1 = AtomValue $ String "123"
    let testVal2 = AtomValue $ Int 12
    let testVal3 = AtomValue Nil
    let testEnv = pushEnv emptyEnv  [(Sym "foo", testVal1),
                                     (Sym "bar", testVal2),
                                     (Sym "foobar", testVal3)]

    it "returns correct value from env" $ do
      lookupEnv testEnv (Sym "foo")    `shouldBe` testVal1
      lookupEnv testEnv (Sym "bar")    `shouldBe` testVal2
      lookupEnv testEnv (Sym "foobar") `shouldBe` testVal3
