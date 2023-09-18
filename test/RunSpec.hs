{-# LANGUAGE OverloadedStrings #-}
module RunSpec (spec) where

import           Test.Hspec

import           Run
import qualified Format

spec :: Spec
spec = around_ (Env.without "GHC_ENVIRONMENT") $ do
  describe "parseOptions" $ do
    it "" $ do
      parseOptions ["Foo.Bar", "Baz"] `shouldReturn` defaultOptions { modules = [Module ["Foo", "Bar"], Module ["Baz"]] }
      parseOptions ["--raw"] `shouldReturn` defaultOptions { mode = RawMode }
      parseOptions ["--stdout"] `shouldReturn` defaultOptions { output = StdOut }

  describe "parseModule" $ do
    it "" $ do
      parseModule "Foo" `shouldBe` Just (Module ["Foo"])

    it "" $ do
      parseModule "Foo.Bar" `shouldBe` Just (Module ["Foo", "Bar"])

  describe "dumpModuleApi" $ do
    let
      modules = [
          Module ["Data", "Maybe"]
        , Module ["Data", "String"]
        , Module ["System", "IO"]
        ]
    modules.for_ $ \ module_ -> do
      context "with {module_}".unpack $ do
        it "dumps the API" $ do
          expected <- readFile ("api" </> module_.toString.asFilePath)
          dumpModuleApi Format.format module_ `shouldReturn` expected
