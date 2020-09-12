{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module RowComponentsSpec
  ( spec,
  )
where

import Data.Row.Records
  ( (.!),
    (.+),
    (.==),
    Rec,
    type (.+),
    type (.==),
  )
import RowComponents (noDeps, withDep, withDeps, withRenamedDeps)
import System.Random (randomRIO)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = it "passes" $ system `seq` (1 :: Int) `shouldBe` 1

system :: _
system =
  #foo .== (makeFoo & noDeps)
    .+ #bar .== (makeBar & withDep @"foo")
    .+ #qwe
      .== ( makeQwe
              & withDep @"foo"
              & withDep @"bar"
          )
    .+ #asd .== (makeAsd & withDeps @'["foo", "bar", "qwe"])
    .+ #app .== (makeApp & withRenamedDeps @("first" .== "foo" .+ "second" .== "asd"))

type IFoo f = "getFoo" .== f Int

type Foo = Rec (IFoo IO)

makeFoo :: IO Foo
makeFoo = pure $ #getFoo .== getFoo
  where
    getFoo = randomRIO (1, 100)

type IBar f = "getBarS" .== f String .+ "getBarI" .== f Int

type Bar = Rec (IBar IO)

makeBar :: Rec ("foo" .== Foo) -> IO Bar
makeBar deps =
  pure $
    #getBarI .== getBarI
      .+ #getBarS .== pure "bar"
  where
    getBarI = ((3 *) <$> deps .! #foo .! #getFoo)

type IQwe f = "getQwe" .== f Int

type Qwe = Rec (IQwe IO)

makeQwe ::
  Rec ("foo" .== Foo .+ "bar" .== Bar) -> IO Qwe
makeQwe deps = pure $ #getQwe .== getQwe
  where
    getQwe = (*) <$> deps .! #foo .! #getFoo <*> deps .! #bar .! #getBarI

-- makeQwe deps =

type IAsd f = "getAsd" .== f String

type Asd = Rec (IAsd IO)

makeAsd ::
  Rec ("foo" .== Foo .+ "bar" .== Bar .+ "qwe" .== Qwe) -> IO Asd
makeAsd deps = pure $ #getAsd .== getAsd
  where
    getAsd = do
      foo <- deps .! #foo .! #getFoo
      barI <- deps .! #bar .! #getBarI
      barS <- deps .! #bar .! #getBarS
      qwe <- deps .! #qwe .! #getQwe
      pure $ show foo <> show barI <> barS <> show qwe

type IApp f = "getApp" .== f [String]

type App = Rec (IApp IO)

makeApp ::
  Rec ("first" .== Foo .+ "second" .== Asd) -> IO App
makeApp deps = pure $ #getApp .== getApp
  where
    getApp =
      sequence
        [ show <$> deps .! #first .! #getFoo,
          deps .! #second .! #getAsd
        ]
