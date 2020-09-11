module RowComponentsSpec
    ( spec,
    )
where

import RowComponents (projectName)
import Test.Hspec (SpecWith, it, shouldBe)

spec :: SpecWith ()
spec = it "passes" $ projectName `shouldBe` "row-components"
