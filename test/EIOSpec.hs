module EIOSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "EIO tests" $ do
    it "basic check" $
      pendingWith "not implemented yet"