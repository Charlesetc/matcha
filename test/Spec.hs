import Test.Hspec
import Matcha.Parser

assert_parses input asserted = runner . run $ input where
  runner (Right as) = show as `shouldBe` asserted
  runner as = as `shouldBe` Right []

main :: IO ()
main = hspec $ do

  describe "initial parser" $ do

    it "parses a single symbol correctly" $ do
      assert_parses "a" "[Symbol \"a\"]"
