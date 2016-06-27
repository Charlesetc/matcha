import Test.Hspec
import Matcha.Parser
import Data.List (isInfixOf)

assert_parses input asserted = runner . run $ input where
  runner (Right as) = show as `shouldBe` asserted
  runner as = as `shouldBe` Right []

assert_fails input line column message = do
  let output = show . run $ input
  output `shouldSatisfy` isInfixOf message
  output `shouldSatisfy` isInfixOf ("line " ++ show line)
  output `shouldSatisfy` isInfixOf ("column " ++ show column)

main :: IO ()
main = hspec $ do

  describe "initial parser with" $ do

    it "a single symbol" $ do
      assert_parses "a" "[Symbol \"a\"]"

    describe "function definition" $ do

      it "parses most basic" $ do
        assert_parses "{:}" "[FDef [] []]"

      it "can take more than one argument" $ do
        assert_parses "{a b c: (a b c)}" "[FDef [Symbol \"a\",Symbol \"b\",Symbol \"c\"] [FApp [FApp [Symbol \"a\",Symbol \"b\",Symbol \"c\"]]]]"

      it "parses basic with newlines" $ do
        assert_parses "{\n:\n\n\n}\n\n" "[FDef [] []]"

      it "parses nested function definition" $ do
        assert_parses "{: {x:}}" "[FDef [] [FApp [FDef [Symbol \"x\"] []]]]"

      it "semicolons work fine" $ do
        assert_parses "{: a; b}" "[FDef [] [FApp [Symbol \"a\"],FApp [Symbol \"b\"]]]"

      it "does not parse mismatched brackets" $ do
        assert_fails "{" 1 2 "end of input"
        assert_fails "a\n}" 2 1 "unexpected \"}"

      it "fails on non-symbols for function arguments" $ do
        assert_fails "{ () : }" 1 3 "unexpected \"("

    describe "function application" $ do

      it "parses simple function application" $ do
        assert_parses "(a b)" "[FApp [Symbol \"a\",Symbol \"b\"]]"

      it "parses simple function application with newline" $ do
        assert_parses "(\n\na \nb)\n" "[FApp [Symbol \"a\",Symbol \"b\"]]"

      it "parses function application within function def" $ do
        assert_parses "{a: a b}" "[FDef [Symbol \"a\"] [FApp [Symbol \"a\",Symbol \"b\"]]]"

      it "does not parse mismatched parens" $ do
        assert_fails "(" 1 2 "end of input"
        assert_fails "a\n)" 2 1 "unexpected \")"
