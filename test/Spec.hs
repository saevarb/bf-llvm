
import Test.Tasty
import Test.Tasty.Hspec
import Text.Megaparsec
import Text.Megaparsec.Error
import Data.Either
import Data.Bifunctor

import Parser
import TestData

parse' = first parseErrorPretty . parse brainfuckP "test"

main :: IO ()
main = do
    specs <- testSpec "Parser tests" parserTests
    let tests = testGroup "All tests" [specs]
    defaultMain tests

parserTests =
    describe "parser" $ do
       it "can parse hello world without comments" $
           parse' helloWorldNoComments `shouldSatisfy` isRight
       it "can parse hello world with comments" $
           parse' helloWorldComments `shouldSatisfy` isRight
       it "fails on an empty loop construct" $
           parse' "[]" `shouldSatisfy` isLeft
       it "fails when a [ is missing" $
           parse' "[+++]]" `shouldSatisfy` isLeft
       it "fails when a ] is missing" $
           parse' "[[+++]" `shouldSatisfy` isLeft

