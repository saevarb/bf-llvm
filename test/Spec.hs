
import Test.Tasty
import Test.Tasty.Hspec
import Text.Megaparsec
import Data.Either

import Parser
import TestData

parse' = parse brainfuckP "test"

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

