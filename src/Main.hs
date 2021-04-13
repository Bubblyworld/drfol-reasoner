module Main where

import           Control.Monad (foldM, (<=<))
import           Data.Functor  ((<&>))
import           Language
import           Parser
import           Reasoner
import           Z3.Monad

tweetyTest :: String
tweetyTest = "b(X)~>f(X)\n\
\b(tweety)"

workerTest :: String
workerTest = "b(X)->w(X)\n\
\b(X)/\\hs(X,Y)->!w(Y)\n\
\b(X)~>r(X)\n\
\w(X)/\\hs(X,Y)~>b(Y)"

penguinTest :: String
penguinTest = "p->b\n\
\p->!f\n\
\b~>f"

hardTest :: String
hardTest = "a(X)~>b(X)\n\
\b(X)->'F"

testEntailment :: String -> String -> IO ()
testEntailment progStr exprStr =
  do
    let parsed =
          do
            prog <- parseProgram progStr
            expr <- parseExpression exprStr
            return (prog, expr)
    case parsed of
      Left err           -> putStrLn $ "Parser error: " ++ show err
      Right (prog, expr) ->
        do
          print =<< classicallyEntails prog expr
          print =<< rankExpressions prog []

main :: IO ()
main =
  do
    testEntailment hardTest "a(X)~>'F"
    testEntailment hardTest "a(X)->'F"
