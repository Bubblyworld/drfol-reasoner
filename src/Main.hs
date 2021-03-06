module Main where

import           Control.Monad (foldM, (<=<))
import           Data.Functor  ((<&>))
import           Language
import           Parser
import           Reasoner
import           Z3.Monad

tweetyTest :: String
tweetyTest = "b(X)~>f(X)\n\
\b(tweety)\n\
\!f(tweety)"

workerTest :: String
workerTest = "b(X)->w(X)\n\
\b(X)/\\hs(X,Y)->!w(Y)\n\
\b(X)~>r(X)\n\
\w(X)/\\hs(X,Y)~>b(Y)"

penguinTest :: String
penguinTest = "p(X) -> b(X)\n\
\p(X) ~> !f(X)\n\
\b(X) ~> f(X)"

hardTest :: String
hardTest = "a(X)~>b(X)\n\
\b(X)->'F"

removeSpaces :: String -> String
removeSpaces = filter (/= ' ')

testEntailment :: String -> String -> IO ()
testEntailment progStr exprStr =
  do
    let parsed =
          do
            prog <- parseProgram  $ removeSpaces progStr
            expr <- parseExpression $ removeSpaces exprStr
            return (prog, expr)
    case parsed of
      Left err           -> putStrLn $ "Parser error: " ++ show err
      Right (prog, expr) ->
        do
          putStrLn "{"
          putStrLn "  Program:"
          putStrLn progStr
          putStrLn "  Expression:"
          putStrLn exprStr

          entailed <- rationallyEntails prog expr
          putStrLn $ "\n  In Rational Closure: " ++ show entailed
          putStrLn "}"

main :: IO ()
main =
  do
    testEntailment penguinTest "b(X) ~> !p(X)"
    --testEntailment tweetyTest "b(X)~>'F"
