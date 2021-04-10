module Main where

import           Parser

testParser :: String -> IO ()
testParser input = do
  putStrLn $ "Testing: " ++ input
  case parse input of
    Left err  -> putStrLn $ "Parser Error: " ++ show err
    Right cmp -> putStrLn $ "Parser Success: " ++ show cmp
  putStrLn ""

main :: IO ()
main = do
  testParser "a(X,c)"
  testParser "a(X,Y,d)"
  testParser "!a(b,c,D)"
  testParser "((a(X,Y)))"
  testParser "!a(c,D)/\\!b(X)"
  testParser "a(X,c)->b(Y,Z)"
  testParser "a(X,c)~>b(Y,Z)"
  testParser "a(X,c)~>b(y,a,g)\n\
             \b(a,b,c)->z(A,B)\n\
             \a(X,c)"
