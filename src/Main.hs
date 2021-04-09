module Main where

import           Data.Functor                  ((<$>), (<&>))
import           Data.List.Extra               (cons, snoc)
import           Text.ParserCombinators.Parsec

-- Label represents a unique identifier for a atom, constant or variable.
newtype Label = Label String deriving Eq

instance Show Label where
  show (Label s) = s

-- Term represents a first-order constant or variable.
data Term = Constant Label
          | Variable Label
            deriving Eq

instance Show Term where
  show (Constant label) = show label
  show (Variable label) = show label

-- Compound represents a boolean combination of atoms.
data Compound = Atom Label [Term]
              | Conjunction Compound Compound
              | Disjunction Compound Compound
              | Negation Compound
                deriving Eq

instance Show Compound where
  show (Negation arg)          = "!" ++ show arg
  show (Conjunction argl argr) = "(" ++ show argl ++ "/\\" ++ show argr ++ ")"
  show (Disjunction argl argr) = "(" ++ show argl ++ "\\/" ++ show argr ++ ")"
  show (Atom label args)  = show label ++ "(" ++ joinStr args ++ ")"
    where joinStr [x]    = show x
          joinStr (x:xs) = show x ++ "," ++ joinStr xs

-- Parsec combinators for DRFOL expressions.
term :: CharParser st Term
term = constant <|> variable

constant :: CharParser st Term
constant = many1 lower <&> Constant . Label

variable :: CharParser st Term
variable = many1 upper <&> Variable . Label

termList :: CharParser st [Term]
termList = (string "(" >> term <&> cons) <*> termListTail <* string ")"

termListTail :: CharParser st [Term]
termListTail = many $ string "," *> term

compound :: CharParser st Compound
compound = string "(" *> compound <* string ")"
       <|> ((atom <&> maybeApply) <*> compound')
       <|> ((negation <&> maybeApply) <*> compound')
    where
       maybeApply x mf = case mf of
          Just f  -> f x
          Nothing -> x

compound' :: CharParser st (Maybe (Compound -> Compound))
compound' = (string "/\\" *> compound <&> Just . flip Conjunction)
        <|> (string "\\/" *> compound <&> Just . flip Disjunction)
        <|> return Nothing

atom :: CharParser st Compound
atom = (many1 lower <&> Atom . Label) <*> termList

negation :: CharParser st Compound
negation = string "!" *> compound <&> Negation

testParser :: String -> IO ()
testParser input = do
  putStrLn $ "Testing: " ++ input
  let res = parse compound "drfol" input
  case res of
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
