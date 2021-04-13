module Parser (parseProgram, parseExpression) where

import           Data.Functor                  (($>), (<$>), (<&>))
import           Data.List.Extra               (cons)
import           Language
import           Text.Parsec.Char
import           Text.ParserCombinators.Parsec

  {-
     Parser combinators for DRFOL programs and expressions.
     -}

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
       <|> ((topbot <&> maybeApply) <*> compound')
       <|> ((negation <&> maybeApply) <*> compound')
  where
    maybeApply x = maybe x (\f -> f x)

compound' :: CharParser st (Maybe (Compound -> Compound))
compound' = (string "/\\" *> compound <&> Just . flip Conjunction)
        <|> (string "\\/" *> compound <&> Just . flip Disjunction)
        <|> return Nothing

topbot :: CharParser st Compound
topbot = string "'" *> (string "T" $> Top <|> string "F" $> Bot)

atom :: CharParser st Compound
atom = (many1 lower <&> Atom . Label) <*> (termList <|> return [])

negation :: CharParser st Compound
negation = string "!" *> compound <&> Negation

expression :: CharParser st Expression
expression = (compound <&> (\x -> maybeApply x (Fact x))) <*> expression'
  where
    maybeApply x y = maybe y (\f -> f x)

expression' :: CharParser st (Maybe (Compound -> Expression))
expression' = (string "->" *> compound <&> Just . flip ClassicalRule)
          <|> (string "~>" *> compound <&> Just . flip DefeasibleRule)
          <|> return Nothing

program :: CharParser st Program
program = many (expression <* ((eof $> ' ') <|> endOfLine)) <&> Program

parseProgram :: String -> Either ParseError Program
parseProgram = parse program "drfol"

parseExpression :: String -> Either ParseError Expression
parseExpression = parse expression "drfol"
