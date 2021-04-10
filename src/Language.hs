module Language where

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

-- Expression represents a fact, rule or defeasible rule.
data Expression = Fact Compound
                | ClassicalRule Compound Compound
                | DefeasibleRule Compound Compound
                  deriving Eq

instance Show Expression where
  show (Fact arg)                 = show arg
  show (ClassicalRule argl argr)  = show argl ++ "->" ++ show argr
  show (DefeasibleRule argl argr) = show argl ++ "~>" ++ show argr

-- Program represents a collection of DRFOL expressions.
newtype Program = Program [Expression] deriving Eq

instance Show Program where
  show (Program []) = ""
  show (Program (x:xs)) = show x ++ "\n" ++ show (Program xs)
