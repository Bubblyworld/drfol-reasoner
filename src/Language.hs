module Language where

import           Data.List (group, sort)

-- Labels represent unique identifiers for a atom, constant or variable.
newtype Label = Label String deriving (Eq, Ord)

instance Show Label where
  show (Label s) = s

-- Terms represent first-order constants or variables.
data GenTerm l = Constant l
               | Variable l

instance (Show l) => Show (GenTerm l) where
  show (Constant l) = show l
  show (Variable l) = show l

instance Functor GenTerm where
  fmap f (Constant l) = Constant (f l)
  fmap f (Variable l) = Variable (f l)

type Term = GenTerm Label

-- Compounds represent boolean combination of atoms.
data GenCompound l t = Atom l [t]
              | Negation (GenCompound l t)
              | Conjunction (GenCompound l t) (GenCompound l t)
              | Disjunction (GenCompound l t) (GenCompound l t)

instance (Show l, Show t) => Show (GenCompound l t) where
  show (Negation c)          = "!" ++ show c
  show (Conjunction cl cr) = "(" ++ show cl ++ "/\\" ++ show cr ++ ")"
  show (Disjunction cl cr) = "(" ++ show cl ++ "\\/" ++ show cr ++ ")"
  show (Atom l ts)  = show l ++ "(" ++ joinStr ts ++ ")"
    where joinStr []     = ""
          joinStr [t]    = show t
          joinStr (t:ts) = show t ++ "," ++ joinStr ts

instance Functor (GenCompound l) where
  fmap f (Atom l ts)         = Atom l (fmap f ts)
  fmap f (Negation c)        = Negation (fmap f c)
  fmap f (Conjunction cl cr) = Conjunction (fmap f cl) (fmap f cr)
  fmap f (Disjunction cl cr) = Disjunction (fmap f cl) (fmap f cr)

instance Foldable (GenCompound l) where
  foldMap f (Atom l ts)         = mconcat $ fmap f ts
  foldMap f (Negation c)        = foldMap f c
  foldMap f (Conjunction cl cr) = mappend (foldMap f cl) (foldMap f cr)
  foldMap f (Disjunction cl cr) = mappend (foldMap f cl) (foldMap f cr)

type Compound = GenCompound Label Term

-- Expressions represent facts, rules and defeasible rules.
data (GenExpression c) = Fact c
                | ClassicalRule c c
                | DefeasibleRule c c

instance (Show c) => Show (GenExpression c) where
  show (Fact c)               = show c
  show (ClassicalRule cl cr)  = show cl ++ "->" ++ show cr
  show (DefeasibleRule cl cr) = show cl ++ "~>" ++ show cr

instance Functor GenExpression where
  fmap f (Fact c)               = Fact (f c)
  fmap f (ClassicalRule cl cr)  = ClassicalRule (f cl) (f cr)
  fmap f (DefeasibleRule cl cr) = DefeasibleRule (f cl) (f cr)

instance Foldable GenExpression where
  foldMap f (Fact c)               = f c
  foldMap f (ClassicalRule cl cr)  = mappend (f cl) (f cr)
  foldMap f (DefeasibleRule cl cr) = mappend (f cl) (f cr)

type Expression = GenExpression Compound

-- Programs represent collections of DRFOL expressions.
newtype GenProgram e = Program [e]

instance (Show e) => Show (GenProgram e) where
  show (Program [])     = ""
  show (Program (e:es)) = show e ++ "\n" ++ show (Program es)

instance Functor GenProgram where
  fmap f (Program es) = Program (fmap f es)

instance Foldable GenProgram where
  foldMap f (Program es) = foldMap f es

type Program = GenProgram Expression

-- ValidationError represents some reason a program is invalid, such as having
-- predicates with ambiguous arity, or unsafe rules (potentially).
data ValidationError = AmbiguousArity Label
                     | UnsafeRule Expression

-- util functions for working with programs
validate :: Program -> Maybe ValidationError
validate = undefined

variables :: Program -> [Label]
variables = rmdups . foldMap (foldMap $ foldMap onlyvar)
  where
    onlyvar :: Term -> [Label]
    onlyvar (Variable l) = [l]
    onlyvar (Constant l) = []

constants :: Program -> [Label]
constants = rmdups . foldMap (foldMap $ foldMap onlyconst)
  where
    onlyconst :: Term -> [Label]
    onlyconst (Variable l) = []
    onlyconst (Constant l) = [l]

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort
