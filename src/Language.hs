module Language where

import           Data.List         (group, sort)
import           Data.List.Ordered (subset)
import           Data.Map          hiding (filter)

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
data GenCompound l t = Top | Bot
                     | Atom l [t]
                     | Negation (GenCompound l t)
                     | Conjunction (GenCompound l t) (GenCompound l t)
                     | Disjunction (GenCompound l t) (GenCompound l t)

instance (Show l, Show t) => Show (GenCompound l t) where
  show Top = "'T"
  show Bot = "'F"
  show (Negation c) = "!" ++ show c
  show (Conjunction cl cr) = "(" ++ show cl ++ "/\\" ++ show cr ++ ")"
  show (Disjunction cl cr) = "(" ++ show cl ++ "\\/" ++ show cr ++ ")"
  show (Atom l ts) = show l ++ "(" ++ joinStr ts ++ ")"
    where joinStr []     = ""
          joinStr [t]    = show t
          joinStr (t:ts) = show t ++ "," ++ joinStr ts

instance Functor (GenCompound l) where
  fmap f Top                 = Top
  fmap f Bot                 = Bot
  fmap f (Atom l ts)         = Atom l (fmap f ts)
  fmap f (Negation c)        = Negation (fmap f c)
  fmap f (Conjunction cl cr) = Conjunction (fmap f cl) (fmap f cr)
  fmap f (Disjunction cl cr) = Disjunction (fmap f cl) (fmap f cr)

instance Foldable (GenCompound l) where
  foldMap f Top                 = mempty
  foldMap f Bot                 = mempty
  foldMap f (Atom l ts)         = mconcat $ fmap f ts
  foldMap f (Negation c)        = foldMap f c
  foldMap f (Conjunction cl cr) = mappend (foldMap f cl) (foldMap f cr)
  foldMap f (Disjunction cl cr) = mappend (foldMap f cl) (foldMap f cr)

type Compound = GenCompound Label Term

-- Expressions represent facts, rules and defeasible rules. Logically,
data GenExpression c = Fact c
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
                       deriving (Show)

  {-
     Program validation - DRFOL programs are not allowed to define predicates
     that have ambiguous arities, and all classical or defeasible rules are
     required to be safe. This last requirement is to ensure that satisfaction
     for materialised knowledge bases is decidable (TODO: check this).
     -}
validate :: Program -> Maybe ValidationError
validate p =
  case validateArities p >> validateSafety p of
    Left err -> Just err
    _        -> Nothing

validateArities :: Program -> Either ValidationError ()
validateArities p = mapM_ (checkArity $ predicates p) . compounds $ p
  where
    checkArity m (Atom l ts) = if length (m ! l) == length ts
                                  then Right ()
                                  else Left $ AmbiguousArity l
    checkArity m _ = Right ()

validateSafety :: Program -> Either ValidationError ()
validateSafety = mapM_ checkSafety . expressions
  where
    checkSafety (Fact _)                   = Right ()
    checkSafety e @ (ClassicalRule cl cr)  = _checkSafety e cl cr
    checkSafety e @ (DefeasibleRule cl cr) = _checkSafety e cl cr
    _checkSafety e cl cr = if foldMap (wrap . label) cr `subset` foldMap (wrap .label) cl
                            then Right ()
                            else Left $ UnsafeRule e

  {-
     Utilities for manipulating and extracting data from programs.
     -}

-- materialise returns the materialisation of the given program, in which
-- all defeasible rules A(x) ~> B(y) are replaced with their classical
-- counterparts A(X) -> B(y).
materialise :: Program -> Program
materialise =
  let _materialise (DefeasibleRule cl cr) = ClassicalRule cl cr
      _materialise e                      = e
   in fmap _materialise

-- expressions returns the list of expressions in the given program.
expressions :: Program -> [Expression]
expressions (Program es) = es

-- compounds returns the list of compounds used in the given program.
compounds :: Program -> [Compound]
compounds = foldMap $ foldMap wrap

-- terms returns the list of terms used in the given program.
terms :: Program -> [Term]
terms = foldMap $ foldMap (foldMap wrap)

-- label returns the label of the given term.
label :: Term -> Label
label (Variable l) = l
label (Constant l) = l

-- exprVariables returns the list of variables used in the given expression.
exprVariables :: Expression -> [Label]
exprVariables = rmdups . foldMap compVariables

-- compVariables returns the list of variables used in the given compound.
compVariables :: Compound -> [Label]
compVariables =
  let onlyvars t =
        case t of
          Variable l -> True
          _          -> False
   in rmdups . fmap label . filter onlyvars . foldMap wrap

-- variables returns the list of variables used in the given program.
variables :: Program -> [Label]
variables  =
  let onlyvars t =
        case t of
          Variable l -> True
          _          -> False
  in rmdups . fmap label . filter onlyvars . terms

-- constants returns the list of constant labels used in the given program.
constants :: Program -> [Label]
constants =
  let onlyconsts t =
        case t of
          Constant l -> True
          _          -> False
  in rmdups . fmap label . filter onlyconsts . terms

-- predicates returns a map from predicate labels to arities for the given program.
predicates :: Program -> Map Label [Term]
predicates =
  let mapAtoms c =
        case c of
          Atom l ts -> singleton l ts
          _         -> empty
  in foldMap mapAtoms . compounds

  {-
     Generic utilities for generic types.
     -}

-- rmdups sorts and removes all duplicate values from the given list.
rmdups :: (Ord a) => [a] -> [a]
rmdups = fmap head . group . sort

-- wrap creates a singleton list out of the given value.
wrap :: a -> [a]
wrap x = [x]
