module Reasoner where

import           Control.Monad (foldM, join, (<=<))
import           Data.Functor  ((<&>))
import           Data.Map      hiding (filter, null)
import           Language
import           Z3.Monad

-- Ranks of compounds are either integers, representing the level of typicality
-- that the compound has with respect to some program, or infinity, which
-- represents an impossibly atypical compound (i.e. hard constraint).
data Rank = Rank Int | Infinity deriving (Eq)

instance Show Rank where
  show (Rank i) = show i
  show Infinity = "∞"

instance Ord Rank where
  Rank i1 <= Rank i2 = i1 <= i2
  _ <= Infinity      = True

  {-
     Functions for reasoning classically and/or defeasibly over a DRFOL
     program. Since the Z3 reasoner has side effects, all reasoning runs
     in the IO monad.
     -}

-- classicallyEntails returns true if the given program entails the given
-- expression classically.
classicallyEntails :: Program -> Expression -> IO Bool
classicallyEntails prog expr =
  do
    res <- evalZ3 $ do
      unsort <- mkStringSymbol "drfol" >>= mkUninterpretedSort
      smap <- declareTerms unsort prog
      mapM_ (assert <=< declareExpression unsort smap) $ expressions prog
      assert =<< mkNot =<< declareExpression unsort smap expr
      check
    case res of
      Unsat -> return True -- prog ^ !expr is unsatisfiable iff prog => exprProgram
      _     -> return False

-- rationallyEntails returns true if the rational closure of the given
-- program contains the given expression.
rationallyEntails :: Program -> Expression -> IO Bool
rationallyEntails prog expr@(Fact _) = classicallyEntails prog expr
rationallyEntails prog expr@(ClassicalRule _ _) = classicallyEntails prog expr
rationallyEntails prog (DefeasibleRule compl compr) =
  do
    let cpos = Conjunction compl compr
        cneg = Conjunction compl (Negation compr)
    ranks <- rankExpressions prog [cpos, cneg]
    return $ (ranks ! cpos == Infinity) || (ranks ! cpos) < (ranks ! cneg)

-- rankExpressions runs the rational closure ranking algorithm on the given
-- compounds to rank them according to typicality. The lower the rank, the
-- more typical it is considered to be with respect to the given program. The
-- result contains rankings for compounds that occur in the program as well.
rankExpressions :: Program -> [Compound] -> IO (Map Compound Rank)
rankExpressions = rankExpressions' 0

rankExpressions' :: Int -> Program -> [Compound] -> IO (Map Compound Rank)
rankExpressions' _ (Program []) [] = return empty
rankExpressions' i prog comps =
  do
    -- ensure all compounds in the program are in the compounds list.
    let comps' = rmdups $ comps ++ compounds prog

    exceptions <- mapExceptional prog comps'
    let typicalComps = filter (not . (!) exceptions) comps'
    let exceptionalComps = filter (exceptions !) comps'
    let exceptionalExprs =
          let takeExpr (Fact _)                = True
              takeExpr (ClassicalRule _ _)     = True
              takeExpr (DefeasibleRule comp _) = exceptions ! comp
           in filter takeExpr $ expressions prog

    base <- foldM (\m c -> return $ insert c (Rank i) m) empty typicalComps
    if null typicalComps || null exceptionalComps
       then do
         rest <- foldM (\m c -> return $ insert c Infinity m) empty exceptionalComps
         return $ union base rest
       else do
         rest <- rankExpressions' (i + 1) (Program exceptionalExprs) exceptionalComps
         return $ union base rest

-- mapExceptional returns a map describing which of the given compounds
-- are exceptional (i.e. have no inhabitants) with respect to the given
-- program.
mapExceptional :: Program -> [Compound] -> IO (Map Compound Bool)
mapExceptional prog =
  let insertComp res comp = isExceptional prog comp <&> flip (insert comp) res
   in foldM insertComp empty

-- isExceptional returns true if the given compound is exceptional with
-- respect to the given program, i.e. if the program entails that the
-- compound has no typical instances.
isExceptional :: Program -> Compound -> IO Bool
isExceptional prog comp = classicallyEntails prog $ DefeasibleRule comp Bot

  {-
     Utilities for converting DRFOL programs into Z3 theories.
     Examples usings the Haskell Z3 bindings can be found here:
       https://github.com/IagoAbal/haskell-z3/tree/master/examples/Example/Monad
     -}

-- declareTerms declares fresh constant and variable synbols for all of the
-- program's terms using the given z3 sort.
declareTerms :: Sort -> Program -> Z3 (Map Label AST)
declareTerms sort prog = do
  consts <- declareConstants sort prog
  vars <- declareVariables sort prog
  return $ union consts vars

-- declareConstants declares fresh constant symbols for the given program.
declareConstants :: Sort -> Program -> Z3 (Map Label AST)
declareConstants sort =
  let appendConst res label@(Label labelStr) =
        do
          newConst <- mkFreshConst labelStr sort
          return $ insert label newConst res
   in foldM appendConst empty . constants

-- declareVariables declares fresh variable symbols for the given program.
declareVariables :: Sort -> Program -> Z3 (Map Label AST)
declareVariables sort =
  let appendVar res label@(Label labelStr) =
        do
          newVar <- mkFreshVar labelStr sort
          return $ insert label newVar res
   in foldM appendVar empty . variables

-- declareCompound constructs a non-universally closed z3 formula for the given
-- compound using the given z3 sort.
declareCompound :: Sort -> Map Label AST -> Compound -> Z3 AST
declareCompound _ _ Top = mkTrue
declareCompound _ _ Bot = mkFalse
declareCompound sort smap (Atom (Label labelStr) terms) =
  do
    boolSort <- mkBoolSort
    funcSym <- mkStringSymbol labelStr
    funcDecl <- mkFuncDecl funcSym (sort <$ terms) boolSort
    mkApp funcDecl $ fmap ((!) smap . label) terms
declareCompound sort smap (Conjunction compl compr) =
  do
    astl <- declareCompound sort smap compl
    astr <- declareCompound sort smap compr
    mkAnd [astl, astr]
declareCompound sort smap (Disjunction compl compr) =
  do
    astl <- declareCompound sort smap compl
    astr <- declareCompound sort smap compr
    mkOr [astl, astr]
declareCompound sort smap (Negation comp) =
  mkNot =<< declareCompound sort smap comp

-- declareTypicality constructs a formula declaring that the variables in
-- the given compound are typical. This is represented by a conjunction of
-- monadic typicality atoms, one for each variable.
declareTypicality :: Sort -> Map Label AST -> Expression -> Z3 AST
declareTypicality sort smap expr =
  do
    typFn <- mkTypicalityFunc sort
    typTerms <- let vars = (!) smap <$> exprVariables expr
                 in mapM (mkApp typFn . wrap) vars
    if null typTerms
       then mkTrue
       else mkAnd typTerms

-- declareExpression constructs a universally closed z3 formula for the
-- given expression using the given z3 sort.
declareExpression :: Sort -> Map Label AST -> Expression -> Z3 AST
declareExpression sort smap (Fact comp)               =
  do
    cdecl <- declareCompound sort smap comp
    apps <- mapM (toApp . (!) smap) $ compVariables comp
    if null apps
       then return cdecl
       else mkForallConst [] apps cdecl
declareExpression sort smap expr@(ClassicalRule compl compr)  =
  do
    cdecll <- declareCompound sort smap compl
    cdeclr <- declareCompound sort smap compr
    apps <- mapM (toApp . (!) smap) $ exprVariables expr
    if null apps
       then mkImplies cdecll cdeclr
       else mkForallConst [] apps =<< mkImplies cdecll cdeclr
declareExpression sort smap expr@(DefeasibleRule compl compr) =
  do
    cdecll <- declareCompound sort smap compl
    cdeclr <- declareCompound sort smap compr
    tdecl <- declareTypicality sort smap expr
    apps <- mapM (toApp . (!) smap) $ exprVariables expr
    if null apps
       then mkImplies cdecll cdeclr
       else mkForallConst [] apps =<< flip mkImplies cdeclr =<< mkAnd [tdecl, cdecll]

-- mkTypicalityFunc is the monadic predicate used to represent typicality.
mkTypicalityFunc :: Sort -> Z3 FuncDecl
mkTypicalityFunc sort = do
  boolSort <- mkBoolSort
  funcSym <- mkStringSymbol "__typ__"
  mkFuncDecl funcSym [sort] boolSort
