module Reasoner where

import           Control.Monad (foldM, join, (<=<))
import           Data.Functor  ((<&>))
import           Data.Map      hiding (null)
import           Language
import           Z3.Monad

  {-
     Functions for reasoning classically and/or defeasibly over a DRFOL
     program. Since the Z3 reasoner has side effects, all reasoning runs 
     in the IO monad.
     -}

-- classicallyEntails returns true if the given program entails the given
-- expression classically.
classicallyEntails :: Program -> Expression -> IO Bool
classicallyEntails program expr =
  do
    res <- evalZ3 $ do
      unsort <- mkStringSymbol "drfol" >>= mkUninterpretedSort
      smap <- declareTerms unsort program
      mapM_ (assert <=< declareExpression unsort smap) $ expressions program
      assert =<< mkNot =<< declareExpression unsort smap expr
      check
    case res of
      Unsat -> return True -- prog ^ !expr is unsatisfiable iff prog => exprProgram
      _     -> return False

-- rankExpressions runs the rational closure ranking algorithm on the given
-- expressions to rank them according to typicality. The lower the rank, the
-- more typical the expression is considered with respect to the given program.
rankExpressions :: Program -> [Expression] -> IO [[Expression]]
rankExpressions prog exprs = undefined

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

-- mkTypicalityFunc is the monadic predicate used to represent typicality.
mkTypicalityFunc :: Sort -> Z3 FuncDecl
mkTypicalityFunc sort = do
  boolSort <- mkBoolSort
  funcSym <- mkStringSymbol "__typ__"
  mkFuncDecl funcSym [sort] boolSort

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
