module Reasoner where

import           Control.Monad (foldM, join, (<=<))
import           Data.Functor  ((<&>))
import           Data.Map      hiding (null)
import           Language
import           Z3.Monad

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
declareCompound sort smap (Atom (Label labelStr) terms) =
  do
    boolSort <- mkBoolSort
    funcSym <- mkStringSymbol labelStr
    funcDecl <- mkFuncDecl funcSym (sort <$ terms) boolSort
    mkApp funcDecl $ fmap ((!) smap . label) terms
declareCompound sort smap (Negation comp) =
  mkNot =<< declareCompound sort smap comp
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

-- declareExpression constructs a universally closed z3 formula for the
-- given expression using the given z3 sort. Defeasible rules are converted
-- into their materialisations, i.e. classical counterparts.
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
    apps <- mapM (toApp . (!) smap) $ exprVariables expr
    if null apps
       then mkImplies cdecll cdeclr
       else mkForallConst [] apps =<< mkImplies cdecll cdeclr

  {-
     Since running
     Z3 typically involves caching and other side effects, the reasoner runs
     in the IO monad.
     -}

-- classicallyEntails returns true if the given program entails the given
-- expression classically. Defeasible rules, both in the program and the
-- query, are first materialised.
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
--
-- An example script to feel out the Z3 bindings.
script :: Z3 String
script = do
  unSort <- mkStringSymbol "drfol" >>= mkUninterpretedSort
  boolSort <- mkBoolSort
  birdSym <- mkStringSymbol "bird"
  flySym <- mkStringSymbol "fly"
  bird <- mkFuncDecl birdSym [unSort] boolSort
  fly <- mkFuncDecl flySym [unSort] boolSort
  tweety <- mkFreshConst "Tweety" unSort
  tommie <- mkFreshConst "Tommie" unSort

  assert =<< mkApp bird [tweety]
  assert =<< mkNot =<< mkApp fly [tommie]
  assert =<< do
    x <- mkFreshVar "x" unSort
    x' <- toApp x
    birdX <- mkApp bird [x]
    flyX <- mkApp fly [x]
    rule <- mkImplies birdX flyX
    mkForallConst [] [x'] rule

  model <- snd <$> getModel
  case model of
    Just m -> modelToString m
    _      -> return "No model found."

