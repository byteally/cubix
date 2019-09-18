{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.Imp.Parametric.Full.Names (
    origASTTypes
  , newASTTypes
  , impSigNames
  , makeSubsts
  ) where

import           Data.Map ( Map )
import qualified Data.Map as Map

import           Language.Haskell.TH hiding ( Name )
import qualified Language.Haskell.TH as TH
import           Language.Imp ( CompUnit )

import           Data.Comp.Trans ( runCompTrans, generateNameLists, getTypeParamVars )

import           Cubix.Language.Parametric.Syntax.Base
import           Cubix.Language.Parametric.Syntax.Functor

runCompTrans $ generateNameLists ''CompUnit


impSigNames :: [TH.Name]
impSigNames = newASTTypes ++ [''PairF, ''TripleF, ''ListF, ''MaybeF, ''EitherF, ''BoolF, ''IntF, ''IntegerF, ''UnitF, ''CharF]

makeSubsts :: Q (Map TH.Name Type)
makeSubsts = do
  vars <- runCompTrans $ getTypeParamVars origASTTypes
  let substs = Map.fromList (zip vars (repeat $ TupleT 0))
  pure substs
  
