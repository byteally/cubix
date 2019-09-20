{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | 

module Cubix.Language.Imp.Parametric.Common.Types where

#ifndef ONLY_ONE_LANGUAGE
import Data.List ( (\\) )

import Language.Haskell.TH ( mkName )

import Data.Comp.Multi ( HFunctor, Cxt, Term, (:<:), project', project, (:&:) )
import Data.Comp.Trans ( runCompTrans, makeSumType )

import Cubix.Language.Imp.Parametric.Full.Names
import Cubix.Language.Imp.Parametric.Full.Types as Imp
import Cubix.Language.Info
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import qualified Cubix.Language.Parametric.Syntax as P

createSortInclusionTypes [ ''P.IdentL, ''P.SingleLocalVarDeclL, ''Imp.ExprL ]
                            [ ''Imp.IdentL, ''Imp.DeclarationL, ''P.LocalVarInitL ]

deriveAll [ ''IdentIsIdent
           , ''SingleLocalVarDeclIsDeclaration
           , ''ExprIsLocalVarInit
           ]

createSortInclusionInfers [ ''P.IdentL, ''P.SingleLocalVarDeclL, ''Imp.ExprL ]
                             [ ''Imp.IdentL, ''Imp.DeclarationL, ''P.LocalVarInitL]

do let impSortInjections = [ ''IdentIsIdent
                           , ''SingleLocalVarDeclIsDeclaration
                           , ''ExprIsLocalVarInit
                           ] ++
                           [ ''P.Ident
                           , ''P.SingleLocalVarDecl
                           ]
   let names = impSigNames ++ impSortInjections
   sumDec <- runCompTrans $ makeSumType "MImpSig" names
   return sumDec

type MImpTerm    = Term MImpSig
type MSigTermLab = TermLab MImpSig

type MSigCxt h a    = Cxt h MImpSig a
type MSigCxtA h a p = Cxt h (MImpSig :&: p) a

pattern ImpFunctionCall' :: () => (Expr :<: f, HFunctor f) => Cxt h f a Imp.IdentL -> Cxt h f a [ExprL] -> Cxt h f a ExprL
pattern ImpFunctionCall' b as <- (project -> Just (FunctionCall b as)) where
  ImpFunctionCall' b as = iFunctionCall b as


#endif

