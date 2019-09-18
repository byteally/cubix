{-# OPTIONS_GHC -ddump-splices #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Cubix.Language.Imp.Parametric.Full.Types where

import qualified Language.Imp.Syntax as Imp ( CompUnit )

import Data.Comp.Multi ( Term )
import Data.Comp.Trans ( runCompTrans, withSubstitutions, deriveMultiComp, makeSumType )

import Cubix.Language.Info
import Cubix.Language.Imp.Parametric.Full.Names
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.Base

do substs <- makeSubsts
   runCompTrans $ withSubstitutions substs $ deriveMultiComp ''Imp.CompUnit

deriveAll newASTTypes
runCompTrans $ makeSumType "ImpSig" impSigNames

type ImpTerm      = Term    ImpSig
type ImpTermLab l = TermLab ImpSig l
