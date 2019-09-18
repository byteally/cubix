{-
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -ddump-splices #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

-- This is a separate file due to GHC's phase restriction

module Cubix.Language.CSharp.Parametric.Full.Types where

#ifndef ONLY_ONE_LANGUAGE
import qualified Language.CSharp.Syntax as CS ( CompUnit )

import Data.Comp.Multi ( Term )
import Data.Comp.Trans ( runCompTrans, withSubstitutions, deriveMultiComp, makeSumType )

import Cubix.Language.Info
import Cubix.Language.CSharp.Parametric.Full.Names
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.Syntax.Base

-----------------------------------------------------------

do substs <- makeSubsts
   runCompTrans $ withSubstitutions substs $ deriveMultiComp ''CS.CompUnit

deriveAll newASTTypes
runCompTrans $ makeSumType "CSharpSig" csharpSigNames

type CSharpTerm      = Term    CSharpSig
type CSharpTermLab l = TermLab CSharpSig l
#endif
-}
