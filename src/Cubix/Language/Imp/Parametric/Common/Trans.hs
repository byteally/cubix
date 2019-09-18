{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.Imp.Parametric.Common.Trans () where
#else
module Cubix.Language.Imp.Parametric.Common.Trans (
    translate
  , untranslate
  ) where

import Control.Monad.Identity ( Identity(..) )

import Data.Monoid ( Any(..) )
import Data.List( (\\) )
import Language.Haskell.TH.Syntax ( Type(ConT), Exp(VarE) )

import Data.Comp.Multi ( project, inject, unTerm, (:<:), (:+:), caseH, HFunctor(..) )
import Data.Comp.Multi.Strategic ( crushtdT, addFail, promoteTF )

import qualified Language.Imp as ImpOrig

import Cubix.Language.Imp.Parametric.Common.Types
import qualified Cubix.Language.Imp.Parametric.Full.Types as F
import Cubix.Language.Parametric.Derive
import Cubix.Language.Parametric.InjF
import Cubix.Language.Parametric.Syntax
import qualified Data.Text as T

translate :: F.ImpTerm l -> MImpTerm l
translate = trans . unTerm

translate' :: (InjF MImpSig l l') => F.ImpTerm l -> MImpTerm l'
translate' = injF . translate

class Trans f where
  trans :: f F.ImpTerm l -> MImpTerm l

instance {-# OVERLAPPING #-} (Trans f, Trans g) => Trans (f :+: g) where
  trans = caseH trans trans

transDefault :: (HFunctor f, f :<: MImpSig, f :<: F.ImpSig) => f F.ImpTerm l -> MImpTerm l
transDefault = inject . hfmap translate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :<: MImpSig, f :<: F.ImpSig) => Trans f where
  trans = transDefault

instance Trans F.Ident where
  trans (F.Ident n) = iIdent (T.unpack n)

untranslate :: MImpTerm l -> F.ImpTerm l
untranslate = untrans . unTerm

class Untrans f where
  untrans :: f MImpTerm l -> F.ImpTerm l

instance (Untrans f, Untrans g) => Untrans (f :+: g) where
  untrans = caseH untrans untrans

untransDefault :: (HFunctor f, f :<: F.ImpSig) => f MImpTerm l -> F.ImpTerm l
untransDefault = inject . hfmap untranslate

instance {-# OVERLAPPABLE #-} (HFunctor f, f :<: F.ImpSig) => Untrans f where
  untrans = untransDefault


instance {-# OVERLAPPING #-} Untrans IdentIsIdent where
  untrans (IdentIsIdent v) = untransIdent v
  
untransIdent :: MImpTerm IdentL -> F.ImpTerm F.IdentL
untransIdent (Ident' s) = F.iIdent (T.pack s)

{-
instance {-# OVERLAPPING #-} Untrans SingleLocalVarDeclIsDeclaration where
  untrans (SingleLocalVarDeclIsDeclaration v) = untransDecl v

untransDecl :: MImpTerm SingleLocalVarDeclL -> F.ImpTerm F.DeclarationL
untransDecl (SingleLocalVarDecl' _ bind init) =
  F.iDecl (untranslate bind) (untranslate init)
-}

-- instance {-# OVERLAPPING #-} Untrans ExprIsLocalVarInit where

untransError :: (HFunctor f, f :<: MImpSig) => f MImpTerm l -> F.ImpTerm l
untransError t = error $ "Cannot untranslate root node: " ++ (show $ (inject t :: MImpTerm _))


do ipsNames <- sumToNames ''MImpSig
   modNames <- sumToNames ''F.ImpSig
   let targTs = map ConT $ (ipsNames \\ modNames) \\ [ ''IdentIsIdent
                                                     , ''SingleLocalVarDeclIsDeclaration
                                                     , ''ExprIsLocalVarInit
                                                     ]
   return $ makeDefaultInstances targTs ''Untrans 'untrans (VarE 'untransError)

#endif

