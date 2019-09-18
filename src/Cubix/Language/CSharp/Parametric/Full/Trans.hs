{-# OPTIONS_GHC -ddump-splices -fno-code #-}

{-

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

#ifdef ONLY_ONE_LANGUAGE
module Cubix.Language.CSharp.Parametric.Full.Trans () where
#else
module Cubix.Language.CSharp.Parametric.Full.Trans (
    translate
  , untranslate
  ) where

import Data.Typeable ( Typeable )

import qualified Language.CSharp as CS
import qualified Language.Haskell.TH as TH

import Data.Comp.Multi ( caseH, (:+:) )
import Data.Comp.Trans ( runCompTrans, withSubstitutions, deriveTrans, deriveUntrans )

import Cubix.Language.CSharp.Parametric.Full.Names
import Cubix.Language.CSharp.Parametric.Full.Types
import Cubix.Language.Parametric.Syntax.Base
import Cubix.Language.Parametric.Syntax.Functor

do substs <- makeSubsts
   runCompTrans $ withSubstitutions substs $ deriveTrans origASTTypes (TH.ConT ''CSharpTerm)

translate :: CS.CompUnit -> CSharpTerm CompUnitL
translate = trans

instance (Trans c l, Typeable l) => Trans [c] [l] where
  trans [] = riNilF
  trans (x:xs) = (trans x :: CSharpTerm l) `iConsF` (trans xs)

instance (Trans c l, Typeable l) => Trans (CS.NonEmpty c) (CS.NonEmpty l) where
  trans (x `CS.NonEmpty` xs) = (trans x :: CSharpTerm l) `iNonEmptyF` (trans xs)  

instance (Trans c l, Typeable l) => Trans (Maybe c) (Maybe l) where
  trans Nothing  = riNothingF
  trans (Just x) = iJustF $ (trans x :: CSharpTerm l)

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (c, d) (l, l')  where
  trans (x, y) = riPairF (trans x) (trans y)

instance (Trans c l, Trans d l', Trans e l'', Typeable l, Typeable l', Typeable l'') => Trans (c, d, e) (l, l', l'') where
  trans (x, y, z) = riTripleF (trans x) (trans y) (trans z)

instance (Trans c l, Trans d l', Typeable l, Typeable l') => Trans (Either c d) (Either l l') where
  trans (Left x)  = riLeftF (trans x)
  trans (Right x) = riRightF (trans x)

instance Trans Char CharL where
  trans x = iCharF x

instance Trans Bool BoolL where
  trans x = iBoolF x

instance Trans Int IntL where
  trans x = iIntF x

instance Trans Integer IntegerL where
  trans x = iIntegerF x

instance Trans () () where
  trans _ = iUnitF


do substs <- makeSubsts
   runCompTrans $ withSubstitutions substs $ deriveUntrans origASTTypes (TH.ConT ''CSharpTerm)

type instance Targ [l] = [Targ l]
instance Untrans ListF where
  untrans NilF = T []
  untrans (ConsF a b) = T ((t a) : (t b))

type instance Targ (CS.NonEmpty l) = CS.NonEmpty (Targ l)
instance Untrans NonEmptyF where
  untrans (NonEmptyF a b) = T ((t a) `CS.NonEmpty` (t b))

type instance Targ (Maybe l) = Maybe (Targ l)
instance Untrans MaybeF where
  untrans NothingF = T Nothing
  untrans (JustF x) = T (Just (t x))

type instance Targ (l, l') = (Targ l, Targ l')
instance Untrans PairF where
  untrans (PairF x y) = T (t x, t y)

type instance Targ (l, l', l'') = (Targ l, Targ l', Targ l'')
instance Untrans TripleF where
  untrans (TripleF x y z) = T (t x, t y, t z)
  
type instance Targ (Either l l') = Either (Targ l) (Targ l')
instance Untrans EitherF where
  untrans (LeftF x)  = T (Left (t x))
  untrans (RightF x) = T (Right (t x))

type instance Targ BoolL = Bool
instance Untrans BoolF where
  untrans (BoolF x) = T x

type instance Targ IntL = Int
instance Untrans IntF where
  untrans (IntF x) = T x

type instance Targ IntegerL = Integer
instance Untrans IntegerF where
  untrans (IntegerF x) = T x

type instance Targ () = ()
instance Untrans UnitF where
  untrans UnitF = T ()

instance (Untrans f, Untrans g) => Untrans (f :+: g) where
  untrans = caseH untrans untrans

type instance Targ IntL = Int
type instance Targ BoolL = Bool
type instance Targ CharL = Char

instance Untrans CharF where
  untrans (CharF x) = T x

type instance Targ () = ()

#endif

-}
