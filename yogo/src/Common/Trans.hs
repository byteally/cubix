{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Common.Trans where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Control.Lens ( makeLenses, over, use, (.=), (%=), (+=), (-=) )
import Control.Monad ( MonadPlus, mzero, liftM )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Control.Monad.Identity ( Identity(..), runIdentity )
import Control.Monad.State ( MonadState, execStateT, evalStateT, runStateT, modify, get )
import Data.Comp.Multi
import Data.Comp.Multi.Strategic
import Data.List ( intercalate )
import Data.Map ( Map )
import qualified Data.Map as Map

import Cubix.Language.Info
import qualified Cubix.Language.Parametric.Syntax as Cx

-- Types in Yogo
data PrimitiveT
data AddressT
data ValueT
data MemoryT
data MemValT
data FArgT

-- Syntax types. Do not correspond to any node
data OpT -- Operators
data StatementT
data ScopeT -- functions, modules

type Depth = Int

data Primitive = IntegerF Integer | IntF Int | BoolF Bool | StringF String

data CommonOp' = And | Or | Not
               | Exponent | Plus | Minus | Multiply
               | LessThan | GreaterThan | Equals | GreaterThanEquals | LessThanEquals | NotEquals

-- Engine nodes. Provided by Yogo
data AnyNode (e :: * -> *) t
data AnyLValue (e :: * -> *) t
data AnyMem (e :: * -> *) t

data Q (e :: * -> *) t where
  Q :: e AddressT -> e MemoryT -> Q e ValueT

-- Non-nodes

data CommonOp (e :: * -> *) t where
  CommonOp :: CommonOp' -> CommonOp e OpT

-- Generic nodes
data AnyStack (e :: * -> *) t
data AnyHeap (e :: * -> *) t

data MemGenesisF (e :: * -> *) t where
  MemGenesisF :: MemGenesisF e MemoryT

data MemF e t where
  MemF :: e MemValT -> MemF e MemoryT

data ValF e t where
  ValF :: e MemValT -> ValF e ValueT

data UnknownF e t where
  UnknownF :: e MemoryT -> UnknownF e MemoryT

data NothingF (e :: * -> *) t where
  NothingF :: NothingF e t

data TempF (e :: * -> *) t where
  TempF :: Int -> Depth -> TempF e t

data ConstF (e :: * -> *) t where
  ConstF :: Primitive -> ConstF e ValueT

data IdentF (e :: * -> *) t where
  IdentF :: String -> IdentF e AddressT

data SelF e t where
  SelF :: e ValueT -> e ValueT -> SelF e AddressT

data AtF e t where
  AtF :: e ValueT -> AtF e AddressT

-- (deref mem ref) = (q mem (at mem ref))
-- This is a lazy version so that we don't over-generate at-nodes, which is an lvalue
data DerefF e t where
  DerefF :: e ValueT -> e MemoryT -> DerefF e ValueT

-- Pure operations, don't deref their arguments and don't depend on memory
data UnopF e t where
  UnopF :: e OpT -> e ValueT -> UnopF e ValueT

data BinopF e t where
  BinopF :: e OpT -> e ValueT -> e ValueT -> BinopF e ValueT

-- Arguments by order of execution
data AssignF e t where
  AssignF :: e ValueT -> e AddressT -> e MemoryT -> AssignF e MemValT

-- Function call implicitly deref their arguments
data FunctionCallF e t where
  FunctionCallF :: e ValueT -> e [FArgT] -> e MemoryT -> FunctionCallF e MemValT

data FunctionArgsF e t where
  FunctionArgsF :: e ValueT -> e [FArgT] -> FunctionArgsF e [FArgT]

data CondF e t where
  CondF :: e ValueT -> e t -> e t -> CondF e t

data CondMemF e t where
  CondMemF :: e ValueT -> e MemoryT -> e MemoryT -> CondMemF e MemoryT

data LoopF e t where
  -- depth, init, next
  LoopF :: Depth -> e ValueT -> e ValueT -> LoopF e ValueT

data LoopMemF e t where
  -- depth, init, next
  LoopMemF :: Depth -> e MemoryT -> e MemoryT -> LoopMemF e MemoryT

data FinalF e t where
  -- depth, cond, loop
  FinalF :: Depth -> e ValueT -> e ValueT -> FinalF e ValueT

data FinalMemF e t where
  -- depth, cond, loop
  FinalMemF :: Depth -> e ValueT -> e MemoryT -> FinalMemF e MemoryT

data IterVF e t where
  IterVF :: Depth -> e ValueT -> IterVF e ValueT

data IterPF e t where
  IterPF :: Depth -> e ValueT -> IterPF e ValueT

type YGenericSig = IterPF :+: IterVF :+: FinalMemF :+: FinalF :+: LoopMemF :+: LoopF :+: CondMemF :+: CondF :+: FunctionArgsF :+: FunctionCallF :+: AssignF :+: UnopF :+: BinopF :+: DerefF :+: AtF :+: SelF :+: IdentF :+: ConstF :+: TempF :+: NothingF :+: UnknownF :+: ValF :+: MemF :+: MemGenesisF :+: AnyHeap :+: AnyStack :+: CommonOp :+: Q :+: AnyMem :+: AnyLValue :+: AnyNode

newtype Name = Name [Char] deriving (Eq, Show, Ord)
newtype Occurrence = Occurrence [Label] deriving (Eq, Show)

data ID (f :: (* -> *) -> * -> *) t where
  ID :: Int -> ID f t
  IDs :: [(Label, ID f t)] -> ID f [t]
  Statement :: ID f StatementT
  Scope :: ID f ScopeT

instance Show (ID f t) where
  show (ID n) = "(ID " ++ show n ++ ")"
  show (IDs ids) = "[IDS " ++ (intercalate " " $ map show ids) ++ "]"
  show Scope = "Scope"

fromIds :: ID f [t] -> [(Label, ID f t)]
fromIds (IDs ids) = ids
fromIds t = error $ "ID is not a list: " ++ show t

data Node (f :: (* -> *) -> * -> *) t = Node (f (ID f) t)

data YGraphEntry f = YGraphNode (E (ID f)) (E (Node f)) Occurrence
                   | YGraphEq (E (ID f)) (E (ID f))

type YGraph f = [YGraphEntry f]
type YFile f = Map Name (YGraph f)
type YProject f = Map FilePath (YFile f)

data YogoState f = YogoState { _nameScope :: [Name]
                             , _memScope :: [ID f MemoryT]
                             , _file  :: YFile f
                             , _lastID :: Int
                             , _loopDepth :: Depth
                             }
makeLenses ''YogoState

type MonadYogo f m = (MonadPlus m, MonadState (YogoState f) m)

type CanYTrans f = (HFunctor f, ShowHF f, Typeable f)

type YTranslateM m f sig ysig t = GTranslateM m ((f :&: Label) (TermLab sig)) (ID ysig t)

instance Show (YGraphEntry f) where
  show (YGraphNode (E id) _ _) = show id
  show (YGraphEq (E id1) (E id2)) = "Eq " ++ show id1 ++  " " ++ show id2

getScopeName :: (MonadYogo f m) => m Name
getScopeName = liftM head $ use nameScope

newScope :: (MonadYogo f m, MemGenesisF :<: f) => Name -> m (ID f MemoryT)
newScope name = do
  nameScope %= (name :)
  file .= Map.singleton name []
  insertNode [] MemGenesisF >>= updateMem

getNextID :: (MonadYogo f m) => m (ID f t)
getNextID = lastID += 1 >> liftM ID (use lastID)

enterLoop :: (MonadYogo f m, TempF :<: f) => m (ID f MemoryT)
enterLoop = do
  loopDepth += 1
  TempF <$> (use lastID >>= return . (1 +)) <*> use loopDepth >>= insertNode [] >>= pushMem

exitLoop :: (MonadYogo f m, LoopMemF :<: f, FinalMemF :<: f)
         => [Label] -> ID f MemoryT -> ID f ValueT -> m (ID f MemoryT)
exitLoop labels temp cond = do
  next <- popMem
  init <- popMem
  depth <- use loopDepth
  loop <- insertNode labels $ LoopMemF depth init next
  addEntry $ YGraphEq (E temp) (E loop)
  final <- insertNode [] $ FinalMemF depth cond loop
  loopDepth -= 1
  pushMem final

addEntry :: (MonadYogo f m) => YGraphEntry f -> m ()
addEntry entry = getScopeName >>= (file %=) . (Map.adjust (entry :))

insertNode' :: (MonadYogo f m, g :<: f) => Occurrence -> g (ID f) t -> m (ID f t)
insertNode' occ node = do
  id <- getNextID
  addEntry $ YGraphNode (E id) ((E . Node . inj) node) occ
  return id

insertNode :: (MonadYogo f m, g :<: f) => [Label] -> g (ID f) t -> m (ID f t)
insertNode labels = insertNode' (Occurrence labels)

iMemF :: (MonadYogo f m, MemF :<: f) => [Label] -> ID f MemValT -> m (ID f MemoryT)
iMemF labels memVal = insertNode labels (MemF memVal) >>= updateMem

iQ :: (MonadYogo f m, Q :<: f) => [Label] -> ID f AddressT -> m (ID f ValueT)
iQ labels lvalue = getMem >>= (insertNode labels) . (Q lvalue)

iIterV :: (MonadYogo f m, IterVF :<: f) => [Label] -> ID f ValueT -> m (ID f ValueT)
iIterV labels iterable = use loopDepth >>= \depth -> insertNode labels (IterVF depth iterable)

iIterP :: (MonadYogo f m, IterPF :<: f) => [Label] -> ID f ValueT -> m (ID f ValueT)
iIterP labels iterable = use loopDepth >>= \depth -> insertNode labels (IterPF depth iterable)

getMem :: (MonadYogo f m) => m (ID f MemoryT)
getMem = liftM head $ use memScope

pushMem :: (MonadYogo f m) => ID f MemoryT -> m (ID f MemoryT)
pushMem mem = memScope %= (mem :) >> return mem

popMem :: (MonadYogo f m) => m (ID f MemoryT)
popMem = getMem >>= \mem -> memScope %= tail >> return mem

updateMem :: (MonadYogo f m) => ID f MemoryT -> m (ID f MemoryT)
updateMem mem = popMem >> pushMem mem

updateMemVal :: (MonadYogo f m, MemF :<: f) => ID f MemValT -> m (ID f MemValT)
updateMemVal memVal = iMemF [] memVal >> return memVal

ytransUnknown :: (CanYTrans f, MonadYogo y m, UnknownF :<: y) => YTranslateM m f g y MemoryT
ytransUnknown (f :&: l) = getMem >>= (insertNode [l]) . UnknownF >>= updateMem

-- s is needed, otherwise cause Incoherent Instances when f is a signature type
class (CanYTrans f) => YTrans f g y t where
  ytrans :: (MonadYogo y m) => YTranslateM m f g y t

ytranslate :: (CanYTrans f, MonadYogo y m, YTrans f f y t)
           => GTranslateM m (TermLab f) (ID y t)
ytranslate f = (ytrans . unTerm) f

instance {-# OVERLAPPABLE #-} (CanYTrans f) => YTrans f g y t where
  ytrans = const mzero
  -- ytrans = error $ show (typeRep (Proxy :: Proxy f))

instance {-# OVERLAPPING #-} (YTrans f1 g y t, YTrans f2 g y t) => YTrans (f1 :+: f2) g y t where
  ytrans = caseH' ytrans ytrans

getLabel :: TermLab f t -> Label
getLabel (Term (_ :&: l)) = l

unrollPairF :: (Cx.PairF :<: g) => TermLab g (i, j) -> (Label, (TermLab g i, TermLab g j))
unrollPairF t@(project' -> Just (Cx.PairF i j)) = (getLabel t, (i, j))
unrollPairF _ = error "Cannot unroll PairF"

unrollListF :: (Cx.ListF :<: g) => TermLab g [t] -> [(Label, TermLab g t)]
unrollListF t@(project' -> Just (Cx.NilF)) = []
unrollListF t@(project' -> Just (Cx.ConsF x xs)) = (getLabel t, x) : unrollListF xs

-- Not every kinds of ListF needs to become a list in Yogo graph, therefore just
-- return ID f [t] for the caller to decide what to do with it.
instance (Cx.ListF :<: g, YTrans g g y t) => YTrans Cx.ListF g y [t] where
  ytrans (Cx.NilF :&: _) = return $ IDs []
  ytrans t@(Cx.ConsF _ _ :&: _) =
    -- Unroll the ListF into a normal list and translate each element
    let elems = unrollListF $ inject' t in
      traverse (traverse ytranslate) elems >>= return . IDs

instance (Cx.Ident :<: g, IdentF :<: y) => YTrans Cx.Ident g y AddressT where
  ytrans (Cx.Ident name :&: l) = insertNode [l] (IdentF name)

instance (Cx.Assign :<: g, Cx.AssignOpEquals :<: g, AssignF :<: y, MemF :<: y, YTrans g g y AddressT, YTrans g g y ValueT) => YTrans Cx.Assign g y MemValT where
  -- Assumes to be :=. Not true outside of Python
  ytrans (Cx.Assign lv (project' -> Just (Cx.AssignOpEquals)) rv :&: l) =
    AssignF <$> ytranslate rv <*> ytranslate lv <*> getMem >>= insertNode [l] >>= updateMemVal

instance (Cx.FunctionCall :<: g, FunctionCallF :<: y, MemF :<: y, YTrans g g y ValueT, YTrans g g y [FArgT]) => YTrans Cx.FunctionCall g y MemValT where
  -- Assumes to be :=
  ytrans (Cx.FunctionCall _ fn fargs :&: l) =
    FunctionCallF <$> ytranslate fn <*> ytranslate fargs <*> getMem >>= insertNode [l] >>= updateMemVal

instance (Cx.FunctionArgumentList :<: g, FunctionArgsF :<: y, NothingF :<: y, YTrans g g y [ValueT]) => YTrans Cx.FunctionArgumentList g y [FArgT] where
  ytrans (Cx.FunctionArgumentList t :&: l) = do
    fargs <- ytranslate t
    go (fromIds fargs)
      where
        go [] = insertNode [] NothingF
        go ((l', id) : xs) = go xs >>= (insertNode [l, l']) . (FunctionArgsF id)

instance (Cx.PositionalArgument :<: g, YTrans g g y ValueT) => YTrans Cx.PositionalArgument g y ValueT where
  ytrans (Cx.PositionalArgument t :&: _) = ytranslate t