{-# LANGUAGE FlexibleContexts #-}
-- | Functions for computing variable usage inside terms and types.
module Language.PlutusIR.Analysis.Usages (runTermUsages, runTypeUsages, Usages, isUsed, allUsed) where

import           Language.PlutusIR

import qualified Language.PlutusCore      as PLC
import qualified Language.PlutusCore.Name as PLC

import           Control.Lens
import           Control.Monad.State

import           Data.Coerce
import           Data.Foldable
import qualified Data.Map                 as Map
import qualified Data.Set                 as Set

import qualified Data.Text

-- | Variable uses, as a map from the 'PLC.Unique' to its usage count. Unused variables may be missing
-- or have usage count 0.
type Usages = Map.Map (PLC.Unique,Data.Text.Text) Int

addUsage :: (PLC.HasUnique n unique) => n -> Usages -> Usages
addUsage n usages =
    let
        u = coerce $ n ^. PLC.unique
        old = Map.findWithDefault 0 (u, n ^. PLC.str) usages
    in Map.insert (u,n ^. PLC.str) (old+1) usages

isUsed :: (PLC.HasUnique n unique) => n -> Usages -> Bool
isUsed n usages = Map.findWithDefault 0 (n ^. PLC.unique . coerced, n ^. PLC.str) usages > 0

allUsed :: Usages -> Set.Set (PLC.Unique,Data.Text.Text)
allUsed usages = Map.keysSet $ Map.filter (> 0) usages

-- | Compute the 'Usages' for a 'Term'.
runTermUsages
    :: (PLC.HasUnique (name a) PLC.TermUnique, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Term tyname name a
    -> Usages
runTermUsages term = execState (termUsages term) mempty

-- | Compute the 'Usages' for a 'Type'.
runTypeUsages
    ::(PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Type tyname a
    -> Usages
runTypeUsages ty = execState (typeUsages ty) mempty

termUsages
    :: (MonadState Usages m, PLC.HasUnique (name a) PLC.TermUnique, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Term tyname name a
    -> m ()
termUsages (Var _ n) = modify (addUsage n)
termUsages term      = traverse_ termUsages (term ^.. termSubterms) >> traverse_ typeUsages (term ^.. termSubtypes)

-- TODO: move to language-plutus-core
typeUsages
    :: (MonadState Usages m, PLC.HasUnique (tyname a) PLC.TypeUnique)
    => Type tyname a
    -> m ()
typeUsages (TyVar _ n) = modify (addUsage n)
typeUsages ty          = traverse_ typeUsages (ty ^.. typeSubtypes)
