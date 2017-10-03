module Language.Haskell.TH.Compat.TypeVariables.V210 (
  fromType,
  ) where

import Data.Set (Set, (\\), fromList)
import qualified Data.Set as Set
import Language.Haskell.TH (Name, TyVarBndr (..), Type (..))


fromBndr :: TyVarBndr -> Name
fromBndr b = case b of
  PlainTV  n     ->  n
  KindedTV n _k  ->  n

-- | take free variables set from 'Type'.
fromType :: Type -> Set Name
fromType = rec'
  where
    rec2 ty1 ty2 = rec' ty1 `Set.union` rec' ty2
    rec' ty0 = case ty0 of
      ForallT bs _cxt ty  ->  rec' ty \\ fromList [ fromBndr b | b <- bs ]
                              -- only take free variables
      AppT ty1 ty2        ->  rec2 ty1 ty2
      SigT ty _kind       ->  rec' ty
      VarT n              ->  Set.singleton n
      -- ConT _n             ->  []
      -- PromotedT _name     ->  []
      -- TupleT _i           ->  []
      -- UnboxedTupleT _i    ->  []
      -- ArrowT              ->  []
      -- EqualityT           ->  []
      -- ListT               ->  []
      -- PromotedTupleT _i   ->  []
      -- PromotedNilT        ->  []
      -- PromotedConsT       ->  []
      -- StarT               ->  []
      -- ConstraintT         ->  []
      -- LitT _lit           ->  []
      _                      ->  Set.empty
