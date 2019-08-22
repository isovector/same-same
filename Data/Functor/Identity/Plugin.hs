{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Functor.Identity.Plugin (plugin) where

import Coercion (mkUnsafeCo)
import Control.Monad (guard, join)
import CoreSyn (Expr (..))
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Maybe (listToMaybe, catMaybes)
import Data.Traversable (for)
import Module (mkModuleName)
import OccName (mkTcOcc)
import Plugins (Plugin (..), defaultPlugin)
import TcEvidence (EvTerm (..))
import TcPluginM
import TcRnTypes
import TcType (tcSplitTyConApp, tcSplitTyConApp_maybe, eqType)
import TyCoRep (Type (..))
import TyCon (TyCon, Role(..))
import Type (mkPrimEqPred)


------------------------------------------------------------------------------
-- | The same-same plugin, which generates nominal coercion proofs that `a
-- ~ Identity a`.
plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const $ Just sameSamePlugin
  }


sameSamePlugin :: TcPlugin
sameSamePlugin = TcPlugin
  { tcPluginInit  = lookupIdentityTyCon
  , tcPluginSolve = solve
  , tcPluginStop  = pure $ pure ()
  }


------------------------------------------------------------------------------
-- | Finds the 'Data.Functor.Identity' 'TyCon'.
lookupIdentityTyCon :: TcPluginM TyCon
lookupIdentityTyCon = do
    Found _ md  <- findImportedModule emergeModule Nothing
    emergeTcNm  <- lookupOrig md $ mkTcOcc "Identity"
    tcLookupTyCon emergeTcNm
  where
    emergeModule  = mkModuleName "Data.Functor.Identity"


------------------------------------------------------------------------------
-- | Given an insoluble constraint of the form @a ~ Identity b@, attempt to
-- build an 'EvTerm' providing a nominal coercion from one to the other.
-- Additionally, return the de-identified type variables from either side
-- allowing us to unify them.
getInsolEv :: TyCon -> Ct -> Maybe (EvTerm, Type, Type)
getInsolEv idTyCon (CIrredCan ev True) = do
  let t = ctev_pred ev
      (_, ts) = tcSplitTyConApp t
      [t1, t2] = drop 2 ts
      x1 = lowerIdTower idTyCon t1
      x2 = lowerIdTower idTyCon t2
  guard $ couldPossiblyUnify x1 x2
  pure (EvExpr . Coercion $ mkUnsafeCo Nominal t1 t2, x1, x2)
getInsolEv _ _ = Nothing


------------------------------------------------------------------------------
-- | Given types 'a' and 'b', emit a @a ~ b@ wanted constraint, in effect
-- asking the unifier to unify these types for us.
unify :: CtLoc -> Type -> Type -> TcPluginM (Maybe Ct)
unify ctloc t1 t2 = do
  w <- newWanted ctloc $ mkPrimEqPred t1 t2
  pure . listToMaybe . toList $ wc_simple $ mkSimpleWC [w]


------------------------------------------------------------------------------
-- | Helper function to swizzle together 'getInsolEv' and 'unify'.
buildPluginResults :: TyCon -> Ct -> TcPluginM (Maybe ((EvTerm, Ct), [Ct]))
buildPluginResults idTyCon ct = do
  let ctloc = ctev_loc $ cc_ev ct
  for (getInsolEv idTyCon ct) $ \(ev, t1, t2) -> do
    cts <- toList <$> unify ctloc t1 t2
    pure ((ev, ct), cts)


------------------------------------------------------------------------------
-- | Given a tower of @Identity (Identity (Identity (... a)))@, give back 'a'.
lowerIdTower :: TyCon -> Type -> Type
lowerIdTower idTyCon t =
  case tcSplitTyConApp_maybe t of
    Just (tyCon, [v]) | idTyCon == tyCon
      -> lowerIdTower idTyCon v
    _ -> t


------------------------------------------------------------------------------
-- | Returns 'True' if at least one of the types is a type variable, or if both
-- types are equal.
couldPossiblyUnify :: Type -> Type -> Bool
couldPossiblyUnify (TyVarTy _) _ = True
couldPossiblyUnify _ (TyVarTy _) = True
couldPossiblyUnify a b           = eqType a b


solve
    :: TyCon
    -> [Ct]  -- ^ [G]iven constraints
    -> [Ct]  -- ^ [D]erived constraints
    -> [Ct]  -- ^ [W]anted constraints
    -> TcPluginM TcPluginResult
solve idTyCon _ ds ws = do
  z <- fmap (second join . unzip . catMaybes)
     . for (ds ++ ws)
     $ buildPluginResults idTyCon
  pure $ uncurry TcPluginOk z

