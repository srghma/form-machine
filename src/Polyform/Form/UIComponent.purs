module Polyform.Form.UIComponent where

import Prelude

import Data.Tuple.Nested ((/\), type (/\))
import Control.Monad.State.Trans (StateT)
import Control.Monad.Writer (Writer)
import Control.Monad.Reader.Trans (ReaderT)
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Traced (Traced, traced)
import Control.Comonad.Env.Trans (EnvT(..))
import Polyform.Form.Moore (Moore(..), step)

type UIComponentM :: forall k. (k -> Type) -> Type -> Type -> k -> Type -> Type
type UIComponentM eff ps st act = ReaderT ps (StateT st (Writer (Array (eff act))))

type UIComponentC :: forall k. (k -> Type) -> Type -> Type -> k -> Type -> Type
type UIComponentC eff ps st act = EnvT ps (StoreT st (Traced (Array (eff act))))

type UIComponent eff ps st act = Moore (UIComponentC eff ps st act) (UIComponentM eff ps st act) act (st /\ (Array (eff act)))

stepUIComponent :: forall act eff ps st. act -> UIComponent eff ps st act -> UIComponent eff ps st act
stepUIComponent = step

unfoldUIComponent :: forall act eff ps st. ps -> st -> (act -> UIComponentM eff ps st act Unit) -> UIComponent eff ps st act
unfoldUIComponent ps st f = Moore w f
  where
  w = EnvT $ ps /\ StoreT (traced (flip (/\)) /\ st)
