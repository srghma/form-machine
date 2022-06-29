module Polyform.Form.Moore where

import Prelude

import Control.Comonad (class Comonad, duplicate, extract, (=>>))
import Control.Extend (class Extend)
import Data.Functor.Pairing.Class (class Pairing, zap)

data Moore :: (Type -> Type) -> (Type -> Type) -> Type -> Type -> Type
data Moore w m i a = Moore (w a) (i -> m Unit)

derive instance (Functor w, Functor m) => Functor (Moore w m i)

instance (Extend w, Functor m) => Extend (Moore w m i) where
  extend g m = g' m
    where
    g' (Moore w f) = do
      let wb = w =>> \w' -> g (Moore w' f)
      Moore wb f

instance (Functor m, Comonad w) => Comonad (Moore w m i) where
  extract (Moore wa _) = extract wa

step
  :: forall a i m w
   . Pairing w m
  => Extend w
  => Functor m
  => i
  -> Moore w m i a
  -> Moore w m i a
step i m = do
  let
    zapMoore (Moore w fm) = zap const w (fm i)
    m' = duplicate m
  zapMoore <$> m'
