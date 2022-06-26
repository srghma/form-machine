module Polyform.Form.Machine where

import Prelude

import Control.Comonad (class Comonad, extract, (=>>))
import Control.Comonad.Store (Store, store)
import Control.Comonad.Store.Class (seek)
import Control.Extend (class Extend)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

data Moore m i a = Moore a (i -> m (Moore m i a))

derive instance Functor m => Functor (Moore m i)

instance Functor m => Extend (Moore m i) where
  extend g m = g' m
    where
    g' m'@(Moore _ f) = do
      let b = g m'
      Moore b (map g' <$> f)

instance Functor m => Comonad (Moore m i) where
  extract (Moore a _) = a

step :: forall a i m. Moore m i a -> i -> m (Moore m i a)
step (Moore _ f) = f

unfoldMoore :: forall a i m s. Monad m => (s -> (a /\ (i -> m s))) -> s -> Moore m i a
unfoldMoore f = go
  where
  go s = case f s of
    (a /\ g) -> Moore a \i -> do
      s' <- g i
      pure $ go s'

data MooreCT :: forall k. (Type -> Type) -> Type -> (k -> Type) -> k -> Type
data MooreCT m i w a = MooreCT (w a) (i -> m (MooreCT m i w a))

derive instance (Functor w, Functor m) => Functor (MooreCT m i w)

instance (Extend w, Functor m) => Extend (MooreCT m i w) where
  extend g m = g' m
    where
    g' (MooreCT w f) = do
      let wb = w =>> \w' -> g (MooreCT w' f)
      MooreCT wb (map g' <$> f)

instance (Functor m, Comonad w) => Comonad (MooreCT m i w) where
  extract (MooreCT wa _) = extract wa

hoist :: forall a i m m' w. Functor m' => (m ~> m') -> MooreCT m i w a -> MooreCT m' i w a
hoist f (MooreCT wa cnt) = MooreCT wa (map hoist' cnt)
  where
  hoist' mm = hoist f <$> f mm

newtype UIComponent m state action ui = UIComponent (MooreCT m action (Store state) ui)

derive newtype instance Functor m => Functor (UIComponent m state action)
derive newtype instance Functor m => Extend (UIComponent m state action)

-- | Let's do manual pairing as a first step so we have a working form...
-- | then we can generalize.
unfoldUIComponent :: forall action m state ui. Monad m => (state -> (ui /\ (action -> m state))) -> state -> UIComponent m state action ui
unfoldUIComponent f state = UIComponent (MooreCT wa f')
  where
  wa = store (map fst f) state
  f' = map (\state' -> MooreCT (seek state' wa) f') <$> snd (f state)

hoistUIComponent :: forall action m m' state ui. Functor m' => (m ~> m') -> UIComponent m state action ui -> UIComponent m' state action ui
hoistUIComponent f (UIComponent m) = UIComponent (hoist f m)
