module Data.Functor.Pairing.Class where

import Data.Tuple.Nested ((/\), type (/\))
import Control.Monad.State.Trans (StateT(..))
import Control.Monad.Writer.Trans (WriterT(..))
import Control.Monad.Reader.Trans (ReaderT(..))
import Control.Comonad.Store.Trans (StoreT(..))
import Control.Comonad.Traced.Trans (TracedT(..))
import Control.Comonad.Env.Trans (EnvT(..))

class Pairing f g where
  zap :: forall a b c. (a -> b -> c) -> f a -> g b -> c

instance Pairing ((->) r) ((/\) r) where
  zap f gf (r /\ b) = f (gf r) b

instance Pairing ((/\) r) ((->) r) where
  zap f (r /\ a) gf = f a (gf r)

instance Pairing f g => Pairing (StateT s f) (StoreT s g) where
  zap f (StateT exs) (StoreT (wsf /\ s)) = zap (\(a /\ s') sf -> f a (sf s')) (exs s) wsf

instance Pairing f g => Pairing (StoreT s f) (StateT s g) where
  zap f (StoreT (wsf /\ s)) (StateT exs) = zap (\sf (b /\ s') -> f (sf s') b) wsf (exs s)

instance Pairing f g => Pairing (WriterT w f) (TracedT w g) where
  zap f (WriterT macc) (TracedT tf) = zap (\(a /\ acc) tf' -> f a (tf' acc)) macc tf

instance Pairing f g => Pairing (TracedT w f) (WriterT w g) where
  zap f (TracedT tf) (WriterT macc) = zap (\tf' (b /\ acc) -> f (tf' acc) b) tf macc

instance Pairing f g => Pairing (ReaderT env f) (EnvT env g) where
  zap f (ReaderT rf) (EnvT (env /\ wb)) = zap (\a b -> f a b) (rf env) wb

instance Pairing f g => Pairing (EnvT env f) (ReaderT env g) where
  zap f (EnvT (env /\ wa)) (ReaderT rf) = zap (\a b -> f a b) wa (rf env)
