{-# LANGUAGE InstanceSigs #-}
module ExceptT where
import Control.Applicative (liftA2)
import MonadTrans

newtype Except e a = Except { runExcept :: Either e a }

newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }

except :: Monad m => Either e a -> ExceptT e m a
except = ExceptT . return

instance Functor (Except e) where
    fmap :: (a -> b) -> Except e a -> Except e b
    fmap f = Except . fmap f. runExcept

instance Functor m => Functor (ExceptT e m) where
    fmap :: (a -> b) -> ExceptT e m a -> ExceptT e m b
    fmap f = ExceptT . fmap (fmap f) . runExceptT
