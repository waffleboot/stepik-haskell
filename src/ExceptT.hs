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

instance Applicative (Except e) where
    pure :: a -> Except e a
    pure = Except . Right
    (<*>) :: Except e (a -> b) -> Except e a -> Except e b
    f <*> v = Except $ (runExcept f) <*> (runExcept v)
    -- f <*> v = Except $ updater (runExcept f) (runExcept v) where
    --     updater (Left e) _  = Left e
    --     updater (Right g) x = fmap g x

instance Applicative m => Applicative (ExceptT e m) where
    pure :: a -> ExceptT e m a
    pure = ExceptT . pure . Right
    (<*>) :: ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
    f <*> v = ExceptT $ liftA2 (<*>) (runExceptT f) (runExceptT v)
    -- f <*> v = ExceptT $ liftA2 updater (runExceptT f) (runExceptT v) where
    --     updater (Left e) _  = Left e
    --     updater (Right g) x = fmap g x
