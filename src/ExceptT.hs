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

instance Monad m => Applicative (ExceptT e m) where
    pure :: a -> ExceptT e m a
    pure = ExceptT . pure . Right
    (<*>) :: ExceptT e m (a -> b) -> ExceptT e m a -> ExceptT e m b
    ExceptT mef <*> ExceptT mea = ExceptT $ do
        ef <- mef
        case ef of
            Left e  -> return (Left e)
            Right f -> fmap (fmap f) mea

instance Monad (Except e) where
    m >>= k = case runExcept m of
        Left e  -> Except $ Left e
        Right x -> k x

instance Monad m => Monad (ExceptT e m) where
    m >>= k = ExceptT $ do
        a <- runExceptT m
        case a of
            Left e  -> return (Left e)
            Right x -> runExceptT (k x)

instance MonadFail m => MonadFail (ExceptT e m) where
    fail = ExceptT . fail

instance MonadTrans (ExceptT e) where
    lift :: Monad m => m a -> ExceptT e m a
    lift = ExceptT . fmap Right

throwE :: Monad m => e -> ExceptT e m a
throwE = ExceptT . return . Left

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
m `catchE` h = ExceptT $ do
    a <- runExceptT m
    case a of
        Left l  -> runExceptT (h l)
        Right r -> return (Right r)