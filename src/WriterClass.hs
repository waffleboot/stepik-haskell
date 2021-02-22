{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module WriterClass where
import MonadTrans
import ReaderT
import qualified WriterT

class (Monoid w, Monad m) => MonadWriter w m | m -> w where
    writer :: (a,w) -> m a
    tell   :: w -> m ()
    listen :: m a -> m (a, w)

listens :: MonadWriter w m => (w -> b) -> m a -> m (a, b)
listens f m = do
    ~(a, w) <- listen m
    return (a, f w)

instance (Monoid w, Monad m) => MonadWriter w (WriterT.WriterT w m) where
    writer = WriterT.writer
    tell   = WriterT.tell
    listen = WriterT.listen

instance MonadWriter w m => MonadWriter w (ReaderT r m) where
    writer = lift . writer
    tell   = lift . tell
    listen m = ReaderT $ \r -> do
        ~(a,w) <- listen (runReaderT m r)
        return (a,w)
