{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Err where

{-

type MonadState :: * -> (* -> *) -> Constraint
class Monad m => MonadState s m | m -> s where
  Control.Monad.State.get   :: m s
  Control.Monad.State.put   :: s -> m ()
  Control.Monad.State.state :: (s -> (a, s)) -> m a
instance (Monoid w, MonadState s m) => MonadState s (Control.Monad.Trans.Writer.WriterT w m)
instance Monad m                    => MonadState s (StateT s m)
instance MonadState s m             => MonadState s (MaybeT m)
-}

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.List
import Debug.Trace
-- import Data.Functor.Identity

-- s1 :: State Int Int
-- s1 = do
--   s <- get
--   put $ succ s
--   return $ s + 10

-- s2 :: State Int Int
-- s2 = do
--   s <- get
--   put $ succ s
--   return $ s * 10

-- s3 :: State Int Int
-- s3 = do
--   s <- get
--   put $ succ s
--   return $ s + 27
  
-- zoo = traverse' :: Either Int ([Int],Int) where
--     traverse' = fmap' $ runStateT limit1 0
--     fmap' (Left x)        = Left x
--     fmap' (Right (a, s')) = Right ([a], s')

--     limit1 :: StateT Int (Either Int) Int
--     limit1 = do
--       a <- state (runState s1)
--       stateIsBad <- gets (not . (<10))
--       let m = (when stateIsBad $ throwError 0)
--       let k = (\s -> Right (a, s))
--       StateT $ \s -> (runStateT m s >>= \(a, s') -> k s')

-- gg = runLimited1 (<3) (modify (+1)) 0

-- runLimited1 p f s = run1 (limit1 p (0,f)) s

-- s -> Identity ((Either Int a),s)

-- Read a 
-- MonadState s m * -> (* -> *) -> C




-- run1 :: StateT s IdentityT (Either e a) -> s -> (Either e a,s)
-- run1 m s = undefined

-- gg :: IdentityT (Either Int) Int
-- gg = limit1 (<3) (0, state $ \s -> (1,s) )


-- runStateT m s :: IdentityT (Either e) (a,s) :: (Either e) (a,s)

-- (IdentityT (Either e) a, s)

-- ( IdentityT { Either e a   } , s )

-- gg :: (Int, State Int Int) -> Maybe
-- gg = (limit1 (<3) (1,( modify (+1) ))) :: StateT Int ([]) (Either Int Int)


-- modify :: MonadState s m => (s -> s) -> m ()
-- modify f = state (\s -> ((), f s))

data MyMonad s e a = MyMonad { runMyMonad :: s -> Either (e,s) (a,s) }

instance Functor (MyMonad s e) where
  fmap = liftM

instance Applicative (MyMonad s e) where
  pure  = return
  (<*>) = ap

instance Monad (MyMonad s e) where
  return x = MyMonad $ \s -> Right (x,s)
  (MyMonad f) >>= k = MyMonad $ \s -> case f s of
    Left x       -> Left x
    Right (a,s') -> runMyMonad (k a) s

instance MonadState s (MyMonad s e) where
  -- get   = lift get
  -- put   = lift . put
  -- state = lift . state
  -- state :: (s -> (a, s)) -> m a
  state f = MyMonad $ \s -> Right (f s)

instance MonadError e (MyMonad s e) where
  throwError e = MyMonad $ \s -> Left (e,s)


-- -- limit1 :: (MonadState s m, MonadError e m) => (s -> Bool) -> (e, State s a) -> MyMonad s Identity e -- m a
-- -- limit1 :: (s -> Bool) -> (e, State s a) -> MyMonad s e a -- m a
-- limit1 p (i, f) = do
--   a <- state (runState f)
--   stateIsBad <- gets (not . p)
--   when stateIsBad $ throwError i
--   pure a

-- runLimited1 :: (s -> Bool) -> State s a -> s -> (Either Int a, s)
-- runLimited1 p fs s = run1 (limit1 p (0,fs)) s

-- run1 :: MyMonad s e a -> s -> (Either e a,s)
-- run1 (MyMonad e) s = case e of
--   Left (e,s)  -> (Left e,s)
--   Right (a,s) -> (Right a,s)

-- tt = runLimited1 (<3) ( modify (+1) ) 0

-- -- limited :: (MonadState s m, MonadError e m) => (a -> Bool) -> [State a b] -> f [b]
-- limited p fs = traverse limit1 (zip [0..] fs)
--   where
--     limit1 (i, f) = do
--       a <- state (runState f)
--       stateIsBad <- gets (not . p)
--       when stateIsBad $ throwError i
--       pure a

-- -- runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s) -- (Left i,s) | (Right [a],s)
-- runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
-- runLimited1 p fs s = run1 (limited p fs) s



-- -- run1 :: Num q => StateT s (ListT Maybe) a -> s -> (Either q [a], s)
-- run1 :: f [b] -> s -> (Either Int [a], s)
-- run1 :: StateT s Identity [Either Int a] -> s -> (Either Int [a],s)
-- run1 m s = let (a,s) = runState m s in (sequence a ,s)

-- -- p1 = runLimited1 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- -- p2 = runLimited2 (< 3) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- -- p3 = runLimited1 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
-- -- p4 = runLimited2 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0

-- runLimited2 :: (s -> Bool) -> [State s a] -> s -> Either Int ([a], s)
-- runLimited2 p fs s = run2 (limited p fs) s
-- run2 :: StateT s m a -> s -> m (a, s)
-- run2 = runStateT

-- a :: Num a => ListT Maybe a
-- a = ListT $ pure [1,2]

-- ss :: [State Int ()]
-- ss = [modify (+1),modify (+1),modify (+1),modify (+1),modify (+1)]

-- a1 :: Num a => State Int (Either Int a)
-- a1 = state $ \s -> (Right 1,s)



-- s8 :: State Int (Either Int Int)
-- s8 = state $ \s -> (pure s,s+1)

limited p fs = traverse limit1 (zip [0..] fs)
  where
    limit1 (i, f) = do
      a <- state (runState f)
      stateIsBad <- gets (not . p)
      when stateIsBad $ throwError i
      pure a

runLimited1 :: (s -> Bool) -> [State s a] -> s -> (Either Int [a], s)
runLimited1 p fs s = run1 (limited p fs) s

run1 :: MyMonad s e a -> s -> (Either e a, s)
run1 m s = case runMyMonad m s of
  Left (e,s)  -> (Left e,s)
  Right (a,s) -> (Right a,s)

g1 = runLimited1 (< 3)   [modify (+1), modify (+1), modify (+1), modify (+1)] 0
g2 = runLimited1 (< 100) [modify (+1), modify (+1), modify (+1), modify (+1)] 0
