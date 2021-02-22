import ExceptT
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Char (isNumber, isPunctuation)
import Control.Applicative
import MonadTrans as T
import Text.Read as R

main :: IO ()
main = putStrLn "Test suite not yet implemented"

data Tile = Floor | Chasm | Snake deriving Show

data DeathReason = Fallen | Poisoned deriving (Eq, Show)

type Point = (Integer, Integer)

type GameMap = Point -> Tile

moves :: GameMap -> Int -> Point -> [Either DeathReason Point]
moves map k = runExceptT . moves' k where
    moves' 0 p       = verify p
    moves' k p@(x,y) = do
        verify p
        n <- ExceptT $ Right <$> [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
        moves' (k-1) n
    verify p = do
        case map p of
            Floor -> return p
            Chasm -> throwE Fallen
            Snake -> throwE Poisoned

waysToDie :: DeathReason -> GameMap -> Int -> Point -> Int
waysToDie r map steps p = length (filter ( == (Left r)) (moves map steps p))

map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm

foo :: Int -> Int -> ExceptT Int [] Int
foo 0 p = verify p
foo count p = verify p >> ExceptT (next p) >>= foo (count-1)

next :: Int -> [Either Int Int]
next p = fmap Right [p-1,p+1]

verify :: Int -> ExceptT Int [] Int
verify p = do
    if p `mod` 3 == 0
    then ExceptT [Left p]
    else return p

askPassword0 :: MaybeT IO ()
askPassword0 = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword0
  liftIO $ putStrLn "Storing in database..."

getValidPassword0 :: MaybeT IO String
getValidPassword0 = do
  s <- liftIO getLine
  guard (isValid0 s)
  return s

isValid0 :: String -> Bool
isValid0 s = length s >= 8
            && any isNumber s
            && any isPunctuation s

data PwdError = PwdError String

type PwdErrorIOMonad = ExceptT PwdError IO

instance (MonadIO m) => MonadIO (ExceptT e m) where
    liftIO = T.lift . liftIO

instance (Functor m, Monad m, Monoid e) => Alternative (ExceptT e m) where
    empty = ExceptT $ return (Left mempty)
    ExceptT mx <|> ExceptT my = ExceptT $ do
        ex <- mx
        case ex of
            Left e  -> liftM (either (Left . mappend e) Right) my
            Right x -> return (Right x)

instance (Monad m, Monoid e) => MonadPlus (ExceptT e m) where

instance Semigroup PwdError

instance Monoid PwdError where
    mempty = PwdError ""

askPassword :: PwdErrorIOMonad ()
askPassword = do
  liftIO $ putStrLn "Enter your new password:"
  value <- msum $ repeat getValidPassword
  liftIO $ putStrLn "Storing in database..."

getValidPassword :: PwdErrorIOMonad String
getValidPassword = do
    s <- liftIO getLine
    (validate s) `catchE` (\(PwdError a) -> do liftIO $ putStrLn a ; throwE $ PwdError a)

validate :: String -> PwdErrorIOMonad String
validate s = do
    unless (length s >= 8)  (throwE $ PwdError "Incorrect input: password is too short!")
    unless (any isNumber s) (throwE $ PwdError "Incorrect input: password must contain some digits!")
    unless (any isPunctuation s) (throwE $ PwdError "Incorrect input: password must contain some punctuation!")
    return s

test f = runIdentity (runStateT(runExceptT f) 3)

data ReadError = EmptyInput | NoParse String deriving Show

tryRead :: (Read a, Monad m) => String -> ExceptT ReadError m a
tryRead [] = throwE EmptyInput
tryRead s  = case readMaybe s of
    Nothing -> throwE $ NoParse s
    Just a  -> return a
