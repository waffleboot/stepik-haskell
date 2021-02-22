import ExceptT

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
