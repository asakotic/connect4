{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use <$>" #-}

import Data.Either (rights)
import Data.List
import Data.Maybe (mapMaybe)
import GHC.Exts.Heap (GenClosure (fun))
import Text.Parsec hiding (State)

data Rose a = Node a [Rose a] deriving (Show)

size :: Rose a -> Int
size (Node _ c) = 1 + sum (map size c)

height :: Rose a -> Int
height (Node _ c) = 1 + maximum (0 : map size c)

leavesCount :: Rose a -> Int
leavesCount (Node p []) = 0
leavesCount (Node p c) = sum (map leavesCount c)

leaves :: Rose a -> [a]
leaves (Node p []) = [p]
leaves (Node p c) = concatMap leaves c

elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node p c) = [p]
elemsOnDepth n (Node p c)
  | n < 0 = error "bad input"
  | otherwise = concatMap (elemsOnDepth (n - 1)) c

instance Functor Rose where
  fmap f (Node p c) = Node (f p) (map (fmap f) c)

foldRose :: (b -> a -> b) -> b -> Rose a -> b
-- fja acc stablo rez
foldRose f acc node = foldl f acc (flatuj node)

flatuj :: Rose a -> [a]
flatuj (Node p c) = p : concatMap flatuj c

-- (\x->[x+1,x+2,x+3]),
generateRose :: (a -> [a]) -> Int -> a -> Rose a
generateRose f 0 start = Node start []
generateRose f dub start = Node start [generateRose f (dub - 1) ye | ye <- f start]

-- drugi deo
data State = Z | C | P deriving (Eq)

data Player = Red | Yellow deriving (Show, Eq)

instance Show State where
  show Z = "Z"
  show C = "C"
  show P = " "

data Table = Table Int Int [[State]] Player

instance Show Table where
  show (Table i j state _) = drop 1 (prikazi i j state)

prikazi :: Int -> Int -> [[State]] -> String
prikazi (-1) j state = ""
prikazi i j state = prikazi (i - 1) j state ++ "\n" ++ concatMap crta (state !! i) ++ "|"
  where
    crta :: State -> String
    crta a = "|" ++ show a

allValidMoves :: Table -> [Int]
allValidMoves (Table i j state player) =
  filter
    (/= -1)
    [ if el == P then index else -1
      | let list = head state,
        (index, el) <- zip [1 ..] list
    ]

addMove :: Table -> Int -> Either String Table
addMove table@(Table i j state player) kol
  | checkIfOver table = Left "Game is already finished"
  | kol `notElem` allValidMoves table = Left "Bad move"
  | otherwise = Right (Table i j mat (other player))
  where
    mat = transpose [if index == kol then reverse $ put player kol (reverse kolona) else kolona | (index, kolona) <- zip [1 ..] (transpose state)]
    other Yellow = Red
    other Red = Yellow

put :: Player -> Int -> [State] -> [State]
put pl kol (x : xs)
  | x == P = potez pl : xs
  | otherwise = x : put pl kol xs
  where
    potez Red = C
    potez Yellow = Z

checkIfOver :: Table -> Bool
checkIfOver (Table i j state player) =
  or [pobedio row | row <- state]
    || or [pobedio row | row <- transpose state]
    || or [pobedio row | row <- leftD i j state]
    || or [pobedio row | row <- rightD i j state]
    || and [P `notElem` row | row <- state]

pobedio :: [State] -> Bool
pobedio str = isInfixOf [C, C, C, C] str || isInfixOf [Z, Z, Z, Z] str

leftD :: Int -> Int -> [[State]] -> [[State]]
leftD i j mat =
  [dij1 i (j - ind) mat | ind <- [0 .. min i j], i - ind >= 0 && j - ind >= 0]
    ++ [dij1 (i - ind) j mat | ind <- [0 .. min i j], i - ind >= 0 && j - ind >= 0]

rightD :: Int -> Int -> [[State]] -> [[State]]
rightD i j mat =
  [dij2 i ind mat | ind <- [0 .. min i j], j - ind >= 0]
    ++ [dij2 (i - ind) 0 mat | ind <- [0 .. min i j], i - ind >= 0 && j - ind >= 0]
  where
    dij2 :: Int -> Int -> [[State]] -> [State]
    dij2 a b mat = [mat !! (a - inde) !! (b + inde) | inde <- [0 .. min i j], a - inde >= 0 && b + inde <= j]

dij1 :: Int -> Int -> [[State]] -> [State]
dij1 i j mat = [mat !! (i - ind) !! (j - ind) | ind <- [0 .. min i j], i - ind >= 0 && j - ind >= 0]

-- generateRose :: (a -> [a]) -> Int -> a -> Rose a

generateBigTree :: Table -> Int -> Rose Table
generateBigTree table dub = generateRose (\tab -> rights $ map (addMove tab) (allValidMoves tab)) dub table

-- treci deo
data GameState = Game Table | StopGame Table String

instance Show GameState where
  show (Game table) = show table
  show (StopGame table string) = show table ++ string

data GameStateOp a = GameStateOp {runGame :: GameState -> (a, GameState)}

instance Functor GameStateOp where
  fmap f (GameStateOp g) = GameStateOp fun
    where
      fun oldState = (f a, newState)
        where
          (a, newState) = g oldState

instance Applicative GameStateOp where
  pure a = GameStateOp (\state -> (a, state))
  GameStateOp f' <*> GameStateOp a' = GameStateOp fun
    where
      fun state = (f a, newnewState)
        where
          (f, newState) = f' state
          (a, newnewState) = a' newState

instance Monad GameStateOp where
  GameStateOp a' >>= fun = GameStateOp funkc
    where
      funkc state = runGame (fun a) newState
        where
          (a, newState) = a' state

applyMove :: Int -> GameStateOp ()
applyMove kol = GameStateOp fun
  where
    fun :: GameState -> ((), GameState)
    fun (StopGame table str) = ((), StopGame table str)
    fun (Game table) = ((), pom (addMove table kol))
      where
        pom :: Either String Table -> GameState
        pom (Left str) = StopGame table str
        pom (Right table) = Game table

applyMoves :: [Int] -> GameStateOp ()
applyMoves [] = return ()
applyMoves (x : xs) = applyMove x >> applyMoves xs

testGame :: GameState
testGame = snd $ runGame (applyMoves [1, 2, 2, 1, 1]) (Game (Table 3 3 [[P, P, P, P], [P, P, P, P], [P, P, P, P], [P, P, P, P]] Red))

-- parsiranje
parserGame :: Parsec String () GameState
parserGame = do
  p1 <- parser
  p2 <- movesParser
  return (snd $ runGame (applyMoves p2) (Game p1))

parser :: Parsec String () Table
parser = do
  table <-
    many1
      ( do
          char '|'
          manyTill
            ( do
                s <- state
                char '|'
                return s
            )
            (string "\n")
      )
  let i = length table - 1
  let j = length (head table) - 1
  return (Table i j table Yellow)

movesParser :: Parsec String () [Int]
movesParser = do
  manyTill number eof

number :: Parsec String () Int
number = do
  spaces
  num <- many1 digit
  spaces
  return (read num)

state :: Parsec String () State
state = fmap convert (oneOf " CZ")

convert :: Char -> State
convert ' ' = P
convert 'C' = C
convert 'Z' = Z

main :: IO ()
main = do
  input <- readFile "unos.txt"
  if null input
    then return ()
    else do
      case runParser parserGame () "unos.txt" input of
        Right x -> print x
        Left x -> print x