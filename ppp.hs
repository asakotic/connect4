import Data.List
import Data.Maybe (catMaybes, mapMaybe)
import Text.XHtml (table)

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
prikazi 0 j state = ""
prikazi i j state = prikazi (i - 1) j state ++ "\n" ++ concatMap crta (state !! (i - 1)) ++ "|"
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

addMove :: Table -> Int -> Maybe Table
addMove table@(Table i j state player) kol
  | kol `notElem` allValidMoves table = Nothing
  | otherwise = Just (Table i j mat (other player))
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
-- catMaybes :(
generateBigTree table dub = generateRose (\tab -> mapMaybe (addMove tab) (allValidMoves tab)) dub table
