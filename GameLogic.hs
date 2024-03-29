module GameLogic
( Board(Board),
  getBoard,
  getEmptyBoard,
  boardRep,
  drawBoard,
  isWon,
  isDrawn,
  placeMove,
  isValidMove,
  other,
  getAIMove
)
       where

import           Control.Monad.State
import           Data.List           (intersperse, transpose, findIndex, sort)
import           Data.Maybe          (fromJust)

newtype Board = Board { getBoard :: String }
           deriving Show

-- initial board constructor
getEmptyBoard :: Board
getEmptyBoard = Board $ replicate 9 ' '

-- board representation functions
--------------------------------------------------------------------------------
rowSep = "--+---+--"

myIntersperse :: String -> String -> String
myIntersperse str (x:xs) = x:(concatMap (\x -> str ++ [x]) xs)

getSBoard board = [take 3 board, (take 3.drop 3) board, drop 6 board] 

boardRep :: State Board [String]
boardRep = do board <- fmap getBoard get
              let lines = map (myIntersperse " | ") sboard
                  sboard = getSBoard board
              return $ intersperse rowSep lines

drawBoard :: Board -> IO ()
drawBoard (Board board) = do
  let sboard = getSBoard board
      lines = map (myIntersperse " | ") sboard
      flines = intersperse rowSep lines
  sequence_ $ map putStrLn flines
--------------------------------------------------------------------------------

-- games rules
--------------------------------------------------------------------------------
allequal :: String -> Bool
allequal s = (and $ map ((s!!0)==) s) && (s!!0 /= ' ')

isWon :: Board -> Bool
isWon (Board board) = or [hori, vert, d1, d2]
  where sboard = getSBoard board
        hori = or $ map allequal sboard
        vert = or $ map allequal $ transpose sboard
        d1 = allequal $ [board !! 0, board !! 4, board !! 8]
        d2 = allequal $ [board !! 2, board !! 4, board !! 6]

isDrawn :: Board -> Bool
isDrawn board = (not $ isWon board) && (countSpace == 0)
  where countSpace = length $ filter (==' ') $ getBoard board

placeMove :: Char -> Int -> Board -> Board
placeMove c n (Board board) = Board $ (take n board) ++ [c] ++ (drop (n+1) board)

isValidMove :: Int -> Board -> Bool
isValidMove n (Board board) = ' ' == (board !! n)

other :: Char -> Char
other 'x' = 'o'
other 'o' = 'x'

--------------------------------------------------------------------------------

-- computer player
--------------------------------------------------------------------------------
getAIMove :: Board -> Int
getAIMove (Board board) = getBestMove (Board board) aiChar
  where xcount = length $ filter (=='x') board
        ocount = length $ filter (=='o') board
        aiChar = if xcount == ocount then 'x' else 'o'

getBestMove :: Board -> Char -> Int
getBestMove board c = snd $ getBestMove' board c

getBestMove' :: Board -> Char -> (Int, Int)
getBestMove' board c | length wonBoards > 0 = (1, fst $ head wonBoards)
                     | length drawnBoards > 0 = (0, fst $ head drawnBoards)
                     | otherwise = head $ (reverse.sort) myBestMoves
  where bstr = getBoard board
        spPos = map fst $ filter (\(x, y) -> y == ' ') $ zip [0..] bstr
        nextBoards = map (\x -> (x, placeMove c x board)) spPos
        wonBoards = filter (\x -> isWon $ snd x)  nextBoards
        drawnBoards = filter (\x -> isDrawn $ snd x) nextBoards
        remBoards = filter (\x -> not (isWon (snd x) || isDrawn (snd x))) nextBoards
        c' = other c
        bestOppMoves = map (\x -> (getBestMove' (snd x) c', fst x)) remBoards
        myBestMoves = map (\((s, om), p) -> (-s, p)) bestOppMoves
  
--------------------------------------------------------------------------------
