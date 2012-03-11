
import Data.Char (toLower)
import Data.List (intersperse, transpose)
import System.IO

data Board = Board String

rowSep = "--+---+--"

myIntersperse :: String -> String -> String
myIntersperse str (x:xs) = x:(concatMap (\x -> str ++ [x]) xs)

getSBoard board = [take 3 board, (take 3.drop 3) board, drop 6 board] 

drawBoard :: Board -> IO ()
drawBoard (Board board) = do
  let sboard = getSBoard board
      lines = map (myIntersperse " | ") sboard
      flines = intersperse rowSep lines
  sequence_ $ map putStrLn flines

getEmptyBoard :: Board
getEmptyBoard = Board $ replicate 9 ' '

getUserMove :: IO Int
getUserMove = do
  c <- fmap read getLine
  if c > 0 && c < 10
    then return (c-1)
    else getUserMove

allequal :: String -> Bool
allequal s = (and $ map ((s!!0)==) s) && (s!!0 /= ' ')

gameOver :: Board -> Bool
gameOver (Board board) = or [hori, vert, d1, d2]
  where sboard = getSBoard board
        hori = or $ map allequal sboard
        vert = or $ map allequal $ transpose sboard
        d1 = allequal $ [board !! 0, board !! 4, board !! 8]
        d2 = allequal $ [board !! 2, board !! 4, board !! 6]

placeMove :: Char -> Int -> Board -> Board
placeMove c n (Board board) = Board $ (take n board) ++ [c] ++ (drop (n+1) board)

isValidMove (Board board) n = ' ' == (board !! n)

getValidUserMove board = do
  c <- getUserMove
  if isValidMove board c
    then return c
    else getValidUserMove board

other 'x' = 'o'
other 'o' = 'x'

playGame :: Board -> Char -> Int -> IO ()
playGame board player mno = do
  drawBoard board
  if mno == 9
    then do putStrLn "Draw! Game Over!"
            return ()
    else do n <- getValidUserMove board
            putStrLn "\n"
            let nb = placeMove player n board
            if gameOver nb
              then do drawBoard nb
                      putStrLn (player:" wins")
                      return ()
              else playGame nb (other player) (mno + 1)

main = playGame getEmptyBoard 'x' 0
