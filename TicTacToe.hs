
import Data.Char (toLower)
import Data.List (intersperse, transpose)
import System.IO

data Board = Board [[Char]]

rowSep = "--+---+--"

myIntersperse :: String -> String -> String
myIntersperse str (x:xs) = x:(concatMap (\x -> str ++ [x]) xs)

drawBoard :: Board -> IO ()
drawBoard (Board board) = do
  let lines = map (myIntersperse " | ") board
      flines = intersperse rowSep lines
  sequence_ $ map putStrLn flines

decode n = (n `div` 3, n `rem` 3)

getEmptyBoard :: Board
getEmptyBoard = Board $ replicate 3 $ replicate 3 ' '

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
  where hori = or $ map allequal board
        vert = or $ map allequal $ transpose board
        d1 = allequal $ [board !! 0 !! 0, board !! 1 !! 1, board !! 2 !! 2]
        d2 = allequal $ [board !! 0 !! 2, board !! 1 !! 1, board !! 2 !! 0]

placeMove :: Char -> Int -> Board -> Board
placeMove c n (Board board) = Board $ t1 ++ t2 ++ t3
  where (rowNo, colNo) = decode n
        t1 = take rowNo board
        t2 = [updateRow (board !! rowNo)]
        t3 = drop (rowNo+1) board
        updateRow row = (take colNo row) ++ [c] ++ (drop (colNo+1) row)

isValidMove (Board board) n = ' ' == ((board !! rn) !! cn)
  where (rn, cn) = decode n

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

main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  -- putStr "Do you want to play first? (Y/n) "
  -- choice <- getChar
  -- if toLower choice == 'n'
  --   then undefined
  --   else undefined
  playGame getEmptyBoard 'x' 0


{-
o | o | o
--+---+--
x | o | o
--+---+--
x | x | x
-}

