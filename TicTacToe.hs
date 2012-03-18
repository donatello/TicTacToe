module Main
       where

import           GameLogic
import           Control.Concurrent
import           Control.Monad.State
import           Control.Monad     (forM_)
import           UI.NCurses
import qualified Data.Text         as T
import           System.IO

data Status = InProgress | Won | Drawn | Abandon
            deriving (Show, Eq)
data DBoard = DBoard {
  cursor :: Int,
  dispC :: Bool,
  board :: Board,
  turn :: Char,
  status :: Status
  }

getNewDBoard :: DBoard
getNewDBoard = DBoard {
  cursor = 0,
  dispC = True,
  board = getEmptyBoard,
  turn = 'x',
  status = InProgress
  }

flipCursor :: State DBoard ()
flipCursor = modify (\dboard -> if status dboard /= InProgress 
                                then dboard { dispC = False }
                                else dboard { dispC = not $ dispC dboard }
                    )

putCursor :: Int -> State DBoard Board
putCursor pos = do toDisp <- fmap dispC get
                   bd <- fmap (getBoard.board) get
                   let c = if toDisp then "_" else [bd !! pos]
                   return $ Board $ (take pos bd) ++ c ++ (drop (pos+1) bd)

renderBoard :: Integer -> Integer -> DBoard -> Update ()
renderBoard rs cs dboard = do
  let cboard = evalState (putCursor $ cursor dboard) dboard
      bstrings = evalState boardRep cboard
      tr = (rs `div` 2) - 5
      tups = zip bstrings [tr..]
  forM_ tups $ \(line, rno) -> do
    moveCursor rno (cs `div` 2)
    drawText $ T.pack $ line

updateCursor :: Int -> Bool -> DBoard -> DBoard
updateCursor n dir dboard | length fh == 0 && length rh == 0 = dboard
                          | dir = dboard { cursor = head fh } 
                          | otherwise = dboard { cursor = head rh }
  where cond x = isValidMove x $ board dboard
        fh = filter cond $ [n..8] ++ [0..(n-1)]
        rh = filter cond $ reverse $ [(n+1)..8] ++ [0..n]

updateBoard :: Key -> DBoard -> DBoard
updateBoard key dboard =
  let cpos = cursor dboard
      checkPos c = (c + 9) `mod` 9
  in case key of
    KeyUpArrow -> updateCursor (checkPos $ cpos - (cpos `mod` 3) - 1) False dboard
    KeyDownArrow -> updateCursor (checkPos $ cpos + 3 - (cpos `mod` 3)) True dboard
    KeyLeftArrow -> updateCursor (checkPos $ cpos - 1) False dboard
    KeyRightArrow -> updateCursor (checkPos $ cpos + 1) True dboard
    _ -> dboard

makeMove :: DBoard -> DBoard
makeMove dboard | isDrawn bd    = tdb' { status = Drawn }
                | isGameOver bd = tdb' { status = Won }
                | otherwise     = tdb' { turn = oturn }
  where tdb = dboard { board = bd }
        tdb' = tdb { cursor = cursor $ updateCursor (cursor tdb) True tdb }
        bd = placeMove (turn dboard) (cursor dboard) (board dboard)
        oturn = other $ turn dboard

makeAIMove :: DBoard -> DBoard
makeAIMove dboard = if status dboard == InProgress
                    then makeMove tdboard
                    else dboard
  where aiMove = getAIMove (board dboard)
        tdboard = dboard { cursor = aiMove }

boardEvent :: Maybe Event -> State DBoard ()
boardEvent event = do
  dboard <- get
  case event of
    Just (EventCharacter 'q') -> put (dboard { status = Abandon })
    _ -> if status dboard == InProgress
         then case event of
           Just (EventSpecialKey key) -> modify (updateBoard key)
           Just (EventCharacter ' ') -> do modify makeMove
                                           modify makeAIMove
           _ -> return ()
         else return ()

endGameMessage :: State DBoard String
endGameMessage = do
  st <- fmap status get
  winner <- fmap turn get
  case st of
    Won -> return $ winner:" has won the game."
    Drawn -> return "Game is drawn :-("
    Abandon -> return "You quitter!"
    _ -> return ""

drawMessages :: DBoard -> Update ()
drawMessages dboard = do
  let (rno, colno) = (10, 40)
      messages = ["'x' plays first.\n",
                  "Arrow keys move cursor.",
                  "Space makes a move at cursor position.",
                  "'q' exits."]
  forM_ (zip [rno..] messages) $ \(r, m) -> do
    moveCursor r colno
    drawText $ T.pack m
  endMessage <- return $ evalState endGameMessage dboard
  moveCursor 50 40
  drawText $ T.pack endMessage

showScene :: Window -> DBoard -> Curses ()
showScene w dboard = do
  (r, c) <- screenSize
  updateWindow w $ do
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    drawMessages dboard
    renderBoard r c dboard
  render
  event <- getEvent w (Just 0)
  newBoard <- return $ execState (do boardEvent event
                                     flipCursor 
                                 ) dboard
  case status newBoard of
    Abandon -> return ()
    _ -> do liftIO $ threadDelay $ 1000 * 100
            showScene w newBoard

drawTitle :: Window -> Curses ()
drawTitle w = updateWindow w $ do
  moveCursor 2 30
  drawText $ T.pack "TIC-TAC-TOE Challenge"
  moveCursor 3 30
  drawText $ T.pack "---------------------"

promptUser :: Window -> DBoard -> Curses DBoard
promptUser w dboard = do
  updateWindow w $ do
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 5 30
    drawText $ T.pack "Would you like to play first? (y/n)"
  render
  event <- getEvent w (Just 0)
  let aiMove = getAIMove (board dboard)
      aidboard = dboard { cursor = aiMove }
  case event of
    Just (EventCharacter 'y') -> return dboard
    Just (EventCharacter 'n') -> return $ execState (modify makeMove) aidboard 
    _ -> promptUser w dboard

main :: IO ()
main = do
  hSetBinaryMode stdin True
  hSetBuffering stdin NoBuffering
  let dboard = getNewDBoard
  runCurses $ do
    setEcho False
    w <- defaultWindow
    setKeypad w True
    drawTitle w
    ndboard <- promptUser w dboard
    showScene w ndboard
  putStrLn "GoodBye!"
