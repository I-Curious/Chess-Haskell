module ChessGame(main, horizontalPieceCheck) where

import Board (Board, bishopMove, getListOfPieces, kingMove, knightMove, pawnMove, queenMove, rookMove, getKingLocation)
import Data.Array
import Data.Char (chr, ord)
import Data.List
import Data.Maybe
import Data.Store
import Data.ByteString qualified
import Pieces (Color (..), Location (..), PieceCategory (..), PieceOnBoard (..), initialPieces, switchColor)
import Text.Read (readMaybe)
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = do
  putStrLn "Welcome to the Chess Game"
  putStrLn "1 to start a new Game"
  putStrLn "2 to load an existing Game"
  x <- getLine
  case readMaybe x of
    Nothing -> do 
               putStrLn "Enter Valid Choice"
    Just x -> case x of
                1 -> do
                      putStrLn "Player 1 starts with white"
                      putStrLn "Player 2 starts with Black"
                      play initialBoard White
                2 -> do
                      putStrLn "Loading saved game..."
                      byteData <- Data.ByteString.readFile "file.txt"
                      case decode byteData of
                        Left _ -> do 
                                    putStrLn "Loading game failed"
                                    putStrLn "Starting a new game"
                                    play initialBoard White
                        Right a ->do 
                                    putStrLn "Successfully loaded the game!"
                                    play loadGame color
                          where (loadGame,color) = a
                _ -> putStrLn "Invalid Input"

play :: Board -> Color -> IO ()
play b color = do
  putStrLn (draw b)
  let promptMessage = getPlayerMessage color
  currentMove <- promptForAndValidate promptMessage (\(i, j) -> isLegalMove b color (i, j))
  if currentMove == ((-1,-1),(-1,-1)) then
    do 
      Data.ByteString.writeFile "file.txt" (encode (b,color))
      putStrLn "Successfully saved the game!"
      return ()
  else do
    let updatedBoard = updateBoard b currentMove color
    if isCheckMate updatedBoard (switchColor color) (getKingLocation updatedBoard (switchColor color))
      then do
        putStrLn (draw(updatedBoard))
        putStrLn (getCheckMateMessage color)

      else
        if isCheck updatedBoard color (getKingLocation updatedBoard color)
          then do
            putStrLn (getCheckMessage color)
            play b color
          else play updatedBoard (switchColor color)

initialBoard :: Board
initialBoard = listArray ((1, 1), (8, 8)) initialPieces

draw :: Board -> String
draw board = Data.List.replicate 41 '-' ++ "\n" ++ Data.List.concat eachRow ++ Data.List.concat ["  " ++ show x ++ "  " | x <- [1 :: Integer .. 8]]
  where
    eachRow = getListOfPieces board

getPlayerMessage :: Color -> String
getPlayerMessage color = "Player " ++ playerNumer ++ "'s move"
  where
    playerNumer = if color == White then "1" else "2"

promptForAndValidate :: Read a => String -> (a -> Bool) -> IO a
promptForAndValidate msg f = do
  putStrLn userPrompt
  s <- Prelude.getLine
  case readMaybe s of
    Nothing -> do
      putStrLn errorMsg
      promptForAndValidate msg f
    Just x ->
      if f x
        then return x
        else do
          putStrLn errorMsg
          promptForAndValidate msg f
  where
    userPrompt = "Enter " ++ msg ++ ":"
    errorMsg = "!! Invalid input for " ++ msg ++ ". !!"

updateBoard :: Board -> ((Int, Int), (Int, Int)) -> Color -> Board
updateBoard board ((sx, sy), (ex, ey)) _ = board // [((sx, sy), Nothing), ((ex, ey), Just updatedPiece)]
  where
    PieceOnBoard pCol pCat _ = case board ! (sx, sy) of
      Nothing -> PieceOnBoard Black King (0, 0)
      Just x -> x
    updatedPiece = PieceOnBoard pCol pCat (ex, ey)

isCheckMate :: Board -> Color -> (Int,Int) -> Bool
isCheckMate board color (i,j) = isCheck board color (i,j) && 
                                (isCheck board color (i+1,j) || pieceColor (i+1,j) == color) &&
                                (isCheck board color (i-1,j) || pieceColor (i+1,j) == color) &&
                                (isCheck board color (i-1,j-1) || pieceColor (i+1,j) == color) && 
                                (isCheck board color (i-1,j+1) || pieceColor (i+1,j) == color) && 
                                (isCheck board color (i+1,j-1) || pieceColor (i+1,j) == color) && 
                                (isCheck board color (i+1,j+1) || pieceColor (i+1,j) == color) && 
                                (isCheck board color (i,j+1) || pieceColor (i+1,j) == color) &&
                                (isCheck board color (i,j-1) || pieceColor (i+1,j) == color)
  where pieceColor x = case board ! x of
                  Nothing -> switchColor color
                  Just (PieceOnBoard c _ _) -> c

isCheck :: Board -> Color -> (Int, Int) -> Bool
isCheck board color kingLocation = knightCheck || horizontalCheck || verticalCheck || diagonalCheck
  where
    leftHorizontalPoints = allleftHorizontalPoints board kingLocation
    rightHorizontalPoints = allRightHorizontalPoints board kingLocation
    topVerticalCheck = allTopVerticalPoints board kingLocation
    bottomVerticalCheck = allBottomVerticalPoints board kingLocation
    topLeftPoints = allTopLeftPoints board kingLocation
    topRightPoints = allTopRightPoints board kingLocation
    bottomLeftPoints = allBottomLeftPoints board kingLocation
    bottomRightPoints = allBottomRightPoints board kingLocation
    horizontalCheck = isCheckHorizontal leftHorizontalPoints color kingLocation || isCheckHorizontal rightHorizontalPoints color kingLocation
    verticalCheck = isCheckVertical topVerticalCheck color kingLocation || isCheckVertical bottomVerticalCheck color kingLocation
    diagonalCheck = isCheckDiagonal topLeftPoints color kingLocation ||
                    isCheckDiagonal topRightPoints color kingLocation ||
                    isCheckDiagonal bottomLeftPoints color kingLocation ||
                    isCheckDiagonal bottomRightPoints color kingLocation

    knightCheck = isCheckFromKnight (allKnightPoints board kingLocation) color

isCheckHorizontal :: [PieceOnBoard] -> Color -> (Int, Int) -> Bool
isCheckHorizontal [] _ _ = False
isCheckHorizontal pieces color point = horizontalPieceCheck (head pieces) color point

isCheckVertical :: [PieceOnBoard] -> Color -> (Int, Int) -> Bool
isCheckVertical [] _ _ = False
isCheckVertical pieces color point = verticalPieceCheck (head pieces) color point

isCheckDiagonal :: [PieceOnBoard] -> Color -> (Int, Int) -> Bool
isCheckDiagonal [] _ _ = False
isCheckDiagonal pieces color point = diagonalPieceCheck (head pieces) color point

allTopLeftPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allTopLeftPoints board (cx, cy) = catMaybes allTopLeftPointsOnBoard
  where
    allTopLeftPointsOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(cx-y, cy-y) | y <- [1..8]]
  
allTopRightPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allTopRightPoints board (cx, cy) = catMaybes allTopRightOnBoard
  where
    allTopRightOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(cx-y, cy+y) | y <- [1 .. 8]]

allBottomLeftPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allBottomLeftPoints board (cx, cy) = catMaybes allBottomLeftPointsOnBoard
  where
    allBottomLeftPointsOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(cx+x, cy-x) | x <- [1.. 8]]

allBottomRightPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allBottomRightPoints board (cx, cy) = catMaybes allBottomRightPointsOnBoard
  where
    allBottomRightPointsOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(cx+x, cy+x) | x <- [1.. 8]]

allTopVerticalPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allTopVerticalPoints board (cx, cy) = catMaybes allTopPointsOnBoard
  where
    allTopPointsOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(cx-x, cy) | x <- [1..8]]

allBottomVerticalPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allBottomVerticalPoints board (cx, cy) = catMaybes allBottomPointsOnBoard
  where
    allBottomPointsOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(x, cy) | x <- [cx + 1.. 8]]

horizontalPieceCheck :: PieceOnBoard -> Color -> (Int, Int) -> Bool
horizontalPieceCheck (PieceOnBoard pieceColor pieceCategory (_, py)) color (_, cy) =
  pieceColor /= color
    && ( case pieceCategory of
           Rook -> True
           Queen -> True
           King -> abs (cy - py) == 1
           _ -> False
       )

verticalPieceCheck :: PieceOnBoard -> Color -> (Int, Int) -> Bool
verticalPieceCheck (PieceOnBoard pieceColor pieceCategory (px, _)) color (cx, _) =
  pieceColor /= color
    && ( case pieceCategory of
           Rook -> True
           Queen -> True
           King -> abs (cx - px) == 1
           _ -> False
       )

diagonalPieceCheck :: PieceOnBoard -> Color -> (Int, Int) -> Bool
diagonalPieceCheck (PieceOnBoard pieceColor pieceCategory (px, py)) color (cx, cy) =
  pieceColor /= color
    && ( case pieceCategory of
           Bishop -> True
           Queen -> True
           King -> abs (cx - px) == 1 && abs (cy - py) == 1
           _ -> False
       )

allleftHorizontalPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allleftHorizontalPoints board (cx, cy) = catMaybes allHorizontalPointsOnBoard
  where
    allHorizontalPointsOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(cx, y) | y <- [cy-1 .. 1]]

allRightHorizontalPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allRightHorizontalPoints board (cx, cy) = catMaybes allHorizontalPointsOnBoard
  where
    allHorizontalPointsOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(cx, y) | y <- [cy + 1 .. 8]]

allKnightPoints :: Board -> (Int, Int) -> [PieceOnBoard]
allKnightPoints board (cx, cy) = catMaybes allRookPointsOnBoard
  where
    allRookPointsOnBoard = map (\(x, y) -> board ! (x, y)) listOfValidLocation
    listOfValidLocation = filter isWithinBoundary [(cx + 2, cy - 1), (cx + 2, cy + 1), (cx - 2, cy - 1), (cx - 2, cy + 1), (cx + 1, cy + 2), (cx + 1, cy - 2), (cx - 1, cy + 2), (cx + 1, cy + 2)]

isCheckFromKnight :: [PieceOnBoard] -> Color -> Bool
isCheckFromKnight [] _ = False
isCheckFromKnight (PieceOnBoard pieceColor pieceCategory _ : xs) color = pieceColor /= color && pieceCategory == Knight || isCheckFromKnight xs color

isWithinBoundary :: (Int, Int) -> Bool
isWithinBoundary (sx, sy) = sx >= 1 && sx <= 8 && sy >= 1 && sy <= 8

callPieceCheck :: PieceCategory -> Board -> (Int, Int) -> (Int, Int) -> Bool
callPieceCheck piece =
  case piece of
    Pawn -> pawnMove
    Knight -> knightMove
    Bishop -> bishopMove
    Rook -> rookMove
    Queen -> queenMove
    King -> kingMove

isLegalMove :: Board -> Color -> ((Int, Int), (Int, Int)) -> Bool
isLegalMove board color ((sx, sy), (ex, ey)) = (sx == -1 && sy == -1 && ex == -1 && ey == -1) ||
  (isWithinBoundary (sx, sy) && isWithinBoundary (ex, ey))
    && ( case board ! (sx, sy) of
           Nothing -> False
           Just (PieceOnBoard pColor pieceCategory _) -> pColor == color && callPieceCheck pieceCategory board (sx, sy) (ex, ey)
       )

getCheckMateMessage :: Color -> String
getCheckMateMessage color = "Check Mate! Player " ++ playerNumber ++ " wins"
  where
    playerNumber = if color == White then "1" else "2"

getCheckMessage :: Color -> String
getCheckMessage color = "Check! Player " ++ playerNumber ++ " to make sure they get their king to safety"
  where
    playerNumber = if color == White then "1" else "2"
