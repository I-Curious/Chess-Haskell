module Board (Board (..), getListOfPieces, pawnMove, rookMove, kingMove, queenMove, bishopMove, knightMove, getKingLocation) where

import Data.Array
import Data.Maybe (isJust, isNothing, catMaybes)
import Data.Store
import Pieces (Color (..), Location (..), PieceOnBoard (..), PieceCategory (..), getPieceStrSymbol)

type Board = Array Location (Maybe PieceOnBoard)

getKingLocation :: Board -> Color -> (Int, Int)
getKingLocation board color = filterKingPiece listOfPieces color
  where
    listOfPieces = getAllThePiecesFromBoard board
    filterKingPiece :: [PieceOnBoard] -> Color -> (Int, Int)
    filterKingPiece [] _ = (-1, -1)
    filterKingPiece (PieceOnBoard pCol pCat pLoc : xs) side = if pCol == color && pCat == King then pLoc else filterKingPiece xs side

getAllThePiecesFromBoard :: Board -> [PieceOnBoard]
getAllThePiecesFromBoard board = catMaybes (filter isJust [board ! (x, y) | x <- [1 .. 8], y <- [1 .. 8]])

getListOfPieces :: Board -> [String]
getListOfPieces board =
  [ if y == 8
      then
        positionRepresentation x y ++ "| " ++ show (x) ++ "\n"
          ++ replicate 41 '-'
          ++ "\n"
      else positionRepresentation x y
    | x <- [1 .. 8],
      y <- [1 .. 8]
  ]
  where
    positionRepresentation x y = case board ! (x, y) of
      Nothing -> "|    "
      Just (PieceOnBoard color catagory _) -> getPieceStrSymbol color catagory

checkDestCoOrd :: Board -> (Int, Int) -> (Int, Int) -> Bool
checkDestCoOrd board (sx, sy) (ex, ey)= isNothing color' || color /= color'
  where
    color' = case board ! (ex, ey) of
      Just (PieceOnBoard pc _ _) -> Just pc
      _ -> Nothing

    color = case board ! (sx, sy) of
      Just (PieceOnBoard pc _ _) -> Just pc
      _ -> Nothing

checkIfAnyPiecesInLine :: Board -> (Int, Int) -> (Int, Int) -> Bool
checkIfAnyPiecesInLine board (sx, sy) (ex, ey)
  | sx == ex && ey > sy = and [isNothing (board ! (sx, y)) | y <- [sy + 1 .. ey - 1]]
  | sx == ex && ey < sy = and [isNothing (board ! (sx, y)) | y <- [sy - 1 .. ey + 1]]
  | sy == ey && ex > sx = and [isNothing (board ! (x, sy)) | x <- [sx + 1 .. ex - 1]]
  | sy == ey && ex < sx = and [isNothing (board ! (x, sy)) | x <- [sx - 1 .. ex + 1]]
  | otherwise = False

checkIfAnyPiecesInDiag :: Board -> (Int, Int) -> (Int, Int) -> Bool
checkIfAnyPiecesInDiag board (sx, sy) (ex, ey)
  | sx > ex && ey > sy = and [isNothing (board ! (x, y)) | x <- [sx + 1 .. ex - 1], y <- [sy + 1 .. ey - 1]]
  | sx > ex && ey < sy = and [isNothing (board ! (x, y)) | x <- [sx + 1 .. ex - 1], y <- [sy - 1 .. ey + 1]]
  | sx < ex && ey > sy = and [isNothing (board ! (x, y)) | x <- [sx - 1 .. ex + 1], y <- [sy + 1 .. ey - 1]]
  | sx < ex && ey < sy = and [isNothing (board ! (x, y)) | x <- [sx - 1 .. ex + 1], y <- [sy - 1 .. ey + 1]]
  | otherwise = False

pawnMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
pawnMove board (sx, sy) (ex, ey) =
  (isNothing color2 && (t sx == ex) && (sy == ey))
    || (isJust color2 && color2 /= color && (t sx == ex) && ((sy == ey + 1) || (sy == ey - 1)))
  where
    color = case board ! (sx, sy) of
      Just (PieceOnBoard pCol _ _) -> Just pCol
      _ -> Nothing
    color2 = case board ! (ex, ey) of
      Just (PieceOnBoard pCol _ _) -> Just pCol
      _ -> Nothing
    t = case color of
      Just White -> pred
      Just Black -> succ
      Nothing -> pred

kingMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
kingMove board (sx, sy) (ex, ey) = movementCheck && destCoOrdCheck
  where
    destCoOrdCheck = checkDestCoOrd board (sx, sy) (ex, ey)
    movementCheck = kingMovementCheck (sx, sy) (ex, ey)

kingMovementCheck :: (Int, Int)  -> (Int, Int) -> Bool
kingMovementCheck (sx, sy) (ex, ey) 
  | sx == (ex + 1) || sx == (ex - 1) = (sy - ey) `elem` [-1, 0, 1]
  | sx == ex = (sy - ey) `elem` [-1, 1]
  | otherwise = False
  
queenMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
queenMove board (sx, sy) (ex, ey) =  movementCheck && checkIntermediate && checkDestination
  where
    movementCheck = queenMovementCheck (sx, sy) (ex, ey)
    checkIntermediate = if sx == ex || sy == ey then checkIfAnyPiecesInLine board (sx, sy) (ex, ey) else checkIfAnyPiecesInDiag board (sx, sy) (ex, ey)
    checkDestination = checkDestCoOrd board (sx, sy) (ex, ey)

queenMovementCheck :: (Int, Int) -> (Int, Int) -> Bool
queenMovementCheck (sx, sy) (ex, ey) = bishopTypeMove || rookTypeMove
  where 
    bishopTypeMove = bishopMovementCheck (sx, sy) (ex, ey)
    rookTypeMove = rookMoveMentCheck (sx, sy) (ex, ey)

bishopMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
bishopMove board (sx, sy) (ex, ey) =  movementCheck && checkIntermediate && checkDestination
  where
    movementCheck = bishopMovementCheck (sx, sy) (ex, ey)
    checkIntermediate = checkIfAnyPiecesInDiag board (sx, sy) (ex, ey)
    checkDestination = checkDestCoOrd board (sx, sy) (ex, ey)

bishopMovementCheck :: (Int, Int) -> (Int, Int) -> Bool
bishopMovementCheck (sx, sy) (ex, ey) = abs xDiff == abs yDiff
  where
    xDiff = sx - ex
    yDiff = sy - ey

knightMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
knightMove board (sx, sy) (ex, ey) = movementCheck && checkDestination
  where
    movementCheck = knightMovementCheck (sx, sy) (ex, ey)
    checkDestination = checkDestCoOrd board (sx, sy) (ex, ey)

knightMovementCheck :: (Int, Int) -> (Int, Int) -> Bool
knightMovementCheck (sx, sy) (ex, ey) 
  | sx == (ex-2) || sx == (ex+2) = (sy == ey-1) || (sy == ey+1)
  | sy == (ey+2) || sy == (ey-2) = (sx == ex-1) || (sx == ex+1)
  | otherwise = False

rookMove :: Board -> (Int, Int) -> (Int, Int) -> Bool
rookMove board (sx, sy) (ex, ey) = movementCheck && checkIntermediate && checkDestination
  where
    movementCheck = rookMoveMentCheck (sx, sy) (ex, ey)
    checkIntermediate = checkIfAnyPiecesInLine board (sx, sy) (ex, ey)
    checkDestination = checkDestCoOrd board (sx, sy) (ex, ey)

rookMoveMentCheck :: (Int, Int) -> (Int, Int) -> Bool
rookMoveMentCheck (sx, sy) (ex, ey )= sx == ex || sy == ey