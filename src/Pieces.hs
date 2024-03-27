module Pieces (Location (..), PieceOnBoard (..), PieceCategory (..), Color (..), switchColor, getPieceStrSymbol, initialPieces) where

import Data.Array
import Data.Store
import GHC.Generics

type Location = (Int, Int)

data PieceCategory = Pawn | Knight | Bishop | Rook | Queen | King deriving (Show, Read, Eq, Ord, Store, GHC.Generics.Generic)

data Color = Black | White deriving (Eq, Show, Store, GHC.Generics.Generic)

data PieceOnBoard = PieceOnBoard Color PieceCategory Location
  deriving (Show, Eq, Store, GHC.Generics.Generic)

switchColor :: Color -> Color
switchColor Black = White
switchColor White = Black

getPieceSymbol :: String -> String
getPieceSymbol val
  | val == "bK" = "♔"
  | val == "bQ" = "♕"
  | val == "bR" = "♖"
  | val == "bB" = "♗"
  | val == "bN" = "♘"
  | val == "bP" = "♙"
  | val == "wK" = "♚"
  | val == "wQ" = "♛"
  | val == "wR" = "♜"
  | val == "wB" = "♝"
  | val == "wN" = "♞"
  | otherwise = "♟︎"

getPieceStrSymbol :: Color -> PieceCategory -> String
getPieceStrSymbol color piece =
  case color of
    Black -> case piece of
      Pawn -> "| bP "
      Bishop -> "| bB "
      Rook -> "| bR "
      King -> "| bK "
      Queen -> "| bQ "
      Knight -> "| bN "
    White -> case piece of
      Pawn -> "| wP "
      Bishop -> "| wB "
      Rook -> "| wR "
      King -> "| wK "
      Queen -> "| wQ "
      Knight -> "| wN "

initialPieces :: [Maybe PieceOnBoard]
initialPieces =
  [Just (PieceOnBoard Black (piecesInitial !! (x - 1)) (1, x)) | x <- [1 .. 8]]
    ++ [Just (PieceOnBoard Black Pawn (x, 2)) | x <- [1 .. 8]]
    ++ [Nothing | _ <- [1 :: Integer .. 8]]
    ++ [Nothing | _ <- [1 :: Integer .. 8]]
    ++ [Nothing | _ <- [1 :: Integer .. 8]]
    ++ [Nothing | _ <- [1 :: Integer .. 8]]
    ++ [Just (PieceOnBoard White Pawn (x, 7)) | x <- [1 .. 8]]
    ++ [Just (PieceOnBoard White (piecesInitial !! (x - 1)) (8, x)) | x <- [1 .. 8]]
  where
    piecesInitial = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
