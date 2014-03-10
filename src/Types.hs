module Types where


import Data.Word
import Data.Char
import Data.List
import Data.List.Split

import Data.Bits
import Control.Monad

import Test.QuickCheck
import Control.Applicative


data PieceType = Elephant | Camel | Horse | Dog | Cat | Rabbit deriving (Ord, Eq, Show, Enum, Bounded)


newtype Square = Square { unSq :: Word8 } deriving (Eq, Ord)


  
data Colour = Gold | Silver deriving (Show, Ord, Eq, Enum, Bounded)  
type Piece = (PieceType, Colour)
  
data Board = Board { 
  _bMoves   :: (MoveNumber, [Move]),
  _bPieces  :: [Maybe Piece]
} deriving Eq
  
data Direction = North | East | South | West deriving (Show, Ord, Eq, Enum, Bounded)
  

data Move = Move { 
  _mvPiece  :: Piece,
  _mvSquare :: Square,    
  _mvDirection :: Direction 
} deriving (Eq)
  

data MoveNumber = MoveNumber Int Colour deriving (Eq, Ord)

instance Show MoveNumber where
  show (MoveNumber n c) = show n ++ [colourChar c] where
    colourChar Silver = 's'
    colourChar Gold = 'g'

instance Show Square where
  show (Square n) = [chr (ord 'a' + col), chr (ord '1' + rank)] where
    (rank, col) = (fromIntegral n) `divMod` 8

    
typeChar :: PieceType -> Char
typeChar  Elephant = 'E'
typeChar  Camel    = 'M'
typeChar  Horse    = 'H'
typeChar  Dog      = 'D'
typeChar  Cat      = 'C'
typeChar  Rabbit   = 'R'

pieceChar :: Piece -> Char
pieceChar  (t, Gold) = typeChar t
pieceChar  (t, Silver) = toLower (typeChar t)

dirChar :: Direction -> Char
dirChar North = 'n'
dirChar South = 's'
dirChar East  = 'e'
dirChar West  = 'w'
 
concatWith :: [a] -> [[a]] -> [a]
concatWith joiner = concat . intersperse joiner

showMoves :: (MoveNumber, [Move]) -> String
showMoves (n, moves) = show n ++ " " ++ (concatWith " " . map show $ moves)
 
instance Show Board where
  show (Board moves pieces) = 
    showMoves moves ++ "\n" 
    ++ " +-----------------+\n"
    ++ concat (zipWith showLine [8,7..1] pieces')
    ++ " +-----------------+\n"
    ++ "   a b c d e f g h\n"
  
    where
    
      showLine n pieces = show n ++ "| " ++ showPieces pieces ++ " |\n"
      showPieces = concatWith " " . map showPiece 
      
      showPiece (Nothing) = " "
      showPiece (Just piece) = [pieceChar piece]
      
      pieces' = (chunksOf 8) pieces
    


instance Show Move where
  show (Move p s d) = pieceChar p : show s ++ [dirChar d]
  
  
instance Arbitrary Colour where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary MoveNumber where
  arbitrary = MoveNumber <$> ((1 + ) . abs <$> arbitrary) <*> arbitrary
    
instance Arbitrary Direction where
  arbitrary = arbitraryBoundedEnum
  
instance Arbitrary PieceType where
  arbitrary = arbitraryBoundedEnum
  
instance Arbitrary Square where
  arbitrary = Square . (`mod` 64) <$> arbitrary   
 
instance Arbitrary Move where
  arbitrary = Move <$> arbitrary <*> arbitrary <*> arbitrary
  
  
instance Arbitrary Board where
  arbitrary = do
    n <- arbitrary 
    moveLine <- (,) <$> arbitrary <*> replicateM (n `mod` 4) arbitrary
    pieces   <- replicateM 64 arbitrary
    return $ Board moveLine pieces
    
    