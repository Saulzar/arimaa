module Board where

import Data.Word 


data BoardPieces = BoardPieces {
  _bRabbits :: Word64,
  _bCats    :: Word64,
  _bDog     :: Word64,
  _bHorses  :: Word64,
  _bCamels  :: Word64,
  _bElephants :: Word64,
  _bAll       :: Word64
}
  


data BitBoard = BitBoard {
  
  _bGold :: BoardPieces,
  _bSilver :: BoardPieces,
  _bAll    :: BoardPieces,
    
  bFrom :: Square,
  bPiece :: PieceType
  
}