module Parser where



import Text.Parsec hiding (space)
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char hiding (space)

import Data.Char
import Types


import Control.Applicative hiding (many, (<|>))

import Test.QuickCheck

square :: Parser Square
square = (toSquare <$> oneOf "abcdefgh" <*> digit)  <?> "square" where
  
  toSquare :: Char -> Char -> Square
  toSquare col rank = Square . fromIntegral $ (ord col - ord 'a') + (8 * (ord rank - ord '1'))

piece :: Parser Piece
piece = (toPiece <$> oneOf "rcdhmeRCDHME") <?> "piece"  where
  toPiece c = (pieceType (toLower c), if isLower c then Silver else Gold)
  
  pieceType 'r' = Rabbit
  pieceType 'c' = Cat
  pieceType 'd' = Dog
  pieceType 'h' = Horse
  pieceType 'm' = Camel
  pieceType 'e' = Elephant
   
  

dir :: Parser Direction
dir = (toDir <$> oneOf "nwse") <?> "direction"  where
  toDir 'n' = North
  toDir 's' = South
  toDir 'e' = East
  toDir 'w' = West
  
  
move :: Parser Move
move = (Move <$> piece <*> square <*> dir) <?> "move"


maybeParse :: Parser a -> String -> Maybe a
maybeParse p input = case parse p "" input of
                          Left _  -> Nothing
                          Right x -> Just x


colour :: Parser Colour
colour = (toColour <$> oneOf "gswb") <?> "colour" where
  toColour 'w' = Gold
  toColour 'g' = Gold
  toColour 's' = Silver 
  toColour 'b' = Silver
  
int :: Parser Int
int = read <$> many1 digit 
  
  

moveNumber :: Parser MoveNumber
moveNumber = MoveNumber <$> int <*> colour

space :: Parser Char
space = (char ' ')

moveLine :: Parser (MoveNumber, [Move])
moveLine = (,) <$> moveNumber <*> (space *> move `sepBy` space)

edge :: Parser ()
edge = (string " +-----------------+" >> return ()) <?> "board edge"

boardCols :: Parser ()
boardCols = (string "   a b c d e f g h" >> return ()) <?> "column labels" 
 
boardLine :: Int -> Parser [Maybe Piece]
boardLine n = (do 
  char number >> pipe >> space 
  many1 (boardSquare <* space)  <* char '|') <?> "board rank " ++ (show n)
  
  where
    number = chr $ ord '0' + n
    pipe = char '|' <?> "board edge"
    
    boardSquare = ((Just <$> piece) <|> const Nothing <$> oneOf "xX ")  <?> "board square"
    
    
    

board :: Parser Board 
board = do
  moves <- moveLine <* newline
  edge <* newline
  
  pieces <- mapM (\n -> boardLine n <* newline) [8, 7..1]  
  edge <* newline
  boardCols
  
  return $ Board moves (concat pieces)


prop_moveIdentity m = Just m == maybeParse move (show m)
prop_boardIdentity b = Just b == maybeParse board (show b)
