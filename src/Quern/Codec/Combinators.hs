{-# Language FlexibleContexts #-}

module Quern.Codec.Combinators
  ( spaces1
  , hspace
  , hspaces
  , hspaces1
  , end
  , lexeme
  , int
  , float
  , sign
  , nat
  , expo
  , frac
  , restOfLine
  , prefixed
  , float0
  , int0
  , vec2
  , vec3
  , index
  , face
  ) where

import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Foreign
import Linear hiding (el)
import Text.Parsec

-- parser


{-# INLINE spaces1 #-}
spaces1 :: Stream s m Char => ParsecT s u m ()
spaces1 = space *> spaces

{-# INLINE hspaces #-}
{-# INLINE hspace #-}
hspace, hspaces, hspaces1 :: Stream s m Char => ParsecT s u m ()
hspace = oneOf " \t" *> pure ()
hspaces = many hspace *> pure ()
hspaces1 = hspace *> hspaces

{-# INLINE end #-}
end :: Stream s m Char => ParsecT s u m ()
end = eof <|> (hspaces *> many1 endOfLine) *> pure ()

{-# INLINE lexeme #-}
lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme p = p <* hspaces

{-# INLINE int #-}
int :: Stream s m Char => ParsecT s u m Int32
int = sign <*> nat

{-# INLINE float #-}
float :: Stream s m Char => ParsecT s u m Float
float = sign <*> (nat >>= frac)

{-# INLINE sign #-}
sign :: (Num i, Stream s m Char) => ParsecT s u m (i -> i)
sign =  char '-' *> pure negate
    <|> char '+' *> pure id
    <|> pure id

{-# INLINE nat #-}
nat :: (Num i, Stream s m Char) => ParsecT s u m i
nat = fromIntegral . accum <$> many1 digit
  where accum = foldl' (\x d -> 10 * x + digitToInt d) (0 :: Int)

{-# INLINE expo #-}
expo :: Stream s m Char => ParsecT s u m Float
expo =  oneOf "Ee" *> ((10**) . fromIntegral <$> int)
    <|> pure 1

{-# INLINE frac #-}
frac :: Stream s m Char => Int32 -> ParsecT s u m Float
frac x =  char '.' *> (collect <$> many1 digit <*> expo) -- does not handle 1.e10
      <|> pure y
  where
    y = fromIntegral x
    collect :: String -> Float -> Float
    collect s e = e * (y + frc)
      where frc = foldr (\d acc -> (acc + fromIntegral (digitToInt d)) / 10) 0 s

{-# INLINE restOfLine #-}
restOfLine :: Stream s m Char => ParsecT s u m String
restOfLine = many (noneOf "\n\r")

-- some obj-specific things
{-# INLINE prefixed #-}
prefixed :: Stream s m Char => String -> ParsecT s u m a -> ParsecT s u m a
prefixed s p = hspaces *> try (string s) *> spaces1 *> p -- <* rest

-- defaulting to zero
{-# INLINE float0 #-}
float0 :: Stream s m Char => ParsecT s u m Float
float0 = lexeme float <|> pure 0

{-# INLINE int0 #-}
int0 :: Stream s m Char => ParsecT s u m Int32
int0 = int <|> pure 0

-- general vectors
{-# INLINE vec2 #-}
vec2 :: Stream s m Char => ParsecT s u m (V2 Float)
vec2 = (V2 <$> lexeme float <*> float0) <* (optional float)

{-# INLINE vec3 #-}
vec3 :: Stream s m Char => ParsecT s u m (V3 Float)
vec3 = V3 <$> lexeme float <*> float0 <*> float0

{-# INLINE index #-}
index :: Stream s m Char => ParsecT s u m (V3 Int32)
index = V3 <$> int <*> (option 0 (char '/' *> int0)) <*> (option 0 (char '/' *> int0))

{-# INLINE face #-}
face :: Stream s m Char => ParsecT s u m [V3 Int32]
face = many1 (lexeme index)
