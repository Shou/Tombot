
{-# LANGUAGE GADTs #-}

module Tombot.Parser.Bot where


-- {{{ Imports
import Control.Applicative

import qualified Data.Attoparsec.Text as Atto
import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as Vec

import Debug.Trace
-- }}}


-- {{{ Data
data Syntax where
  Operator :: Syntax -> Text -> Syntax -> Syntax
  -- ^ Operators
  Let :: Text -> Syntax -> Syntax
  -- ^ Let variables
  Lambda :: Text -> Syntax -> Syntax -> Syntax
  -- ^ Lambda functions
  Parens :: Syntax -> Syntax
  -- ^ Variable scope
  Value :: Text -> Syntax
  -- ^ A constant value

deriving instance Show Syntax
-- }}}


takeName = Atto.takeWhile1 isAlphaNum

skipSpaces1 = Atto.skipWhile isSpace

skipSpaces = skipSpaces1 <|> return ()

surround :: Atto.Parser a -> Atto.Parser b -> Atto.Parser b
surround wrap center = wrap *> center <* wrap

spacesAround :: Atto.Parser a -> Atto.Parser a
spacesAround = surround skipSpaces


-- | Main entry point to syntax parser
syntax :: Atto.Parser Syntax
syntax = spacesAround
       $ Atto.choice [ operator, value ]


variable :: Atto.Parser Syntax
variable = do
    Atto.string "let"
    arg <- spacesAround takeName
    Atto.char '='
    Let arg <$> syntax

lambda :: Atto.Parser Syntax
lambda = do
    Atto.char '\\'
    arg <- spacesAround takeName
    Atto.string "->"
    skipSpaces
    Lambda arg <$> syntax <*> syntax

parens = do
    Atto.char '('
    skipSpaces
    Parens <$> syntax <* skipSpaces <* Atto.char ')'

operator :: Atto.Parser Syntax
operator = do
    left <- syntax
    name <- spacesAround $ Atto.takeWhile1 isSymbol
    right <- syntax

    return $ Operator left name right

value :: Atto.Parser Syntax
value = Value <$> Atto.takeWhile1 (not . isSpace)

