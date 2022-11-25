module Sgf
  ( parseSgf,
  )
where

import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree (Tree (..))
import Text.Trifecta

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf =
  case parseString (parens sgfTree) mempty (Text.unpack sgf) of
    Success result -> Just result
    Failure _ -> Nothing

sgfTree :: Parser SgfTree
sgfTree = Node <$> sgfNode <*> (count 1 sgfTree <|> many (parens sgfTree))

sgfNode :: Parser SgfNode
sgfNode = char ';' *> (Map.fromList <$> many sgfProperty)

sgfProperty :: Parser (Text, [Text])
sgfProperty =
  (,)
    <$> (Text.pack <$> some upper)
    <*> (map Text.pack <$> some (brackets sgfValue))

sgfValue :: Parser String
sgfValue = concat <$> some sanitizedChar
  where
    sanitizedChar = escapedChar <|> (sanitize <$> notChar ']')

    escapedChar = char '\\' *> (escape <$> anyChar)

    escape '\n' = ""
    escape c = sanitize c

    sanitize '\n' = "\n"
    sanitize '\t' = " "
    sanitize c = [c]
