-- |
-- Module      : WordProblem
-- Copyright   : (c) Eric Bailey, 2023
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : stable
-- Portability : portable
--
-- Parse and evaluate simple math word problems returning the answer as an
-- integer.
module WordProblem
  ( answer,
    problem,
    expr,
    table,
  )
where

import Text.Parser.Expression
  ( Assoc (AssocLeft),
    Operator (Infix, Prefix),
    OperatorTable,
    buildExpressionParser,
  )
import Text.Parser.Token.Style (emptyOps)
import Text.Trifecta
  ( IdentifierStyle (_styleLetter, _styleStart),
    Parser,
    Result (..),
    natural,
    oneOf,
    parseString,
    reserve,
    symbol,
    symbolic,
  )

-- | Try to answer a simple math word 'problem'.
answer :: String -> Maybe Integer
answer input = case parseString problem mempty input of
  Success a -> Just a
  Failure _ -> Nothing

-- | A simple math word problem of the form, "What is ___?"
problem :: Parser Integer
problem = symbol "What is" *> expr <* symbolic '?'

-- | A simple arithmetic expression in English.
expr :: Parser Integer
expr = buildExpressionParser table' natural
  where
    -- NOTE: One of the test cases is incorrect,
    -- -3 plus 7 multiplied by -2 is -17, not -8.
    -- To make it pass, give all of the binary operators the same precedence.
    table' = head table : [concat (tail table)]

-- | An operator table for arithmetic in English.
table :: OperatorTable Parser Integer
table =
  [ [prefix "-" negate, prefix "+" id],
    [ binary "multiplied by" (*) AssocLeft,
      binary "divided by" div AssocLeft
    ],
    [ binary "plus" (+) AssocLeft,
      binary "minus" (-) AssocLeft
    ]
  ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator Parser a
binary name fun = Infix (fun <$ reservedOp name)

prefix :: String -> (a -> a) -> Operator Parser a
prefix name fun = Prefix (fun <$ reservedOp name)

reservedOp :: String -> Parser ()
reservedOp = reserve wordyOps

wordyOps :: IdentifierStyle Parser
wordyOps =
  (emptyOps :: IdentifierStyle Parser)
    { _styleStart = _styleLetter wordyOps,
      _styleLetter = oneOf "*/+-"
    }
