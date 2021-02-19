{-# language DerivingVia #-}
{-# language ImportQualifiedPost #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TupleSections #-}

import Control.Applicative (Alternative((<|>)))
import Control.Arrow (ArrowChoice(left))
import Control.Monad (forever)
import Data.Foldable (foldl')
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Void (Void)
import System.IO (utf8, hSetEncoding, stdout, hFlush)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

type Parser = M.Parsec Void Text
newtype Name = Name { getName :: Text }
  deriving (Eq, Show, Ord) via Text

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme MC.hspace

symbol :: Text -> Parser Text
symbol = MCL.symbol MC.hspace

name :: Parser Name
name = fmap (Name . Text.pack) $ (:) <$> MC.letterChar <*> M.many MC.numberChar

data Expr
  = Var Name
  | Not Expr
  | And [Expr]
  | Or [Expr]

expression :: Parser Expr
expression = expr
  where
    negatable :: Parser Expr -> Parser Expr
    negatable p = do
      p' <- p
      i <- M.option False (MC.char '\'' $> True)
      pure $ if i then Not p' else p'

    expr, factor, term :: Parser Expr
    expr = Or <$> lexeme term `M.sepBy1` symbol "+"
    factor = negatable $ (Var <$> name) <|> M.between (symbol "(") (symbol ")") (lexeme expr)
    term = And <$> M.some (lexeme factor)

evalExpr :: Map Name Bool -> Expr -> Either Text Bool
evalExpr assignments = go
  where
    go = \case
      Var n -> case assignments Map.!? n of
        Nothing -> Left $ "Variable not found: " <> getName n
        Just b  -> pure b
      Not e -> not <$> go e
      And es -> and <$> traverse go es
      Or es -> or <$> traverse go es

parseExpression :: Text -> Either Text (Set Name, Map Name Bool -> Either Text Bool)
parseExpression = left (Text.pack . M.errorBundlePretty) . fmap (\e -> (getVars e, flip evalExpr e)) .  M.runParser expression "expression"
  where
    getVars :: Expr -> Set Name
    getVars = \case
      Var n -> Set.singleton n
      Not e -> getVars e
      And es -> foldl' Set.union Set.empty $ map getVars es
      Or es -> foldl' Set.union Set.empty $ map getVars es

runExpr :: Set Name -> (Map Name Bool -> Either Text Bool) -> Either Text ([Name], [([Bool], Bool)])
runExpr names func =
  let headers = Set.toAscList names
      assignments = sequence $ sequence . (,[False, True]) <$> headers
      varValues = map snd <$> assignments
      results = traverse func (Map.fromList <$> assignments)
  in (headers,) . zip varValues <$> results

renderTable :: NonEmpty Name -> [([Bool], Bool)] -> Text
renderTable headers rows = Text.unlines $ renderHeaders : renderBar : map renderRow rows
  where
    renderRow (row, res) = leftPart <> " │ " <> renderBool res
      where
        renderBool b = if b then "1" else "0"
        renderItem (Name h) b =
          let b' = renderBool b
          in b' <> Text.replicate (max 0 (Text.length h - 1)) " "
        leftPart = Text.unwords $ zipWith renderItem (NE.toList headers) row
    renderHeaders = Text.unwords (map renderHeader (NE.toList headers)) <> " │ " <> "res"
      where renderHeader (Name h) = if Text.null h then " " else h
    renderBar = Text.replicate (totalHeaderWidth + 1) "─" <> "┼────"
      where
        headerWidth = max 1 . Text.length . getName
        totalHeaderWidth = NE.length headers - 1 + foldl' (+) 0 (NE.map headerWidth headers)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  forever $ do
    putStr "> " *> hFlush stdout
    l <- getLine
    Text.IO.putStrLn $ either id id $ do
      (names, table) <- uncurry runExpr =<< parseExpression (Text.pack l)
      case NE.nonEmpty names of
        Nothing -> Left "No variable names found in expression"
        Just headers -> pure $ renderTable headers table

-- vim: set ft=haskell:

