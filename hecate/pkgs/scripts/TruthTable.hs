{-# language DerivingVia #-}
{-# language ImportQualifiedPost #-}
{-# language LambdaCase #-}
{-# language TupleSections #-}

import Data.Char (isSpace)
import Data.Functor (($>))
import Data.List
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void)
import System.Console.Haskeline qualified as H
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char qualified as MC
import Text.Megaparsec.Char.Lexer qualified as MCL

type Parser = M.Parsec Void String
newtype Name = Name { getName :: String }
  deriving (Eq, Ord) via String

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme MC.hspace

symbol :: String -> Parser String
symbol = MCL.symbol MC.hspace

name :: Parser Name
name = fmap Name $ (:) <$> MC.letterChar <*> M.many MC.numberChar

data Expr
  = Var Name
  | Not Expr
  | And [Expr]
  | Or [Expr]
  | Lit Bool

expression :: Parser Expr
expression = expr
  where
    negatable :: Parser Expr -> Parser Expr
    negatable p = do
      p' <- p
      i <- M.option False (MC.char '\'' $> True)
      pure $ if i then Not p' else p'

    expr, factor, term :: Parser Expr
    expr = Or <$> lexeme term `M.sepBy1` lexeme (MC.char '+' M.<|> MC.char '|')
    factor = negatable $ M.choice
      [ MC.string "0" $> Lit False
      , MC.string "1" $> Lit True
      , Var <$> name
      , M.between (symbol "(") (symbol ")") (lexeme expr)
      ]
    term = And <$> lexeme factor `M.sepBy1` M.optional (lexeme (MC.char '*' M.<|> MC.char '&'))

line :: Parser (Expr, Expr)
line = (,) <$> expression <*> M.option (Lit False) (symbol ";" *> expression)

evalExpr :: Map Name Bool -> Expr -> Either String Bool
evalExpr assignments = go
  where
    go = \case
      Var n -> case assignments Map.!? n of
        Nothing -> Left $ "Variable not found: " <> getName n
        Just b  -> pure b
      Not e -> not <$> go e
      And es -> and <$> traverse go es
      Or es -> or <$> traverse go es
      Lit b -> pure b

evalLine :: Map Name Bool -> (Expr, Expr) -> Either String (Maybe Bool)
evalLine assignments (expr, dontCares) = do
  dontCare <- evalExpr assignments dontCares
  if dontCare
  then pure Nothing
  else Just <$> evalExpr assignments expr

parseLine :: String -> Either String (Set Name, Map Name Bool -> Either String (Maybe Bool))
parseLine input = case M.runParser (line <* M.eof) "expression" input of
    Left e -> Left $ M.errorBundlePretty e
    Right (expr, dontCares) -> Right (getVars expr `Set.union` getVars dontCares, flip evalLine (expr, dontCares))
  where
    getVars :: Expr -> Set Name
    getVars = \case
      Var n -> Set.singleton n
      Not e -> getVars e
      And es -> foldl' Set.union Set.empty $ map getVars es
      Or es -> foldl' Set.union Set.empty $ map getVars es
      Lit _ -> mempty

runExpr :: Set Name -> (Map Name Bool -> Either String (Maybe Bool)) -> Either String ([Name], [([Bool], Maybe Bool)])
runExpr names func =
  let headers = Set.toAscList names
      assignments = sequence $ sequence . (,[False, True]) <$> headers
      varValues = map snd <$> assignments
      results = traverse func (Map.fromList <$> assignments)
  in (headers,) . zip varValues <$> results

renderTable :: NonEmpty Name -> [([Bool], Maybe Bool)] -> String
renderTable headers rows = unlines $ renderHeaders : renderBar : map renderRow rows
  where
    renderBool b = if b then "1" else "0"
    renderOutput = maybe "X" renderBool
    renderRow (row, res) = leftPart <> " │ " <> renderOutput res
      where
        renderItem (Name h) b =
          let b' = renderBool b
          in b' <> replicate (max 0 (length h - 1)) ' '
        leftPart = unwords $ zipWith renderItem (NE.toList headers) row
    renderHeaders = unwords (map renderHeader (NE.toList headers)) <> " │ " <> "res"
      where renderHeader (Name h) = if null h then " " else h
    renderBar = replicate (totalHeaderWidth + 1) '─' <> "┼────"
      where
        headerWidth = max 1 . length . getName
        totalHeaderWidth = NE.length headers - 1 + foldl' (+) 0 (NE.map headerWidth headers)

main :: IO ()
main = H.runInputT H.defaultSettings loop
  where
    trim = dropWhile isSpace . dropWhileEnd isSpace
    prompt = fmap trim <$> H.getInputLine "> "
    handle input = H.outputStrLn $ either id id $ do
      (names, table) <- uncurry runExpr =<< parseLine input
      case NE.nonEmpty names of
        Nothing -> Left "No variable names found in expression"
        Just headers -> pure $ renderTable headers table
    loop = prompt >>= \case
      Nothing -> pure ()
      Just input
        | input == "" -> loop
        | input == ":q" || input == ":quit" -> pure ()
        | otherwise -> handle input *> loop

