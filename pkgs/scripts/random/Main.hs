{-# language RecordWildCards #-}

import Data.List (genericTake, unfoldr)
import System.Environment (getArgs)
import qualified System.Random.SplitMix as SM
import Options.Applicative

data Args = Args
  { _argsCount :: Integer
  , _argsLow   :: Integer
  , _argsHigh  :: Integer
  }

args :: Parser Args
args = Args
  <$> option auto
    ( long "count"
    <> short 'c'
    <> value 1
    <> help "The amount of numbers to generate (default: 1)"
    )
  <*> option auto
    ( long "lower"
    <> short 'l'
    <> value 1
    <> help "A lower bound for numbers (default: 1)"
    )
  <*> option auto
    ( long "upper"
    <> short 'u'
    <> help "An upper bound for numbers"
    )

parseArgs :: IO Args
parseArgs = customExecParser opts $ info (args <**> helper) mempty
  where opts = prefs showHelpOnError

randomRs :: Integer -> Integer -> SM.SMGen -> [Integer]
randomRs low high = unfoldr (Just . SM.nextInteger low high)

main :: IO ()
main = do
  Args{..} <- parseArgs
  nums <- genericTake _argsCount . randomRs _argsLow _argsHigh <$> SM.newSMGen
  putStr $ unlines $ map show nums

-- vim: set ft=haskell:
