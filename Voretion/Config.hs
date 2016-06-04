module Voretion.Config (
    Config(..)
  , Engine(..)
  ) where

data Config =
  Config {
    _decay_optional
  , _decay_multi :: Float
  , _pathToRuleset :: String
  , _cost_cost :: Float
  , _epsilon :: Float
  , _engine :: Engine
  }
  deriving Show

data Engine = Deterministic | MonteCarlo
  deriving (Show, Read)
