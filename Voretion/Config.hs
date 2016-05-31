module Voretion.Config (
    Config(..)
  ) where

data Config =
  Config {
    _decay_optional
  , _decay_multi :: Float
  , _pathToRuleset :: String
  , _cost_cost :: Float
  , _epsilon :: Float
  }
  deriving Show
