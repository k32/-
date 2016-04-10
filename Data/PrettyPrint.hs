{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Data.PrettyPrint (PrettyPrint(..)) where

class PrettyPrint a where
  pretty :: a -> String

instance PrettyPrint String where
  pretty a = a

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  pretty (Right a) = pretty a

-- instance (PrettyPrint a) => PrettyPrint [a] where
--   pretty = unwords . map pretty

-- instance (Show a) => PrettyPrint a where
--   pretty = show
