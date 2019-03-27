module Task6
  ( e1
  , e2
  ) where

import Data.Maybe (mapMaybe)
import Task1 (distributivity)

-- WHNF: ( Left ("harold" ++ " hide " ++ "the " ++ "pain"))
--       , Left ("harold" ++ " hide " ++ "the " ++ "pain")))
e1 :: (Either [Char] b, Either [Char] c)
e1 = distributivity (Left ("harold" ++ " hide " ++ "the " ++ "pain"))

-- WHNF: False
e2 :: Bool
e2 = null $ mapMaybe foo "pole chudes ochen' chudesno"

foo :: Char -> Maybe Double
foo char =
  case char == 'o' of
    True  -> Just $ exp pi
    False -> Nothing
