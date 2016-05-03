
module Player where

-- own modules
import Types

-- | TODO: refactor to Player.hs
getPlayer :: [Person] -> Int -> Maybe Person
getPlayer us n = case filter (\u -> perId u == n) us of
  []  -> Nothing
  x:_ -> Just x
