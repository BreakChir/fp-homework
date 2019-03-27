module Block3_1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving (Show)

intToDay :: Int -> Day
intToDay x
  | x == 0    = Monday
  | x == 1    = Tuesday
  | x == 2    = Wednesday
  | x == 3    = Thursday
  | x == 4    = Friday
  | x == 5    = Saturday
  | x == 6    = Sunday
  | otherwise = error "intToDay: bad argument"

dayToInt :: Day -> Int
dayToInt Monday    = 0
dayToInt Tuesday   = 1
dayToInt Wednesday = 2
dayToInt Thursday  = 3
dayToInt Friday    = 4
dayToInt Saturday  = 5
dayToInt Sunday    = 6

-- | Finds next day
-- >>> nextDay Sunday
-- Monday
-- >>> nextDay Monday
-- Tuesday
nextDay :: Day -> Day
nextDay = afterDays 1

-- | Finds day, which will come in given count of days
-- >>> afterDays 7 Sunday
-- Sunday
-- >>> afterDays 7004 Monday
-- Friday
afterDays :: Int -> Day -> Day
afterDays x = intToDay . (`mod` 7) . (+ x) . dayToInt

-- | Checks if the given day is weekend
-- >>> isWeekend Sunday
-- True
-- >>> isWeekend Saturday
-- True
-- >>> isWeekend Friday
-- False
isWeekend :: Day -> Bool
isWeekend = (> 4) . dayToInt

-- | Calculates count of days to Friday 
-- >>> daysToParty Friday
-- 0
-- >>> daysToParty Sunday
-- 5
-- >>> daysToParty Thursday
-- 1
daysToParty :: Day -> Int
daysToParty = (\x -> if (x < 0) then 7 + x else x) . (4 -) . dayToInt
