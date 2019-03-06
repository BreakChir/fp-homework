module Block3_2
  ( Castle (..)
  , Church (..)
  , Family (..)
  , House (..)
  , Library (..)
  , Lord (..)
  , Town (..)
  , Wall (..)
  , buildCastle
  , buildChurchOrLibrary
  , buildHouse
  , buildWalls
  , moveToCastle
  ) where

import Block4 (NonEmpty (..))

data Lord = Lord
  deriving (Show)

data Castle
  = EmptyCastle
  | LordCastle Lord
  deriving (Show)

data Church = Church
  deriving (Show)

data Library = Library
  deriving (Show)

data Family
  = One
  | Two
  | Three
  | Four
  deriving (Show)

newtype House = House Family
  deriving (Show)

data Wall = Wall
  deriving (Show)

data Town = Town
  { castle  :: Maybe (Castle, Maybe Wall)
  , service :: Maybe (Either Church Library)
  , houses  :: NonEmpty House
  } deriving (Show)

-- | Builds castle in the given town
-- >>> buildCastle (Town Nothing Nothing (House One :| []))
-- (Town {castle = Just (EmptyCastle,Nothing), service = Nothing, houses = House One :| []},True)
-- >>> buildCastle (Town (Just (EmptyCastle, Nothing)) Nothing (House One :|  []))
-- (Town {castle = Just (EmptyCastle,Nothing), service = Nothing, houses = House One :| []},False)
buildCastle :: Town -> (Town, Bool)
buildCastle t@(Town (Just _) _ _) = (t, False)
buildCastle t                     = (t { castle = Just (EmptyCastle, Nothing) }, True)

-- | Builds church or library or in the given town
-- >>> buildChurchOrLibrary (Right Library) (Town Nothing Nothing (House One :| []))
-- (Town {castle = Nothing, service = Just (Right Library), houses = House One :| []},True)
-- >>> buildChurchOrLibrary (Left Church) (Town Nothing (Just (Right Library)) (House One :| []))
-- (Town {castle = Nothing, service = Just (Right Library), houses = House One :| []},False)
buildChurchOrLibrary
  :: Either Church Library
  -> Town
  -> (Town, Bool)
buildChurchOrLibrary _        t@(Town _ (Just _) _) = (t, False)
buildChurchOrLibrary building t                     = (t { service = Just building }, True)

-- | Builds new house in the given town
-- >>> buildHouse Two (Town Nothing Nothing (House One :| []))
-- Town {castle = Nothing, service = Nothing, houses = House One :| [House Two]}
-- >>> buildHouse Two (Town Nothing Nothing (House One :| [House Two, House Three]))
-- Town {castle = Nothing, service = Nothing, houses = House One :| [House Two,House Two,House Three]}
buildHouse :: Family -> Town -> Town
buildHouse family t@(Town _ _ (x :| xs)) = t { houses = x :| ((House family) : xs) }

-- | Transports lord to the given town
-- >>> moveToCastle Lord (Town Nothing Nothing (House One :| []))
-- Right "There is not a castle"
-- >>> moveToCastle Lord (Town (Just (LordCastle Lord, Nothing)) Nothing (House One :| []))
-- Right "There is already lord in the castle"
-- >>> moveToCastle Lord (Town (Just (EmptyCastle, Nothing)) Nothing (House One :| []))
-- Left (Town {castle = Just (LordCastle Lord,Nothing), service = Nothing, houses = House One :| []})
moveToCastle :: Lord -> Town -> Either Town String
moveToCastle _    (Town Nothing _ _)                       =
  Right "There is not a castle"
moveToCastle _    (Town (Just (LordCastle _, _)) _ _)      =
  Right "There is already lord in the castle"
moveToCastle lord t@(Town (Just (EmptyCastle, walls)) _ _) =
  Left (t { castle = Just (LordCastle lord, walls) })

peopleSize :: (NonEmpty House) -> Int
peopleSize = foldr (\x xs -> houseSize x + xs) 0
 where
   houseSize :: House -> Int
   houseSize (House family) =
     case family of
       One   -> 1
       Two   -> 2
       Three -> 3
       Four  -> 4

-- | Builds walls in the given town
-- >>> let listHouse = [House One, House Three, House Four]
-- >>> buildWalls (Town Nothing Nothing (House One :| []))
-- Right "There is not a castle and a lord"
-- >>> buildWalls (Town (Just (EmptyCastle, Nothing)) Nothing (House One :| []))
-- Right "There is not a lord"
-- >>> buildWalls (Town (Just (LordCastle Lord, Just Wall)) Nothing (House One :| []))
-- Right "There are already walls in the town"
-- >>> buildWalls (Town (Just (LordCastle Lord, Nothing)) Nothing (House One :| listHouse))
-- Right "There are less than 10 people in the town"
-- >>> buildWalls (Town (Just (LordCastle Lord, Nothing)) Nothing (House Two :| listHouse))
-- Left (Town {castle = Just (LordCastle Lord,Just Wall), service = Nothing, houses = House Two :| [House One,House Three,House Four]})
buildWalls :: Town -> Either Town String
buildWalls (Town Nothing _ _)                     =
  Right "There is not a castle and a lord"
buildWalls (Town (Just (EmptyCastle, _)) _ _)     =
  Right "There is not a lord"
buildWalls (Town (Just (_, Just _)) _ _)          =
  Right "There are already walls in the town"
buildWalls t@(Town (Just (tCastle, _)) _ tHouses)
  | peopleSize tHouses < 10 = Right "There are less than 10 people in the town"
  | otherwise               = Left (t { castle = Just (tCastle, Just Wall) })
