module Task7
  ( expr1
  , expr2
  , expr3
  ) where

expr1 :: Bool
expr1 = (((($) :: ([String] -> Bool) -> [String] -> Bool)
  (compNullHead :: [String] -> Bool) :: [String] -> Bool)
  (list1 :: [String]) :: Bool)  
  where
    list :: [(String -> String, String)]
    list = [(firstEl :: String -> String, (" Grey" :: String))]

    firstEl :: String -> String
    firstEl = (((++) :: [a] -> [a] -> [a]) ("Dorian " :: String) :: String -> String)

    {-
      (uncurry :: (a -> b -> c) -> (a, b) -> c) (id :: x -> x)
      x -> x = a -> b -> c
      x = a
      x = b -> c
      a = b -> c
    -}
    mapper :: (b -> c, b) -> c
    mapper = ((uncurry :: ((b -> c) -> b -> c) -> (b -> c, b) -> c)
      (id :: (b -> c) -> b -> c) :: (b -> c, b) -> c)

    {-
      (map :: (a -> b) -> [a] -> [b]) (mapper :: (x -> y, x) -> y)
      a -> b = (x -> y, x) -> y
      a = (x -> y, x)
      b = y
    -}
    funcMap :: [(x -> y, x)] -> [y]
    funcMap = ((map :: ((x -> y, x) -> y) -> [(x -> y, x)] -> [y])
      (mapper :: (x -> y, x) -> y) :: [(x -> y, x)] -> [y])

    list1 :: [String]
    list1 = ((funcMap :: [(String -> String, String)] -> [String])
      (list :: [(String -> String, String)]) :: [String])
    
    compNullHead :: [String] -> Bool
    compNullHead = ((((.) :: (String -> Bool) -> ([String] -> String) -> [String] -> Bool)
      (null :: String -> Bool) :: ([String] -> String) -> [String] -> Bool)
      (head :: [String] -> String) :: [String] -> Bool)

expr2 :: [(Integer, Integer)]
expr2 = (\x ->
  (((zip :: [Integer] -> [Integer] -> [(Integer, Integer)])
  ((lefts :: [Either Integer Integer] -> [Integer])
  (x :: [Either Integer Integer]) :: [Integer]) :: [Integer] -> [(Integer, Integer)])
  ((rights :: [Either Integer Integer] -> [Integer])
  (x :: [Either Integer Integer]) :: [Integer]) :: [(Integer, Integer)]))
  (list :: [Either Integer Integer])
  where
    list :: [Either Integer Integer]
    list = [left :: Either Integer b, right :: Either a Integer]

    left :: Either Integer b
    left = (Left :: Integer -> Either Integer b)
      ((((+) :: Integer -> Integer -> Integer) (1 :: Integer) :: Integer -> Integer)
      (2 :: Integer) :: Integer)

    right :: Either a Integer
    right = (Right :: Integer -> Either a Integer)
      ((((^) :: Integer -> Integer -> Integer) (2 :: Integer) :: Integer -> Integer)
      (6 :: Integer) :: Integer)

    lefts :: [Either a b] -> [a]
    lefts x = [a | Left a <- x]

    rights :: [Either a b] -> [b]
    rights x = [a | Right a <- x]

expr3 :: Integer -> Bool
expr3 =
  let
    impl :: Bool -> Bool -> Bool
    impl = \x y ->
      ((((||) :: Bool -> Bool -> Bool)
      (((not :: Bool -> Bool) (x :: Bool)) :: Bool) :: Bool -> Bool)
      (y :: Bool) :: Bool)
  in let
       isMod2 :: Integer -> Bool
       isMod2 = \x -> ((((==) :: Integer -> Integer -> Bool)
         (((mod :: Integer -> Integer -> Integer) (x :: Integer) :: Integer -> Integer)
         (2 :: Integer) :: Integer) :: Integer -> Bool)
         (0 :: Integer) :: Bool)
     in let
          isMod4 :: Integer -> Bool
          isMod4 = \x -> ((((==) :: Integer -> Integer -> Bool)
            (((mod :: Integer -> Integer -> Integer) (x :: Integer) :: Integer -> Integer)
            (4 :: Integer) :: Integer) :: Integer -> Bool)
            (0 :: Integer) :: Bool)
        in \x ->
          (impl :: Bool -> Bool -> Bool)
          ((isMod4 :: Integer -> Bool) (x :: Integer) :: Bool)
          ((isMod2 :: Integer -> Bool) (x :: Integer) :: Bool)
