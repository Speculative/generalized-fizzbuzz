module Lib
  ( fizzbuzz,
    levinFizzbuzz,
    factorbuzz,
  )
where

fizzbuzz :: [String]
fizzbuzz =
  map
    ( applyRules
        [ (\n -> n `mod` 3 == 0, const "fizz"),
          (\n -> n `mod` 5 == 0, const "buzz")
        ]
        show
    )
    [1 :: Integer ..]

levinFizzbuzz :: [String]
levinFizzbuzz =
  map
    ( applyRules
        [ (\n -> n `mod` 7 == 0, const "fizz"),
          (contains7, const "buzz")
        ]
        show
    )
    [1 :: Integer ..]

contains7 :: Integer -> Bool
contains7 0 = False
contains7 n = (n `mod` 10 == 7) || contains7 (n `div` 10)

factorbuzz :: [[Integer]]
factorbuzz =
  map
    ( applyLimit
        [ (even, const [2]),
          (\n -> n `mod` 3 == 0, const [3]),
          (\n -> n `mod` 5 == 0, const [5])
        ]
        (: [])
        3
    )
    [1 :: Integer ..]

applyRules :: Monoid b => [(a -> Bool, a -> b)] -> (a -> b) -> a -> b
applyRules allRules fallback val = applyRules' allRules False
  where
    applyRules' ((predicate, result) : rules) matchedAny
      | predicate val = result val <> applyRules' rules True
      | otherwise = applyRules' rules matchedAny
    applyRules' [] True = mempty
    applyRules' [] False = fallback val

applyFirst :: Monoid b => [(a -> Bool, a -> b)] -> (a -> b) -> a -> b
applyFirst allRules fallback val = applyFirst' allRules
  where
    applyFirst' ((predicate, result) : rules)
      | predicate val = result val
      | otherwise = applyFirst' rules
    applyFirst' [] = fallback val

applyLimit :: Monoid b => [(a -> Bool, a -> b)] -> (a -> b) -> Int -> a -> b
applyLimit allRules fallback limit val = applyLimit' allRules limit
  where
    applyLimit' [] remaining
      | remaining == limit = fallback val
      | otherwise = mempty
    applyLimit' _ 0 = mempty
    applyLimit' ((predicate, result) : rules) remaining
      | predicate val = result val <> applyLimit' rules (remaining - 1)
      | otherwise = applyLimit' rules remaining

-- applyFirst
-- applyMany
-- applyLimit
-- withFallback
