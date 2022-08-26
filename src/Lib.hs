module Lib
  ( fizzbuzz,
    levinFizzbuzz,
  )
where

fizzbuzz :: [String]
fizzbuzz =
  map
    ( applyRulesString
        [ (\n -> n `mod` 3 == 0, const "fizz"),
          (\n -> n `mod` 5 == 0, const "buzz")
        ]
        show
    )
    [1 :: Integer ..]

levinFizzbuzz :: [String]
levinFizzbuzz =
  map
    ( applyRulesString
        [ (\n -> n `mod` 7 == 0, const "fizz"),
          (contains7, const "buzz")
        ]
        show
    )
    [1 :: Integer ..]

contains7 :: Integer -> Bool
contains7 0 = False
contains7 n = (n `mod` 10 == 7) || contains7 (n `div` 10)

applyRulesString :: [(a -> Bool, a -> String)] -> (a -> String) -> a -> String
applyRulesString = applyRules (++) ""

applyRules :: (b -> b -> b) -> b -> [(a -> Bool, a -> b)] -> (a -> b) -> a -> b
applyRules combine empty allRules fallback val = applyRules' allRules False
  where
    applyRules' ((predicate, result) : rules) matchedAny
      | predicate val = result val `combine` applyRules' rules True
      | otherwise = applyRules' rules matchedAny
    applyRules' [] True = empty
    applyRules' [] False = fallback val
