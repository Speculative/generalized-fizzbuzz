module Lib
  ( fizzbuzz,
  )
where

fizzbuzz =
  map
    ( applyRules
        [ (\n -> n `mod` 3 == 0, "fizz"),
          (\n -> n `mod` 5 == 0, "buzz")
        ]
        show
    )
    [1 ..]

applyRules :: [(a -> Bool, String)] -> (a -> String) -> a -> String
applyRules allRules fallback val = applyRules' allRules False
  where
    applyRules' ((predicate, result) : rules) matchedAny
      | predicate val = result ++ applyRules' rules True
      | otherwise = applyRules' rules matchedAny
    applyRules' [] True = ""
    applyRules' [] False = fallback val
