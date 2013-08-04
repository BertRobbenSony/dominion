module Caesar (

  encode,
  decode
  
) where

encode :: Int -> String -> String
encode n s = map shift s
  where shift = (shiftChar 'a' 'z' n) . (shiftChar 'A' 'Z' n) 
         
shiftChar :: Char -> Char -> Int -> Char -> Char
shiftChar a z n c
  | a <= c && c <= z = let a' = fromEnum a
                           z' = fromEnum z
                           c' = fromEnum c
                       in toEnum (a' + (c' - a' + n) `mod` (z' - a'))
  | otherwise = c
  
decode :: Int -> String -> String
decode n = encode (-n)