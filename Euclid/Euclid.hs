
gcdDiff :: (Integral a) => a -> a -> a
gcdDiff a b
  | a == b    = a
  | a < b     = gcdDiff (b-a) a
  | otherwise = gcdDiff (a-b) b

gcdMod :: (Integral a) => a -> a -> a
gcdMod a 0 = a
gcdMod a b = gcdMod b (mod a b)
