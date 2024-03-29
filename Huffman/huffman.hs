import qualified Data.Map as M
import           Data.List (insert, sort)

data Tree a = Nil
            | Leaf Int a
            | Node Int (Tree a) (Tree a)
                   deriving (Show, Eq)

freq :: Tree a -> Int
freq Nil          = 0
freq (Leaf i _)   = i
freq (Node i _ _) = i

instance (Eq a) => Ord (Tree a) where
  compare t1 t2 = compare (freq t1) (freq t2)

getFrequencies :: Ord a => [a] -> [(Int, a)]
getFrequencies = toSortedList . M.fromListWith (+) . flip zip (repeat 1)
  where toSortedList = sort . map swap . M.toList
        swap (a, i) = (i, a)

buildTree :: (Ord a) => [a] -> Tree a
buildTree = build . map (uncurry Leaf) . getFrequencies
  where build []         = Nil
        build [t]        = t
        build (t1:t2:ts) = build $ insert (Node (freq t1 + freq t2) t1 t2) ts

data Bit = Zero | One

instance Show Bit where
  show Zero = "0"
  show One = "1"

encode :: (Ord a) => [a] -> (Tree a, [Bit])
encode s = (tree, msg)
  where
  tree = buildTree s
  msg = concatMap (table M.!) s
  table = M.fromList $ mkTable (tree, [])
  mkTable (Nil, _)          = []
  mkTable (Leaf _ a, p)     = [(a, reverse p)]
  mkTable (Node _ t1 t2, p) = concatMap mkTable [(t1,  Zero:p), (t2, One:p)]

decode :: (Ord a) => Tree a -> [Bit] -> [a]
decode (Leaf i a) _ = replicate i a
decode t msg = path t msg
  where path (Leaf _ a) m            = a : path t m
        path (Node _ t1 _) (Zero: m) = path t1 m
        path (Node _ _ t2) (One: m)  = path t2 m
        path _ _                     = []

main = do
  let msg = "bibbity bobbity"
      (tree, encoded) = encode msg
      decoded = decode tree encoded
  putStrLn $ "Encoding \"" ++ msg ++ "\": " ++ concatMap show encoded
  putStrLn $ "Length: " ++ (show $ length encoded)
  putStrLn $ "Decoding: " ++ decoded
