data Tree a = Empty
            | Node {node :: a,
                    forest :: [Tree a]}
                       deriving (Show)

dfs :: Tree a -> [a]
dfs Empty = []
dfs (Node x ts) = x : concatMap dfs ts

bfs :: Tree a -> [a]
bfs Empty = []
bfs (Node x ts) = x : go ts
  where go [] = []
        go ts = map node ts ++ go (concatMap forest ts)

main = do
  print $ dfs testTree
  print $ bfs testTree

testTree :: Tree Int
testTree = Node 1 [Node 2 [Node 3 [],
                           Node 4 [ Node 5 []]],
                   Node 6 [Node 7 [],
                           Node 8 [ Node 9 [Node 10 [Node 11 []],
                                            Node 12 []]],
                           Node 13 [Node 14 []]],
                   Node 15 []]
