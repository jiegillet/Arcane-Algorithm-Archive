import System.Random

monteCarloPi :: RandomGen g =>  g -> Int -> Float
monteCarloPi g n = (*4) . (/fromIntegral n) . fromIntegral . length . 
                   filter (uncurry inCircle) . take n . pair $ rds
  where rds = randomRs (0, 1) g :: [Float]
        pair (x:y:rest) = (x, y) : pair rest
        inCircle x y = x^2 + y^2 < 1

main = do
  g <- newStdGen
  let p = monteCarloPi g 100000 
  putStrLn $ "Estimated pi: " ++ show p
  putStrLn $ "Percent error: " ++ show (100*(pi-p)/pi)
