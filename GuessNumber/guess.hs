import System.Random
import Data.Maybe (listToMaybe)

readInt :: String -> Maybe Int
readInt = listToMaybe . map fst . reads

guess :: Int -> IO ()
guess x = do
  putStr "Pick a number: "
  input <- readInt <$> getLine
  case input of
    Nothing -> putStrLn "Not a number." >> guess x
    Just g -> case compare g x of
      LT -> putStrLn "Too small." >> guess x
      GT -> putStrLn "Too big." >> guess x
      EQ -> putStrLn "You found the number, well done!"

main = do
  putStrLn "Guess a number between 0 and 100."
  randomRIO (0, 100) >>= guess
