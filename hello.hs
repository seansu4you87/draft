module Main where
import System.Environment

main :: IO ()
main = do 
  -- args <- getArgs
  -- let arg1 = read $ args !! 0
  -- let arg2 = read $ args !! 1
  -- let res = show arg1 + arg2
  putStrLn "What is your name?"
  input <- getLine
  putStrLn ("Hello, " ++ input)
