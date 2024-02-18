module Main where
import System.Random

guessFor number randomGen
  | number == randomGen = "Congrats! You guessed the number"
  | number > randomGen  = "Your number is smaller than the number to be guessed. Try again"
  | number < randomGen  = "Your number is bigger than the number to be guessed. Try again"
  | otherwise = "Invalid input"
guessingLogic maxAttempts curNum randomGen =
  if curNum == 1
  then putStrLn "You have no more attempts. Try again"
  else do
    putStrLn "Guess a number from 1 to 10"
    numberString <- getLine
    let number = read numberString :: Int
        resultString = guessFor number randomGen
        minusAttempt = curNum - 1
    putStrLn resultString
    if resultString == "Congrats! You guessed the number"
    then putStrLn "gg cho :)"
    else guessingLogic maxAttempts minusAttempt randomGen

main = do
  randomGen <- getStdRandom $ randomR (1, 10)
  guessingLogic 3 5 randomGen