module Main where
import System.Random

guessFor number randomGen
  | number == randomGen = "Congrats! You guessed the number"
  | number > randomGen  = "Your number is smaller than the number to be guessed"
  | number < randomGen  = "Your number is bigger than the number to be guessed"
  | otherwise = "Invalid input"
guessingLogic curNum randomGen =
  if curNum == 0
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
    else guessingLogic minusAttempt randomGen

main = do
  randomGen <- getStdRandom $ randomR (1, 10)
  guessingLogic 3 randomGen