-- number-guessing game ver3 & ver4

-- can you reuse code from the previous version?
--       Yes, I reused verdict and readNumber from Aj. Chin's version. I modified runGame for version3 and version4.

import Text.Read
import System.Random

--pure part
verdict :: Ord a => a -> a -> Either String String
verdict target guess =
    case compare guess target of
        EQ -> Right "You win!"
        LT -> Left "Too low"
        GT -> Left "Too high"

--impure parts
readNumber :: [Char] -> IO Int
readNumber msg = do
    putStr $ msg ++ ": "
    line <- getLine
    case readEither line :: Either String Int of
        Left e -> do
            putStrLn e
            readNumber msg
        Right n -> return n

runGame :: Int -> Int -> (Int -> Bool) -> Maybe Int -> Maybe Int -> Bool -> IO ()
runGame num count cont lowestHigh highestLow enableHint = do
    guess <- readNumber "Guess"
    let v = verdict num guess
    let newLowestHigh = if guess > num then Just (maybe guess (min guess) lowestHigh) else lowestHigh
    let newHighestLow = if guess < num then Just (maybe guess (max guess) highestLow) else highestLow
    
    if enableHint && ((maybe False (guess >=) lowestHigh) || (maybe False (guess <=) highestLow))
        then do
            putStrLn "This guess is impossible. Try again."
            runGame num count cont lowestHigh highestLow enableHint
        else do
            case v of
                Right m -> putStrLn m
                Left m -> do
                    putStrLn m
                    if cont count
                        then runGame num (count+1) cont newLowestHigh newHighestLow enableHint
                        else putStrLn "Game over"

version3 :: IO ()
version3 = do
    gen <- newStdGen
    lowerBound <- readNumber "Enter lower bound"
    upperBound <- readNumber "Enter upper bound"
    let (num, _) = uniformR (lowerBound, upperBound) gen
    lim <- readNumber "Guess limit"
    runGame num 1 (<lim) Nothing Nothing False

version4 :: IO ()
version4 = do
    gen <- newStdGen
    lowerBound <- readNumber "Enter lower bound"
    upperBound <- readNumber "Enter upper bound"
    let (num, _) = uniformR (lowerBound, upperBound) gen
    lim <- readNumber "Guess limit"
    runGame num 1 (<lim) Nothing Nothing True


-- write function nRandomRs that generates a list of n random values, each in a given range
nRandomRs :: (RandomGen g, UniformRange a, Integral n) => (a, a) -> n -> g -> ([a], g)
nRandomRs _ 0 gen = ([], gen)
nRandomRs range n gen =
    let (x, gen') = uniformR range gen
        (xs, gen'') = nRandomRs range (n - 1) gen'
    in (x : xs, gen'')
