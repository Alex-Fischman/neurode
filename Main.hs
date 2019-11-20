import Neurode

main :: IO ()
main = print $ layer (iterate (train layer training) initial !! 5) [5, 199::Double]

training :: (Num a, Enum a) => [([a], [a])]
training = [([a, b], [a + b]) | a <- [-10..10], b <- [-10..10]]

initial :: Num a => [a]
initial = replicate 3 0

layer :: Num a => [a] -> [a] -> [a]
layer state input = zipWith (+) biases $ map (sum . zipWith (*) input) $ splitEvery (length input) weights
    where
        (weights, biases) = splitAt (length state - (length state `div` (length input + 1))) state
        splitEvery n = (:) . take n <*> splitEvery n . drop n
