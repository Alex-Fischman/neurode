import Neurode

main = print $ layer (iterate (train layer training) initial !! 10) [5, 199]

training = [([a, b], [a + b]) | a <- [-10..10], b <- [-10..10]]

initial = replicate 3 0

layer state input = zipWith (+) biases $ map (sum . zipWith (*) input) $ chunks (length input) weights
    where
        (weights, biases) = splitAt (totalLen - outLen) state

        outLen = totalLen `div` (inLen + 1)
        inLen = length input
        totalLen = length state

        chunks _ [] = []
        chunks n xs =
            let (ys, zs) = splitAt n xs
            in  ys : chunks n zs
