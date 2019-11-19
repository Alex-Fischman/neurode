import Neurode

main :: IO ()
main = print $ map (fromRational :: Rational -> Double) $ (!!10) $ iterate (train model training) state

training :: (Num a, Enum a) => [([a], a)]
training = [([a,b], a + b) | a <- [-10..10], b <- [-10..10]]

state :: [Rational]
state = [3 / 2, 2]

model :: Num a => [a] -> [a] -> a
model = (.) sum . zipWith (*)
