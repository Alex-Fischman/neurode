module Neurode (train) where

import Dual

train :: (Fractional a, Eq a) => ([Dual a] -> b -> Dual a) -> [(b, Dual a)] -> [a] -> [a]
train model training start = descend (\state -> sum . map (\(input,output) -> loss (model state input) output) $ training) start
    where loss a b = (a - b) ^ (2::Int) 

descend :: (Fractional a, Eq a) => ([Dual a] -> Dual a) -> [a] -> [a]
descend f xs = zipWith (-) xs . map (\x -> if x == 0 then 0 else result / x) . gradient f $ xs
    where result = evaluate . f . zipWith Dual xs $ repeat 0

gradient :: Num a => ([Dual a] -> Dual b) -> [a] -> [b] 
gradient f xs = map (differentiate . f . zipWith Dual xs) . i . length $ xs
    where i n = iterate ((:) (1:repeat 0) . map (0:)) [] !! n
