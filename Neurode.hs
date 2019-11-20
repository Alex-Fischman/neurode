module Neurode (train) where

import Dual

train model training start = descend (\state -> sum . map (\(input,output) -> loss (model state input) output) $ training) start
    where loss = (.) sum . zipWith (\a b -> (a - b) * (a - b))
    
descend f xs = zipWith (-) xs . map (\x -> if x == 0 then 0 else result / x) . gradient f $ xs
    where result = evaluate . f . zipWith Dual xs $ repeat 0

gradient f xs = map (differentiate . f . zipWith Dual xs) . i . length $ xs
    where i n = iterate ((:) (1:repeat 0) . map (0:)) [] !! n
