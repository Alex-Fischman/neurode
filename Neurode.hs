module Neurode (gradient, identity) where

import Dual

type Vector a = [a]
type Matrix a = Vector (Vector a)

gradient :: Num a => (Vector (Dual a) -> Dual b) -> Vector a -> Vector b
gradient f xs = map (differentiate . f . zipWith Dual xs . map fromIntegral) . identity $ length xs

identity :: Int -> Matrix Int
identity n = map (take n) . take n $ ii
    where ii = (1:repeat 0) : map (0:) ii
