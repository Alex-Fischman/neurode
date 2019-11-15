import Neurode

main = print $ gradient (\[x,y] -> log x + 3 * y) [0.1, 3]
