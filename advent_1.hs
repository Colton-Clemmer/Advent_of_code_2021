import System.IO

fn :: [Int] -> [Bool]
fn [] = []
fn [x] = []
fn (x:y:depths) = (x < y) : fn (y : depths)

-- main = print $ fn2 $ fn [199,200,208,210,200,207,240,269,260,263]
main = do
    contents <- readFile "./advent_1.dat"
    print $ length $ filter id $ fn $ map read $ lines contents