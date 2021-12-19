import System.IO

moveSub :: [(String, Int)] -> (Int, Int)
moveSub = aux (0, 0)
    where 
    aux :: (Int, Int) -> [(String, Int)] -> (Int, Int)
    aux position [] = position
    aux (horizontal, depth) (currentCommand:commands) =
        aux (aux2 currentCommand (horizontal, depth)) commands 
        where
        aux2 :: (String, Int) -> (Int, Int) -> (Int, Int)
        aux2 (command, amount) (horizontal, depth)
            | command == "forward" = (horizontal + amount, depth)
            | command == "down" = (horizontal, depth + amount)
            | otherwise = (horizontal, depth - amount)

main = do
    contents <- readFile "./advent_2.dat"
    print $ uncurry (*) $ moveSub $ map (\s -> (head $ words s, read $ last $ words s)) $ lines contents