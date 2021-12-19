import System.IO

getTup :: String -> (String, Int)
getTup s = (head $ words s, read $ last $ words s)

movement :: [(String, Int)] -> (Int, Int) -> (Int, Int)
movement [] position = position
movement (currentCommand:commands) (horizontal, depth) =
    movement commands $ moveSub currentCommand (horizontal, depth)
    where
        moveSub :: (String, Int) -> (Int, Int) -> (Int, Int)
        moveSub (command, amount) (horizontal, depth)
            | command == "forward" = (horizontal + amount, depth)
            | command == "down" = (horizontal, depth + amount)
            | otherwise = (horizontal, depth - amount)

testData = [("forward", 5), ("down", 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2)]
-- main = print $ uncurry (*) $ (\commands -> movement commands (0, 0)) testData
main = do
    contents <- readFile "./advent_2.dat"
    print $ uncurry (*) $ (\commands -> movement commands (0, 0)) $ map getTup $ lines contents