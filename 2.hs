import System.IO

example = [
    ("forward", 5),
    ("down", 5),
    ("forward", 8),
    ("up", 3),
    ("down", 8),
    ("forward", 2)]

part1 :: [(String, Integer)] -> Integer
part1 xs = x * y
    where (x, y) = part1' xs (0, 0)

part1' :: [(String, Integer)] -> (Integer, Integer) -> (Integer, Integer)
part1' [] (x,y) = (x, y)
part1' ((direction, distance):xs) (x, y) = part1' xs (
    case direction of
        "forward" -> (x + distance, y)
        "up" -> (x, y - distance)
        "down" -> (x, y + distance)
        _ -> error "a"
    )


part2 :: [(String, Integer)] -> Integer
part2 xs = x * y
    where (x, y, _) = part2' xs (0, 0, 0)

part2' :: [(String, Integer)] -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
part2' [] (x,y, aim) = (x, y, aim)
part2' ((direction, distance):xs) (x, y, aim) = part2' xs (
    case direction of
        "forward" -> (x + distance, y + distance * aim, aim)
        "up" -> (x, y, aim - distance)
        "down" -> (x, y, aim + distance)
        _ -> error "a"
    )

helper :: [[String]] -> [(String, Integer)]
helper [] = []
helper ([direction, distance]:xs) = [(direction, (read::String->Integer) distance)] ++ helper xs
helper _ = error "woah"

main :: IO () 
main = do  
    handle <- openFile "2.txt" ReadMode  
    contents <- hGetContents handle  
    let processed = map words (lines contents)
    let typed = helper processed
    putStr $ show (part1 typed) ++ "\n"
    putStr $ show (part2 typed) ++ "\n"
    hClose handle 