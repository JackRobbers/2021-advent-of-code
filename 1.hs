import System.IO  

example = [
    199,
    200,
    208,
    210,
    200,
    207,
    240,
    269,
    260,
    263]

depth :: [Integer] -> Integer -> Integer
depth [] _ = 0
depth (x:xs) prev = depth xs x + if x > prev then 1 else 0

-- we assume the 1st 3 numbers are already in prev
depth3 :: [Integer] -> [Integer] -> Integer
depth3 [] _ = 0
depth3 (x:xs) prev = depth3 xs curr3 + if (sum curr3) > (sum prev) then 1 else 0
    where curr3 = (drop 1 prev) ++ [x]

main :: IO () 
main = do  
    handle <- openFile "1.txt" ReadMode  
    contents <- hGetContents handle  
    let processed = map (read::String->Integer) (words contents)
    putStr ((show (depth processed (processed !! 0))) ++ "\n")
    putStr ((show (depth3 (drop 3 processed) (take 3 processed))) ++ "\n")
    hClose handle  