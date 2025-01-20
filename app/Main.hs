module Main where

fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

evennums :: [Int] -> [Int]
evennums [] = []
evennums n = filter even n

firsteven :: [Int] -> Int
firsteven n = head (evennums n)

strxtimes :: String -> Int -> IO ()
strxtimes s n
    | n == 0 = return ()
    | otherwise = do 
        putStrLn s
        strxtimes s (n - 1)

printeach :: [Int] -> IO () 
printeach [] = return ()
printeach (x:xs) = do
    print x
    printeach xs

dupfirst :: [Int] -> [Int]
dupfirst [] = []
dupfirst (x:xs) = x:x:xs

remfirst :: [Int] -> [Int]
remfirst [] = []
remfirst (_:xs) = xs

movetoend :: [Int] -> [Int]
movetoend [] = []
movetoend (x:xs) = xs ++ [x]

shift :: Int -> [Int] -> [Int]
shift _ [] = []
shift 0 n = n
shift n l = shift (n-1) (movetoend l)

addlist :: [Int] -> Int
addlist [] = 0
addlist [x] = x
addlist (x:xs) = addlist ((x + head xs):remfirst xs)

addorsub :: (a -> b -> b) -> a -> b -> b
addorsub f = f

revsumlist :: [Int] -> Int
revsumlist [] = 0 
revsumlist (x:xs) = foldl (-) x xs

addthensub :: Int -> Int -> Int -> (Int -> Int -> Int) -> Int
addthensub n a b f 
    | n == 0 = a
    | odd n = addthensub (n-1) (addorsub f b a) (addorsub f a a) (-)
    | even n = addthensub (n-1) (addorsub f a b) (addorsub f b a) (+) 
    | otherwise = 0

main :: IO ()
main = do
    --input <- getLine  
    --strxtimes input 15
    --num <- getLine 
    --print (fib (read num :: Int))
    --print (fib 10)
    --print (evennums [0..20])
    --print (firsteven [1..20])
    --printeach [1..10] 0
    --print (dupfirst [1..10])
    --print (remfirst [1..10])
    --print (movetoend [1..10])
    --print (shift 5 (evennums [0..20]))
    --print (addlist [1..30])
    --print (addorsub (+) 1 2)
    --print (addorsub (-) 2 1) 
    --print (revsumlist [100, 1, 2, 3, 4])
    --print (addthensub 200 3 2 (+))

    printeach [0..10]