testFn x 
    | x == 0    = 0
    | x <= 100  = x + (testFn (x-1))
    | otherwise = error "too large"

repeatString str n =
    if n == 0
        then ""
    else str ++ (repeatString str (n-1))

removeOdd [] = []
removeOdd (x : xs)
  | mod x 2 == 0    = x : (removeOdd xs)
  | otherwise       = removeOdd xs

numEven nums =
    let evenNums = removeOdd nums
    in length evenNums

numEven' nums = 
    length evenNums
    where evenNums = removeOdd nums

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
	larger = filter (> x) xs
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger
    
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys