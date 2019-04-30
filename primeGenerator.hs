prods    ::  Int  -> [Int]              -- gen inf list of products
prods x = [x * n | n <- [x ..]]

mix      :: [Int] -> [Int] -> [Int]     -- mix two inf lists
mix lst [] = lst
mix [] lst = lst
mix (h1:t1) (h2:t2)
    | h1 == h2 = h1:(mix t1 t2)
    | h1 < h2 = h1:(mix t1 (h2:t2))
    | otherwise = h2:(mix (h1:t1) t2)

sieve    :: [Int] -> [Int] -> [Int]     -- sieve of eratosthenes
sieve (potentialPrime:potentialTail) [] = potentialPrime : (sieve potentialTail (prods potentialPrime))
sieve (potentialPrime:potentialTail) (composite:compositesTail) 
    | potentialPrime == composite = sieve potentialTail compositesTail
    | otherwise = potentialPrime : (sieve potentialTail (mix (composite:compositesTail) (prods potentialPrime)))

firstn   ::  Int  -> [Int]              -- returns first n primes
firstn n = take n (sieve [2..] [])

primesto ::  Int  -> [Int]              -- returns primes up to p
primesto n = takeWhile (<=n) (sieve [2..] [])

merge :: [Int] -> [Int] -> [Int]
merge lst [] = lst
merge [] lst = lst
merge (h1:t1) (h2:t2)
    | h1 <= h2 = h1:(merge t1 (h2:t2))
    | otherwise = h2:(merge (h1:t1) t2)

mergesort :: [Int]  -> [Int]     -- sorts list using merge sort alg
mergesort [] = []
mergesort lst
    | length lst == 1 = lst
    | otherwise = merge (mergesort (fst splitList)) (mergesort (snd splitList)) 
    where splitList = splitAt (div (length lst) 2) lst

quicksort :: [Int]  -> [Int]     -- sorts list using quicksort alg
quicksort [] = []
quicksort (pivot:rest) = lower ++ [pivot] ++ upper
    where 
        lower = (quicksort (filter (<=pivot) rest))
        upper = (quicksort (filter (>pivot) rest))
        
-- infix2rpn :: String -> String    -- converts expression from infix to
--                                  -- reverse polish notation
-- evalrpn   :: String ->  Int      -- evaluates expression given in
--                                  -- reverse polish notation

-- Similar to words function, but handles missing/extra spaces
-- between ops, digits, parens.  Note: not general or robust!
mywords :: String -> [String]
mywords s = parser [] s
    where
        isparen c = (c == '(' || c == ')')
        isop    c = (c == '+' || c == '*')
        isdigit c = (c >= '0' && c <= '9')
        -- readint reads digits until non-digit
        -- returns ("digit","remainder of string")
        readint "" = ("","")
        readint (h:t)
            | (isdigit h) = (h:(fst (readint t)), snd (readint t))
            | otherwise  = ("", h:t)
        parser l "" = l
        parser l (h:t)
            | (isdigit h) = parser (l ++ [fst (readint (h:t))])
                                   (snd (readint (h:t)))
            | (isop h)    = parser (l ++ [h:""]) t
            | (isparen h) = parser (l ++ [h:""]) t
            | h == ' '    = parser l t
