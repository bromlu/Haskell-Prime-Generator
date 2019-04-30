prods    ::  Int  -> [Int]              -- gen inf list of products
mix      :: [Int] -> [Int] -> [Int]     -- mix two inf lists
sieve    :: [Int] -> [Int] -> [Int]     -- sieve of eratosthenes
firstn   ::  Int  -> [Int]              -- returns first n primes
primesto ::  Int  -> [Int]              -- returns primes up to p

mergesort :: [Int]  -> [Int]     -- sorts list using merge sort alg
quicksort :: [Int]  -> [Int]     -- sorts list using quicksort alg
infix2rpn :: String -> String    -- converts expression from infix to
                                 -- reverse polish notation
evalrpn   :: String ->  Int      -- evaluates expression given in
                                 -- reverse polish notation
                                 
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
