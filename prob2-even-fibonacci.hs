-- simple solution
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibWithDivider :: Int -> Int -> Int
fibWithDivider n divider = if ((rem (fib n) divider) == 0) then (fib n) else 0

simpleFibSumWithCounter :: Int -> Int -> Int -> Int
simpleFibSumWithCounter max divider counter = if (fib counter) >= max then 0
    else (fibWithDivider counter divider) + (simpleFibSumWithCounter max divider (counter + 1))

simpleFibSum :: Int -> Int -> Int
simpleFibSum max divider = simpleFibSumWithCounter max divider 0

-- assuming even solution
evenFib :: Int -> Int
evenFib 0 = 0
evenFib n = (fib ((n * 3) - 2)) + (fib ((n * 3) - 1))

evenFibSumWithCounter :: Int ->  Int -> Int
evenFibSumWithCounter max counter = 
    let evenResult = (evenFib counter) in
        if evenResult >= max then 0
        else evenResult + (evenFibSumWithCounter max (counter + 1))

evenFibSum :: Int -> Int
evenFibSum max = evenFibSumWithCounter max 0

-- list solution
createFibList :: Int -> [Int] -> [Int]
createFibList max list = 
    let evenResult = (evenFib (length list)) in
        if evenResult >= max then list
        else createFibList max (evenResult : list)

listFibSum :: Int -> Int
listFibSum max = sum (createFibList max [])