import Debug.Trace

cleanDivision :: Int -> Int -> Bool
cleanDivision num divisor = ((rem num divisor) == 0)

isEven :: Int -> Bool
isEven num = cleanDivision num 2

toOdd :: Int -> Int
toOdd num = if (isEven num) then num - 1 else num

isfactorLargerThanRoot :: Int -> Int -> Bool
isfactorLargerThanRoot num factor = (fromIntegral factor) > (sqrt (fromIntegral num))

isPrimeContinuation :: Int -> Int -> Bool
isPrimeContinuation num counter = if isfactorLargerThanRoot num counter then True
    else if cleanDivision num counter then False
    else isPrimeContinuation num (counter + 2)

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = not (isEven n) && (isPrimeContinuation n 3)

isFactor :: Int -> Int -> Bool
isFactor num potentialFactor = (cleanDivision num potentialFactor)  && (isPrime potentialFactor)

largestPrimeFactorWithCurrent :: Int -> Int -> Int
largestPrimeFactorWithCurrent num current = 
    let oddCurrent = toOdd current in
        if isFactor num oddCurrent then oddCurrent
        else largestPrimeFactorWithCurrent num (oddCurrent - 2)
 
-- naive implementation, too slow
largestPrimeFactor :: Int -> Int
largestPrimeFactor num = largestPrimeFactorWithCurrent num (num - 1)

-- quick implementation
largestPrimeFactorContinuation :: Int -> Int -> Int
largestPrimeFactorContinuation num 0 = num
largestPrimeFactorContinuation num 1 = num
largestPrimeFactorContinuation 1 potentialFactor = potentialFactor
largestPrimeFactorContinuation num potentialFactor = 
    if isEven potentialFactor then largestPrimeFactorContinuation num (potentialFactor - 1)
    else if num == potentialFactor then potentialFactor
        else if isFactor num potentialFactor then max potentialFactor (quickLargestPrimeFactor (div num potentialFactor))
        else largestPrimeFactorContinuation num (potentialFactor - 2)

quickLargestPrimeFactor :: Int -> Int
quickLargestPrimeFactor 1 = 1
quickLargestPrimeFactor 2 = 2
quickLargestPrimeFactor num = 
    if cleanDivision num 2 
        then quickLargestPrimeFactor (div num 2)
        else largestPrimeFactorContinuation num (floor (sqrt (fromIntegral num)))