multiplesSumSingleNum :: Int -> Int -> Int -> Int
multiplesSumSingleNum number multiplier max = if (number * multiplier) >= max then 0
    else (number * multiplier) + (multiplesSumSingleNum number (multiplier + 1) max)

multiplesSumTwoNums :: Int -> Int -> Int -> Int
multiplesSumTwoNums first second max = (multiplesSumSingleNum first 1 max) + 
    (multiplesSumSingleNum second 1 max) - 
    (multiplesSumSingleNum (first * second) 1 max)