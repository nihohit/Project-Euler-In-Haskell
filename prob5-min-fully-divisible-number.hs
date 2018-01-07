nonDivisibleNumbersBelowRange :: Int -> Int -> Int
nonDivisibleNumbersBelowRange 0 accumulator = accumulator
nonDivisibleNumbersBelowRange num 0 = nonDivisibleNumbersBelowRange (num - 1) num
nonDivisibleNumbersBelowRange num accumulator = 
    let commonDivisor = gcd num accumulator in
        nonDivisibleNumbersBelowRange (num - 1) (accumulator * (div num commonDivisor))
