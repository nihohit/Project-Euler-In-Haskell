isSixDigit :: Int -> Bool
isSixDigit num = (num > 99999) && (num < 1000000)

isPalindromeIterations :: Int -> Int -> Bool
isPalindromeIterations num iteration = 
    if 0 >= iteration then True else 
    let powerOfTen = 10 ^ iteration in
        let remainder = div num powerOfTen in
            ((rem num 10) == remainder) &&
            isPalindromeIterations (div (num - (powerOfTen * remainder)) 10) (iteration - 2)

isSixDigitPalindrome :: Int -> Bool
isSixDigitPalindrome num = (isSixDigit num) && 
    isPalindromeIterations num 5

iterateOverMultiplies :: Int -> Int -> Int
iterateOverMultiplies 1000 second = 0
iterateOverMultiplies first 1000 = iterateOverMultiplies (first + 1) 100
iterateOverMultiplies first second = 
    let multiplication = first * second in
        if multiplication < 100000 then iterateOverMultiplies first (second + 1)
        else if multiplication > 999999 then 0 
        else if isSixDigitPalindrome multiplication then max multiplication (iterateOverMultiplies first (second + 1))
        else (iterateOverMultiplies first (second + 1))
            