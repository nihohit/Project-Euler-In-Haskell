cleanDivision :: Int -> Int -> Bool
cleanDivision num divisor = ((rem num divisor) == 0)

cleanlyDivisbleByAnyInList :: Int -> [Int] -> Bool
cleanlyDivisbleByAnyInList num list = any (cleanDivision num) list

getLastPrime :: Int -> Int -> [Int] -> Int
getLastPrime num targetIndex primeList = 
    if length primeList == targetIndex 
        then last primeList
        else if cleanlyDivisbleByAnyInList num primeList
            then getLastPrime (num + 2) targetIndex primeList
            else getLastPrime (num + 2) targetIndex (primeList ++ [num])