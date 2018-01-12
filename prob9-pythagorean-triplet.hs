import Debug.Trace

-- SquareData was not used in final solution
data SquareData = Square { source :: Int, difference :: Int, square :: Int } deriving (Show)

squareDataFromSource :: Int -> SquareData
squareDataFromSource 0 = Square { source = 0, difference = 0, square = 0 }
squareDataFromSource num = 
    let prior = num - 1  
        square = (num ^ 2)
        difference = square - (prior ^ 2) in
        Square { source = num, difference = difference, square = square }

nextSquare :: SquareData -> SquareData
nextSquare currentSquare = 
    let newDifference = (difference currentSquare) + 2 in
        Square { source = (source currentSquare) + 1, difference = newDifference, square = (square currentSquare) + newDifference }

squareList :: Int -> [Int]
squareList num = map (^2) [0..num]

data Triplet = Triplet { a :: Int, b :: Int, c :: Int, listOfSquares :: [Int] } deriving (Show)

isASmallerThanB :: Triplet -> Bool
isASmallerThanB triplet = (a triplet < b triplet)

isBSmallerThanC :: Triplet -> Bool
isBSmallerThanC triplet = (b triplet < c triplet)

isOrdered :: Triplet -> Bool
isOrdered triplet = isASmallerThanB triplet && isBSmallerThanC triplet

isPythagorean :: Triplet -> Bool
isPythagorean triplet = 
    let squares = listOfSquares triplet in
        (squares !! (a triplet)) + (squares !! (b triplet)) == (squares !! (c triplet))

tripletSum :: Triplet -> Int
tripletSum triplet = a triplet + b triplet + c triplet

nextA :: Triplet -> Triplet
nextA triplet = Triplet {a = a triplet + 1, b = b triplet - 1, c = c triplet, listOfSquares = listOfSquares triplet}

previousC :: Triplet -> Triplet
previousC triplet = 
    let reducedC = c triplet - 1 
        remainder = tripletSum triplet - reducedC in
        Triplet {a=1, b=remainder - 1, c = reducedC, listOfSquares = listOfSquares triplet}

initialTripletForSum :: Int -> Triplet
initialTripletForSum num = Triplet {a = 1, b = 2, c = num - 3, listOfSquares = squareList num}

findPythagoreanTriplet :: Triplet -> Triplet
findPythagoreanTriplet triplet = 
    if c triplet < 0 then error "illegal c" else
    if isASmallerThanB triplet
        then if isBSmallerThanC triplet && isPythagorean triplet
            then  triplet
            else findPythagoreanTriplet (nextA triplet)
        else findPythagoreanTriplet (previousC triplet)

matchingTripletOfSum :: Int -> Triplet
matchingTripletOfSum num = findPythagoreanTriplet (initialTripletForSum num)







