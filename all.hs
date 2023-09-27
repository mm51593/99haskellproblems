import System.Random

-- 01
hello :: a -> a
hello x = x

head' :: [a] -> a
head' [] = error "err"
head' (x:_) = x

myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- 02
myButLast :: [a] -> a
myButLast (x:[]) = error "err"
myButLast (x:[_]) = x
myButLast (x:xs) = myButLast xs

-- 03
elementAt :: [a] -> Int -> a
elementAt [] _ = error "err"
elementAt (x:_) 1 = x
elementAt (x:xs) y = elementAt xs (y-1)

-- 04
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' x = myLengthAux' x 0
  where
    myLengthAux' [] n = n
    myLengthAux' (x:xs) n = myLengthAux' xs n + 1

-- 05
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- 06
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == myReverse x

-- 07
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

flatten2 :: NestedList a -> [a]
flatten2 (Elem x) = [x]
flatten2 (List []) = []
flatten2 (List (x:xs)) = prepend (flatten x) (flatten  (List xs))
  where
  prepend [] y = y
  prepend (x:xs) y = x : prepend xs y

-- 08
compress :: (Eq a) => [a] -> [a]
compress x = compressAux x Nothing
  where
  compressAux (l:ls) Nothing = [l] ++ compressAux ls (Just l)
  compressAux (l:ls) (Just i) 
    | l == i     = compressAux ls (Just l)
    | otherwise  = [l] ++ compressAux ls (Just l)
  compressAux [] _ = []

-- 09
pack :: (Eq a) => [a] -> [[a]]
pack = packInit Nothing
  where
  packInit Nothing (l:ls) = packAux l (l:ls) : packInit (Just l) ls 
  packInit _ [] = [] 
  packInit (Just i) (l:ls)
    | i == l     = packInit (Just i) ls
    | otherwise  = packAux l (l:ls) : packInit (Just l) ls

  packAux _ [] = []
  packAux i (l:ls)
    | i == l     = l : packAux i ls
    | otherwise  = []

-- 10
encode :: (Eq a) => [a] -> [(Int, a)]
encode = encodeAux . pack
  where
    encodeAux [] = []
    encodeAux (x:xs) = (len x 0, head' x) : encodeAux xs

    len [] n = n
    len (x:xs) n = len xs (n + 1)

  
-- 11
data EMItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [EMItem a]
encodeModified = encodeModifiedAux . encode
  where
    encodeModifiedAux [] = []
    encodeModifiedAux (x:xs) = transform x : encodeModifiedAux xs

    transform (1, y) = Single y
    transform (x, y) = Multiple x y

-- 12
decodeModified :: [EMItem a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeModifiedAux x ++  decodeModified xs
  where
  decodeModifiedAux (Single y) = [y]
  decodeModifiedAux (Multiple x y) = repeat x y

  repeat 0 y = []
  repeat x y = y : repeat (x - 1) y

-- 13
encodeDirect :: (Eq a) => [a] -> [EMItem a]
encodeDirect = encodeDirectInit Nothing
  where
  encodeDirectInit Nothing (x:xs) = transform (encodeDirectAux Nothing (x:xs)) x : encodeDirectInit (Just x) xs
  encodeDirectInit (Just y) (x:xs)
    | y == x    = encodeDirectInit (Just x) xs
    | otherwise = transform (encodeDirectAux Nothing (x:xs)) x : encodeDirectInit (Just x) xs
  encodeDirectInit _ [] = [] 
  
  encodeDirectAux Nothing (x:xs) = 1 + encodeDirectAux (Just x) xs
  encodeDirectAux (Just y) (x:xs)
    | y == x    = 1 + encodeDirectAux (Just x) xs
    | otherwise = 0
  encodeDirectAux _ [] = 0

  transform 1 y = Single y
  transform x y = Multiple x y

-- 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = prepend x n (repli xs n)
  where
  prepend _ 0 l = l
  prepend x n l = x : prepend x (n - 1) l
  
-- 16
dropEvery :: [a] -> Int -> [a]
dropEvery l n = dropEveryAux l n n
  where
  dropEveryAux [] _ _ = []
  dropEveryAux (x:xs) n i 
    | i == 1    = dropEveryAux xs n n
    | otherwise = x : dropEveryAux xs n (i - 1)

-- 17
split :: [a] -> Int -> ([a], [a])
split l n = splitAux l 0 n ([], [])
  where
  splitAux [] _ _ x = x
  splitAux (x:xs) i n (f, s)
    | i < n     = appendFirst (splitAux xs (i + 1) n (f, s)) x
    | otherwise = appendSecond (splitAux xs (i + 1) n (f, s)) x

  appendFirst (f, s) x = (x:f, s)
  appendSecond (f, s) x = (f, x:s)

-- 18
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x:xs) l u 
  | l > 1     = slice xs (l - 1) (u - 1)
  | u > 1     = x : slice xs (-1) (u - 1)
  | otherwise = [x] 

-- 19
rotate :: [a] -> Int -> [a]
rotate x n = rotateAux x (mod n (length x)) []
  where
  rotateAux [] _ acc = acc
  rotateAux x 0 acc = x ++ acc
  rotateAux (x:xs) n acc = rotateAux xs (n - 1) (acc ++ [x])

-- 20
removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = (l, x:r)
  where (l, r) = removeAt (n - 1) xs

-- 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x y 1 = x : y
insertAt x (y:ys) n = y : (insertAt x ys (n - 1))

-- 22
range :: Int -> Int -> [Int]
range l u
  | l < u      = l : range (l + 1) u
  | l == u     = [l]
  | otherwise  = []

-- 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select x n = do 
  g <- getStdGen
  rnd_selectAux x n g

  where
    rnd_selectAux _ 0 _ = return []
    rnd_selectAux x n g =
      let 
        (idx, g') = getRand 1 (myLength x) g
        r = removeAt <$> idx <*> pure x
      in
        do
          (ex, rem) <- r
          m <- rnd_selectAux rem (n - 1) g'
          return (ex : m)
  
    getRand :: Int -> Int -> StdGen -> (IO Int, StdGen)
    getRand l u g = do
      let (i, g') = randomR (l, u) g
      (return i, g')
    
-- 24
diff_select :: Int -> Int -> IO [Int]
diff_select n m = rnd_select [1..(m + 1)] n

-- 25
rnd_permu :: [a] -> IO [a]
rnd_permu l = rnd_select l (myLength l)

-- 26
combinations :: Int -> [a] -> [[a]]
combinations 0 l = [[]]
combinations n l = [x:xs | (x, rest)  <- extract l, xs <- combinations (n - 1) rest]
  where
    extract l = [removeAt' i l | i <- [1..myLength l]]
    removeAt' 1 (x:xs) = (x, xs)
    removeAt' n (x:xs) = removeAt' (n - 1) xs

combinations2 :: Int -> [a] -> [[a]]
combinations2 0 l = [[]]
combinations2 _ [] = []
combinations2 n (l:ls) = ts ++ ds
  where
    ts = [ l:xs | xs <- combinations2 (n-1) ls ]
    ds = [ xs   | xs <- combinations2 n ls ]

-- 27
group3 :: [a] -> [[[a]]]
group3 x = [ x1 : x2 : x3 : [] | (x1, x1s) <- combination 2 x, (x2, x2s) <- combination 3 x1s, (x3, x3s) <- combination 4 x2s ]
  where
    combination 0 xs = [([], xs)]
    combination _ [] = []
    combination n (x:xs) =
      let
        ts = [ (x:ys, zs) | (ys, zs) <- combination (n-1) xs ]
        ds = [ (ys, x:zs) | (ys, zs) <- combination n xs ]
      in
        ts ++ ds

group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) l = [ x : gs | (x, xs) <- combination n l, gs <- group ns xs  ]
  where
    combination 0 xs = [([], xs)]
    combination _ [] = []
    combination n (x:xs) =
      let
        ts = [ (x:ys, zs) | (ys, zs) <- combination (n-1) xs ]
        ds = [ (ys, x:zs) | (ys, zs) <- combination n xs ]
      in
        ts ++ ds

-- 28
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = lsort xl ++ x : lsort xh
  where
    xlen = myLength x
    xl = [y | y <- xs, myLength y < xlen]
    xh = [y | y <- xs, myLength y >= xlen]

lfsort :: [[a]] -> [[a]]
lfsort = myConcat . lsort . group ((==) `on` myLength)
  where
    group :: (a -> a -> Bool) -> [a] -> [[a]]
    group _ [] = []
    group p (x:xs) = [x:ts] ++ group p ds
      where
        ts = [ a | a <- xs, p x a ]
        ds = [ a | a <- xs, not (p x a) ]

    myConcat [] = []
    myConcat (x:xs) = x ++ myConcat xs

    on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    on f1 f2 x y = f1 (f2 x) (f2 y)

-- 31
isPrime :: Int -> Bool
isPrime n = findDivisor 2 n
  where
    findDivisor :: Int -> Int -> Bool
    findDivisor i n
      | fromIntegral i > sqrt (fromIntegral n)  = True
      | n `mod` i == 0                          = False
      | otherwise                               = findDivisor (i+1) n

-- 32
myGCD :: Int -> Int -> Int
myGCD x y = myGCDAux (abs x) (abs y) 1
  where
    primes = [ i | i <- [2..], isPrime i ]

    gcdStep x y (d:ds)
      | (x `mod` d == 0) && (y `mod` d == 0)  = d
      | (x < d) || (y < d)                    = 1
      | otherwise                             = gcdStep x y ds

    myGCDAux x y p
      | divPart == 1  = p
      | otherwise     = myGCDAux (x `div` divPart) (y `div` divPart) (p * divPart)
        where divPart = gcdStep x y primes

-- 33
coprime :: Int -> Int -> Bool
coprime x y = (myGCD x y) == 1

-- 34
totient :: Int -> Int
totient 1 = 1
totient x = myLength [i | i <- [1..x - 1], coprime x i]

-- 35
primeFactors :: Int -> [Int]
primeFactors x
  | divPart == 1  = []
  | otherwise     = divPart : primeFactors (x `div` divPart)
  where
    primes = [ i | i <- [2..], isPrime i ]
    
    factStep x (d:ds)
      | x `mod` d == 0  = d
      | d > x           = 1
      | otherwise       = factStep x ds

    divPart = factStep x primes

-- 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult = encode . primeFactors

-- 37
totient2 :: Int -> Int
totient2 = foldr (*) 1 . map (\(m, p) -> (p-1) * p ^ (m - 1)) . prime_factors_mult

-- 39
primesR :: Int -> Int -> [Int]
primesR l u = [ x | x <- [l..u], isPrime x ]

-- 40
goldbach :: Int -> (Int, Int)
goldbach x = goldbachAux (floor half, ceiling half)
  where
    half = (fromIntegral x / 2)
    goldbachAux (x, y)
      | x == 1 || y == 1        = (0, 0)
      | isPrime x && isPrime y  = (x, y)
      | otherwise               = goldbachAux (x - 1, y + 1)

goldbach2 :: Int -> [(Int, Int)]
goldbach2 x = goldbachAux (floor half, ceiling half)
  where
    half = (fromIntegral x / 2)
    goldbachAux (x, y)
      | x == 1 || y == 1        = []
      | isPrime x && isPrime y  = (x, y) : goldbachAux (x - 1, y + 1)
      | otherwise               = goldbachAux (x - 1, y + 1)

-- 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList l u = [ goldbach x | x <- [l..u], x `mod` 2 == 0 ]

-- 46
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _       = False

or' :: Bool -> Bool -> Bool
or' False False  = False
or' _ _          = True

nand' :: Bool -> Bool -> Bool
nand' True True  = False
nand' _ _        = True

nor' :: Bool -> Bool -> Bool
nor' False False  = True
nor' _ _          = False

xor' :: Bool -> Bool -> Bool
xor' x y
  | x == y  = True
  | otherwise = False

impl' :: Bool -> Bool -> Bool
impl' True False  = False
impl' _ _         = True

equ' :: Bool -> Bool -> Bool
equ' x y
  | x == y    = True
  | otherwise = False

table :: (Bool -> Bool -> Bool) -> IO ()
table f = putStrLn $ listToString [show a ++ " " ++ show b ++ " " ++ show (f a b) | a <- [False, True], b <- [False, True]]
  where
    listToString []      = ""
    listToString (x:xs)  = x ++ "\n" ++ listToString xs

-- 47
infixl 2 `or'`
infixl 3 `and'`
infixl 3 `nand'`
infixl 2 `nor'`

-- 48
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = putStrLn $ listToString $ (execute f) <$> (generateCombinations n [])
  where
    generateCombinations :: Int -> [Bool] -> [[Bool]]
    generateCombinations 0 x = return x
    generateCombinations n x = comb >>= next
      where
        comb :: [[Bool]]
        comb = (((++)x) <$> (pure <$> [False, True]))

        next :: [Bool] -> [[Bool]]
        next = (generateCombinations (n-1))

    execute f x = printOperands x ++ show (f x)

    printOperands []     = ""
    printOperands (x:xs) = show x ++ " " ++ printOperands xs
    
    listToString []      = ""
    listToString (x:xs)  = x ++ "\n" ++ listToString xs

-- 49
gray :: Int -> [[Char]]
gray 0 = [[]]
gray x = (('0':) <$> xs) ++ (('1':) <$> (myReverse $ xs))
  where
    xs = gray (x-1)

-- 50
data Htree a = Leaf a Int | HNode (Htree a) (Htree a) deriving Show

huffman :: [(Char, Int)] -> [(Char, String)]
huffman x = showResult "" (huffmanAux $ parseInput x)
  where
    sort []      = []
    sort (x:xs)  = sort ls ++ x : sort hs
      where
        freq (Leaf _ f) = f
        freq (HNode l r) = freq l + freq r
        ls = [ l | l <- xs, freq l < freq x ]
        hs = [ h | h <- xs, freq h >= freq x ]

    parseInput :: [(a, Int)] -> [Htree a]
    parseInput xs = convertData <$> xs
    convertData (c, f) = Leaf c f

    huffmanAux (x1:x2:xs) = huffmanAux $ hufmanStep $ sort (x1:x2:xs)
    huffmanAux (x:xs) = x

    hufmanStep (x1:x2:xs) = HNode x1 x2 : xs

    showResult a (Leaf c _) = [(c, a)]
    showResult a (HNode l r) = showResult (a++['0']) l ++ showResult (a++['1']) r

-- 55
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree n
  | l == u     = ds
  | otherwise  = ds ++ ts
    where
      l = floor $ fromIntegral (n - 1) / 2
      u = ceiling $ fromIntegral (n - 1) / 2

      ds = [Branch 'x' d t | d <- cbalTree l, t <- cbalTree u]
      ts = [Branch 'x' d t | d <- cbalTree u, t <- cbalTree l]

-- 56
symmetric :: (Eq a) => Tree a -> Bool
symmetric (Empty) = True
symmetric (Branch _ l r) = mirror l r

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) = (mirror l1 r2) && (mirror r1 l2)
mirror _ _ = False

-- 57
construct :: [Int] -> Tree Int
construct [] = Empty
construct (x:xs) = Branch x (construct ls) (construct hs)
  where
    ls = [ l | l <- xs, l <= x ]
    hs = [ h | h <- xs, h > x  ]

-- 58
symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree

-- 59
hbalTree :: a -> Int -> [Tree a]
hbalTree _ 0 = [Empty]
hbalTree x 1 = [Branch x Empty Empty]
hbalTree x n = ts ++ ds
  where
    ts = [ Branch x t d | t <- hbalTree x (n-1), d <- hbalTree x (n-2) ]
    ds = [ Branch x t d | t <- hbalTree x (n-2), d <- hbalTree x (n-1) ]

-- 60
hbalTreeNodes x n = 
  let
    l = minHeight n
    u = maxHeight n
  in
    concat [constructHbal h n | h <- [l..u]]
  where
    maxNodes h = 2^h - 1
    minNodes h = fibs !! (h+2) - 1
    maxHeight n = length (takeWhile (<= n+1) fibs) - 3
    minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

    constructHbal _ 0 = [Empty]
    constructHbal 1 _ = [Branch x Empty Empty]
    constructHbal h n = [ Branch x l r | 
          (hl, hr) <- [(h-1, h-2), (h-1, h-1), (h-2, h-1)],
          let minL = max (minNodes hl) (n - 1 - maxNodes hr),
          let maxL = min (maxNodes hl) (n - 1 - minNodes hr),
          nl <- [minL..maxL],
          let nr = n - 1 - nl,
          l <- constructHbal hl nl,
          r <- constructHbal hr nr ]

-- 61
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                  (Branch 2 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- 61A
leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

-- 62
internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x : (internals l ++ internals r)

-- 62B
atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x _ _) 1 = [x]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

-- 63
completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = completeBTAux 1
  where
    completeBTAux :: Int -> Tree Char
    completeBTAux x
      | x > n     = Empty
      | otherwise = Branch 'x' (completeBTAux (x*2)) (completeBTAux (x*2+1))

-- 64
tree64 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'h'
                                        (Branch 'g'
                                                (Branch 'e' Empty Empty)
                                                Empty
                                        )
                                        Empty
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 's'
                                        (Branch 'q' Empty Empty)
                                        Empty
                                )
                        )
                        Empty
                )

layout :: Tree a -> Tree (a, (Int, Int))
layout x = layoutAux x 1 0
  where
    layoutAux :: Tree a -> Int -> Int -> Tree (a, (Int, Int))
    layoutAux Empty _ _ = Empty
    layoutAux (Branch x l r) h w = Branch (x, (vx, vy)) (newl) (newr)
      where
        newl = layoutAux l (h+1) w
        newr = layoutAux r (h+1) vx
        vy = h
        vx = rightmost newl + 1
        rightmost Empty = w
        rightmost (Branch (_, (nvx, _)) _ Empty) = nvx
        rightmost (Branch _ _ r) = rightmost r

-- 65
tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

layout2 :: Tree a -> Tree (a, (Int, Int))
layout2 x = layout2Aux x (d-1) 0
  where
    depth Empty = 0
    depth (Branch _ l r) = max (depth l) (depth r) + 1

    d = depth x

    layout2Aux :: Tree a -> Int -> Int -> Tree (a, (Int, Int))
    layout2Aux Empty _ _ = Empty
    layout2Aux (Branch x _ _) 0 w = Branch (x, (w, d)) Empty Empty
    layout2Aux (Branch x l r) h w = Branch (x, (vx, vy)) newl newr
      where
        vy = d - h
        offset = 2^(h-1)
        newl = layout2Aux l (h-1) (w - offset)
        vx = alignSelf (leftx newl) offset
        newr = layout2Aux r (h-1) (vx + offset)

        leftx Empty = max (w - offset) 0
        leftx (Branch (_, (nvx, _)) _ _) = nvx

        alignSelf 0 _ = 1
        alignSelf l o = l + o

-- 66
layout3 :: Tree a -> Traa (a, (Int, Int))
layout3 x = _
  where
    layout3Aux Empty _ = Empty 
    layout3Aux (Branch x l r) w h = Branch (x, (w, h)) newL newR
      where
        (newL, ll, lr) = layout3Aux l (w-shift) (h+1)
        (newR, rl, rr) = layour3Aux r (w+shift) (h+1)

        chainDistance = tail $ zipWith (-) lChainSum rChainSum
        chainOverlap = filter (>=0) chainDistance
        maxOverlap = listMaximum chainOverlap
        shift = ceiling (fromIntegral maxOverlap / 2)

        newLChain = (-shift):lChain
        newRChain =  (shift):rChain

        listMaximum [] = error "list empty"
        listMaximum (x:xs) = foldl (\x y -> max x y) x xs
    