import System.Environment (getArgs)
import System.IO (hSetBinaryMode, hSetBuffering, stdin, stdout, BufferMode (NoBuffering))

data Expr = I Int | L Expr | A Expr Expr

a :: Expr -> Expr -> Expr
a (L x) y = b 0 x y
    where
        b :: Int -> Expr -> Expr -> Expr
        b i (I j) x
            | i < j = I $ j - 1
            | i == j = b' 0 i x
            | otherwise = I j
        b i (L x) y = L $ b (i + 1) x y
        b i (A x y) z = a (b i x z) $ b i y z

        b' :: Int -> Int -> Expr -> Expr
        b' i j (I k)
            | i <= k = I $ j + k
            | otherwise = I k
        b' i j (L x) = L $ b' (i + 1) j x
        b' i j (A x y) = a (b' i j x) $ b' i j y
a x y = A x y

parse :: [Char] -> Expr
parse = head . parse' . filter (`elem` "01") . concatMap (takeWhile (/= '#')) . lines
    where
        parse' :: [Char] -> [Expr]
        parse' ('0' : '0' : t) = (\ (x : t) -> L x : t) $ parse' t
        parse' ('0' : '1' : t) = (\ (x : y : t) -> a x y : t) $ parse' t
        parse' ('1' : '0' : t) = I 0 : parse' t
        parse' ('1' : t) = (\ (I i : t) -> I (i + 1) : t) $ parse' t
        parse' _ = []

church :: [Char] -> Expr
church = cL . map (cB . (== '1')) . filter (`elem` "01")
    where
        cL :: [Expr] -> Expr
        cL = foldr (\ x -> L . a (A (I 0) x)) $ L $ L $ I 0

        cB :: Bool -> Expr
        cB = L . L . I . fromEnum

unchurch :: Expr -> [Char]
unchurch = map (("01" !!) . fromEnum . uncB) . uncL
    where
        uncB :: Expr -> Bool
        uncB (L (L (I 1))) = True
        uncB (L (L (I 0))) = False
        uncB x = uncB $ a (a x $ L $ L $ I 1) $ L $ L $ I 0

        uncL :: Expr -> [Expr]
        uncL x
            | uncB $ a (a x $ L $ L $ L $ L $ L $ I 0) $ L $ L $ I 1 = []
            | otherwise = a x (L $ L $ I 1) : uncL (a x $ L $ L $ I 0)

main :: IO ()
main = do
    args <- getArgs
    (b, codes) <- f args
    hSetBinaryMode stdin b
    hSetBinaryMode stdout b
    hSetBuffering stdout NoBuffering
    input <- getContents
    putStrLn $ unchurch $ foldl1 (flip a) $ church input : map parse codes
    where
        f :: [[Char]] -> IO (Bool, [[Char]])
        f ("-b" : x) = sequence (True, sequence $ g x)
        f x = sequence (False, sequence $ g x)

        g :: [[Char]] -> [IO [Char]]
        g ("-e" : x : t) = pure x : if null t then [] else g t
        g (x : t) = readFile x : if null t then [] else g t
        g _ = [getLine]
