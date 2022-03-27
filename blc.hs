import Data.List (unfoldr)
import System.Environment (getArgs)
import System.IO (hSetBinaryMode, hSetBuffering, stdin, stdout, BufferMode (NoBuffering))

data Expr = I Int | L Expr | A Expr Expr

app :: Expr -> Expr -> Expr
app (L x) y = beta 0 x y
    where
        beta :: Int -> Expr -> Expr -> Expr
        beta i (I j) x
            | i < j = I $ j - 1
            | i == j = beta' 0 i x
            | otherwise = I j
        beta i (L x) y = L $ beta (i + 1) x y
        beta i (A x y) z = app (beta i x z) $ beta i y z

        beta' :: Int -> Int -> Expr -> Expr
        beta' i j (I k)
            | i <= k = I $ j + k
            | otherwise = I k
        beta' i j (L x) = L $ beta' (i + 1) j x
        beta' i j (A x y) = app (beta' i j x) $ beta' i j y
app x y = A x y

parse :: [Char] -> Expr
parse = head . parse' . filter (`elem` "01")
    where
        parse' :: [Char] -> [Expr]
        parse' ('0' : '0' : t) = (\ (x : t) -> L x : t) $ parse' t
        parse' ('0' : '1' : t) = (\ (x : y : t) -> app x y : t) $ parse' t
        parse' ('1' : '0' : t) = I 0 : parse' t
        parse' ('1' : t) = (\ (I i : t) -> I (i + 1) : t) $ parse' t
        parse' _ = []

church :: [Char] -> Expr
church = cList . map (cBool . toBin) . filter (`elem` "01")
    where
        cList :: [Expr] -> Expr
        cList = foldr (\ x -> L . app (app (I 0) x)) $ L $ L $ I 0

        cBool :: Bool -> Expr
        cBool = L . L . I . fromEnum

        toBin :: Char -> Bool
        toBin = (== '1')

unchurch :: Expr -> [Char]
unchurch = map (fromBin . uncBool) . uncList
    where
        fromBin :: Bool -> Char
        fromBin x = if x then '1' else '0'

        uncBool :: Expr -> Bool
        uncBool (L (L (I 1))) = True
        uncBool (L (L (I 0))) = False
        uncBool x = uncBool $ app (app x $ L $ L $ I 1) $ L $ L $ I 0

        uncList :: Expr -> [Expr]
        uncList (L (L (I 0))) = []
        uncList (L (A (A (I 0) x) y)) = x : uncList y
        uncList x
            | uncBool (app (app x $ L $ L $ L $ L $ L $ I 0) $ L $ L $ I 1) = []
            | otherwise = app x (L $ L $ I 1) : uncList (app x $ L $ L $ I 0)

main :: IO ()
main = do
    args <- getArgs
    (b, codes) <- f args
    hSetBinaryMode stdin b
    hSetBinaryMode stdout b
    hSetBuffering stdout NoBuffering
    eval codes
    where
        f :: [[Char]] -> IO (Bool, [[Char]])
        f ("-b" : x) = sequence (True, sequence $ g x)
        f x = sequence (False, sequence $ g x)

        g :: [[Char]] -> [IO [Char]]
        g ("-e" : x : t) = pure x : g t
        g (x : t) = readFile x : g t
        g _ = []

        eval :: [[Char]] -> IO ()
        eval [] = do
            code <- getLine
            eval [code]
        eval codes = do
            input <- getContents
            putStr $ unchurch $ foldl1 (flip app) $ (if null codes then parse else church) input : (parse <$> codes)
