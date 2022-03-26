import Data.List (unfoldr)
import System.Environment (getArgs)

data Expr = I Int | L Expr | A Expr Expr

eval :: Expr -> Expr
eval (I i) = I i
eval (L x) = L $ eval x
eval (A x y) = apply (eval x) $ eval y
    where
        apply :: Expr -> Expr -> Expr
        apply (L x) y = beta 0 x y
        apply x y = A x y

        beta :: Int -> Expr -> Expr -> Expr
        beta i (I j) x
            | i < j = I $ j - 1
            | i == j = beta' 0 i x
            | otherwise = I j
        beta i (L x) y = L $ beta (i + 1) x y
        beta i (A x y) z = apply (beta i x z) $ beta i y z

        beta' :: Int -> Int -> Expr -> Expr
        beta' i j (I k)
            | i <= k = I $ j + k
            | otherwise = I k
        beta' i j (L x) = L $ beta' (i + 1) j x
        beta' i j (A x y) = apply (beta' i j x) $ beta' i j y

parse :: [Char] -> Expr
parse = head . parse' . filter (`elem` "01")
    where
        parse' :: [Char] -> [Expr]
        parse' ('0' : '0' : t) = (\ (x : t) -> L x : t) $ parse' t
        parse' ('0' : '1' : t) = (\ (x : y : t) -> A x y : t) $ parse' t
        parse' ('1' : '0' : t) = I 0 : parse' t
        parse' ('1' : t) = (\ (I i : t) -> I (i + 1) : t) $ parse' t
        parse' _ = []

church :: [Char] -> Expr
church = cList . map (cBool . toBin) . filter (`elem` "01")
    where
        cList :: [Expr] -> Expr
        cList = foldr (\ x -> L . A (A (I 0) x)) $ L $ L $ I 0

        cBool :: Bool -> Expr
        cBool = L . L . I . fromEnum

        toBin :: Char -> Bool
        toBin = (== '1')

unchurch :: Expr -> [Char]
unchurch = map (fromBin . uncBool) . uncList
    where
        fromBin :: Bool -> Char
        fromBin True = '1'
        fromBin False = '0'

        uncBool :: Expr -> Bool
        uncBool (L (L (I 1))) = True
        uncBool (L (L (I 0))) = False
        uncBool x = uncBool $ eval $ A (A x $ L $ L $ I 1) $ L $ L $ I 0

        uncList :: Expr -> [Expr]
        uncList (L (L (I 0))) = []
        uncList (L (A (A (I 0) x) y)) = x : uncList y
        uncList x
            | uncBool (eval $ A (A x $ L $ L $ L $ L $ L $ I 0) $ L $ L $ I 1) = []
            | otherwise = eval (A x $ L $ L $ I 1) : uncList (eval $ A x $ L $ L $ I 0)

main :: IO ()
main = do
    [path] <- getArgs
    code <- readFile path
    input <- getContents
    putStrLn $ unchurch $ A (parse code) $ church input
