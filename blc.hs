import Data.List (unfoldr)
import System.Environment (getArgs)

data Expr = I Int | L Expr | A Expr Expr

eval :: Expr -> Expr
eval (I i) = I i
eval (L x) = L $ eval x
eval (A x y) = apply (eval x) $ eval y
    where
        apply :: Expr -> Expr -> Expr
        apply (L x) y = eval $ beta 0 x y
        apply x y = A x y

        beta :: Int -> Expr -> Expr -> Expr
        beta i (I j) x
            | i < j = I $ j - 1
            | i == j = beta' 0 i x
            | otherwise = I j
        beta i (L x) y = L $ beta (i + 1) x y
        beta i (A x y) z = A (beta i x z) $ beta i y z

        beta' :: Int -> Int -> Expr -> Expr
        beta' i j (I k)
            | i <= k = I $ j + k
            | otherwise = I k
        beta' i j (L x) = L $ beta' (i + 1) j x
        beta' i j (A x y) = A (beta' i j x) $ beta' i j y

fromCode :: [Char] -> Expr
fromCode = head . foldl f [] . tail . foldl g [0]
    where
        f :: [Expr] -> Int -> [Expr]
        f (x : t) 0 = L x : t
        f (x : y : t) 1 = A x y : t
        f t i = I (i - 2) : t

        g :: [Int] -> Char -> [Int]
        g (0 : t) '0' = 1 : t
        g (0 : t) '1' = 2 : t
        g (1 : t) '0' = 0 : 0 : t
        g (1 : t) '1' = 0 : 1 : t
        g (i : t) '0' = 0 : i : t
        g (i : t) '1' = i + 1 : t
        g t _ = t

church :: [Char] -> Expr
church = clist . map cbool . toBin
    where
        clist :: [Expr] -> Expr
        clist = foldr (\ x -> L . A (A (I 0) x)) $ L $ L $ I 0

        cbool :: Bool -> Expr
        cbool = L . L . I . fromEnum

        toBin :: [Char] -> [Bool]
        toBin = concatMap $ reverse . take 8 . unfoldr (\ x -> Just (odd x, div x 2)) . fromEnum

unchurch :: Expr -> [Char]
unchurch = fromBin . map uncbool . unclist
    where
        fromBin :: [Bool] -> [Char]
        fromBin x
            | null $ drop 7 x = []
            | otherwise = toEnum (foldl (\ y z -> 2 * y + fromEnum z) 0 $ take 8 x) : fromBin (drop 8 x)

        uncbool :: Expr -> Bool
        uncbool (L (L (I 1))) = True
        uncbool (L (L (I 0))) = False
        uncbool x = uncbool $ eval $ A (A x $ L $ L $ I 1) $ L $ L $ I 0

        unclist :: Expr -> [Expr]
        unclist (L (L (I 0))) = []
        unclist (L (A (A (I 0) x) y)) = x : unclist y
        unclist x
            | uncbool $ eval $ A (A x $ L $ L $ L $ L $ L $ I 0) $ L $ L $ I 1 = []
            | otherwise = eval (A x $ L $ L $ I 1) : unclist (eval $ A x $ L $ L $ I 0)

main :: IO ()
main = do
    [path] <- getArgs
    code <- readFile path
    input <- getContents
    putStr $ unchurch $ A (fromCode code) $ church input
