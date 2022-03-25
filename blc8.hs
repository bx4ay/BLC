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

parse :: [Char] -> Expr
parse = head . parse'
    where
        parse' :: [Char] -> [Expr]
        parse' ('0' : '0' : t) = (\ (x : y) -> L x : y) $ parse' t
        parse' ('0' : '1' : t) = (\ (x : y : z) -> A x y : z) $ parse' t
        parse' ('1' : '0' : t) = I 0 : parse' t
        parse' ('1' : t) = (\ (I i : x) -> I (i + 1) : x) $ parse' t
        parse' (_ : t) = parse' t
        parse' _ = []

church8 :: [Char] -> Expr
church8 = clist . map (clist . map cbool . toBin8)
    where
        clist :: [Expr] -> Expr
        clist = foldr (\ x -> L . A (A (I 0) x)) $ L $ L $ I 0

        cbool :: Bool -> Expr
        cbool = L . L . I . fromEnum

        toBin8 :: Char -> [Bool]
        toBin8 = reverse . take 8 . unfoldr (\ x -> Just (odd x, div x 2)) . fromEnum

unchurch8 :: Expr -> [Char]
unchurch8 = map (fromBin8 . map uncbool . unclist) . unclist
    where
        fromBin8 :: [Bool] -> Char
        fromBin8 = toEnum . foldl (\ y z -> 2 * y + fromEnum z) 0

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
    putStr $ unchurch8 $ A (parse code) $ church8 input
