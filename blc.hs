import Data.List (unfoldr)
import System.Environment (getArgs)

data Expr = I Int | L Expr | A Expr Expr
    | Cint Int | Csuc | Cbool Bool -- for Church decoding

-- evaluation
eval :: Expr -> Expr
eval (L x) = L $ eval x
eval (A x y) = apply (eval x) $ eval y
    where
        apply :: Expr -> Expr -> Expr
        apply (L x) y = eval $ beta 0 x y
        apply Csuc (Cint i) = Cint $ i + 1
        apply x y = A x y
eval x = x

beta :: Int -> Expr -> Expr -> Expr
beta i (I j) x
    | i < j = I $ j - 1
    | i > j = I j
    | otherwise = beta' 0 i x
    where
        beta' :: Int -> Int -> Expr -> Expr
        beta' i j (I k)
            | i > k = I k
            | otherwise = I $ j + k
        beta' i j (L x) = L $ beta' (i + 1) j x
        beta' i j (A x y) = A (beta' i j x) $ beta' i j y
        beta' _ _ x = x
beta i (L x) y = L $ beta (i + 1) x y
beta i (A x y) z = A (beta i x z) $ beta i y z
beta _ x _ = x

-- Church encoding
cint :: Int -> Expr
cint = (iterate (\ (L (L x)) -> L $ L $ A (I 1) x) (L $ L $ I 0) !!)

uncint :: Expr -> Int
uncint (Cint i) = i
uncint x = uncint $ eval $ A (A x Csuc) $ Cint 0

cbool :: Bool -> Expr
cbool = L . L . I . fromEnum

uncbool :: Expr -> Bool
uncbool (Cbool b) = b
uncbool x = uncbool $ eval $ A (A x $ Cbool True) $ Cbool False

clist :: [Expr] -> Expr
clist = foldr (\ x -> L . A (A (I 0) x)) $ L $ L $ I 0

unclist :: Expr -> [Expr]
unclist x
    | uncbool $ eval $ A (A x $ L $ L $ L $ Cbool False) $ Cbool True = []
    | otherwise = eval (A x $ L $ L $ I 1) : unclist (eval $ A x $ L $ L $ I 0)

toBin :: [Char] -> [Bool]
toBin = concatMap (reverse . take 8 . unfoldr (\ x -> Just (odd x, div x 2)) . fromEnum)

fromBin :: [Bool] -> [Char]
fromBin x
    | length x < 8 = ""
    | otherwise = toEnum (foldl (\ y z -> 2 * y + fromEnum z) 0 $ take 8 x) : fromBin (drop 8 x)

encode :: [Char] -> Expr
encode = clist . map cbool . toBin

decode :: Expr -> [Char]
decode = fromBin . map uncbool . unclist

encode8 :: [Char] -> Expr
encode8 = clist . map (cint . fromEnum)

decode8 :: Expr -> [Char]
decode8 = map (toEnum . uncint) . unclist

-- parser
toExpr :: [Char] -> Expr
toExpr = head . foldl f [] . snd . foldl g (Nothing, [])
    where
        f :: [Expr] -> Int -> [Expr]
        f (x : t) (- 2) = L x : t
        f (x : y : t) (- 1) = A x y : t
        f t i = I i : t

        g :: (Maybe Int, [Int]) -> Char -> (Maybe Int, [Int])
        g (Nothing, t) '0' = (Just 0, t)
        g (Nothing, t) '1' = (Just 1, t)
        g (Just 0, t) '0' = (Nothing, - 2 : t)
        g (Just 0, t) '1' = (Nothing, - 1 : t)
        g (Just i, t) '0' = (Nothing, i - 1 : t)
        g (Just i, t) '1' = (Just $ i + 1, t)
        g t _ = t

-- run
run :: [Char] -> [Char] -> [Char]
run x y = decode $ A (toExpr x) $ encode y

run8 :: [Char] -> [Char] -> [Char]
run8 x y = decode8 $ A (toExpr x) $ encode8 y

main :: IO ()
main = do
    [path] <- getArgs
    code <- readFile path
    input <- getContents
    putStr $ run code input
