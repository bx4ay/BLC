import Data.List (unfoldr)
import System.Environment (getArgs)
import System.IO (hSetBinaryMode, hSetBuffering, stdin, stdout, BufferMode (NoBuffering))

data Expr = V Int | S | K | I | B | C | N | P | A Expr Expr

a :: Expr -> Expr -> Expr
a (A (A S x) y) z = a (a x z) $ a y z
a (A K x) _ = x
a I x = x
a (A (A B x) y) z = a x $ a y z
a (A (A C x) y) z = a (a x z) y
a (A N _) x = x
a (A (A P x) y) z = a (a z x) y
a x y = A x y

parse :: [Char] -> Expr
parse = eval . head . parse' . filter (`elem` "01") . concatMap (takeWhile (/= '#')) . lines
    where
        eval :: Expr -> Expr
        eval (A x y) = a (eval x) $ eval y
        eval x = x

        parse' :: [Char] -> [Expr]
        parse' ('0' : '0' : t) = (\ (x : t) -> l x : t) $ parse' t
        parse' ('0' : '1' : t) = (\ (x : y : t) -> A x y : t) $ parse' t
        parse' ('1' : '0' : t) = V 0 : parse' t
        parse' ('1' : t) = (\ (V i : t) -> V (i + 1) : t) $ parse' t
        parse' _ = []

        l :: Expr -> Expr
        l (V 0) = I
        l (A x y) = case (free x, free y) of
            (False, True) -> case y of
                V 0 -> l' x
                y -> A (A B $ l' x) $ l y
            (True, False) -> A (A C $ l x) $ l' y
            (True, True) -> A (A S $ l x) $ l y
            _ -> A K $ A (l' x) $ l' y
        l x = A K $ l' x

        free :: Expr -> Bool
        free (V 0) = True
        free (A x y) = free x || free y
        free _ = False

        l' :: Expr -> Expr
        l' (V i) = V $ i - 1
        l' (A x y) = A (l' x) $ l' y
        l' x = x

church8 :: [Char] -> Expr
church8 = cL . map (cL . map cB . reverse . take 8 . unfoldr (\ x -> Just (odd x, div x 2)) . fromEnum)
    where
        cL :: [Expr] -> Expr
        cL = foldr (A . A P) N

        cB :: Bool -> Expr
        cB True = K
        cB False = N

unchurch8 :: Expr -> [Char]
unchurch8 = map (toEnum . foldl (\ x y -> 2 * x + fromEnum y) 0 . take 8 . map uncB . uncL) . uncL
    where
        uncB :: Expr -> Bool
        uncB K = True
        uncB N = False
        uncB x = uncB $ a (a x K) N

        uncL :: Expr -> [Expr]
        uncL x
            | uncB $ a (a x $ A K $ A K $ A K N) K = []
            | otherwise = a x K : uncL (a x N)

main :: IO ()
main = do
    args <- getArgs
    (b, codes) <- f args
    hSetBinaryMode stdin b
    hSetBinaryMode stdout b
    hSetBuffering stdout NoBuffering
    input <- getContents
    putStr $ unchurch8 $ foldl1 (flip a) $ church8 input : map parse codes
    where
        f :: [[Char]] -> IO (Bool, [[Char]])
        f ("-b" : x) = sequence (True, sequence $ g x)
        f x = sequence (False, sequence $ g x)

        g :: [[Char]] -> [IO [Char]]
        g ("-e" : x : t) = pure x : if null t then [] else g t
        g (x : t) = readFile x : if null t then [] else g t
        g _ = [getLine]
