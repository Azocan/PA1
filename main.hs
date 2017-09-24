import PA1Helper
import Data.Set
import qualified Data.Set as Set
import Data.Map
import System.Environment (getArgs)

-- Haskell representation of lambda expression
-- data Lexp = Atom String | Lambda String Lexp | Apply Lexp  Lexp 

-- Given a filename and function for reducing lambda expressions,
-- reduce all valid lambda expressions in the file and output results.
-- runProgram :: String -> (Lexp -> Lexp) -> IO ()

-- This is the identity function for the Lexp datatype, which is
-- used to illustrate pattern matching with the datatype. "_" was
-- used since I did not need to use bound variable. For your code,
-- however, you can replace "_" with an actual variable name so you
-- can use the bound variable. The "@" allows you to retain a variable
-- that represents the entire structure, while pattern matching on
-- components of the structure.
id' :: Lexp -> Lexp
id' v@(Atom _) = v
id' lexp@(Lambda _ _) = lexp
id' lexp@(Apply _ _) = lexp 

generate :: [Char] -> [String]
generate char = Prelude.map (:[]) char


reducer :: Lexp -> Lexp
reducer ex = do
    let set1 = Set.fromList (generate ['a'..'z'])
    alpha set1 ex


beta :: Lexp -> Lexp
beta v@(Atom _) = v
beta lexp@(Lambda x e) = Lambda x (beta e)
beta lexp@(Apply l r) = reduce (beta l) (beta r)

reduce :: Lexp -> Lexp -> Lexp
reduce (Lambda x e) m = beta (trav_lexp x e m)
reduce l r = Apply l r

trav_lexp :: String -> Lexp -> Lexp -> Lexp
trav_lexp x e@(Atom _) m = replace x e m
trav_lexp x (Lambda v t) m = Lambda v (trav_lexp x t m)
trav_lexp x (Apply a b) m = Apply (trav_lexp x a m) (trav_lexp x b m)

-- Compares the string a to the string x within the atom b. 
-- If x == a, then returns the replacement Lexp c.
-- Otherwise, returns the original Atom b. 
-- The string to replace -> the atom being examined -> the replacement ->the result
replace :: String -> Lexp -> Lexp -> Lexp
replace a b@(Atom x) c = if x == a then c else b


-- Recursively find eta-convertable expressions \x.(f y)
eta :: Lexp -> Lexp
eta v@(Atom _) = v
eta lexp@(Lambda x (Apply l r)) = convert x l r
eta lexp@(Lambda x e) = Lambda x (eta e)
eta lexp@(Apply l r) = Apply (eta l) (eta r)

-- Check x == y and x is not free in f
convert :: String -> Lexp -> Lexp -> Lexp
convert x a@(Atom v) b@(Atom vx) = if x == vx && a /= b then Atom v else Lambda x (Apply a b)
convert x a@(Apply l r) b@(Atom vx) = if x == vx && l /= b && r /= b then eta (Apply l r) else Lambda x (Apply a b)
convert x a b = Lambda x (Apply a b)

alpha :: Set String -> Lexp -> Lexp
alpha s v@(Atom _) = v
alpha s (Lambda x m) = Lambda new (alpha new_set (a_rename new x m)) where
    temp_set = Set.delete x s
    tuple = (Set.deleteFindMin temp_set)
    new = fst(tuple)
    new_set = snd(tuple)
alpha s lexp@(Apply a b) = Apply (alpha s a) (alpha s b)

a_rename :: String -> String -> Lexp -> Lexp
a_rename x y m@(Atom b) = if y == b then Atom x else m
a_rename x y m@(Lambda l r) = Lambda l (a_rename x y r)
a_rename x y m@(Apply l r) = Apply (a_rename x y l) (a_rename x y r)


-- Entry point of program
main = do
    args <- getArgs
    let filename = case args of { [x] -> x; _ -> "input.lambda" }
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram filename reducer
