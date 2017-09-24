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

reducer :: Lexp -> Lexp
reducer v@(Atom _) = v
reducer lexp@(Lambda x m) = Lambda x (reducer m)
reducer lexp@(Apply m e) = reduce (reducer m) (reducer e)

reduce :: Lexp -> Lexp -> Lexp
reduce (Lambda x e) m = reducer (trav_lexp x e m)	-- (\x.e m)
reduce x y = Apply x y															-- (x y)

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

alpha :: Lexp -> Lexp
alpha Lambda(x m) = rename x m

rename :: String -> Lexp -> Lexp
rename x (Lambda _ lexp) = 
rename x (Apply _ lexp) =

-- Entry point of program
main = do
    args <- getArgs
    let filename = case args of { [x] -> x; _ -> "input.lambda" }
    -- id' simply returns its input, so runProgram will result
    -- in printing each lambda expression twice. 
    runProgram filename reducer
