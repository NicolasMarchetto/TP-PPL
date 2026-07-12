module Primitive where

import qualified Data.Map as Map
import qualified Distribution
import Data.List (transpose)
import DataVD


-- Auxiliary functions

relu :: Double -> Double
relu x = max x 0.0

repmat :: [[Double]] -> Int -> Int -> [[Double]]
repmat m r c =
    concat (replicate r expanded)
    where
        expanded = map (\row -> concat (replicate c row)) m


matAdd :: [[Double]] -> [[Double]] -> [[Double]]
matAdd a b =
    zipWith (zipWith (+)) a b

matMul :: [[Double]] -> [[Double]] -> [[Double]]
matMul a b =
    let bt = transpose b
    in [[sum (zipWith (*) row col) | col <- bt] | row <- a]

---------------------------------------------------------
type Primitive = [Value] -> Value


numPrim :: Value -> Double
numPrim (Number x) = x
numPrim (Integer x) = fromIntegral x
numPrim (Boolean True) = 1
numPrim (Boolean False) = 0
numPrim _ = error "Expected number"

addPrim :: Primitive
addPrim xs =
    Number (sum (map numPrim xs))

subPrim :: Primitive
subPrim [] =
    error "- needs arguments"

subPrim (x:xs) =
    Number (foldl (-) (numPrim x) (map numPrim xs))

subPrim [x] =
    Number (- numPrim x)

mulPrim :: Primitive
mulPrim xs =
    Number (product (map numPrim xs))



dividePrim :: Primitive
dividePrim [] =
    error "/ needs arguments"

dividePrim (x:xs) =
    Number (foldl (/) (numPrim x) (map numPrim xs))

dividePrim [x] =
    Number (1 / numPrim x)


sqrtPrim :: Primitive
sqrtPrim [x] =
    Number (sqrt (numPrim x))

sqrtPrim _ =
    error "sqrt expects one argument"



expPrim :: Primitive
expPrim [x] =
    Number (exp (numPrim x))

expPrim _ =
    error "exp expects one argument"



logPrim :: Primitive
logPrim [x] =
    Number (log (numPrim x))

logPrim _ =
    error "log expects one argument"



powPrim :: Primitive
powPrim [x,y] =
    Number ((numPrim x) ** (numPrim y))

powPrim _ =
    error "pow expects two arguments"



absPrim :: Primitive
absPrim [x] =
    Number (abs (numPrim x))

absPrim _ =
    error "abs expects one argument"



floorPrim :: Primitive
floorPrim [x] =
    Integer (floor (numPrim x))

floorPrim _ =
    error "floor expects one argument"



ceilPrim :: Primitive
ceilPrim [x] =
    Integer (ceiling (numPrim x))

ceilPrim _ =
    error "ceil expects one argument"



tanhPrim :: Primitive
tanhPrim [x] =
    Number (tanh (numPrim x))

tanhPrim _ =
    error "tanh expects one argument"

maxPrim :: Primitive
maxPrim xs =
    Number (maximum (map numPrim xs))


minPrim :: Primitive
minPrim xs =
    Number (minimum (map numPrim xs))


modPrim :: Primitive
modPrim [a,b] =
    Integer (mod (round (numPrim a)) (round (numPrim b)))

modPrim _ =
    error "mod expects two arguments"

eqPrim :: Primitive
eqPrim [a,b] =
    Boolean (a == b)

eqPrim _ =
    error "= expects two arguments"

neqPrim :: Primitive
neqPrim [a,b] =
    Boolean (a /= b)

neqPrim _ =
    error "!= expects two arguments"



lessPrim :: Primitive
lessPrim [a,b] =
    Boolean (numPrim a < numPrim b)

lessPrim _ =
    error "< expects two arguments"



greaterPrim :: Primitive
greaterPrim [a,b] =
    Boolean (numPrim a > numPrim b)

greaterPrim _ =
    error "> expects two arguments"



lessEqPrim :: Primitive
lessEqPrim [a,b] =
    Boolean (numPrim a <= numPrim b)

lessEqPrim _ =
    error "<= expects two arguments"



greaterEqPrim :: Primitive
greaterEqPrim [a,b] =
    Boolean (numPrim a >= numPrim b)

greaterEqPrim _ =
    error ">= expects two arguments"



andPrim :: Primitive
andPrim xs =
    Boolean (all isTrue xs)
    where
        isTrue (Boolean b) = b
        isTrue _ = False



orPrim :: Primitive
orPrim xs =
    Boolean (any isTrue xs)
    where
        isTrue (Boolean b) = b
        isTrue _ = False



notPrim :: Primitive
notPrim [Boolean x] =
    Boolean (not x)

notPrim _ =
    error "not expects boolean"


vectorPrim :: Primitive
vectorPrim xs =
    Vector xs


hashPrim :: Primitive
hashPrim xs
    | odd (length xs) =
        error "hash-map requires pairs"
    | otherwise =
        HashMap (build xs)      
    where
        build [] =
            Map.empty
        build (k:v:rest) =
            Map.insert (show k) v (build rest)
        build _ =
            Map.empty



getPrim :: Primitive
getPrim [HashMap m, key] =
    case Map.lookup (show key) m of
        Just x -> x
        Nothing -> Boolean False

getPrim _ =
    error "get error"

putPrim :: Primitive
putPrim [HashMap m, key, value] =
    HashMap (Map.insert (show key) value m)

putPrim _ =
    error "put error"

firstPrim :: Primitive
firstPrim [Vector (x:_)] =
    x

firstPrim _ =
    error "first error"



secondPrim :: Primitive
secondPrim [Vector (_:x:_)] =
    x

secondPrim _ =
    error "second error"



lastPrim :: Primitive
lastPrim [Vector xs] =
    last xs

lastPrim _ =
    error "last error"



restPrim :: Primitive
restPrim [Vector (_:xs)] =
    Vector xs

restPrim _ =
    error "rest error"



nthPrim :: Primitive
nthPrim [Vector xs, Integer i] =
    xs !! i

nthPrim _ =
    error "nth error"

conjPrim :: Primitive
conjPrim (Vector xs : ys) =
    Vector (xs ++ ys)

conjPrim _ =
    error "conj error"

consPrim :: Primitive
consPrim [x, Vector xs] =
    Vector (x : xs)

consPrim _ =
    error "cons error"

appendPrim :: Primitive
appendPrim (Vector xs : ys) =
    Vector (xs ++ ys)

appendPrim _ =
    error "append error"

concatPrim :: Primitive
concatPrim xs =
    Vector (concatMap get xs)

    where
        get (Vector v) = v
        get _ = []

countPrim :: Primitive
countPrim [Vector xs] =
    Integer (length xs)

countPrim _ =
    error "count error"


emptyPrim :: Primitive
emptyPrim [Vector xs] =
    Boolean (null xs)

emptyPrim _ =
    error "empty error"

peekPrim :: Primitive
peekPrim [Vector xs]
    | null xs   = error "peek empty error"
    | otherwise = last xs

peekPrim _ =
    error "peek errpr"

rangePrim :: Primitive
rangePrim xs =
    Vector (map Integer (rangePrim (map toInt xs)))
  where
    toInt (Integer n) = n
    toInt (Number n)  = round n
    toInt _ = error "range error"

    rangePrim [a] = [0 .. a - 1]
    rangePrim [a,b] = [a .. b - 1]
    rangePrim [a,b,s] = [a, a+s .. b-1]
    rangePrim _ = error "range error"


vectorQ :: Value -> Bool
vectorQ (Vector _) = True
vectorQ _ = False


vectorQPrim :: Primitive
vectorQPrim [x] = Boolean (vectorQ x)

mapQ :: Value -> Bool
mapQ (HashMap _) = True
mapQ _           = False


mapQPrim :: Primitive 
mapQPrim [x] = Boolean (mapQ x)

numberQ :: Value -> Bool
numberQ (Number _)  = True
numberQ (Integer _) = True
numberQ _           = False

numberQPrim :: Primitive 
numberQPrim [x] = Boolean (numberQ x)

toMatPrim :: Value -> [[Double]]
toMatPrim (Matrix m) = m

toMatPrim (Vector rows) =
    map rowToList rows
  where
    rowToList (Vector xs) = map Distribution.valueToDouble xs
    rowToList _ = error "matrix rows error"

toMatPrim _ =
    error "matrix error"

matMulPrim :: Primitive
matMulPrim [a, b] =
    Matrix (matMul (toMatPrim a) (toMatPrim b))

matMulPrim _ =
    error "matMul error"

matAddPrim :: Primitive
matAddPrim [a, b] =
    Matrix (matAdd (toMatPrim a) (toMatPrim b))

matAddPrim _ =
    error "matAdd error"

matTransposePrim :: Primitive
matTransposePrim [a] =
    Matrix (transpose (toMatPrim a))

matTransposePrim _ =
    error "matTranspose error"

matTanhPrim :: Primitive
matTanhPrim [a] =
    Matrix (map (map tanh) (toMatPrim a))

matTanhPrim _ =
    error "maTanh error"

matReluPrim :: Primitive
matReluPrim [a] =
    Matrix (map (map relu) (toMatPrim a))

matReluPrim _ =
    error "matRelu error"

matRepmatPrim :: Primitive
matRepmatPrim [a, Integer r, Integer c] =
    Matrix (repmat (toMatPrim a) r c)

matRepmatPrim _ =
    error "matRepmat error"

primitives :: Map.Map String Primitive
primitives = Map.fromList
    [
        ("+", addPrim),
        ("-", subPrim),
        ("*", mulPrim),
        ("/", dividePrim),

        ("sqrt", sqrtPrim),
        ("exp", expPrim),
        ("log", logPrim),
        ("pow", powPrim),
        ("abs", absPrim),
        ("floor", floorPrim),
        ("ceil", ceilPrim),
        ("tanh", tanhPrim),

        ("max", maxPrim),
        ("min", minPrim),
        ("mod", modPrim),

        ("=", eqPrim),
        ("==", eqPrim),
        ("!=", neqPrim),
        ("<", lessPrim),
        (">", greaterPrim),
        ("<=", lessEqPrim),
        (">=", greaterEqPrim),

        ("and", andPrim),
        ("or", orPrim),
        ("not", notPrim),

        ("vector", vectorPrim),
        ("list", vectorPrim),

        ("hash-map", hashPrim),
        ("get", getPrim),
        ("put", putPrim),
        ("assoc", putPrim),

        ("first", firstPrim),
        ("second", secondPrim),
        ("last", lastPrim),
        ("rest", restPrim),
        ("nth", nthPrim),

        ("conj", conjPrim),
        ("cons", consPrim),
        ("append", appendPrim),
        ("concat", concatPrim),

        ("count", countPrim),
        ("empty?", emptyPrim),
        ("peek", peekPrim),
        ("range", rangePrim),

        ("vector?", vectorQPrim),
        ("map?", mapQPrim),
        ("number?", numberQPrim),

        ("mat-mul", matMulPrim),
        ("mat-add", matAddPrim),
        ("mat-transpose", matTransposePrim),
        ("mat-tanh", matTanhPrim),
        ("mat-relu", matReluPrim),
        ("mat-repmat", matRepmatPrim)
    ]
isPrimitive :: String -> Bool
isPrimitive name =
    Map.member name primitives