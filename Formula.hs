module Formula (run) where

import Controller
import DataVD
import Distribution (logProb, valueToDouble)


type Row = (Int, Double, Double)



choose :: Int -> Int -> Integer
choose n k
    | k < 0 || k > n = 0
    | otherwise = div (product [fromIntegral (n-k+1) .. fromIntegral n]) (product [1 .. fromIntegral k])


bits8 :: Expr
bits8 =List[Symbol "let",List[
            Symbol "b1", bit,
            Symbol "b2", bit,
            Symbol "b3", bit,
            Symbol "b4", bit,
            Symbol "b5", bit,
            Symbol "b6", bit,
            Symbol "b7", bit,
            Symbol "b8", bit,
            Symbol "total",
            List (Symbol "+" :map Symbol["b1","b2","b3","b4","b5","b6","b7","b8"])],
        List[Symbol "observe",List[Symbol "normal",NumberLit 7,NumberLit 1],Symbol "total"],Symbol "total"]
    where
        bit = List[Symbol "if",List[Symbol "sample",List[Symbol "bernoulli",NumberLit 0.5]],IntegerLit 1,IntegerLit 0]


formulaTable :: [Row]
formulaTable =
    let
        values = [0..8]
        logMass k = log (fromIntegral (choose 8 k))- 8 * log 2 + logProb (Normal 7 1) (Integer k)
        masses = [ (k, logMass k)
            | k <- values]
        z =logSumExp [lw| (_,lw) <- masses]
    in
        [(k,lw,exp (lw-z))
        | (k,lw) <- masses ]


meanPosterior :: [(String,Double,Double)] -> Double
meanPosterior xs =
    sum [read value * prob
    | (value,_,prob) <- xs]


meanFormula :: [Row] -> Double
meanFormula xs =
    sum[fromIntegral k * p
    | (k,_,p) <- xs]


meanPosteriorString :: [(String,Double,Double)] -> Double
meanPosteriorString xs =
    sum [fromIntegral (read v :: Int) * p
    | (v,_,p) <- xs]


compareTables :: [(String,Double,Double)] -> [(Int,Double,Double)] -> IO ()
compareTables enum formula = do
    putStrLn "\nvalue | prob | probFormula | absDiff"
    let rows =
            [(k, pe, pf, abs(pe-pf))
            |(v,_,pe) <- enum,
                let k = round (read v :: Double),
                (kf,_,pf) <- formula,k == kf ]
    mapM_ print rows
    let maxDiff = maximum [d | (_,_,_,d) <- rows]
    putStrLn "\nposterior mean by enumeration:"
    print (meanPosterior enum)
    putStrLn "posterior mean by formula:"
    print (meanFormula formula)
    putStrLn "max abs probability error:"
    print maxDiff



run :: IO ()
run = do
    putStrLn "=========="
    runs8 <- enumerateTraces bits8 1000000
    let pmf8 = posteriorTable runs8
    putStrLn "\nNumber of complete executions:"
    print (length runs8)
    let formula = formulaTable
    putStrLn "\nComparison "
    compareTables pmf8 formula