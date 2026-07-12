module SSMH (run) where

import qualified System.Random.MWC as MWC
import qualified Controller
import DataVD
import qualified Data.List as List


conj :: Expr
conj = List [Symbol "let",List [Symbol "mu",List[Symbol "sample",List [Symbol "normal",NumberLit 0,NumberLit 1]]]
    ,List [Symbol "observe",List [Symbol "normal", Symbol "mu", NumberLit 1],NumberLit 2.3],Symbol "mu"]


bits :: Expr
bits =List[Symbol "let",List(concatMap(\i ->[Symbol ("b" ++ show i),List[Symbol "if",List[Symbol "sample",List [Symbol "bernoulli", NumberLit 0.5]],IntegerLit 1,IntegerLit 0]])[1..8])
    ,List(Symbol "observe":[List [Symbol "normal", NumberLit 7, NumberLit 2],List(Symbol "+" :[Symbol ("b" ++ show i) | i <- [1..8]])])
    ,List(Symbol "+" :[Symbol ("b" ++ show i) | i <- [1..8]])]


run :: IO ()
run = do
    putStrLn "Select inference algorithm:"
    putStrLn "1 - SSMH"
    putStrLn "2 - Likelihood Weighting (LW)"
    putStrLn "3 - Sequential Monte Carlo (SMC)"

    option <- getLine

    case option of
        "1" -> runSSMH
        "2" -> runLW
        "3" -> runSMC'
        _   -> putStrLn "Invalid option"


runSSMH :: IO ()
runSSMH = do
    rng1 <- MWC.createSystemRandom
    ch <- Controller.singleSiteMH conj rng1 5000 500
    putStrLn
        ("conj SSMH mean = " ++ show (sum ch / fromIntegral (length ch)))
    rng2 <- MWC.createSystemRandom
    ch2 <- Controller.singleSiteMH bits rng2 5000 500
    putStrLn ("bits SSMH mean = "++ show (sum ch2 / fromIntegral (length ch2)))

runLW :: IO ()
runLW = do
    lwRngs <- mapM (\_ -> MWC.createSystemRandom) [1..5000]
    (vals, weights) <- Controller.likelihoodWeighting conj lwRngs
    let totalWeight = sum weights
        lwMean =if totalWeight == 0 then 0 else sum (zipWith (*) vals weights) / totalWeight
    putStrLn ("LW mean = " ++ show lwMean)


runSMC' :: IO ()
runSMC' = do
    rngs <- mapM (\_ -> MWC.createSystemRandom) [1..500]
    smc <- Controller.runSMC conj rngs
    putStrLn ("SMC mean = "++ show (sum smc / fromIntegral (length smc)))