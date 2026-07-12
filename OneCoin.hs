module OneCoin (run) where

import Controller
import DataVD


oneCoin :: Expr
oneCoin =List [Symbol "sample",List [Symbol "bernoulli",NumberLit 0.3]]

run :: IO ()
run = do
    putStrLn "---------One coin-------------"
    runs <- enumerateTraces oneCoin 1000000
    let table = posteriorTable runs
    putStrLn "Complete executions:"
    print runs
    putStrLn "Posterior:"
    print table
    putStrLn ("Number of traces: " ++ show (length runs))