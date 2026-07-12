module Bits8 (run) where

import Controller
import DataVD


bitLink :: String -> [Expr]
bitLink name =[Symbol name,List[Symbol "if",List[Symbol "sample",List[Symbol "bernoulli",NumberLit 0.5 ]],IntegerLit 1,IntegerLit 0]]


link :: [Expr]
link = concatMap bitLink["b1","b2","b3","b4","b5","b6","b7","b8"]++ [Symbol "total",List(Symbol "+": map Symbol["b1","b2","b3","b4","b5","b6","b7","b8"])]


bits8 :: Expr
bits8 = List[Symbol "let",List link,List[Symbol "observe",List[Symbol "normal",NumberLit 7,NumberLit 1],Symbol "total"],Symbol "total"]


run :: IO ()
run = do
    putStrLn "===Bit 8 ====="
    runs8 <- enumerateTraces bits8 1000000
    putStrLn ("Number of complete executions: " ++ show (length runs8))
    putStrLn "\nPosterior:"
    print (posteriorTable runs8)