module Main where

import qualified Formula
import qualified SSMH
import qualified OneCoin
import qualified Bits8


menu :: IO ()
menu = do
    putStrLn "Choose Test:"
    putStrLn "1 - Formula"
    putStrLn "2 - SSMH / Likelihood Weighting / SMC"
    putStrLn "3 - One Coin Enumeration"
    putStrLn "4 - Bits8 Enumeration"

    option <- getLine

    case option of
        "1" -> Formula.run
        "2" -> SSMH.run
        "3" -> OneCoin.run
        "4" -> Bits8.run
        _   -> putStrLn "Invalid option"


main :: IO ()
main = menu
