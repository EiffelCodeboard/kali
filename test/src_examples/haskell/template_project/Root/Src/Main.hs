-- Main function of the Haskell program.


module Root.Src.Main where

import Root.Src.Finder

main = do
        putStrLn "Hello Haskell World!"
        let x = maxFunction [5, 4, 33, 2, 1]
        print x
