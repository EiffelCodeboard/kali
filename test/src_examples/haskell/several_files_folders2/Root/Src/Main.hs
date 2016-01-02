module Root.Src.Main where
import Root.Src.P1(ff)
import Root.Src.P1.B(foo)
import Root.Src.P1.C(bar)

main = do
  putStrLn "Hello Haskell World!"
  foo
  bar

