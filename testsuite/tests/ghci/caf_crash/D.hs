module D where

import A

data MyFunc = MyFunc String (IO ())

funcCaf :: [MyFunc]
funcCaf = [MyFunc "square" square]

f1 :: MyFunc -> String
f1 (MyFunc s _) = s

f2 :: MyFunc -> IO ()
f2 (MyFunc s d) = d

main :: IO ()
main = do
    putStrLn $ show $ length funcCaf
    putStrLn $ show $ f1 $ head funcCaf
    yay <- f2 $ head funcCaf
    print yay
