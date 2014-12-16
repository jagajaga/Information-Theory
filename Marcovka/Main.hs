module Main where
import           Control.Monad
import           Data.Ratio
import           Numeric.LinearAlgebra.HMatrix


p :: Matrix Double
p = (3><3)  [ (3.0/4.0 - 1),             0.0, 1.0/4.0 
            , 1.0/4.0, (1.0/4.0 - 1.0),         1.0/4.0
            ,     1.0,             1.0,             1.0
            ]
p' = [[3/4, 1/4, 0], [0, 1/4, 3/4], [1/4, 1/4, 1/2]]

p1 :: Matrix Double
p1 = (3><3) [(1/4 - 1), 0, (1/3), 3/4, (1/2 - 1),1/3, 1.0, 1.0, 1.0] 

p1' = [[1/4, 3/4,  0], [ 0,  1/2, 1/2], [1/3, 1/3, 1/3]]

main :: IO ()
main = do
    let answ = (3><1) [0.0, 0.0, 1.0]
    print p1
    print answ
    let bb@[pa, pb, pc] = concat $ toLists $ p1 <\> answ
    let hx = ((-pa) * logBase 2 pa) - (pb * logBase 2 pb) - (pc * logBase 2 pc)
    print $ map (flip approxRational 1e-10) $ bb
    putStr "H (X) = "
    print hx
    let hxx = sum $ zipWith (*) bb $ concat $ map (map (liftM2 (*) negate (logBase 2)) . filter (0.0 /=)) p1'
    putStr "H (X|X) = "
    print hxx
    putStr "H_2 (X) = "
    let h_2x = (hx+hxx) / 2
    print h_2x
    putStrLn $ "H_n (X) = (" ++ show hx ++ " + (n-1)*" ++ show hxx ++ ")/n"

