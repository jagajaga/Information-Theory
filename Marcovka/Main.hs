module Main where
import           Control.Monad
import           Data.Ratio
import           Numeric.LinearAlgebra.HMatrix

main :: IO ()
main = do
    let p = (3><3) [3.0/4.0, 0,   (1.0/4.0 - 1.0), 1.0/4.0, (1.0/4.0 - 1.0), 1.0/4.0, 1.0,   1.0,   1.0] :: Matrix Double
    let p' = [[3/4, 1/4, 0], [0, 1/4, 3/4], [1/4, 1/4, 1/2]]
    let answ = (3><1) [0.0, 0.0, 1.0]
    print p
    print answ
    let bb@[pa, pb, pc] = concat $ toLists $ p <\> answ
    let hx = ((-pa) * log pa) - (pb * log pb) - (pc * log pc)
    print $ map (flip approxRational 1e-10) $ bb
    putStr "H (X) = "
    print hx
    let hxx = sum $ zipWith (*) bb $ concat $ map (map (liftM2 (*) negate log) . filter (0.0 /=)) p'
    putStr "H (X|X) = "
    print hxx
    putStr "H_2 (X) = "
    let h_2x = (hx+hxx) / 2
    print h_2x
    putStrLn $ "H_n (X) = (" ++ show hx ++ " + (n-1)*" ++ show hxx ++ ")/n"

