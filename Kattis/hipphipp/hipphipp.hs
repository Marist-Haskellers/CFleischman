import Control.Monad.Cont (replicateM_)

-- Kattis problem https://open.kattis.com/problems/hipphipp

main :: IO ()
main = print20Times "Hipp hipp hurra!"

print20Times :: String -> IO ()
print20Times text = replicateM_ 20 $ putStrLn text