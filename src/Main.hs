import Control.Monad
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let count = case args of
                   (n:_) -> read n :: Int
                   _     -> 10
    forM_ [1..count] (\n -> putStrLn ("Hi there #" ++ show n))
