module Main where
import System.Environment
import System.Exit
import Data.Char

main :: IO ()
main = getArgs >>= parse >>= putStr

bfilterString :: Int -> [String] -> [String]
bfilterString int [] = []
bfilterString int strings =
    if head strings /= "" then
        (show int ++ " " ++ head strings) : bfilterString (int + 1) (tail strings)
    else
        bfilterString (int + 1) (tail strings)

nfilterString :: Int -> [String] -> [String]
nfilterString int [] = []
nfilterString int strings = (show int ++ " " ++ head strings) : nfilterString (int + 1) (tail strings)

sfilterString :: [String] -> [String]
sfilterString [] = []
sfilterString strings =
    if head strings == "" && strings !! 1 == "" then
        --Eliminar esta linha
        sfilterString (tail strings)
    else
        head strings : sfilterString (tail strings)

vfilterString :: [String] -> [String]
vfilterString strings = filter (not . isPrint) (head strings) : vfilterString (tail strings)

readFileArg :: [String] -> ([String] -> [String]) -> IO String
readFileArg fs fn = fmap (unlines . fn . lines . concat) (mapM readFile (fs))

parse :: [String] -> IO String
parse ["--help"] = usage   >> exit
parse ["--version"] = version >> exit
parse []     = getContents
parse fs
    | head fs == "-b" =
        readFileArg (tail fs) (bfilterString 1)
    | head fs == "-n" =
        readFileArg (tail fs) (nfilterString 1)
    | head fs == "-s" =
        readFileArg (tail fs) sfilterString
    | head fs == "-v" =
        readFileArg (tail fs) vfilterString
    | otherwise =
        concat `fmap` mapM readFile fs

usage   = putStrLn "Usage: cat [-vbns] [files ..]"
version = putStrLn "Haskell cat 0.1"
exit    = exitSuccess
