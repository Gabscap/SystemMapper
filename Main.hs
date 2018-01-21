module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Word
import Numeric


type Address = Word64

main :: IO ()
main = do
    Options{..} <- parseCLI
    let targetAddress = readAddress oAddress

    systemMap <- lines <$> readFile oSystemMap
    let result = last . takeWhile ((<= targetAddress) . systemMapAddress) $ systemMap
    let resultAddress = systemMapAddress result
    let diff = targetAddress - resultAddress

    putStrLn result
    putStrLn $ "Diff: " ++ show diff ++ " (0x" ++ showHex diff "" ++ ")"

systemMapAddress :: String -> Address
systemMapAddress = readAddress . dropHexPrefix . head . words

dropHexPrefix :: String -> String
dropHexPrefix ('0':'x':xs) = xs
dropHexPrefix xs           = xs

readAddress :: String -> Address
readAddress = (\[(x,"")] -> x) . readHex


data Options =
    Options { oSystemMap :: FilePath
            , oAddress   :: String }
    deriving (Show)


parseCLI :: IO Options
parseCLI = execParser (withInfo parseOptions "SystemMapper")
    where withInfo opts h = info (helper <*> opts) $ header h

parseOptions :: Parser Options
parseOptions = Options
    <$> strOption
        ( long "file"
       <> short 'f'
       <> metavar "PATH"
       <> value "/proc/kallsyms"
       <> help "path to System-map (Default: /proc/kallsyms)" )
    <*> argument str
        ( metavar "ADDRESS"
       <> help "Hex Address to resolve" )
