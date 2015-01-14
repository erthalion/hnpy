{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import System.IO
import System.Environment
import Data.Word
import Data.String.Utils
import Data.List.Split
import Data.List
import Data.Serialize.IEEE754
import Data.Serialize.Get

data Header = Header {
    magic           :: BC.ByteString,
    minVersion      :: Word8,
    maxVersion      :: Word8,
    headeLen        :: Integer,
    header          :: BC.ByteString,
    npyData         :: [Double]
    } deriving (Show)

data DictHeader = DictHeader {
    descr           :: String,
    fortran_order   :: Bool,
    shape           :: [Int]
    } deriving (Show)

npyHeader :: Get Header

getNpyData = do
    empty <- isEmpty
    if empty
        then return []
        else do 
            v <- getFloat64le
            rest <- getNpyData
            return (v : rest)

npyHeader = do
    magic <- getByteString 6
    minVersion <- getWord8
    maxVersion <- getWord8
    headeLen <- getWord16le
    header <- getByteString $ fromIntegral headeLen
    npyData <- getNpyData
    return Header {
        magic=magic,
        minVersion=minVersion,
        maxVersion=maxVersion,
        headeLen=fromIntegral headeLen,
        header=header,
        npyData=npyData
    }

unwrap header = process (splitHeader header) DictHeader{descr="", fortran_order=False, shape=[]}
    where
        process (h:hs) parsedHeader
            | isInfixOf "'descr'" h = process hs parsedHeader{descr=getDescrValue . getRestPart $ hs}
            | isInfixOf "'fortran_order'" h =  process hs parsedHeader{fortran_order=getFortranOrderValue . getRestPart $ hs}
            | isInfixOf "'shape'" h = process hs parsedHeader{shape=getShapeValue . getRestPart $ hs}
        process _ parsedHeader = parsedHeader

        splitHeader header' = splitOn ":" (BC.unpack header')
        cleanValue value = strip . replace "'" "" $ value

        getRestPart hs = intercalate ":" hs

        getBool value
            | value == "False"   = False
            | value == "True"    = True
        getBool _               = False

        getInt value = (read $ strip value) :: Int
        getTuple value = map getInt $ splitTuple . takeWhileEndBracket . dropWhileStartBracket $ value
            where
                takeWhileEndBracket = takeWhile (\c -> c /= ')')
                dropWhileStartBracket value = drop 1 $ dropWhile (\c -> c /= '(') value
                splitTuple = reverse . tail . reverse . splitOn ","

        getDescrValue header' = cleanValue . head $ splitOn "," header'
        getFortranOrderValue header' = getBool . cleanValue . head $ splitOn "," header'
        getShapeValue header' = getTuple header'

main :: IO ()
main = do
    (filePath:args) <- getArgs
    file <- openBinaryFile filePath ReadMode
    input <- BL.hGetContents file
    let npyParsedData = runGetLazy npyHeader input
    case npyParsedData of
        Left msg -> print $ msg
        Right npyParsedData -> do
            let dataForParsing = BC.unpack . header $ npyParsedData
            let dictHeader = unwrap . header $ npyParsedData
            print $ header npyParsedData
            print $ dictHeader
            print $ npyData npyParsedData
