module Image
    ( Pixel(..)
      ,Cluster(..)
      ,Clustering(..)
      ,Options (..)
      ,ArgumentType
      ,distance
      ,distanceFloat
      ,mean
      ,applyKmean
      ,linkKluster
      ,createClustering
      ,findPixel
      ,distMinimum
      ,findMinimum
      ,startOption
      ,programUsage
      ,programInvalidArgs
      ,imageCompressor
      ,getConvergeance
      ,eqCluster
      ,newCluster
      ,chunks
      ,parseFile
      ,removeEmptyLine
      ,containEmptyLine
      ,lineToPixel
      ,getPosition
      ,getColor
      ,replaceComma
      ,printCluster
      ,printOneCluster
      ,printClusterPixel
      ,remove
      ,separateBy
      ,checkColor
      ,checkIfDigit
      ,checkSecondParenthese
      ,checkFirstParenthese
      ,checkLine
      ,checkLines
      ,checkFile
      ,getItInt
      ,getItFloat
      ,getPath
      ,getArguments
      ,loopCheckSame
      ,checkRule
      ,isPair
      ,isNb
      ,check
      ,loopCheckArgs
      ,checkArgs
      ,failure
      ,len
    )where

import Data.String
import Data.List
import Data.Maybe
import Text.Printf
import System.Environment
import System.Exit
import Data.Char
import Data.List (unfoldr)

data Cluster = Cluster
    { pos :: [Float]
    } deriving (Eq, Show)

data Clustering = Clustering
    { cluster :: Cluster
    , pixels :: [Pixel]
    } deriving (Eq, Show)

data Pixel = Pixel
    { color    :: [Int]
    , position :: [Int]
    } deriving(Eq, Ord,Show)

data Options = Options
    { colorsNb     :: Int
    , convergLimit :: Float
    , pathImage    :: String
    } deriving Show

data ArgumentType =     Invalid
                    |   Helper
                    |   Other
    deriving (Show, Enum)

startOption :: Options
startOption = Options
    { colorsNb          = 0
    , convergLimit  = 0
    , pathImage         = ""
    }

remove :: String -> Char -> String
remove [] r = []
remove (x:xs) r | x == r = remove xs r
remove (x:xs) r = x:remove xs r

failure :: String -> IO()
failure str = putStrLn str >>
    exitWith (ExitFailure 84)

separateBy :: Eq a => a -> [a] -> [[a]]
separateBy chr = unfoldr sep where
    sep [] = Nothing
    sep l  = Just . fmap (drop 1) . break (== chr) $ l

isPair :: Int -> Bool
isPair n = mod n 2 == 0

isNb :: String -> Bool
isNb (xs:ys) | xs == '.' = isNb ys
isNb (xs:ys) | isNumber xs = isNb ys
isNb (xs:_) = True
isNb n = False

len :: (Num b) => [a] -> b
len [] = 0
len xs = sum [1 | _ <- xs]

checkColor :: Int -> IO()
checkColor i | i < 0 || i > 255 = failure ("The given file is wrong")
checkColor i = return()

checkIfDigit :: String -> Char -> IO()
checkIfDigit s c | all isDigit s == False = failure ("The given file is wrong")
checkIfDigit s c | c == 'c' = checkColor (read s :: Int)
checkIfDigit s c = return()

checkFirstParenthese :: String -> IO()
checkFirstParenthese p | (p!!0) /= '(' = failure ("The given file is wrong")
checkFirstParenthese p = return()

checkSecondParenthese :: String -> IO()
checkSecondParenthese [] = failure ("The given file is wrong")
checkSecondParenthese (x:[]) | x /= ')' = failure ("The given file is wrong")
checkSecondParenthese (x:[]) | x == ')' = return()
checkSecondParenthese (x:xs) = checkSecondParenthese xs

checkLine :: String -> IO()
checkLine s = do
    let splitSpace = separateBy ' ' s
    let splitPoint = separateBy ',' (splitSpace!!0)
    let splitColor = separateBy ',' (splitSpace!!1)
    checkFirstParenthese (splitPoint!!0)
    checkIfDigit (remove (splitPoint!!0) '(') 'p'
    checkSecondParenthese (splitPoint!!1)
    checkIfDigit (remove (splitPoint!!1) ')') 'p'
    checkFirstParenthese (splitColor!!0)
    checkIfDigit (remove (splitColor!!0) '(') 'c'
    checkIfDigit (splitColor!!1) 'c'
    checkSecondParenthese (splitColor!!2)
    checkIfDigit (remove (splitColor!!2) ')') 'c'

checkLines :: [String] -> IO()
checkLines [] = return()
checkLines (x:xs) = checkLine x >> checkLines xs

checkFile :: String -> IO()
checkFile f = do
    content <- readFile f
    let linesOfFiles = lines content
    checkLines linesOfFiles

checkRule :: String -> Int -> IO()
checkRule str n | str /= "-n" && str /= "-l" && str /= "-f" = failure
    ("The argument " ++ show (n+1) ++ " --> \"" ++ str ++ "\" isn't a valid instruction")
checkRule str n = return()

check :: Int -> Int -> [String] -> IO()
check end i args | isPair i = checkRule (args!!i) i
check end i args | (args!!(i-1)) /= "-f" && (isNb (args!!i)) =
    failure ("The argument " ++ show (i+1) ++ " --> \"" ++ (args!!i) ++ "\" must be a number")
check end i args = return()

loopCheckArgs :: Int -> Int -> [String] -> IO()
loopCheckArgs end i args | i == end = return()
loopCheckArgs end i args = check end i args >>
    loopCheckArgs end (i+1) args

checkArgs :: Int -> Int -> [String] -> IO()
checkArgs end i args =
    loopCheckArgs end i args >>
    loopCheckSame "-n" 0 end i args >>
    loopCheckSame "-l" 0 end i args >>
    loopCheckSame "-f" 0 end i args

getItInt :: String -> Int -> Int -> [String] -> Int
getItInt search end i args | (args!!i) == search = read (args!!(i+1)) :: Int
getItInt search end i args = getItInt search end (i+1) args

getItFloat :: String -> Int -> Int -> [String] -> Float
getItFloat search end i args | (args!!i) == search = read (args!!(i+1)) :: Float
getItFloat search end i args = getItFloat search end (i+1) args

getPath :: String -> Int -> Int -> [String] -> String
getPath search end i args | (args!!i) == search = args!!(i+1)
getPath search end i args = getPath search end (i+1) args

getArguments :: Int -> [String] -> IO (Either ArgumentType Options)
getArguments end args = do
    let n = getItInt "-n" end 0 args
    let l = getItFloat "-l" end 0 args
    let f = getPath "-f" end 0 args
    checkFile f
    return $ Right Options { colorsNb = n, convergLimit = l, pathImage = f }

getConvergeance :: [Clustering] -> [Clustering] -> Float -> Bool
getConvergeance old new e
        | length x == length old    = True
        | otherwise                 = False
            where
                x = [function i | i <- take (length old) [0,1..] , function i == True]
                function i = eqCluster (cluster (old !! i)) (cluster (new !! i)) e

loopCheckSame :: String -> Int -> Int -> Int -> [String] -> IO()
loopCheckSame search count end i args | i == (end+1) = return()
loopCheckSame search count end i args | count == 2 =
    failure ("The argument \"" ++ search ++ "\" is present more than one time")
loopCheckSame search count end i args | i < end && (args!!i) == search =
    loopCheckSame search (count+1) end (i+1) args
loopCheckSame search count end i arg = loopCheckSame search count end (i+1) arg

distance :: [Int] -> [Float] -> Float
distance x y = sqrt (x'*x' + y'*y' + z'*z')
    where
        x' = fromIntegral (x !! 0) - y !! 0
        y' = fromIntegral (x !! 1) - y !! 1
        z' = fromIntegral (x !! 2) - y !! 2

distanceFloat :: [Float] -> [Float] -> Float
distanceFloat x y = sqrt (x'*x' + y'*y' + z'*z')
    where
        x' = x !! 0 - y !! 0
        y' = x !! 1 - y !! 1
        z' = x !! 2 - y !! 2

mean :: [Pixel] -> Cluster
mean pixel = Cluster { pos = [ sum r / y',  sum g / y',  sum b / y'] }
        where
            r = [fromIntegral (color (pixel !! i) !! 0) | i <- take y [0,1..]]
            g = [fromIntegral (color (pixel !! i) !! 1) | i <- take y [0,1..]]
            b = [fromIntegral (color (pixel !! i) !! 2) | i <- take y [0,1..]]
            y = length pixel
            y' = fromIntegral (y)

applyKmean :: [Pixel] -> [Cluster] -> [Clustering]
applyKmean a b =  linkKluster a b

linkKluster :: [Pixel] -> [Cluster] -> [Clustering]
linkKluster img clusterList = [createClustering img clusterList i | i <- take (length clusterList) [0,1..]]

findPixel :: [Pixel] -> [Cluster] -> Int -> [Pixel]
findPixel img list counter = [img !! i | i <- take (length img) [0,1..], distMinimum (img !! i) list == counter]

distMinimum :: Pixel -> [Cluster] -> Int
distMinimum pix list = findMinimum [distance (color pix) (pos (list !! i)) | i <-  take (length list) [0,1..]]

findMinimum :: [Float] -> Int
findMinimum x = n
        where
            n = case elemIndex y x of
                Nothing -> -1
                Just n  -> n
            y = minimum x

programUsage :: IO ()
programUsage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n\n\tN\tnumber of colors in the final image\n\tL\tconvergence limit\n\tF\tpath to the file containing the colors of the pixels"

programInvalidArgs :: IO ()
programInvalidArgs = do putStrLn "the given arguments are invalid, please use the --help option"

imageCompressor :: ([Pixel], Int, Float) -> [Clustering] -> [Clustering]
imageCompressor (img, n, e) [] =  imageCompressor (img, n', e) (applyKmean img (newCluster img n'))
            where
                n' = if n > x then x else n
                x = length img
imageCompressor (img, n, e) clustering
        | getConvergeance clustering newClustering e == False = imageCompressor (img, n, e) newClustering
        | otherwise = newClustering
            where
                newClustering = applyKmean img newCluster
                newCluster = [cluster (clustering !! i) | i <- take n [0,1..]]

eqCluster :: Cluster -> Cluster -> Float -> Bool
eqCluster clusterA clusterB e
        | x > e     = False
        | otherwise = True
            where
                a = pos clusterA
                b = pos clusterB
                x = distanceFloat a b

newCluster :: [Pixel] -> Int -> [Cluster]
newCluster img n = [mean (y !! i) | i <- take n [0,1..]]
    where
        y = chunks (length img `div` n) img

createClustering :: [Pixel] -> [Cluster] -> Int -> Clustering
createClustering img list x = Clustering { cluster = if length choosenPixel > 0 then mean choosenPixel else list !! x, pixels = choosenPixel}
            where choosenPixel = findPixel img list x

printCluster :: [Clustering] -> IO ()
printCluster []     = return ()
printCluster (c:cs) = do printOneCluster c ; printCluster cs

printOneCluster :: Clustering -> IO ()
printOneCluster c = do
            printf "--\n(%.2f,%.2f,%.2f)\n-\n" (((pos (cluster c)) !! 0) :: Float) (((pos (cluster c)) !! 1) :: Float) (((pos (cluster c)) !! 2) :: Float)
            printClusterPixel (pixels c)

printClusterPixel :: [Pixel] -> IO ()
printClusterPixel []        = return ()
printClusterPixel (p:ps)    = do
            printf "(%d,%d) (%d,%d,%d)\n" ((position p) !! 0 :: Int) ((position p) !! 1 :: Int) ((color p) !! 0 :: Int) ((color p) !! 1 :: Int) ((color p) !! 2 :: Int)
            printClusterPixel ps

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

parseFile :: String -> [Pixel]
parseFile s = map lineToPixel (removeEmptyLine (lines s))

removeEmptyLine :: [String] -> [String]
removeEmptyLine arr
        | containEmptyLine arr  = delete "" arr
        | otherwise             = arr

containEmptyLine :: [String] -> Bool
containEmptyLine []     = False
containEmptyLine (x:xs)
        | null x        = True
        | otherwise     = containEmptyLine xs

lineToPixel :: String -> Pixel
lineToPixel l = Pixel { position = (getPosition (word1Position, word2Position)), color = (getColor (word1Color, word2Color, word3Color)) }
        where   positionOnly    = (((words l) !! 0) \\ "()")
                colorOnly       = (((words l) !! 1) \\ "()")
                word1Position   = (words (map replaceComma positionOnly)) !! 0
                word2Position   = (words (map replaceComma positionOnly)) !! 1
                word1Color      = (words (map replaceComma colorOnly)) !! 0
                word2Color      = (words (map replaceComma colorOnly)) !! 1
                word3Color      = (words (map replaceComma colorOnly)) !! 2

getPosition :: ([Char], [Char]) -> [Int]
getPosition a = [(read (fst a) :: Int), (read (snd a) :: Int)]

getColor :: ([Char], [Char], [Char]) -> [Int]
getColor (a,b,c) = [(read a :: Int), (read b :: Int), (read c :: Int)]

replaceComma :: Char -> Char
replaceComma ','        = ' '
replaceComma c          = c