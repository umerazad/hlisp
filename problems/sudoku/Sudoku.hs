module Main where

type Row a    = [a]
type Matrix a = [Row a]

type Digit = Char
type Grid = Matrix Digit

digits :: String
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '.')

boxSize :: Int
boxSize = 3

gridLength :: Int
gridLength = boxSize * boxSize

-- | Takes in an input grid and returns a list of all possible solutions.
solve :: Grid -> [Grid]
solve = filter valid . completions

valid :: Grid -> Bool
valid g = all nodups (rows g) && all nodups (cols g) && all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = notElem x xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [] = []
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup . map cols . group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take boxSize xs : group (drop boxSize xs)

ungroup :: [[a]] -> [a]
ungroup = concat

completions :: Grid -> [Grid]
completions = expand . choices

choices :: Grid -> Matrix [Digit]
choices = map (map choice)

choice :: Digit -> [Digit]
choice d = if blank d then digits else [d]

-- TODO: The performance will likely suck here. Figure out a better way.
expand :: Matrix [Digit] -> [Grid]
expand = cartesianProduct . map cartesianProduct

cartesianProduct :: [[a]] -> [[a]]
cartesianProduct [] = [[]]
cartesianProduct (xs:xss) = [x:ys | x <- xs, ys <- yss]
                            where yss = cartesianProduct xss


superEasy :: Grid
superEasy = [".95743861",
             "431865927",
             "876192543",
             "387459216",
             "612387495",
             "549216738",
             "763524189",
             "928671354",
             "15493867."]

easy :: Grid
easy =  ["2....1.38",
         "........5",
         ".7...6...",
         ".......13",
         ".981..257",
         "31....8..",
         "9..8...2.",
         ".5..69784",
         "4..25...."]

gentle :: Grid
gentle =  [".1.42...5",
           "..2.71.39",
           ".......4.",
           "2.71....6",
           "....4....",
           "6....74.3",
           ".7.......",
           "12.73.5..",
           "3...82.7."]

diabolical :: Grid
diabolical =  [".9.7..86.",
               ".31..5.2.",
               "8.6......",
               "..7.5...6",
               "...3.7...",
               "5...1.7..",
               "......1.9",
               ".2.6..35.",
               ".54..8.7."]

unsolvable :: Grid
unsolvable =  ["1..9.7..3",
               ".8.....7.",
               "..9...6..",
               "..72.94..",
               "41.....95",
               "..85.43..",
               "..3...7..",
               ".5.....4.",
               "2..8.6..9"]

minimal :: Grid
minimal =  [".98......",
            "....7....",
            "....15...",
            "1........",
            "...2....9",
            "...9.6.82",
            ".......3.",
            "5.1......",
            "...4...2."]

blankGrid :: Grid
blankGrid =  replicate n (replicate n '.')
                where n = gridLength

printGrid :: Grid -> IO ()
printGrid = mapM_ print

main :: IO ()
main = printGrid $ head $ solve superEasy