--module Uno

{-1 game:
    2 players:
    draw cards
    make discard stack
    loop
        look at discard stack
        choose player
        player play
        has player won?
    back to loop
-}





import System.Random (getStdRandom, randomR)

data Card = Card {color::Color,value::Value}
    deriving (Read, Eq)
instance Show Card where
    show c = show (color c, value c)
instance Ord Card where
    compare c1 c2 = compare ( color c1, value c1 ) (color c2, value c2)
instance Enum Card where
    toEnum n = let (v,s) = n `divMod` 5 in Card (toEnum v) (toEnum s)
    fromEnum c = fromEnum (value c) * 5 + fromEnum (color c)

data Color = Black|Blue|Green|Yellow|Red
    deriving (Read, Show, Eq, Ord, Enum)
data Value = Zero|One|Two|Three|Four|Five|Six|Seven|Eight|Nine|Plus2|Stop|ChDir|Plus4|ChCol
    deriving (Read, Show, Eq, Ord, Enum)
type Deck = [Card]

main:: IO ()
main = do
    let zeroes = [Card c Zero| c<-[Blue .. Red]]
    let ncards = [Card c v|c<-[Blue .. Red],v<-[One .. ChDir]]
    let blacks =[Card Black v |v<-[Plus4,ChCol]]
    let deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
    num_of_players <- getChar --watch out for hflush errors ++make safe++

    (foo,bar) <- drawCards 5 (deck,[])
    putStrLn (show (bar))
    putStrLn (show $ length foo)


drawCards :: Int -> ([Card],[Card]) -> IO ([Card],[Card])
drawCards n (cards,drawn)
            |n==0 = return (cards,drawn)
            |otherwise = do
                (cs, c) <- getRandomCardfrom cards
                drawCards (n-1) (cs,[c]++drawn)

getRandomCardfrom :: [Card] -> IO ([Card],Card)
getRandomCardfrom cards = do
    n <- getStdRandom (randomR (0,(length cards)-1))
    return $ pick n cards

{- Auxiliary Functions -}

nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs

cancel :: Int -> [a] -> [a]
cancel n xs = ((take (n-1) xs)++(drop n xs))

pick :: Int -> [a] -> ([a],a)
pick n xs = (cancel (n+1) xs, xs!!n)
