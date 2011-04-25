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
import Data.List (elemIndex)

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

data Player = HumanPlayer {name::String, hand::[Card], index::Int} -- |AiPlayer {name::String, hand::[Card]}
    deriving (Read, Show, Eq, Ord)

data Direction = Forward | Backward
    deriving (Eq)


main:: IO ()
main = do
    let zeroes = [Card c Zero| c<-[Blue .. Red]]
    let ncards = [Card c v|c <- [Blue .. Red],v <- [One .. ChDir]]
    let blacks =[Card Black v |v<-[Plus4,ChCol]]
    let full_deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
    num_of_players <- return 2 --getChar --watch out for hflush errors ++make safe++
    (rest,players) <- get_HPlayers_Deck 2 full_deck
    n <- getStdRandom (randomR (0,(length players)-1))
    let starting_player = players!!n
    (deck, discard_stack) <- drawCards 1 rest
    game_loop Forward players starting_player discard_stack deck
    putStrLn ((name starting_player)++(show discard_stack)++(show players))

game_loop :: Direction -> [Player] -> Player-> [Card] -> [Card] -> IO ()
game_loop dir players player d_stack deck = do
    putStrLn $ show (last d_stack)
    putStrLn $ show (hand player)
    d_stack' <- putCard player d_stack
    let dir' = getDir (last d_stack') dir
    let nextone = nextPlayer player players dir'
    putStrLn "foo dreck"

get_HPlayers_Deck :: Int -> [Card] -> IO ([Card],[Player])
get_HPlayers_Deck n deck = get_HPlayers_Deck' n deck [] where
    get_HPlayers_Deck' n deck players
        | n<=0 = return (deck, players)
        | otherwise = do
            putStrLn ("please enter player "++show n++"'s name")
            name <- getLine --make safe version
            (deck', hand) <- drawCards 5 deck
            let player = [HumanPlayer name hand n]
            get_HPlayers_Deck' (n-1) deck' (player++players)

drawCards :: Int -> [Card] -> IO ([Card],[Card])
drawCards n cards = drawCards' n (cards, []) where
    drawCards' n (cards, drawn)
            |n<=0 = return (cards,drawn)
            |otherwise = do
                (cs, c) <- getRandomCardfrom cards
                drawCards' (n-1) (cs,[c]++drawn)

getRandomCardfrom :: [Card] -> IO ([Card],Card)
getRandomCardfrom cards = do
    n <- getStdRandom (randomR (0,(length cards)-1))
    return $ pick n cards

putCard :: Player -> [Card] -> IO [Card]
putCard (HumanPlayer name hand i) d_stack = do
    putStrLn "which card do you put"
    c <- getChar
    n <- return (read [c] :: Int)
    let card = last d_stack
    let pcard =  (hand !! (n-1))
    if ((color pcard == color card) || (value pcard == value card)|| color pcard == Black)
        then return (d_stack++[pcard])
        else do
                putStrLn "take a valid card or take one with 0"
                putCard (HumanPlayer name hand i) d_stack

getDir :: Card -> Direction-> Direction
getDir (Card _ value) dir
        |(value == ChDir && dir == Forward) = Backward
        |(value == ChDir && dir == Backward) = Forward
        |otherwise = dir

nextPlayer :: Player -> [Player] -> Direction -> Player
nextPlayer player players dir
    |dir == Forward = players  !! ((n + 1)`mod`l)
    |otherwise = players !! ((n - 1)`mod`l)
    where l = length players
          n = index player




{- Auxiliary Functions -}

nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs

cancel :: Int -> [a] -> [a]
cancel n xs = ((take (n-1) xs)++(drop n xs))

pick :: Int -> [a] -> ([a],a)
pick n xs = (cancel (n+1) xs, xs!!n)
