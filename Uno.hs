-- module Uno (Card, Color, Player, Direction, putCard, get_Players_Deck, drawCards, getRandomCardfrom, nextPlayer, cancel, pick, nplicate)

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
import System.Console.ANSI
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

-- data Color = Black|Blue|Green|Yellow|Red
--    deriving (Read, Show, Eq, Ord, Enum)
data Value = Zero|One|Two|Three|Four|Five|Six|Seven|Eight|Nine|Plus2|Stop|ChDir|Plus4|ChCol
    deriving (Read, Show, Eq, Ord, Enum)

data Player = Player {name::String, hand::[Card]} -- |AiPlayer {name::String, hand::[Card], index::Int}
    deriving (Read, Show, Eq, Ord)

data Direction = Forward | Backward
    deriving (Eq)


main :: IO ()
main = do
    let zeroes = [Card c Zero | c <- [Blue .. Red]]
    let ncards = [Card c v | c <- [Blue .. Red],v <- [One .. ChDir]]
    let blacks = [Card Black v | v <- [Plus4,ChCol]]
    let full_deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
    putStrLn "how many players do play this game?"
    c <- getChar
    num_of_players <- return (read [c] :: Int) -- watch out for hflush errors ++make safe++
    (rest,players) <- get_Players_Deck 2 full_deck
    n <- getStdRandom (randomR (0,(length players) - 1))
    let starting_player = players !! n
    (deck, discard_stack) <- drawCards 1 rest
    game_loop Forward players discard_stack deck
    putStrLn ((name starting_player) ++ (show discard_stack) ++ (show players))

game_loop :: [Player] -> [Card] -> [Card] -> IO ()
game_loop (player:others) d_stack deck = do
    let topcard = last d_stack
    putStrLn $ show (topcard)
{-jibberjabber-}
    case (value topcard) of Stop  -> do
                                        putStrLn "Stop - Hammertime"
                                        game_loop (others++[player]) d_stack deck
                            Plus2 -> do
                                        putStrLn "Nimm 2 denn naschen ist gesund"
                                        let (deck', pluscards) = drawCards 2 deck
                                        let tempplayer = Player "temp" ((hand player) ++ pluscards)
                                        (d_stack', hand') <- putCard tempplayer d_stack
                                        let player' = Player (name player) hand'
                                        game_loop (players' ++ [player']) d_stack' deck'
                            Plus4 -> do
                                        putStrLn "ohooo nimm' doch 4 karten"
                                        let (deck',pluscards) = drawCards 4 deck
                                        let player' = Player (name player) (hand player)++pluscards
                                        putStrLn "foo"
                            _     -> do
                                        let dir' = getDir (last d_stack') dir
                                        putStrLn $ show (hand player)
                                        (d_stack',hand') <- putCard player d_stack
                                        -- wincondition
                                        if hand' /= []
                                          then do
                                            putStrLn "foo"
                                            game_loop dir players' d_stack' deck'
                                          else do
                                            putStrLn ("congratulations " ++ show (name player))

-- todo - make for players not only hplayers --
get_Players_Deck :: Int -> [Card] -> IO ([Card],[Player])
get_Players_Deck n deck = get_Players_Deck' n deck [] where
    get_Players_Deck' n deck players
        | n<=0 = return (deck, players)
        | otherwise = do
            putStrLn ("please enter player " ++ show n ++ "'s name")
            name <- getLine -- make safe version
            (deck', hand) <- drawCards 5 deck
            let player = [Player name hand n]
            get_Players_Deck' (n - 1) deck' (player ++ players)

drawCards :: Int -> [Card] -> IO ([Card],[Card])
drawCards n cards = drawCards' n (cards, []) where
    drawCards' n (cards, drawn)
            |n <= 0 = return (cards,drawn)
            |otherwise = do
                (cs, c) <- getRandomCardfrom cards
                drawCards' (n - 1) (cs,[c] ++ drawn)

getRandomCardfrom :: [Card] -> IO ([Card],Card)
getRandomCardfrom cards = do
    n <- getStdRandom (randomR (0,(length cards) - 1))
    return $ pick n cards

putCard :: Player -> [Card] -> IO ([Card],[Card])
putCard (Player name hand) d_stack = do
    putStrLn "which card do you put down"
    c <- getChar -- make safe version
    n <- return (read [c] :: Int)
    let card = last d_stack
    let pcard = (hand !! (n - 1))
    let hand' = cancel n hand
    if ((color pcard == color card) || (value pcard == value card) || color pcard == Black)
        then return ((d_stack ++ [pcard], hand'))
        else do
                putStrLn "take a valid card or take one with 0"
                putCard (Player name hand) d_stack

getDir :: Card -> Direction-> Direction
getDir (Card _ value) dir
        |(value == ChDir && dir == Forward) = Backward
        |(value == ChDir && dir == Backward) = Forward
        |otherwise = dir

-- elegant für speed -- aber ?? zu lesen ??
{-
nextPlayer :: Player -> [Player] -> Direction -> Player
nextPlayer player players dir
    |dir == Forward = players  !! ((n + 1) `mod` l)
    |otherwise = players !! ((n - 1) `mod` l)
    where l = length players
          n = index player
-}

nextPlayers :: Direction -> [Player] -> [Player]
nextPlayers dir players
    |(dir == Forward) = players
    |otherwise = reverse players


display :: Card -> IO ()
display (Card color value) = do
    setSGR [SetColor Foreground Vivid color]
    putStr $ show (Card color value)
    setSGR [SetColor Foreground Vivid White]

{- Auxiliary Functions -}

nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs

cancel :: Int -> [a] -> [a]
cancel n xs = ((take (n - 1) xs) ++ (drop n xs))

pick :: Int -> [a] -> ([a],a)
pick n xs = (cancel (n + 1) xs, xs !! n)
