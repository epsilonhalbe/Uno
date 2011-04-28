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

---------- idea make datatype round { players :: [Player], dir :: Direction, d_stack :: [Card], deck :: [Card] }

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

data Player = HPlayer {name::String, hand::[Card]} | AiPlayer {name::String, hand::[Card]}
    deriving (Read, Show, Eq, Ord)

data Direction = Forward | Backward
    deriving (Eq)


main:: IO ()
main = do
    let zeroes = [Card c Zero | c <- [Blue .. Red]]
    let ncards = [Card c v | c <- [Blue .. Red],v <- [One .. ChDir]]
    let blacks = [Card Black v | v <- [Plus4,ChCol]]
    let full_deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
    putStrLn "how many players do play this game?"
    c <- getChar
    num_of_players <- return (read [c] :: Int) -- todo watch out for hflush errors ++make safe++
    (rest,players) <- get_Players_Deck 2 full_deck
    n <- getStdRandom (randomR (0,(length players) - 1))
    let starting_player = players !! n
    (deck, discard_stack) <- drawCards 1 rest
    game_loop Forward players discard_stack deck
    putStrLn ((name starting_player) ++ (show discard_stack) ++ (show players))

game_loop :: Direction -> [Player] -> [Card] -> [Card] -> IO ()
game_loop dir (player:others) d_stack deck = do
    let topcard = last d_stack
    putStrLn $ show (topcard)
{- spaghetti code at its best -jibberjabber-}
    case (value topcard) of Stop  -> do
                                        putStrLn "Stop - Hammertime"
                                        game_loop dir (others++[player]) d_stack deck
                            Plus2 -> do
                                        putStrLn "Nimm 2 denn naschen ist gesund"
                                        (deck', pluscards) <- drawCards 2 deck
                                        let tempplayer = updatePlayer player ((hand player) ++ pluscards)
                                        putStrLn $ show (hand player)
                                        (d_stack', hand') <- putCard tempplayer d_stack
                                        let player' = updatePlayer player hand'
                                        let dir' = getDir (last d_stack') dir -- todo put this into nextPlayers
                                        game_loop dir' (others ++ [player']) d_stack' deck'
                            Plus4 -> do
                                        putStrLn "ohooo nimm' doch 4 karten"
                                        (deck', pluscards) <- drawCards 4 deck
                                        let tempplayer = updatePlayer player ((hand player) ++ pluscards)
                                        putStrLn $ show (hand player)
                                        (d_stack', hand') <- putCard tempplayer d_stack
                                        let player' = updatePlayer player hand'
                                        let dir' = getDir (last d_stack') dir -- todo put this into nextPlayers
                                        game_loop dir' (others ++ [player']) (tail d_stack') deck' -- memo special handling for black cards
                            ChCol -> do
                                        putStrLn $ show (hand player)
                                        (d_stack', hand') <- putCard player d_stack
                                        let player' = updatePlayer player hand'
                                        let dir' = getDir (last d_stack') dir -- todo put this into nextPlayers
                                        -- wincondition
                                        game_loop dir' (others ++ [player']) (tail d_stack') deck -- memo special handling for black cards
                            _ -> do
                                        putStrLn $ show (hand player)
                                        (d_stack',hand') <- putCard player d_stack
                                        let dir' = getDir (last d_stack') dir
                                        -- wincondition
                                        if hand' /= [] then do
                                            putStrLn "foo"
                                        else do
                                            putStrLn ("congratulations " ++ show (name player))

-- todo - make for players not only hplayers --
get_Players_Deck :: Int -> [Card] -> IO ([Card],[Player])
get_Players_Deck n deck = get_Players_Deck' n deck [] where
    get_Players_Deck' n deck players
        | n<=0 = return (deck, players)
        | otherwise = do
            putStrLn ("please enter player " ++ show n ++ "'s name")
            name <- getLine -- todo make safe version
            (deck', hand) <- drawCards 5 deck
            let player = HPlayer name hand
            get_Players_Deck' (n - 1) deck' ([player] ++ players)

drawCards :: Int -> [Card] -> IO ([Card],[Card]) -- todo make safe drawing e.g. draw 4 and just 3 cards in deck
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
putCard player d_stack = do
    hand <- return (hand player)
    putStrLn "which card do you put down"
    c <- getChar -- todo make safe version
    n <- return (read [c] :: Int)
    let card = last d_stack
    let pcard = (hand !! (n - 1))
    let hand' = cancel n hand
    if ((color pcard == color card) || (value pcard == value card) || color pcard == Black)
        then return ((d_stack ++ [pcard], hand')) -- make special handling of black cards
        else do
                putStrLn "take a valid card or take one with 0"
                putCard (updatePlayer player hand) d_stack
{- todo make special procedures for black cards -}

updatePlayer :: Player -> [Card] -> Player
updatePlayer (HPlayer name _) hand = HPlayer name hand
updatePlayer (AiPlayer name _) hand = AiPlayer name hand

getDir :: Card -> Direction-> Direction
getDir (Card _ value) dir
        |(value == ChDir && dir == Forward) = Backward
        |(value == ChDir && dir == Backward) = Forward
        |otherwise = dir

-- elegant für speed -- aber ?? zu lesen ??
-- nextPlayer :: Player -> [Player] -> Direction -> Player
-- nextPlayer player players dir
--    |dir == Forward = players  !! ((n + 1) `mod` l)
--    |otherwise = players !! ((n - 1) `mod` l)
--    where l = length players
--          n = index player

nextPlayers :: Direction -> [Player] -> [Player]
nextPlayers dir players
    |(dir == Forward) = players
    |otherwise = reverse players


{- Auxiliary Functions -}

nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs

cancel :: Int -> [a] -> [a]
cancel n xs = ((take (n - 1) xs) ++ (drop n xs))

pick :: Int -> [a] -> ([a],a)
pick n xs = (cancel (n + 1) xs, xs !! n)
