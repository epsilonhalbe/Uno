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
import System.Console.ANSI (setSGR,SGR(SetColor),ColorIntensity(Vivid),ConsoleLayer(Foreground),Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List (elemIndex)

-- data Color = Black|Red|Green|Yellow|Blue|White
--     deriving (Read, Show, Eq, Ord, Enum)

-- additional instances for the Color datatype
instance Eq Color where
   x == y  =  fromEnum x == fromEnum y
instance Ord Color where
   compare x y  =  compare (fromEnum x) (fromEnum y)
instance Read Color where
   readsPrec _ str   = [(color (c), t) |
                               (c, t) <- reads str]
                     where color x = case x of "Black" -> Black
                                               "Red" -> Red
                                               "Green" -> Green
                                               "Yellow" -> Yellow
                                               "Blue" -> Blue
                                               "Magenta" -> Magenta
                                               "Cyan" -> Cyan
                                               "White" -> White


data Card = Card {color::Color ,value::Value}
    deriving (Read, Eq)
instance Show Card where
    show c = show (color c, value c)
instance Ord Card where
    compare c1 c2 = compare ( color c1, value c1 ) (color c2, value c2)
instance Enum Card where
    toEnum n = let (v,s) = n `divMod` 5 in Card (toEnum v) (toEnum s)
    fromEnum c = fromEnum (value c) * 5 + fromEnum (color c)

data Value = Zero|One|Two|Three|Four|Five|Six|Seven|Eight|Nine|Plus2|Stop|ChDir|Plus4|ChCol
    deriving (Read, Show, Eq, Ord, Enum)

data Player = HPlayer {name::String, hand::[Card]} | AiPlayer {name::String, hand::[Card]}
    deriving (Show)


main :: IO ()
main = do
    let zeroes = [Card c Zero | c <- [Red .. Blue]]
    let ncards = [Card c v | c <- [Red .. Blue],v <- [One .. ChDir]]
    let blacks = [Card Black v | v <- [Plus4,ChCol]]
    let full_deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
    putStrLn "how many players do play this game?"
    c <- getChar
    num_of_players <- return (read [c] :: Int) -- todo watch out for hflush errors ++make safe++
    (rest,players) <- get_Players_Deck 2 full_deck
    n <- getStdRandom (randomR (0,(length players) - 1))
    let starting_player = players !! n
    (deck, discard_stack) <- drawCards 1 rest
    game_loop players discard_stack deck
    putStrLn ((name starting_player) ++ (show discard_stack) ++ (show players))

game_loop :: [Player] -> [Card] -> [Card] -> IO ()
game_loop (player:others) d_stack deck = do
    let topcard = last d_stack
    putStrLn $ show (topcard)
{- spaghetti code at its best -jibberjabber-}
    case (value topcard) of Stop  -> do
                                        putStrLn "Stop - Hammertime"
                                        game_loop (others++[player]) d_stack deck
            {-      Take-}  Plus2 -> do
            {-          -}              putStrLn "Nimm 2 denn naschen ist gesund"
            {-          -}              (deck', pluscards) <- drawCards 2 deck
            {-          -}              let temp = updatePlayer player ((hand player) ++ pluscards)
            {-          -}              putStrLn $ show (hand player)
            {-          -}              (d_stack', hand', players') <- putCard_Phase (temp:others) d_stack
            {-          -}              let player' = updatePlayer player hand'
            {-          -}              game_loop (players' ++ [player']) d_stack' deck'
    {-Black {-   -}     -}  Plus4 -> do
    {-      {-   -}     -}              putStrLn "ohooo nimm' doch 4 karten"
    {-      {-   -}     -}              (deck', pluscards) <- drawCards 4 deck
    {-      {-   -}     -}              let temp = updatePlayer player ((hand player) ++ pluscards)
    {-      {-   -}     -}              putStrLn $ show (hand player)
    {-      {-   -}     -}              (d_stack', hand', players') <- putCard_Phase (temp:others) d_stack
    {-      {-   -} Take-}              game_loop players' (tail d_stack') deck'
                                        -- memo special handling for black cards
    {-           -}         ChCol -> do
    {-           -}                     putStrLn $ show (hand player)
    {-           -}                     (d_stack', hand', players') <- putCard_Phase (player:others) d_stack
    {-           -}                     -- wincondition
    {-Black      -}                     game_loop players' (tail d_stack') deck
                                        -- memo special handling for black cards
                            _ -> do
                                        putStrLn $ show (hand player)
                                        (d_stack', hand', players') <- putCard_Phase (player:others) d_stack
                                        -- wincondition
                                        if hand' /= []
                                          then do
                                            putStrLn "foo"
                                            game_loop players' d_stack' deck
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

putCard_Phase :: [Player] -> [Card] -> IO ([Card],[Card],[Player])
putCard_Phase (player:others) d_stack = do
    hand <- return (hand player)
    putStrLn "which card do you put down"
    c <- getChar -- todo make safe version
    n <- return (read [c] :: Int)
    let card = last d_stack
    let pcard = (hand !! (n - 1))
    let hand' = cancel n hand
    if ((color pcard == color card) || (value pcard == value card) || color pcard == Black)
        then do
                case (value pcard) of ChDir -> do
                                          let player' = updatePlayer player hand'
                                          let players' = (reverse others)++[player']
                                          return $ (d_stack ++ [pcard], hand', players')
                                  -- make special handling of black cards
                                      Plus4 -> do
                                          let player' = updatePlayer player hand'
                                          let players' = others++[player']
                                          return $ (d_stack ++ [pcard], hand', players')
                                          -- make special handling of black cards

        else do
                putStrLn "take a valid card or take one with 0"
                putCard_Phase (player:others) d_stack

{- todo make special procedures for black cards -}

updatePlayer :: Player -> [Card] -> Player
updatePlayer (HPlayer name _) hand = HPlayer name hand
updatePlayer (AiPlayer name _) hand = AiPlayer name hand

display :: Card -> IO ()
display (Card color value) = do
    -- setSGR [SetColor Foreground Vivid color]
    putStr $ show (Card color value)
    -- setSGR [SetColor Foreground Vivid White]

{- Auxiliary Functions -}

nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs

cancel :: Int -> [a] -> [a]
cancel n xs = ((take (n - 1) xs) ++ (drop n xs))

pick :: Int -> [a] -> ([a],a)
pick n xs = (cancel (n + 1) xs, xs !! n)
