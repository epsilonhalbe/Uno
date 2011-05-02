-- module Uno (get_Players_Deck, drawCards, putCard_Phase, getRandomCardfrom, updatePlayer, displayC, displayCs, nplicate, cancel, pick) where

import Common
import System.Random (getStdRandom, randomR)
import System.Console.ANSI (setSGR,SGR(SetColor),ColorIntensity(Vivid),ConsoleLayer(Foreground),Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List (elemIndex)
import Control.Applicative ((<$>))

import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
-- additional instances for the Color datatype
main :: IO ()
main = do
    let zeroes = [Card c Zero | c <- [Red .. Blue]]
        ncards = [Card c v | c <- [Red .. Blue],v <- [One .. ChDir]]
        blacks = [Card Black v | v <- [Plus4,ChCol]]
        full_deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
    putStrLn "how many players do play this game?"
    num_of_players <- safeGetInt
    (rest,players) <- get_Players_Deck num_of_players full_deck
    n <- getStdRandom (randomR (0,(length players) - 1))
    (deck, d_stack) <- drawCards 1 rest
    let players' = (drop n players) ++ (take n players)
    putStrLn ((show d_stack) ++ (show players'))
    game_loop (State players' deck d_stack)

game_loop :: State -> IO ()
game_loop state = do
    let topcard = head d_stack
    putStr "The top card on the Stack: "
    displayCs d_stack
    -- displayC topcard
    putStrLn "\n\n\n\n"
    case v of Dummy -> do
                        putStrLn $ "Player: " ++ (name player)
                        displayCs (hand player)
                        -- putStrLn $ show (hand player)
                        (State players' deck' td_stack) <- putCard_Phase1 state
                        let state' =  State players' deck' (remove_fake td_stack)
                        win (last players') (game_loop state')
              _ -> do
                        putStrLn $ "Player: " ++ (name player)
                        putStrLn "0: Take a Card"
                        displayCs (hand player)
                        -- putStrLn $ show (hand player)
                        state'<- putCard_Phase1 state
                        win (last $ players state') (game_loop state')
    where
        (State (player:others) deck d_stack) = state
        v = value (head d_stack)

get_Players_Deck :: Int -> [Card] -> IO (Deck, [Player])
get_Players_Deck n deck = get_Players_Deck' n deck [] where
    get_Players_Deck' n deck players
        | n <= 0 = return (deck, players)
        | otherwise = do
            putStrLn ("please enter player " ++ show n ++ "'s name")
            name <- getLine -- todo make safe version
            (deck', hand) <- drawCards 5 deck
            let player = HPlayer name hand
            get_Players_Deck' (n - 1) deck' ([player] ++ players)

drawCards :: Int -> [Card] -> IO (Deck, Hand)
-- todo make safe drawing e.g. draw 4 and just 3 cards in deck
drawCards n cards = drawCards' n (cards, []) where
    drawCards' n (cards, drawn)
            |n <= 0 = return (cards,drawn)
            |otherwise = do
                (cs, c) <- getRandomCardfrom cards
                drawCards' (n - 1) (cs,[c] ++ drawn)

new_drawCards :: Int -> State -> IO (State, Hand)
new_drawCards n state = new_drawCards' n (state, []) where
    new_drawCards' n (state, drawn)
        |n <= 0 = return (state, drawn)
        |deck == [] = new_drawCards' n (State players d_stack [topcard], drawn)
        |otherwise = new_drawCards' (n - 1) (State players cs d_stack,c:drawn)
        where (State players deck (topcard:d_stack)) = state
              (c:cs) = deck

getRandomCardfrom :: [Card] -> IO ([Card],Card)
getRandomCardfrom cards = do
    n <- getStdRandom (randomR (0,(length cards) - 1))
    return $ pick n cards

putCard_Phase1 :: State -> IO State
putCard_Phase1 state = do
    putStrLn "which card do you put down"
    n <- safeGetInt
    putCard_Phase2 n state

putCard_Phase2 :: Int -> State -> IO State
putCard_Phase2 n state
    | n == 0 = do
        ((State _ deck' d_stack'),hand') <- new_drawCards 1 state
        let player' = updatePlayer player (hand'++p_hand)
        -- putCard_Phase1 (State (others++[player']) deck' d_stack')
        -- in the mean time take cards until you can play
        return $ (State (others++[player']) deck' d_stack')
        -- todo if card = Dummy do not remove the dummycard
    | 1 <= n && n <= (length p_hand) = putCard_Phase3 n state
    | otherwise = do
        putStrLn "number out of bounds - try again"
        putCard_Phase1 state
    where
        State (player:others) deck d_stack = state
        p_hand = hand player

putCard_Phase3 :: Int -> State -> IO State
putCard_Phase3 n state
    | p_card `equiv` s_card = do
        putCard_Phase4 p_card (State (player':others) deck d_stack)
    | otherwise = do
        putStrLn "put down a valid card or take one with 0"
        putCard_Phase1 state
    where
        State (player:others) deck d_stack = state
        s_card = head d_stack
        p_hand = hand player
        p_card = p_hand !! (n - 1)
        new_hand = cancel n p_hand
        player' = updatePlayer player new_hand

putCard_Phase4 :: Card -> State -> IO State
putCard_Phase4 card state
    | v == ChDir = do
                            let players' = (reverse others) ++ [player]
                            return $ (State players' deck ([card]++d_stack))
    | v == Plus2 = do
                            (State _ deck' d_stack', pluscards) <- new_drawCards 2 state
                            let p2' = updatePlayer p2 ((hand p2) ++ pluscards)
                            return $ (State ((p2':others') ++ [player]) deck' (card:d_stack'))
    | v == Plus4 = do
                            (State _ deck' d_stack', pluscards) <- new_drawCards 4 state
                            let p2' = updatePlayer p2 ((hand p2) ++ pluscards)
                            dummy <- getDummy card
                            return $ (State ((p2':others') ++ [player]) deck' (dummy:card:d_stack'))
    | v == Stop = do
                            let players' = (others') ++ [player,p2]
                            return $ (State players' deck ([card]++d_stack))
    | v == ChCol = do
                            dummy <- getDummy card
                            return $ (State (others ++ [player]) deck (dummy:card:d_stack))
    | otherwise = do
                            return $ (State (others ++ [player]) deck (card:d_stack))
    where
        v = value card
        State (player:others) deck d_stack = state
        (p2:others') = others

equiv :: Card -> Card -> Bool
equiv c1 c2
    | color c1 == color c2 = True
    | value c1 == value c2 = True
    | color c1 == Black = True
    | otherwise = False

{- todo make special procedures for black cards -}
getDummy :: Card -> IO Card
getDummy c = do
    putStrLn "please do give me a Color"
    putStrLn "1:Red    2:Green    3:Yellow    4:Blue"
    x <- safeGetInt
    case x of 1 -> do return $ Card Red Dummy
              2 -> do return $ Card Green Dummy
              3 -> do return $ Card Yellow Dummy
              4 -> do return $ Card Blue Dummy
              _ -> do getDummy c


remove_fake :: [a] -> [a]
remove_fake [] = error "too few cards"
remove_fake (_:[]) = error "too few cards"
remove_fake (c:_:cs) = (c:cs)

win :: Player -> IO () -> IO ()
win player f
    |(hand player == []) = putStrLn $ "congratulations "++show (name player)
    |otherwise = f

updatePlayer :: Player -> Hand -> Player
updatePlayer (HPlayer name _) hand = HPlayer name hand
updatePlayer (AiPlayer name _) hand = AiPlayer name hand

displayCs :: [Card] -> IO ()
displayCs cs = displayCs' 1 cs
    where displayCs' n [] = putStr "\n"
          displayCs' n (c:cs) = do
              displayC' n c
              displayCs' (n + 1) cs

displayC' :: Int -> Card -> IO ()
displayC' n (Card color value) = do
    putStr $ show n ++ ":"
    setSGR [SetColor Foreground Vivid color]
    putStr $ "|"++show value ++"|    "++break_at_8 n
    setSGR [SetColor Foreground Vivid White]
    where break_at_8 n
            |(mod n 8) == 0= "\n"
            |otherwise = []

-- displayC :: Card -> IO ()
-- displayC (Card color value) = do
--     setSGR [SetColor Foreground Vivid color]
--     putStr $ "|"++show value ++"|"
--     setSGR [SetColor Foreground Vivid White]

{- Auxiliary Functions -}
-- safeGetInt :: Int
-- safeGetInt = do
--     line <- getLine
--     reads <$> getLine :: [(Int,String)]

safeGetInt :: IO Int
safeGetInt = do
    n <- getInt
    if n == []
      then do
        putStrLn "enter a valid Int"
        safeGetInt
      else return (head $ map fst n)

getInt :: IO [(Int,String)]
getInt = do
    c <- getLine
    return (reads c)

nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs

cancel :: Int -> [a] -> [a]
cancel n xs = ((take (n - 1) xs) ++ (drop n xs))

pick :: Int -> [a] -> ([a],a)
pick n xs = (cancel (n + 1) xs, (xs !! n))
