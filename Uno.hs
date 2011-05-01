module Uno (get_Players_Deck, drawCards, putCard_Phase, getRandomCardfrom, updatePlayer, displayC, nplicate, cancel, pick) where

import Common
import System.Random (getStdRandom, randomR)
import System.Console.ANSI (setSGR,SGR(SetColor),ColorIntensity(Vivid),ConsoleLayer(Foreground),Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List (elemIndex)

import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
-- additional instances for the Color datatype
main :: IO ()
main = do
    let zeroes = [Card c Zero | c <- [Red .. Blue]]
        ncards = [Card c v | c <- [Red .. Blue],v <- [One .. ChDir]]
        blacks = [Card Black v | v <- [Plus4,ChCol]]
        full_deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
    putStrLn "how many players do play this game?"
    c <- getChar
    num_of_players <- return (read [c] :: Int) -- todo watch out for hflush errors ++make safe++
    (rest,players) <- get_Players_Deck 2 full_deck
    n <- getStdRandom (randomR (0,(length players) - 1))
    let starting_player = players !! n
    (deck, discard_stack) <- drawCards 1 rest
    -- game_loop (State players deck d_stack)
    -- game_loop players discard_stack deck
    putStrLn ((name starting_player) ++ (show discard_stack) ++ (show players))

game_loop :: State -> IO ()
game_loop (State (player:others) deck d_stack) = do
    let topcard = last d_stack
    putStrLn $ show (topcard)
{- spaghetti code at its best -jibberjabber-}
    case (value topcard) of
         Stop  -> do
             putStrLn "Stop - Hammertime"
             game_loop (State (others++[player]) deck d_stack)
         Plus2 -> do
             putStrLn "Nimm 2 denn naschen ist gesund"
             (tdeck, pluscards) <- drawCards 2 deck
             let tplayer = updatePlayer player ((hand player) ++ pluscards)
             putStrLn $ show (hand player)
             state' <- putCard_Phase (State (tplayer:others) tdeck d_stack)
             game_loop state'
         Plus4 -> do
             putStrLn "ohooo nimm' doch 4 karten"
             (tdeck, pluscards) <- drawCards 4 deck
             let tplayer = updatePlayer player ((hand player) ++ pluscards)
             putStrLn $ show (hand player)
             (State players' deck' td_stack) <- putCard_Phase (State (tplayer:others) tdeck d_stack)
             game_loop (State players' deck' (remove_fake td_stack))
             -- memo special handling for black cards
         ChCol -> do
             putStrLn $ show (hand player)
             (State players' deck' td_stack) <- putCard_Phase (State (player:others) deck d_stack)
             let state' =  State players' deck' (remove_fake td_stack)
             win (last players') (game_loop state')
             -- memo special handling for black cards
         _ -> do
             putStrLn $ show (hand player)
             state'<- putCard_Phase (State (player:others) deck d_stack)
             win (last $ players state') (game_loop state')

-- todo - make for players not only hplayers --
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

drawCards :: Int -> [Card] -> IO (Hand, Deck)
-- todo make safe drawing e.g. draw 4 and just 3 cards in deck
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

putCard_Phase :: State -> IO State
putCard_Phase (State (player:others) deck d_stack) = do
    hand <- return (hand player)
    putStrLn "which card do you put down"
    c <- getChar -- todo make safe version
    n <- return (read [c] :: Int)
    if n == 0 -- draw a card if entered 0
      then do
        (hand',deck') <- drawCards 1 deck
        player' <- return $ updatePlayer player hand'
        return (State (player:others) deck' d_stack)
      else do
        let card = last d_stack
            pcard = (hand !! (n - 1))
            hand' = cancel n hand
        if ((color pcard == color card) || (value pcard == value card) || color pcard == Black)
          then do
              player <- return $ updatePlayer player hand'
    --        return (State (player:others) deck d_stack)
              case pcard of (Card _ ChDir) -> do players' <- return $ (reverse others)++[player']
                                                return (State players' deck (d_stack ++ [pcard]))
                            (Card Black _) -> do let players' = others++[player']
                                                -- getColor
                                                return $ (State players' deck (d_stack ++ [pcard]))
                            (Card _ _) -> do let players' = others++[player']
                                                return $ (State players' deck (d_stack ++ [pcard]))
          else do
                  putStrLn "take a valid card or take one with 0"
                  putCard_Phase (State (player:others) deck d_stack)

{- todo make special procedures for black cards -}

remove_fake :: [a] -> [a]
remove_fake [] = error "too few cards"
remove_fake (c:[]) = error "too few cards"
remove_fake (c:fake:cs) = (c:cs)

win :: Player -> IO () -> IO ()
win player f
    |(hand player == []) = putStrLn $ "congratulations "++show (name player)
    |otherwise = f


updatePlayer :: Player -> Hand -> Player
updatePlayer (HPlayer name _) hand = HPlayer name hand
updatePlayer (AiPlayer name _) hand = AiPlayer name hand

displayCs :: [Card] -> IO ()
displayCs [] = putStr "\n"
displayCs (c:cs) = do
    displayC c
    displayCs cs

displayC :: Card -> IO ()
displayC (Card color value) = do
    setSGR [SetColor Foreground Vivid color]
    putStr $ show (Card color value)
    setSGR [SetColor Foreground Vivid White]

{- Auxiliary Functions -}

nplicate :: Int -> [a] -> [a]
nplicate n xs = concat $ (take n) $ repeat xs

cancel :: Int -> [a] -> [a]
cancel n xs = ((take (n - 1) xs) ++ (drop n xs))

pick :: Int -> [a] -> ([a],a)
pick n xs = (cancel (n + 1) xs, (xs !! n))


         {-------Take-}
         {-          -}
         {-          -}
         {-          -}
         {-          -}
         {-          -}
         {-          -}
                       
 {-Black {-----}     -}
 {-      {-XXX-}     -}
 {-      {-XXX-}     -}
 {-      {-XXX-}     -}
         {-          -}
         {-          -}
 {-      {-XXX-}     -}
 {-      {-XXX-}     -}
 {-      {-XXX-}     -}
 {-      {-----}-Take-}
 {-           -}       
 {-           -}       
 {-           -}       
 {-           -}       
 {-           -}       
 {-           -}       
 {-Black-------}       
