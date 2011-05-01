import Uno
import System.Console.ANSI (setSGR,SGR(SetColor),ColorIntensity(Vivid),ConsoleLayer(Foreground),Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List (elemIndex)
import Common


main = do
    let zeroes = [Card c Zero | c <- [Red .. Blue]]
    let ncards = [Card c v | c <- [Red .. Blue],v <- [One .. ChDir]]
    let blacks = [Card Black v | v <- [Plus4,ChCol]]
    let full_deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
    display' full_deck
    putStrLn "\nColortest"

display' [] = putStr "\n"
display' (c:cs) = do
    display c
    display' cs

