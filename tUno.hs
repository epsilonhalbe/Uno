import TAP
import Uno
import System.Console.ANSI (setSGR,SGR(SetColor),ColorIntensity(Vivid),ConsoleLayer(Foreground),Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
import Data.List (elemIndex)
import Common

main = runTests $ do
    planTests 7
    let zeroes = [Card c Zero | c <- [Red .. Blue]]
        ncards = [Card c v | c <- [Red .. Blue],v <- [One .. ChDir]]
        blacks = [Card Black v | v <- [Plus4,ChCol]]
        full_deck = (zeroes ++ (nplicate 2 ncards) ++ (nplicate 4 blacks))
        testplayer = HPlayer "testor" [Card Red One, Card Blue Two, Card Green Four, Card Red Stop]

    is ("foo" == "foo") True $ Just "test testing"

    let xs = [10,9..1]
        ys = [1..10]
    is (cancel 3 xs) ([10,9]++[7,6..1]) $ Just "cancel an element from a list"
    is (cancel 3 ys) ([1,2]++[4..10]) $ Just "another cancel test"
    is (ys !! 3) 4 $ Just "!! test - programmers start counting at 0"
    is (pick 3 xs) (([10,9,8]++[6,5..1]),7) $ Just "pick one and give rest"
    is (nplicate 3 xs) (xs++xs++xs) $ Just "n-plicate a list"
    is (updatePlayer testplayer zeroes) (HPlayer "testor" zeroes) $ Just "update a players hand"
    is (
