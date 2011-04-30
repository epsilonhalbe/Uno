import System.Console.ANSI
data Card = Card {color :: Color , value :: Value}
instance Show Card where
    show c = "|"++show (value c)++"|"
           --  setSGR [SetColor Foreground Vivid White]

data Value = Zero | One | Two | Plus4 | Stop
    deriving (Show)


display :: Card -> IO ()
display (Card color value) = do
    setSGR [SetColor Foreground Vivid color]
    putStr $ show (Card color value)
    setSGR [SetColor Foreground Vivid White]

main :: IO ()
main = do
    setTitle "Testosteron"
    putStrLn "foo"
    display (Card Black Two)
    print [1..20]
    putStrLn $ show [1..20]
    putStr $ show [1..20]
