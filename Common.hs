module Common where
import System.Console.ANSI (Color(Black, Red, Green, Yellow, Blue, Magenta, Cyan, White))
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

data Value = Zero|One|Two|Three|Four|Five|Six|Seven|Eight|Nine|Plus2|Stop|ChDir|Plus4|ChCol|Dummy
    deriving (Read, Show, Eq, Ord, Enum)

data Player = HPlayer {name::String, hand::[Card]} | AiPlayer {name::String, hand::[Card]}
    deriving (Show, Eq)
--- state = (players, deck, d_stack)
data State = State {players :: [Player], deck :: Deck, d_stack :: D_Stack}

type Deck = [Card]
type D_Stack = [Card]
type Hand = [Card]
