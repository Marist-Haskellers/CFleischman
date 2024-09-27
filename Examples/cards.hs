-- Defining our own types and names helps create a Domain-Specific language

type Rank = Int -- Note: has much larger range than card rank

-- Algebraic type: Sum
data Suit = Hearts | Diamonds | Spades | Clubs -- four data constructors
  deriving (Show)

-- Algebraic type: Product
data Card = Card Suit Rank -- one data constructor with two parameters

instance Show Card where
  show (Card s r) = show r ++ " of " ++ show s