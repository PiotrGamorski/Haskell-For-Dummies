{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module CustomTypes where
import qualified Data.Foldable as CustomTypes

-- type is working similarlly as interfaces in Typescript
type Point = (Double, Double )
midPoint :: Point -> Point -> Point
midPoint (x1, y1) (x2, y2) = ((x1+x2)/2, (y1+y2)/2)

-- MakeCustomerId is the constructor - can be given any name
newtype CustomerId = MakeCustomerId Int
customer :: CustomerId
customer = MakeCustomerId 13

customerToInt :: CustomerId -> Int
customerToInt (MakeCustomerId i) = i

newtype Name = Name String
nameToString :: Name -> String
nameToString (Name theName) = theName

piotr :: Name
piotr = Name "Piotr"

madzia :: Name
madzia = Name "Madzia"


displayNames :: Name -> Name -> String
displayNames name1 name2 = nameToString name1 ++ " " ++ nameToString name2

-- Records (most similar to classes in otherprogramming languages)
data Customer = Customer
 {customerId :: CustomerId
  , name :: String
  , luckyNumber :: Int
 }

-- First assign type to variable alice
--  Then assign values to alice properties
alice :: Customer
alice = Customer
  {customerId = MakeCustomerId 13
   , name = "Alice"
   , luckyNumber = 47
  }

sally :: Customer
sally = alice
 {name = "Sally"
  , luckyNumber = 1
  }

getCustomerIdFromSally :: Int
getCustomerIdFromSally = customerToInt (customerId sally)

getNameFromSally :: String
getNameFromSally = name sally

getLuckyNumberFromSally :: Int
getLuckyNumberFromSally = luckyNumber sally

getCustomerIdFrom :: Customer -> Int
getCustomerIdFrom record = customerToInt (customerId record)

-- ALGEBRAIC DATA TYPES
-- definition of algebraic data type "MyCustomer"
data MyCustomer = MyCustomer CustomerId String Int
-- declaration
stef :: MyCustomer
stef = MyCustomer (MakeCustomerId 13) "Stef" 42

-- the getter needs to implemented that way, because in albegraic type
-- we don't have the names of properties given directly,
-- so we have to pass the Constructor to get the value from it
getMyCustomerId :: MyCustomer -> CustomerId
getMyCustomerId (MyCustomer cust_id name luckyNumber) = cust_id
getMyCustomerIdShorter :: MyCustomer -> CustomerId
getMyCustomerIdShorter (MyCustomer cust_id _ _) = cust_id
getMyCustomerNameShorter :: MyCustomer -> String
getMyCustomerNameShorter (MyCustomer _ name _) = name
-- By the way, above works the pattern matching - that's way
-- it can take properties when given "stef" as argument

data StringTree = StringTree String [StringTree] deriving Show
hierarchy :: StringTree
hierarchy = StringTree "C:" [StringTree "Program Files" []
              , StringTree "Users" [StringTree "Alice" []]
              , StringTree "Cats" []]

data Bool = False | True deriving Show
x :: CustomTypes.Bool
x = CustomTypes.False
negate :: CustomTypes.Bool -> CustomTypes.Bool
negate CustomTypes.True = CustomTypes.False
negate CustomTypes.False = CustomTypes.True

data MaybeInt = NoInt | JustInt Int deriving Show
defaultInt :: Int -> MaybeInt -> Int
defaultInt defaultValue NoInt = defaultValue
defaultInt _ (JustInt x) = x


-- This approach is like enum type in Java or C#. This is only a special case 
-- of Haskell's Algebraic Type
data StringList = EmptyStringList | ConsStringList String StringList deriving Show
lengthStringList :: StringList -> Int
lengthStringList EmptyStringList = 0
lengthStringList (ConsStringList _ xs) = 1 + lengthStringList xs

-- those functions are alomst indentical - the second one is more generic
-- but the idea to measure the lenght is the same
myLength :: Num p => [a] -> p
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs 

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show
-- we define a function by pattern matching 
isSmall :: Thing -> Prelude.Bool
isSmall Shoe = Prelude.True
isSmall Ship = Prelude.False
isSmall SealingWax = Prelude.True
isSmall Cabbage = Prelude.True
isSmall King = Prelude.False

-- the later gives the following result: isSmall2 Cabbage = True.
--  whatever it takes but not Ship or King, it'll always return true
isSmall2 :: Thing -> Prelude.Bool
isSmall2 Ship = Prelude.False
isSmall2 King = Prelude.False
isSmall2 _ = Prelude.True

-- BEYOND ENUMERATIONS
data FailableDouble = Failure | OK Double deriving (Show, Eq)
a :: FailableDouble
a = Failure
b :: FailableDouble
b = OK 3.14

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK result) = result

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
  Failure -> 0
  OK result -> result

n :: Int
n = case "Hello" of
  [] -> 0
  ('H': xs) -> length xs
  _ -> -1


data MyMaybe a = Just a | Nothing deriving Show
y :: MyMaybe Int
y = CustomTypes.Nothing

z :: MyMaybe Int
z = CustomTypes.Just 10

fromMyMaybe :: a -> MyMaybe a -> a
fromMyMaybe defaultVal CustomTypes.Nothing = defaultVal
fromMyMaybe _ (CustomTypes.Just x) = x


-----------------TYPE CLASS--------------------
element :: Eq a => a -> [a] -> Prelude.Bool
element _ [] = Prelude.False
element x (y : ys)
 | x == y = Prelude.True
 | otherwise = CustomTypes.element x ys


data RGB = RGB Int Int Int
colors :: [RGB]
colors = [RGB 255 0 0, RGB 0 255 0, RGB 0 0 255]
green :: RGB
green = RGB 0 255 0
instance Eq RGB where
  (RGB r1 g1 b1) == (RGB r2 g2 b2) = r1 == r2 && g1 == g2 && b1 == b2
isGreenInColors :: Prelude.Bool
isGreenInColors = CustomTypes.element green colors

instance Show RGB where
  show (RGB r g b) = "RGB" ++ " " ++ show r ++ " " ++ show g ++ " " ++ show b

  -- more complex example for 
data Maybe' a = Nothing' | Just' a deriving Show
instance Eq a => Eq (Maybe' a) where
  Nothing' == Nothing' = Prelude.True 
  Nothing' == (Just' _) = Prelude.False
  (Just' _) == Nothing' = Prelude.False
  (Just' x) == (Just' y) = x == y

fooVar :: Maybe' [Char]
fooVar =  Just' "Hahaha"
fooVar2 :: Maybe' [Char]
fooVar2 = Just' "Madzia is the best" 

fromMaybe' :: a -> Maybe' a -> a
fromMaybe' defaultVal CustomTypes.Nothing' = defaultVal
fromMaybe' _ (CustomTypes.Just' x) = x