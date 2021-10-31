module Models (
    Person (..),
    Car (..)
) where

data Person = Person {
    firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float
} deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

mustang = Car {company = "Ford", model = "Mustang", year = 1967}
