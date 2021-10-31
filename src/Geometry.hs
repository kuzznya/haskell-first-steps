module Geometry (
    Area,
    area,
    Moveable,
    move,
    Vector (..),
    Shape (..)
) where

class Area shape where
    area :: shape -> Float

class Moveable shape where
    move :: Vector -> shape -> shape 

data Vector = Vector Float Float deriving (Show)
data Shape = Circle Vector Float | Rectangle Vector Vector deriving (Show)

instance Area Shape where
    area (Circle _ radius) = pi * radius ^ 2
    area (Rectangle (Vector x1 y1) (Vector x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

instance Moveable Shape where
    move (Vector x y) (Circle (Vector x0 y0) radius) = Circle (Vector (x0 + x) (y0 + y)) radius
    move (Vector x y) (Rectangle (Vector x1 y1) (Vector x2 y2)) = Rectangle (Vector (x1 + x) (y1 + y)) (Vector (x2 + x) (y2 + y))