{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import GHC.Generics

tests :: IO ()
tests = do
  let person = Person "Alice" 30 [1,2,3]
  let shape1 = Circle 5.0
  let shape2 = ShapePerson person
  putStrLn $ gshow person  -- Output: "Person Alice, 30"
  putStrLn $ gshow shape1  -- Output: "Circle 5.0"
  putStrLn $ gshow shape2  -- Output: "Rectangle 4.0, 6.0"

parens :: Bool -> String -> String
parens True x = "(" ++ x ++ ")"
parens False x = x

-- Used to determine if a constructor is applied to any arguments, if so, it
-- will need parentheses when printing
class IsApplied a where
  isApplied :: a -> Bool
  default isApplied :: (Generic a, GIsApplied (Rep a)) => a -> Bool
  isApplied x = gIsApplied (from x)

class GIsApplied f where
  gIsApplied :: f a -> Bool

instance GIsApplied U1 where
  gIsApplied _ = False

instance GIsApplied V1 where
  gIsApplied _ = False

instance GIsApplied (K1 i c) where
  gIsApplied _ = True

instance (GIsApplied f) => GIsApplied (M1 D c f) where
  gIsApplied (M1 x) = gIsApplied x

instance (GIsApplied f) => GIsApplied (M1 C c f) where
  gIsApplied (M1 x) = gIsApplied x

instance (GIsApplied f) => GIsApplied (M1 S c f) where
  gIsApplied (M1 x) = gIsApplied x

instance (GIsApplied f, GIsApplied g) => GIsApplied (f :*: g) where
  gIsApplied _ = True

instance (GIsApplied f, GIsApplied g) => GIsApplied (f :+: g) where
  gIsApplied (L1 x) = gIsApplied x
  gIsApplied (R1 x) = gIsApplied x



class GShow a where
  gshow :: a -> String

  default gshow :: (Generic a, GShow' (Rep a)) => a -> String
  gshow x = gshow' (from x)

class GShow' f where
  gshow' :: f a -> String

instance GShow' U1 where
  gshow' U1 = ""

instance (GShow' f, GShow' g) => GShow' (f :*: g) where
  gshow' (x :*: y) = gshow' x ++ " " ++ gshow' y

instance (GShow' f, GShow' g) => GShow' (f :+: g) where
  gshow' (L1 x) = gshow' x
  gshow' (R1 x) = gshow' x

instance (GShow' f) => GShow' (M1 D c f) where
  gshow' (M1 x) = gshow' x

instance (GShow' f, Constructor c, GIsApplied f) => GShow' (M1 C c f) where
  gshow' m@(M1 x) = parens (gIsApplied x) $
    conName m ++ " " ++ gshow' x

instance (GShow' f, Selector s) => GShow' (M1 S s f) where
  -- Ignore selector names
  gshow' (M1 x) = gshow' x

instance (GShow a) => GShow' (K1 i a) where
  gshow' (K1 x) = gshow x

instance GShow Char where
  gshow = show

instance GShow Int where
  gshow = show

instance GShow Bool where
  gshow = show

instance {-# OVERLAPPING #-} GShow String where
  gshow = show

instance GShow Double where
  gshow = show

instance (GShow a, GShow b) => GShow (a, b) where
  gshow (a, b) = "(" ++ gshow a ++ ", " ++ gshow b ++ ")"

-- instance GShow a => GShow [a] where
--   gshow = show
--
data Person = Person { name :: String, age :: Int, ints :: [Int]}
  deriving (Generic)

instance GShow Person

data Shape = Circle Double | ShapePerson Person
  deriving (Generic)

instance GShow Shape

instance GShow a => GShow [a]
