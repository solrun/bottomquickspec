{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

import QuickSpec
import Test.QuickCheck
import Control.Monad( liftM, liftM2 )
import Data.List( nub, sort )
import Fun
import Bot

--------------------------------------------------------------------------------

class Total a where
  isTotal :: a -> Bool
  isTotal = not . isBot
  
instance Total ()
instance Total Bool
instance Total Int
instance Total X where
  isTotal x | isBot x = False
            | otherwise = case x of
                            Z -> True
                            S y -> isTotal y
instance Total Integer

instance Total a => Total [a] where
  isTotal xs = not (isBot xs) && case xs of
                                   []   -> True
                                   y:ys -> isTotal y && isTotal ys

--------------------------------------------------------------------------------

{-
IDEA: - default generators only generate total values
      - special generators generate values with bottoms in
-}

type X = Nat

genX :: Gen X
genX = arbitrary

genList :: Arbitrary a => Gen a -> Gen [a]
genList gen =
  oneof
  [ arbitrary
  , let botList = frequency
                  [ (1, return bot)
                  , (1, return [])
                  , (5, liftM2 (:) gen botList)
                  ]
     in botList
  ]

genF :: Gen (F X X)
genF =
  oneof
  [ do f <- arbitrary
       return (F (toNat . f . fromNat) True)
  , do f <- apply `fmap` sized genFun
       return (F f False)
  ]
 where
  fromNat Z     = 0 :: Int
  fromNat (S x) = 1 + fromNat x

  toNat k = iterate S Z !! abs (k :: Int)

data F a b = F{ app :: a->b, tot :: Bool }

instance Total (F a b) where
  isTotal f = tot f

foldl' f z []     = z
foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

deepSeq :: Nat -> a -> a
deepSeq Z x = x
deepSeq (S n) x = deepSeq n x

map' f [] = []
map' f (x:xs) = let y = f x in y `seq` y : map' f xs

f1 h f = h (app f)
f2 h f = h (\x y -> app (app f x) y)

add Z y = y
add (S x) y = S (add x y)

main = quickSpec
  [ monoTypeObserve (Proxy :: Proxy X)
  --, monoTypeObserve (Proxy :: Proxy [X])
  -- , con "map"     (f1 map     :: (F A B) -> [A] -> [B])
  -- , con "map'"     (f1 map'     :: (F A B) -> [A] -> [B])
  -- , con "foldl"   (f2 foldl   :: (F B (F A B)) -> B -> [A] -> B)
  -- , con "foldl'"   (f2 foldl'   :: (F B (F A B)) -> B -> [A] -> B)
  -- , con "filter"  (filter  :: (A -> Bool) -> [A] -> [A])
  -- , con "++"      ((++)    :: [X] -> [X] -> [X])
  -- , con "reverse" (reverse :: [X] -> [X])
  -- , con "nub"     (nub     :: [X] -> [X])
  -- , con "sort"    (sort    :: [X] -> [X])
  -- , con "[]"      ([]      :: [X])
  -- , con ":"       ((:)     :: X -> [X] -> [X])
  --, con "_|_"     (bot :: X)
  --, con "_|_"     (bot :: [X])
  -- , con "head" (head :: [X] -> X)
  -- , con "tail" (tail :: [X] -> [X])
  -- , con "init" (init :: [X] -> [X])
  -- , con "last" (last :: [X] -> X)
  -- , con "!!" ((!!) :: [X] -> Int -> X)
  -- , con "drop" (drop :: Int -> [X] -> [X]) 
  , con "@" (app :: F X X -> X -> X)
  , con "add" (add :: X -> X -> X)
  --, con "deepSeq" (deepSeq :: X -> A -> A)
  --, con "seq" (seq :: X -> A -> A)
  , con "True" (True :: Bool)
  
  , predicate "isTotal" (isTotal :: X -> Bool)
  , predicate "isTotal" (isTotal :: F X X -> Bool)
  --, predicate "isTotal" (isTotal :: [X] -> Bool)
  -- , predicate "nonNegative" ((>=0) :: Int -> Bool)
  , instFun (genX :: Gen X)
  -- , instFun (genList :: Gen X -> Gen [X])
  , instFun (genF :: Gen (F X X))
  ]

instance Observe () (List ()) X where
  observe t x | isBot x   = Bot
              | otherwise = case x of
                              S n -> Cons () (observe t n)
                              Z   -> Nil

data List a = Nil | Cons a (List a) | Bot deriving ( Eq, Ord, Show )

instance Observe test obs a => Observe test (List obs) [a] where
  observe t xs
    | isBot xs  = Bot
    | otherwise = case xs of
                    []   -> Nil
                    y:ys -> Cons (observe t y) (observe t ys)

--------------------------------------------------------------------------------

