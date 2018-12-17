{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

import QuickSpec
import Test.QuickCheck
import Control.Monad( liftM, liftM2 )
import Control.Exception
import System.IO.Unsafe
import Data.List( nub, sort )

--------------------------------------------------------------------------------

class Total a where
  isTotal :: a -> Bool
  isTotal = not . isBot
  
instance Total ()
instance Total Bool
instance Total Int
instance Total X
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

data X = X Int deriving ( Eq, Ord, Show )

instance Arbitrary X where
  arbitrary = liftM X arbitrary

genX :: Gen X
genX = oneof
  [ arbitrary -- total
  , frequency
    [ (1, return bot)
    , (5, arbitrary)
    ]
  ]

genList :: Arbitrary a => Gen a -> Gen [a]
genList gen =
  oneof
  [ arbitrary -- total
  , let botList = frequency
                  [ (1, return bot)
                  , (1, return [])
                  , (5, liftM2 (:) gen botList)
                  ]
     in botList
  ]

main = quickSpec
  [ monoTypeObserve (Proxy :: Proxy X)
  , monoTypeObserve (Proxy :: Proxy [X])
  -- , con "map"     (map     :: (A -> B) -> [A] -> [B])
  -- , con "filter"  (filter  :: (A -> Bool) -> [A] -> [A])
  , con "++"      ((++)    :: [X] -> [X] -> [X])
  , con "reverse" (reverse :: [X] -> [X])
  , con "nub"     (nub     :: [X] -> [X])
  -- , con "sort"    (sort    :: [X] -> [X])
  , con "[]"      ([]      :: [X])
  , con ":"       ((:)     :: X -> [X] -> [X])
  , con "_|_"     (bot :: X)
  , con "_|_"     (bot :: [X])
  , con "head" (head :: [X] -> X)
  , con "tail" (tail :: [X] -> [X])
  
  , predicate "isTotal" (isTotal :: X -> Bool)
  , predicate "isTotal" (isTotal :: [X] -> Bool)
  , instFun (genX :: Gen X)
  , instFun (genList :: Gen X -> Gen [X])
  ]

instance Observe () (Maybe X) X where
  observe () x | isBot x   = Nothing
               | otherwise = Just x

data List a = Nil | Cons a (List a) | Bot deriving ( Eq, Ord, Show )

instance Observe test obs a => Observe test (List obs) [a] where
  observe t xs
    | isBot xs  = Bot
    | otherwise = case xs of
                    []   -> Nil
                    y:ys -> Cons (observe t y) (observe t ys)

--------------------------------------------------------------------------------
-- please close your eyes if you're under 18

bot :: a
bot = error "_|_"

isBot :: a -> Bool
isBot x = unsafePerformIO $
  do eea <- try (evaluate x)
     case eea of
       Left e
         | otherwise -> return True
         | take 3 (displayException (e :: ErrorCall)) == "_|_" -> return True
       _ -> return False

--------------------------------------------------------------------------------

