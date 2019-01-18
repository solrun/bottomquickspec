{-# LANGUAGE GADTs #-}
module Fun where

import Test.QuickCheck hiding ( Fun )
import Control.Monad( liftM, liftM2 )
import Bot

--------------------------------------------------------------------------------

data FunNat c where
  Case :: c -> FunNat c -> FunNat c
  
  Unit :: FunNat ()
  Pair :: FunNat a -> FunNat b -> FunNat (a,b)
  Lft  :: FunNat a -> FunNat (Either a b)
  Rgt  :: FunNat b -> FunNat (Either a b)

  Map  :: (b -> c) -> FunNat b -> FunNat c

sh :: Show a => FunNat a -> String
sh f = show [ apply f n | n <- take 5 (iterate S Z) ]

--------------------------------------------------------------------------------

data Nat = Z | S Nat
 deriving ( Eq, Ord, Show )

apply :: FunNat c -> (Nat -> c)
apply (Case zero succ) Z     = zero
apply (Case zero succ) (S n) = apply succ n

apply Unit             _     = ()
apply (Pair f g)       n     = (apply f n, apply g n)
apply (Lft f)          n     = Left (apply f n)
apply (Rgt g)          n     = Right (apply g n)

apply (Map h f)        n     = h (apply f n)

--------------------------------------------------------------------------------

class Arbitrary a => FunResult a where
  genStrict :: Int -> Gen (FunNat a)
  genStrict k | k <= 0 =
    do return (Map (\_ -> bot) Unit)

  genStrict k =
    do z <- arbitrary
       s <- genFun (k-1)
       return (Case z s)
  
  genLazy :: Int -> Gen (FunNat a)
  
  genFun :: Int -> Gen (FunNat a)
  genFun k =
    oneof ( [genStrict k | k > 0]
         ++ [genLazy k]
          )

instance FunResult () where
  genLazy _ = return Unit

instance (FunResult a, FunResult b) => FunResult (a,b) where
  genLazy k =
    do f <- genFun (k-1)
       g <- genFun (k-1)
       return (Pair f g)

instance (FunResult a, FunResult b) => FunResult (Either a b) where
  genLazy k =
    oneof [liftM Lft (genFun (k-1)), liftM Rgt (genFun (k-1))]

instance FunResult a => FunResult [a] where
  genLazy k =
    do f <- genFun k -- :: Gen (Either () (a,[a]))
       return (Map h f)
   where
    h (Left u)       = [] where types = u :: ()
    h (Right (x,xs)) = x:xs

instance FunResult Nat where
  genLazy k =
    do f <- genFun k -- :: Gen (Either () Nat)
       return (Map h f)
   where
    h (Left u)  = Z where types = u :: ()
    h (Right n) = S n

--------------------------------------------------------------------------------

instance Arbitrary Nat where
  arbitrary =
    frequency
    [ (1, return bot)
    , (2, return Z)
    , (5, liftM S arbitrary)
    ]

--------------------------------------------------------------------------------

