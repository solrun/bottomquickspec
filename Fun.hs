{-# LANGUAGE GADTs #-}

import Control.Exception
import System.IO.Unsafe

--------------------------------------------------------------------------------

data Fun a c where
  -- bottom
  Bot  :: Fun a c
  
  -- strict pattern matching
  Unit :: c -> Fun () c
  Pair :: Fun a (Fun b c) -> Fun (a,b) c
  Case :: Fun a c -> Fun b c -> Fun (Either a b) c

  -- lazy result producing
  Cons :: Fun a b -> Fun a c -> Fun a (b,c)
  Lft  :: Fun a b -> Fun a (Either b c)
  Rgt  :: Fun a c -> Fun a (Either b c)

apply :: Fun a c -> (a -> c)
apply Bot        _         = bot

apply (Unit z)   ()        = z
apply (Pair p)   (x,y)     = apply (apply p x) y
apply (Case p _) (Left x)  = apply p x
apply (Case _ q) (Right y) = apply q y

apply (Cons p q) x         = (apply p x, apply q x)
apply (Lft p)    x         = Left  (apply p x)
apply (Rgt q)    x         = Right (apply q x)

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


