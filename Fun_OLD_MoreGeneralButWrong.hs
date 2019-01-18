{-# LANGUAGE GADTs #-}

import Control.Exception
import System.IO.Unsafe
import Test.QuickCheck hiding ( Fun )
import Control.Monad( liftM, liftM2 )

--------------------------------------------------------------------------------

data Fun a c where
  -- bottom
  Bot   :: Fun a c
  
  -- strict pattern matching
  Unit  :: Fun () c
  Pair1 :: Fun a (Fun b c) -> Fun (a,b) c
  Pair2 :: Fun b (Fun a c) -> Fun (a,b) c
  Case  :: Fun a c -> Fun b c -> Fun (Either a b) c

  -- lazy result producing
  Cons  :: Fun a b -> Fun a c -> Fun a (b,c)
  Lft   :: Fun a b -> Fun a (Either b c)
  Rgt   :: Fun a c -> Fun a (Either b c)
  Lazy  :: c -> Fun a c

{-
TODO:

- Add Map constructor so types can change

- Maybe Unit should get a result also?

- Maybe Bot and Lazy should be merged?
-}

--------------------------------------------------------------------------------

apply :: Fun a c -> (a -> c)
apply Bot        _         = bot

apply Unit       ()        = bot
apply (Pair1 p)  (x,y)     = apply (apply p x) y
apply (Pair2 p)  (x,y)     = apply (apply p y) x
apply (Case p _) (Left x)  = apply p x
apply (Case _ q) (Right y) = apply q y

apply (Cons p q) x         = (apply p x, apply q x)
apply (Lft p)    x         = Left  (apply p x)
apply (Rgt q)    x         = Right (apply q x)

apply (Lazy z)   _         = z

--------------------------------------------------------------------------------

-- genStrict is for generating a random function that starts with a pattern match
-- on its argument
class Strict a where
  genStrict :: Lazy b => Gen (Fun a b)

-- genLazy is for generating a random function that starts with constructing
-- (part of) its result
class Lazy b where
  genLazy :: Strict a => Gen (Fun a b)

-- genFun randomly decides between constant bottom, strict or lazy
genFun :: (Strict a, Lazy b) => Gen (Fun a b)
genFun = oneof [return Bot, genStrict, genLazy]

instance Strict () where
  genStrict = return Unit

instance (Strict a, Strict b) => Strict (a,b) where
  genStrict = oneof
    [ liftM Pair1 genFun
    , liftM Pair2 genFun
    ]

instance (Strict a, Strict b) => Strict (Either a b) where
  genStrict = liftM2 Case genFun genFun

instance (Lazy a, Lazy b) => Lazy (a,b) where
  genLazy = liftM2 Cons genFun genFun

instance (Lazy a, Lazy b) => Lazy (Either a b) where
  genLazy = oneof
    [ liftM Lft genFun
    , liftM Rgt genFun
    ]

instance (Strict a, Lazy b) => Lazy (Fun a b) where
  genLazy = liftM Lazy genFun

{-
TODO:

- add a size parameter so that we can deal with infinite types (e.g. lists)

- think about frequencies
-}

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


