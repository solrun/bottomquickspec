module Bot where

import Control.Exception
import System.IO.Unsafe

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


