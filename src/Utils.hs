{-# LANGUAGE ExistentialQuantification #-}

module Utils where

import Text.Printf

data PrintfArgT = forall a. PrintfArg a => P a

printfa :: PrintfType t => String -> [ PrintfArgT ] -> t
printfa format = printfa' format . reverse
  where printfa' :: PrintfType t => String -> [ PrintfArgT ] -> t
        printfa' format [] = printf format
        printfa' format (P a:as) = printfa' format as a
