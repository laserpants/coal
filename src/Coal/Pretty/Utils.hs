{-# LANGUAGE OverloadedStrings #-}

module Coal.Pretty.Utils (parensIf, tupledCompact) where

import Prettyprinter

parensIf :: Bool -> Doc ann -> Doc ann
parensIf True = parens
parensIf False = id

tupledCompact :: [Doc ann] -> Doc ann
tupledCompact = encloseSep "(" ")" ", "
