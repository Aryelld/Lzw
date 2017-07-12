module Lzw where 
data Lzw a = C a (Lzw a) | T (Int,Int) (Lzw a) | End

instance Show a => Lzw a where
    show (C a b) = (show a) ++ (show b)
    show (T (Int,Int) b) = (show a) ++ (show b)
    show (End) = ""

descompacta 