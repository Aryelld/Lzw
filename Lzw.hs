module Lzw where 
data Lzw a = C [a] (Lzw a) | T Int Int (Lzw a) | End

instance Show a => Show (Lzw a) where
    show (C a b) = filter (\x-> (x/='"')) ((show a) ++ (show b))
    show (T a b c) = "(T"++(show a) ++","++ (show b)++")"++(show c)
    show (End) = ""

aux _ _ [] = [] 
aux _ 0 _ = []
aux 1 y (h:t) = (h:(aux 1 (y-1) t))
aux x y (h:t) = aux (x-1) y t

descompacta (C comeco (C meio restoDaExpressao)) = descompacta (C (comeco++meio) restoDaExpressao)
descompacta (C comeco (T x y restoDaExpressao)) = descompacta (C (comeco++adiante) restoDaExpressao)
    where
        adiante = aux x y comeco
descompacta (C comeco (C fim End)) = comeco++fim
descompacta (C comeco End) = comeco