module Lzw where 
data Lzw a = C [a] (Lzw a) | T Int Int (Lzw a) | End

instance Show a => Lzw a where
    show (C a b) = (show a) ++ (show b)
    show (T a b c) = "(T"++(show a) ++","++ (show b)++")"++(show c)
    show (End) = ""

aux _ _ [] = [] 
aux _ 0 _ = []
aux 1 y (h:t) = (h:(aux 1 (y-1) t))
aux x y (h:t) = aux (x-1) y t

descompacta (C comeco (C adiante (T x1 y1 restoDaExpressao))) = descompacta (C novoComeco restoDaExpressao)
    where
        novoComeco = descompacta (C (comeco++adiante) (T x1 y1 End))
descompacta (C comeco (T x y End)) = comeco++adiante
    where
        adiante = aux x y comeco