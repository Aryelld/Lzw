module Lzw where 
data Lzw a = C [a] (Lzw a) | T Int Int (Lzw a) | End

instance Show a => Show (Lzw a) where
    show (C a b) = show a ++ (show b)
    show (T a b c) = "(T" ++ (show a) ++ "," ++ (show b) ++ ")" ++ (show c)
    show (End) = ""

aux _ _ [] = [] 
aux _ 0 _ = []
aux 1 y (h:t) = (h:(aux 1 (y-1) t))
aux x y (h:t) = aux (x-1) y t

descompacta (C comeco (C meio restoDaExpressao)) = descompacta (C (comeco++meio) restoDaExpressao)
descompacta (C comeco (T x y restoDaExpressao)) = descompacta (C (comeco++(aux x y comeco)) restoDaExpressao)
descompacta (C comeco (C fim End)) = comeco++fim
descompacta (C comeco End) = comeco

compacta::Eq a => [a] -> Lzw a
compacta [] = []
compacta l = compactador l [] End

compactador (h:[]) [] a = C (filter (\x-> (x /= '\'') (show h)) a
compactador (h:t) [] a = 

aux (h1:t1) (h2:t2) = 
