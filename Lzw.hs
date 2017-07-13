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

descompacta (C comeco End) = comeco
descompacta (C comeco (C fim End)) = comeco++fim
descompacta (C comeco (C meio restoDaExpressao)) = descompacta (C (comeco++meio) restoDaExpressao)
descompacta (C comeco (T x y restoDaExpressao)) = descompacta (C (comeco++(aux x y comeco)) restoDaExpressao)

compacta [] = []
compacta l = compactador l [] End

-- compactador (h:[]) [] a = C (h:[]) a
-- compactador (h:t) [] a = C (h:[]) (compactador t (h:[]) a)
-- compactador (h:t) (l) a = retorno
--     where 
--         (posicao, extensao, historico, diante) = aux3 (h:t) l
--         retorno = if posicao==0 then (C historico End) else 

aux1 [] _ n = 0
aux1 _ [] n = 0
aux1 (h1:t1) (h2:t2) n
    | (h1 == h2) = n
    | otherwise = (aux1 t1 (h2:t2) (n + 1))


aux2 [] _ _= 0
aux2 _ [] _= 0
aux2 (h1:t1) (h2:t2) bool= 
    if (h1==h2)&&(bool||a/=0) then 1 + a else 0
    where
        a = (aux2 t1 t2 (h1==h2))

--funcao recebe dois argumentos que representao a lista a ser analisada, e o historico
--sob essas condicoes ela analisa essas duas listas sob o ponto de vista de aux2 e aux1
--obtendo a posicao, estensao, string anterior e a que ainda será analisada numa tupla
aux3 [] _ = (0,0,[],[])
aux3 a [] = (0,0,a,[])
aux3 a b = (posicao+1, extensao,(take posicao a),drop (posicao+extensao) a)
    where
        posicao = aux1 a b 0
        extensao = aux2 (drop (posicao) a) b False