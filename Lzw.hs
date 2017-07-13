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
 
compacta [] = C [] End
compacta l = compactador l [] 

compactador [] _ = simpl (C [] End)
compactador sera jafoi = retorno
    where 
        (posicao, extensao,historico, diante) = compAux sera jafoi
        retorno = 
            if (posicao>=1)&&(extensao>1) then  
                (T posicao extensao (simpl (compactador diante historico )))
            else 
            -- if n <= (length historico) then --se não passar do 
                simpl (C [(last historico)] (compactador diante historico))
            -- else 
            --     End

simpl (C a (C b c)) = simpl (C (a++b) c) 
simpl a = a

compAux [] _ = (0,0,[],[])
compAux (h:t) [] = (0,0,(h:[]),t)
compAux a b = 
    (posicao, extensao,historico,diante)
    where
        historico = b++take aux a
        aux = 
            if extensao == 0 then
                1
            else 
                extensao 
        diante = drop aux a
        posicao = aux1 a b
        extensao = aux2 a (drop (posicao-1) b) False

aux1 [] _ = 0
aux1 _ [] = 0
aux1 (h1:t1) (h2:t2)
    | (h1 == h2) = 1 
    | otherwise = 1+(aux1 (h1:t1) (t2) )

aux2 [] _ _= 0
aux2 _ [] _= 0
aux2 (h1:t1) (h2:t2) bool= 
    if (h1==h2)&&(bool||a/=0) then 1 + a else 0
        where
            a = (aux2 t1 t2 (h1==h2))