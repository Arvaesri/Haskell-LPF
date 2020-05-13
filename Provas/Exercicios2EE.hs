--PROVA ESTA NO DRIVE

-- --Repita 1 a 10 que executa a funcao com os valores de 1 a 10
-- devolve  = 10

-- repitaN 0 f = return ()
-- repitaN n f = do
--     f
--     repitaN (n-1) f



-- monad :: () -> () 
-- monad () = return ()
-- monad f = do
-- 	f
-- 	x <- f
-- 	monad x

-- 2EE respondido
data HuffTree = Node HuffTree HuffTree | Code Char

decode :: HuffTree -> String -> String
decode raiz str = aux1 raiz str where
aux1 (Code c) str = c : (aux1 raiz str)
aux1 tree "" = ""
aux1 (Node zero um) ('0':str) = aux1 zero str
aux1 (Node zero um) ('1':str) = aux1 um str


instance Show HuffTree where
show h = aux "" h where
aux prefix (Code c) = c : (" = " ++ prefix)
aux prefix (Node zero um) = (aux (prefix ++ " 0") zero) ++ "\n" ++ (aux (prefix ++ " 1") um)