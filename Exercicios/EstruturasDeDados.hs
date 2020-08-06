data Lista a = No a (Lista a)|LVazio deriving Show

exemploL = No 1 (No 2 (No 3 (LVazio))) 

enfileirar :: a -> Lista a -> Lista a -- Adiciona no Final da Fila
enfileirar elem LVazio = No elem LVazio
enfileirar elem (No a prox) = (No a (enfileirar elem prox))

desenfileirar :: Lista a -> Lista a -- Remove o Primeiro
desenfileirar LVazio = LVazio
desenfileirar (No e prox) = prox

----------------------------------------------------

data Pilha a = Pilha a (Pilha a) | PVazio deriving Show

exemploP = Pilha 1 (Pilha 2 (Pilha 3 (Pilha 4 (PVazio))))

empilhar :: Pilha a -> a -> Pilha a
empilhar PVazio elem = Pilha elem PVazio
empilhar (Pilha a prox) elem = (Pilha a (empilhar prox elem))

desempilhar :: Pilha a -> Pilha a
desempilhar (Pilha a PVazio) = PVazio 
desempilhar (Pilha a prox) = Pilha a (desempilhar prox)


----------------------------------------------------

data Arvore a = Arvore a (Arvore a) (Arvore a) | AVazia deriving Show

exemploA = Arvore 2 (Arvore 1 (AVazia) (AVazia) ) (Arvore 3 (AVazia) (AVazia)) 

buscarA :: (Eq a,Ord a) => Arvore a -> a -> Bool
buscarA AVazia _ = False
buscarA (Arvore a esq dir) elem | elem == a = True
                                | elem < a = buscarA (esq) elem
                                | elem > a = buscarA (dir) elem


listarElementos :: Arvore a -> [a]
listarElementos AVazia = []
listarElementos (Arvore a esq dir) = a:(listarElementos esq ) ++ (listarElementos dir )
