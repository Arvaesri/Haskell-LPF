import System.IO
import Data.Char
import System.Directory

data Livro = Livro String String  deriving (Show,Read,Eq,Ord)
type Biblioteca = [Livro]
listavazia = []
biblioteca1 = [Livro "Cuidorise" "1996",Livro "Buidorise" "1995",Livro "Auidorise" "1994",Livro "steinbush" "1998"]

menu = do
     apagarTela
     putStrLn("\n\n         Digite a Opcao Desejada:")
     putStrLn("          _____________________")
     putStrLn("         | 1 - Adicionar Livro:|")
     putStrLn("         | 2 - Remover Livro:  |")
     putStrLn("         | 3 - Buscar Livro:   |")
     putStrLn("         | 4 - Apagar Tudo:    |")
     putStrLn("         | 5 - Sair            |")
     putStrLn("         |_____________________|")
     
     x <- getLine
     if(x == "1") then opcao1
     else if(x == "2") then opcao1
     else if(x == "3") then opcao1
     else if (x == "4") then writeFile "Teste1.txt" "Vazio"
        else if (x == "5") then return()
     else putStrLn("Escolha Entre As Opcoes Mostradas Acima!!!")


apagarTela = do
     putStr ("\ESC[2J")

opcao1 = do 
     putStr("Digite o Titulo do Livro que Deseja Adicionar: ")
     titulo <- getLine
     putStr("Digite o Ano do Livro que Deseja Adicionar: ")
     ano <- getLine
     adicionarLivro (Livro ("Titulo: "++titulo) ("Ano: "++ano))
     menu


printarLista :: Biblioteca -> IO () -- retorna a lista com \n
printarLista (h:t) = do             -- Lembrar de tratar o caso do arquivo criado mas vazio
     putStrLn ("\t   "++show h)
     if (t == []) then do
     putStr("\n")
     else
        printarLista (t)



file = do
    bool <- doesFileExist "BancoDeLivros.txt"
    handle <- openFile "BancoDeLivros.txt" ReadMode
    conteudo <- hGetLine handle -- 
    let lista = read conteudo
    hClose handle
    if(bool) then do
       printarLista lista
    else
        print("else")
 
--TO DO: Tranformar o caminho do arquivo em uma variavel global
    --   criar umma função para printar a lista de livros direto do arquivo


converteLista = do
     handle <- openFile "Teste1.txt" ReadMode 
     conteudo <- hGetLine handle -- se estiver vazio da uma exeption
     let lista = (read conteudo)::Biblioteca 
     hClose handle
     return (lista)


-- removerDaLista = do
--      handle <- openFile "Teste1.txt" ReadMode 
--      conteudo <- hGetLine handle -- se estiver vazio da uma exeption
--      let lista = (read conteudo)::Biblioteca
--      hClose handle
--      putStrLn("Digite O Nome:")
--      nome <-getLine
--      putStrLn("Digite  O Ano:")
--      ano <- getLine
--      if ((head lista) == )
--      return (lista)




gravarLista biblioteca = do
     writeFile "Teste1.txt" (show biblioteca)



adicionarLivro livro = do --chamar a funcao com parenteses nos argumentos
     biblioteca <- converteLista -- lv vai ser o resto da lista
     gravarLista (livro:biblioteca) 



desencapular = do
    estante <- converteLista
    return(estante)



-- converteEmLib :: Biblioteca
-- converteEmLib = do -- ta pa-ssando para IO Biblioteca
--     handle <- openFile "Teste1.txt" ReadMode
--     conteudo <- hGetLine handle
--     hClose handle
--     let desespero = (read (show conteudo))::Biblioteca
--     return (desespero)



remover ::(Eq a)=> [a] -> a -> [a]
remover [] l = []
remover (h:t) l | l == h = t
                | otherwise = h:(remover t l)


insere :: Livro -> Biblioteca -> Biblioteca
insere livro [] = []
insere livro lib = livro:lib


buscar :: Biblioteca -> String -> Biblioteca -- Devolve apenas livros 
buscar [] l = []                             -- com o titulo passado
buscar  ((Livro a b):t) str  | a == str = (Livro a b):(buscar t str)
                             | otherwise = buscar t str




boubble :: Biblioteca -> Biblioteca -- Apenas para Titulo
boubble [] = []
boubble (h:[]) = [h]
boubble ((Livro a b):(Livro c d):t) | ord(head a) > ord(head c) = (Livro c d):boubble ((Livro a b):t)
                | otherwise = (Livro a b):boubble ((Livro c d):t)

loop :: Int -> Biblioteca -> Biblioteca
loop 0 l = l
loop n l = loop (n-1) (boubble(l))

--instance Show Livro where
  --  show (Livro a b) = "Titulo: " ++ a ++ " ,Ano: " ++ b