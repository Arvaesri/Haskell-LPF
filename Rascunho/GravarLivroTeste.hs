import System.IO


data Livro = Livro String String  deriving (Show,Read,Eq)
type Biblioteca = [Livro]
-- ex [Livro "Titulo:..." "Ano:... ",.....]
listavazia = []
biblioteca1 = [Livro "Guidorise" "1993",Livro "steinbush" "1998"]

main = do
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


opcao1 = do 
     putStr("Digite o Titulo: ")
     titulo <- getLine
     putStr("Digite o Ano: ")
     ano <- getLine
     adicionarLivro (Livro ("Titulo: "++titulo) ("Ano: "++ano))


lerLista = do
     handle <- openFile "Teste1.txt" ReadMode
     --lembrar de escrever -> se for vazio criar uma lista 
     conteudo <- hGetLine handle -- se estiver vazio da uma exeption
     let lista = (read conteudo)::Biblioteca -- pega o conteudo do txt e converte 
     hClose handle
     return lista -- devolve a lista que foi convertida do txt para Biblioteca

adicionarLivro l = do --chamar a funcao com parenteses nos argumentos
     lv <- lerLista -- lv vai ser o resto da lista
     gravarLista (l:lv) -- a lista vai ser gravada na frente

-- p/ remover tem que ter um indice na lista, (função index?)

gravarLista lv = do -- grava qualquer coisa no txt
    handle <- openFile "Teste1.txt" WriteMode
    hPutStr handle (show lv)
    hClose handle






remover :: Biblioteca -> Livro -> Biblioteca
remover [] l = []
remover (h:t) l | l == h = t
                | otherwise = remover t l 


removerArquivo = do
	lista <- converteLista
	listaAtualizada <- remover lista (Livro "a" "b")
	gravarLista listaAtualizada


add = do
     putStr("Nome:  ")
     x <- getLine
     putStr("Ano:  ")
     y <- getLine
     let a = Livro x y
     appendFile "Teste1.txt" ("\nNome:  " ++ x ++"  Ano:  "++y)


gravar = do
     putStr("digite a informacao que deseja gravar:  ")
     informacao <- getLine
     writeFile "Teste1.txt" informacao
     

ler = do
    conteudo <- readFile "Teste1.txt"
    putStrLn ("  \n    A Biblioteca contem : \n" ++ conteudo)
    

adicionar = do
     putStr("Digite a informacao que deseja adicionar:  ")
     informacao <- getLine
     appendFile "Teste1.txt" informacao
     
