import System.IO
import Data.Char
import System.Directory
import System.Exit
data Livro = Livro String String String deriving (Show,Read,Eq,Ord)
type Biblioteca = [Livro]

biblioteca1 = [Livro "UmaNoiteEstudando" "2010" "Biografia",Livro "LinguagemC" "1990" "Computacao",Livro "AsCronicasMarcianas" "1950" "Sci-Fi",Livro "Steinbush" "1998" "Gemometria"]
--biblioteca para testes
-- Lembrar de Criar Um txt e alterar o nome nos metodos do menu



-----------------------------------------------Main------------------------------------------------------
menu = do --- Parte Estetica
     apagarTela
     putStrLn("\n\t  Bem Vindo A Biblioteca Virtual!!!")
     putStrLn("\n\t  Digite a Opcao Desejada:")
     putStrLn("          _____________________")
     putStrLn("         | 1 - Adicionar Livro:|")
     putStrLn("         | 2 - Remover Livro:  |")
     putStrLn("         | 3 - Buscar Livro:   |")
     putStrLn("         | 4 - Ordenar Livros  |")
     putStrLn("         | x - Sair            |")
     putStrLn("         |_____________________|")
     putStr("--: ")
     x <- getLine
     chamaFuncoes (x)
     putStrLn("Pressione Qualquer Tecla Para Continuar....")
     y <- getLine
     putStrLn("")
     apagarTela
     menu



chamaFuncoes :: String -> IO ()
chamaFuncoes x | x == "1" = adicionarLivro 
               | x == "2" = removerArquivo
               | x == "3" = opcoesBuscar
               | x == "4" = opcoesOrdenar
               | otherwise = exitSuccess


----------------------------------------------------FUNCOES------------------------------------------------

printarLista :: Biblioteca -> IO () -- retorna a lista com \n
printarLista (h:t) = do             -- Lembrar de tratar o caso do arquivo criado mas vazio
     putStrLn ("\t   "++show h)
     if (t == []) then do
     putStr("\n")
     else
        printarLista (t)


apagarTela = do
     putStr ("\ESC[2J")


remover :: Biblioteca -> Livro -> Biblioteca
remover [] l = []
remover (h:t) l | l == h = t
                | otherwise = h:(remover t l)


buscar :: Biblioteca -> String -> Biblioteca -- Devolve apenas livros 
buscar [] l = []                             -- com o titulo passado
buscar  ((Livro a b c):t) str  | a == str = (Livro a b c):(buscar t str)
                               | otherwise = buscar t str


buscar2 :: Biblioteca -> String -> Biblioteca -- Devolve apenas livros 
buscar2 [] l = []                             -- com o Ano passado
buscar2  ((Livro a b c):t) str  | b == str = (Livro a b c):(buscar2 t str)
                                | otherwise = buscar2 t str


buscar3 :: Biblioteca -> String -> Biblioteca -- Devolve apenas livros 
buscar3 [] l = []                             -- com o Genero passado
buscar3  ((Livro a b c):t) str  | c == str = (Livro a b c):(buscar3 t str)
                                | otherwise = buscar3 t str

-----------------------------------------------------ORDENAR-----------------------------------------------------

-----------------------------------------------------Titulo-----------------------------------------------------

trocaT :: Biblioteca -> Biblioteca -- Apenas para Titulo
trocaT [] = []
trocaT (h:[]) = [h]
trocaT ((Livro a b c):(Livro d e f):t) | ord(head a) > ord(head d) = (Livro d e f):trocaT ((Livro a b c):t)
                | otherwise = (Livro a b c):trocaT ((Livro d e f):t)


boubbleT :: Int -> Biblioteca -> Biblioteca -- Passar length como parametro
boubbleT 0 l = l
boubbleT n l = boubbleT (n-1) (trocaT(l))

-----------------------------------------------------Ano-----------------------------------------------------

trocaA :: Biblioteca -> Biblioteca -- Apenas para Titulo
trocaA [] = []
trocaA (h:[]) = [h]
trocaA ((Livro a b c):(Livro d e f):t) | ord(head b) > ord(head e) = (Livro d e f):trocaA ((Livro a b c):t)
                | otherwise = (Livro a b c):trocaA ((Livro d e f):t)


boubbleA :: Int -> Biblioteca -> Biblioteca -- Passar length como parametro
boubbleA 0 l = l
boubbleA n l = boubbleA (n-1) (trocaA(l))

-----------------------------------------------------Genero-----------------------------------------------------
trocaG :: Biblioteca -> Biblioteca -- Apenas para Titulo
trocaG [] = []
trocaG (h:[]) = [h]
trocaG ((Livro a b c):(Livro d e f):t) | ord(head b) > ord(head e) = (Livro d e f):trocaG ((Livro a b c):t)
                | otherwise = (Livro a b c):trocaG ((Livro d e f):t)


boubbleG :: Int -> Biblioteca -> Biblioteca -- Passar length como parametro
boubbleG 0 l = l
boubbleG n l = boubbleG (n-1) (trocaG(l))

-----------------------------------------------------ARQUIVOS---------------------------------------------------

-----------------------------------------------------BUSCAR-----------------------------------------------------
opcoesBuscar = do
     bool <- doesFileExist "BancoDeLivros.txt" --- Manda Print caso não exista
     if(bool) then do        
     putStrLn("\t Voce Deseja Buscar Por:\n")
     putStrLn("\t 1-Titulo")
     putStrLn("\t 2-Ano")
     putStrLn("\t 3-Genero")
     putStr("--:")
     x <- getLine
     if(x == "1") then do
        buscarArquivoTitulo
        else if(x == "2") then do
            buscarArquivoAno
            else if (x == "3") then do
                 buscarArquivoGenero
                    else do
                    putStrLn("Opcao Nao Existente..")
                    y <- getLine
                    menu
     else
        putStrLn("\t  Arquivo Nao Existe!!\n\t  Adicione Um Livro Antes de Buscar\n")
-----------------------------------------------------Titulo-----------------------------------------------------

buscarArquivoTitulo = do -- por titulo
   putStr("Digite o Titulo do Livro que Deseja Buscar: ")
   titulo <- getLine
   putStrLn("")
   lista <- converteLista
   let livrosTitulo = buscar lista titulo
   putStrLn("Livros Encontrados: \n")
   if((buscar lista titulo)== []) then do
   putStrLn("[]"++"\n")
    else
       printarLista livrosTitulo

-----------------------------------------------------Genero-----------------------------------------------------

buscarArquivoGenero = do
    putStr("Digite o Genero Que Procura:")
    genero <- getLine
    putStrLn("")
    lista <- converteLista
    let livrosTitulo = buscar3 lista genero
    putStrLn("Livros Encontrados: \n")
    if((buscar3 lista genero)== []) then do
    putStrLn("[]"++"\n")
    else
       printarLista livrosTitulo

-----------------------------------------------------Ano-----------------------------------------------------

buscarArquivoAno = do
    putStr("Digite o Ano Que Procura:")
    ano <- getLine
    putStrLn("")
    lista <- converteLista
    let livrosTitulo = buscar2 lista ano
    putStrLn("Livros Encontrados: \n")
    if((buscar2 lista ano)== []) then do
    putStrLn("[]"++"\n")
    else
       printarLista livrosTitulo
-----------------------------------------------------Removendo da Lista--------------------------------------

removerArquivo = do
   bool <- doesFileExist "BancoDeLivros.txt"
   if(bool) then do
       putStr("Digite o Titulo do Livro que Deseja Remover: ")
       titulo <- getLine
       putStr("Digite o Ano do Livro que Deseja Remover: ")
       ano <- getLine
       putStr("Digite o Genero do Livro que Deseja Remover: ")
       genero <- getLine
       putStrLn("")
       lista <- converteLista
       let listaAtualizada = remover lista (Livro titulo ano genero )
       gravarLista listaAtualizada
       printarLista(listaAtualizada)
    else
        putStrLn("\t  Arquivo Nao Existe!!\n\t  Adicione Um Livro Antes de Remover\n")
 
-----------------------------------------------------ORDENAR---------------------------------------------------
opcoesOrdenar = do -------- Vai ordenar de acordo com a opcao selecionada
    bool <- doesFileExist "BancoDeLivros.txt"
    if(bool) then do
        putStrLn("\t Voce Deseja Ordenar Por:")
        putStrLn("\t 1 - Titulo")
        putStrLn("\t 2 - Ano:")
        putStrLn("\t 3 - Genero")
        putStr("--: ")
        x <- getLine
        if(x == "1") then do
        ordenarArquivoTitulo
             else if(x == "2") then do
             ordenarArquivoAno
                 else if(x == "3") then do
                 ordenarArquivoGenero
                     else do
                     putStrLn("Opcao Nao Existente...")
                     y<-getLine
                     menu
    else
        putStrLn("\t  Arquivo Nao Existe!!\n\t  Adicione Um Livro Antes de Ordenar\n")                 


ordenarArquivoTitulo = do -- pelo Titulo
    lista <- converteLista
    let listaOrdenada = boubbleT (length lista) lista
    gravarLista listaOrdenada
    putStrLn("\t   Livros Ordenados:\n")
    printarLista listaOrdenada



ordenarArquivoAno = do -- pelo Ano
    lista <- converteLista
    let listaOrdenada = boubbleA (length lista) lista
    gravarLista listaOrdenada
    putStrLn("\t   Livros Ordenados:\n")
    printarLista listaOrdenada



ordenarArquivoGenero = do -- pelo Genero
    lista <- converteLista
    let listaOrdenada = boubbleG (length lista) lista
    gravarLista listaOrdenada
    putStrLn("\t   Livros Ordenados:\n")
    printarLista listaOrdenada

----------------------------------------------------------

converteLista = do -- 
     handle <- openFile "BancoDeLivros.txt" ReadMode
     conteudo <- hGetLine handle -- se estiver vazio da uma exeption
     let lista = (read conteudo)::Biblioteca-- pega o conteudo do txt e converte 
     hClose handle
     return lista



gravarLista biblioteca = do
     writeFile "BancoDeLivros.txt" (show biblioteca)
     putStrLn("\t  Livro Gravado Com Sucesso") --solucao temporaria até arrumar o gravar livro para quando nao tiver arquivo



adicionarLivro = do
    bool <- doesFileExist "BancoDeLivros.txt"
    putStr("Digite o Titulo do Livro que Deseja Adiconar: ")
    titulo <- getLine
    putStr("Digite o Ano do Livro que Deseja Adiconar: ")
    ano <- getLine
    putStr("Digite o Genero do Livro que Deseja Adiconar: ")
    genero <- getLine
    putStrLn("\n\t   Livros Disponiveis:\n")
    putStrln(show(ano))
    if(bool) then do
        lib <- converteLista  -- pega os outos itens já adicionados e junta com o novo livro
        gravarLista ((Livro titulo ano genero):lib)
        printarLista((Livro titulo ano genero):lib)
    else
        gravarLista ([Livro titulo ano genero]) -- Adicionar um print aqui mostrando o livro adicionado(dificil?)