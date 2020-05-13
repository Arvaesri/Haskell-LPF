soma :: Int -> Int
soma 0 = 0
soma a | mod a 10 == 0 = mod a 10 + (soma (mod a 10))
       | otherwise = soma (mod a 10) -- corrigir

---------------------------Qts 2----------------------------------

intervalo :: Int -> Int -> [Int] -> [Int]
intervalo n m [] = []
intervalo n m l = (final ((m-n)+1) (inicial n l)) -- chamando o metodo passando inicial como argumento que vai retornar uma lista
                                                  -- o metodo vai pegar uma lista com os valores iniciais que não são do indice removidos e passar como parametro de final
inicial :: Int -> [Int] -> [Int]
inicial 0 l = l
inicial n (h:t) = inicial (n-1) t  -- chamando de n a 0 áté retirar o inicio da lista

final :: Int -> [Int] -> [Int]
final 0 l = []
final variacao [] = []
final variacao (h:t) = h:final (variacao-1) t  --final vai ser a variacao de tamanho que é = (m-n)+1     
                                               -- final vai receber uma lista e juntar os head até a variacao ser 0 e vai descartar o final da lista 

----------------------------Qst 3---------------------------------

quaseDecrescente :: [Int] -> Bool
quaseDecrescente [] = False
quaseDecrescente l | contaOcorrencia l == 1 = True
                   | otherwise = False

contaOcorrencia :: [Int] -> Int
contaOcorrencia [a] = 0
contaOcorrencia (h:x:t) | h>x = contaOcorrencia (x:t)
                        | otherwise = (contaOcorrencia (x:t))+1