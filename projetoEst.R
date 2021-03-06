# Maria Augusta Mota Borba(mamb2)
# Igor Beltr�o Pereira Sim�es (ibps)
# Tiago Sousa Carvalho (tsc2)
  
got = read.csv("PlanilhaGOT.csv", header = TRUE) # l� a planilha e coloca em uma matriz

dimensao = dim(got)
linhas = dimensao[1][1] # pegando quantidade de linhas
colunas = got[0,] # pegando quantidade de colunas
# questao 1
print("----- 1� Quest�o -----")
print(data.frame(got)) # imprime o data frame

# questao 2
print("----- 2� Quest�o -----")
media = mean(got[,3])
print(paste("M�dia:",media))

dp = sd(got[,3])
print(paste("Desvio padr�o:",dp))

freq = table(got$Nota)
moda = names(table(got$Nota))[table(got$Nota) == max(table(got$Nota))]
print(paste("Moda:",moda))

# questao 3
print("----- 3� Quest�o -----")

media2 = mean(got[,5])
print(paste("M�dia:",media))

dp2 = sd(got[,5])
print(paste("Desvio padr�o:",dp2))

mediana = median(got[,5])
print(paste("Mediana:",mediana))

# questao 4
print("----- 4� Quest�o -----")

nomeEp <- function(got,linhas){ # retorna os nomes dos epis�dios que tem a m�dia maior ou igual a 9
  nomeEpisodios = c()
    
  for (i in 1:linhas){ # percorre todas as linhas da coluna
    if(got[i,3] >= 9){ # se a nota for >= 9, entra no if
      nomeEpisodios=c(nomeEpisodios, as.character(got[i,2])) # adiciona no vector, convertendo de factor para string
    }
  }
  return (nomeEpisodios) # retorna o vector
}

nomeEps = nomeEp(got, linhas) 
print("Epis�dios com notas acima de 9:")
print (nomeEps)

#questao 5
print("----- 5� Quest�o -----")

indexMM <- function(got,linhas){ # funcao que retorna indexes da menor e maior nota da temporada
  maior = 0
  indexMaior = 0
  menor = 10
  indexMenor = 0
  tempAtual = 1
  indexEpisodios = c() # vector que ir� possuir os indexes dos epis�dios de menor e maior nota
  
  for (i in 1:linhas){ # percorre toda a linha
    if(got[i,1] != tempAtual) { # quando o epis�dio atual � o inicio de uma temporada, adiciona no vector os episodios de menor e maior nota, respectivamente
      indexEpisodios = c(indexEpisodios, indexMenor, indexMaior)
      maior = 0 # reseta as vari�veis para a pr�x temporada
      indexMaior = 0
      menor = 10
      indexMenor = 0
      tempAtual = tempAtual + 1 
    }
      if(got[i,3] > maior) { #entra nesse if se for o maior epis�dio at� agr da temporada
        indexMaior = i
        maior = got[i,3]
      } else if (got[i,3] == maior) { #entra aqui se for igual ao maior epis�dio
        indexMaior = c(indexMaior, i)
      }
      if(got[i,3] <= menor) { # ifs iguais aos anteriores para os menores da temporada
        indexMenor = i
        menor = got[i,3]
      } else if (got[i,3] == menor) {
        indexMenor = c(indexMenor, i)
      }
  }
  indexEpisodios = c(indexEpisodios, indexMenor, indexMaior) # adiciona os epis�dios da ultima temporada no vector
  return (indexEpisodios)
}
indexesMM = indexMM(got,linhas)

dadosMM <- function(got, indexes, coluna) { #fun��o que retorna informa��es relacionados a coluna que foi passada pelo parametro
  dados = c()
  for(i in indexes) { #para cada indice, pega a caracteristica da coluna e p�e no vector
    dados = c(dados, as.character(got[i,coluna]))
  }
  return (dados)
}

nomesMM = dadosMM(got,indexesMM, 2)
dataframeMM = data.frame(Titulo = nomesMM,
                         Nota = dadosMM(got,indexesMM,3),
                         Temporada = dadosMM(got,indexesMM,1))
print("Nomes dos epis�dios com menor e maior nota:")
print(nomesMM)
print("Dataframe da 5� quest�o:")
print(dataframeMM)

#questao 6
print("----- 6� Quest�o -----")

menorDPTemp <- function(got,linhas){ # retorna qual a temporada com o menor desvio padr�o na audiencia
  tempAtual = 1
  DPs = c() # vector que armazena o desvio padr�o de cada temporada
  audiencias = c() # vector que armazena a audiencia de cada episodio para calcula o desvio padrao da temporada
  
  for (i in 1:linhas){ # percorre todos os epis�dios
    if(got[i,1] != tempAtual) { # quando � o primeiro epis�dio de cada temporada, ele adiciona ao vector o desvio padr�o dessa temporada
      DPs = c(DPs, sd(audiencias))
      audiencias = c() # reseta o array para a nova temporada
      tempAtual = tempAtual + 1
    }
      audiencias = c(audiencias, got[i,5]) # adiciona ao vector a audiencia do episodio de index i
  }
  DPs = c(DPs, sd(audiencias)) # adiciona ao DPs o desvio padr�o da ultima temporada
  minimum = min(DPs) # pega o menor desvio padr�o
  for(i in 1:length(DPs)) {
    if(DPs[i] == minimum) { # entra no if quando o i � o minimo, e retorna ele
      return(i)
    }
  }
}

menorDP = menorDPTemp(got,linhas)
print(paste("Temporada com menor desvio padr�o:",menorDP))

#questao 7
print("----- 7� Quest�o -----")

brienne <- function(got,linhas) {
  notas = c()
  for(i in 1:linhas) {
    personagens = as.character(got[i,4]) # transforma em character o campo de personagens do episodio de index i
    if(grepl("Brienne",personagens)) { # se encontrar Brienne no epis�dio, entra no if
      notas = c(notas, got[i,3]) # adiciona a um vector a nota do epis�dio
    }
    
  }
  return(mean(notas)) # retorna a m�dia do vector de notas
}
mediaBrienne = brienne(got,linhas)
print(paste("M�dia:",mediaBrienne))

#questao 8
print("----- 8� Quest�o -----")

temp4app <- function(got,linhas) {
  characters = c()
  for(i in 1:linhas) { # percorre todos os epis�dios
    if(got[i,1] == 4) { # s� entra no if se for da temporada 4
      listCharacters = strsplit(as.character(got[i,4]), ",")
      characters = c(characters, unlist(listCharacters)) # une todos os personagens em um �nico vector
    } else if(got[i,1] == 5) { # quando acaba os epis�dios da 4 temporada, entra aqui
      frequencia = table(characters) # faz uma planilha com os itens no vector, todo personagem que j� apareceu antes ter� um index que ser� o seu numero de apari��es
      minApp = list(names(frequencia)[frequencia == min(frequencia)]) # cria uma lista com os personagens que apareceram apenas uma vez
      return (minApp) # retorna a lista
    }
  }
}
Resposta = temp4app(got,linhas)
print("Personagens que apareceram apenas em um �nico epis�dio durante a 4 temporada")
print(Resposta)


#questao 9
print("----- 9� Quest�o -----")

questao9 <- function(got,linhas, personagem) { # fun��o que gera um histograma dado o nome de um personagem
  freq = c() # vector que guarda quantas vezes o personagem apareceu em cada temporada
  tempAtual = 1
  for(i in 1:linhas) {
    personagens = as.character(got[i,4])
    if(tempAtual != got[i,1]) {
      tempAtual = tempAtual + 1
    }
    if(grepl(personagem,personagens)) {
      freq = c(freq, tempAtual) # adiciona o index da temporada atual no
    }
    
  }
  hist(freq, main = personagem, xlab = "Temporada", ylab="Ocorrencia", breaks = c(0,1,2,3,4,5,6,7,8), xlim = c(0,8), ylim = c(0, 10), col = "lightblue", border="white")
}

personagem <-readline(prompt = "Digite o nome do personagem para qual o histograma ser� gerado")
questao9(got, linhas, personagem)

