# Maria Augusta Mota Borba(mamb2)
# Igor Beltrão Pereira Simões (ibps)
# Tiago Sousa Carvalho (tsc2)
  
got = read.csv("PlanilhaGOT.csv", header = TRUE) # lê a planilha e coloca em uma matriz

dimensao = dim(got)
linhas = dimensao[1][1] # pegando quantidade de linhas
colunas = got[0,] # pegando quantidade de colunas
# questao 1
print("----- 1ª Questão -----")
print(data.frame(got)) # imprime o data frame

# questao 2
print("----- 2ª Questão -----")
media = mean(got[,3])
print(paste("Média:",media))

dp = sd(got[,3])
print(paste("Desvio padrão:",dp))

freq = table(got$Nota)
moda = names(table(got$Nota))[table(got$Nota) == max(table(got$Nota))]
print(paste("Moda:",moda))

# questao 3
print("----- 3ª Questão -----")

media2 = mean(got[,5])
print(paste("Média:",media))

dp2 = sd(got[,5])
print(paste("Desvio padrão:",dp2))

mediana = median(got[,5])
print(paste("Mediana:",mediana))

# questao 4
print("----- 4ª Questão -----")

nomeEp <- function(got,linhas){ # retorna os nomes dos episódios que tem a média maior ou igual a 9
  nomeEpisodios = c()
    
  for (i in 1:linhas){ # percorre todas as linhas da coluna
    if(got[i,3] >= 9){ # se a nota for >= 9, entra no if
      nomeEpisodios=c(nomeEpisodios, as.character(got[i,2])) # adiciona no vector, convertendo de factor para string
    }
  }
  return (nomeEpisodios) # retorna o vector
}

nomeEps = nomeEp(got, linhas) 
print("Episódios com notas acima de 9:")
print (nomeEps)

#questao 5
print("----- 5ª Questão -----")

indexMM <- function(got,linhas){ # funcao que retorna indexes da menor e maior nota da temporada
  maior = 0
  indexMaior = 0
  menor = 10
  indexMenor = 0
  tempAtual = 1
  indexEpisodios = c() # vector que irá possuir os indexes dos episódios de menor e maior nota
  
  for (i in 1:linhas){ # percorre toda a linha
    if(got[i,1] != tempAtual) { # quando o episódio atual é o inicio de uma temporada, adiciona no vector os episodios de menor e maior nota, respectivamente
      indexEpisodios = c(indexEpisodios, indexMenor, indexMaior)
      maior = 0 # reseta as variáveis para a próx temporada
      indexMaior = 0
      menor = 10
      indexMenor = 0
      tempAtual = tempAtual + 1 
    }
      if(got[i,3] > maior) { #entra nesse if se for o maior episódio até agr da temporada
        indexMaior = i
        maior = got[i,3]
      } else if (got[i,3] == maior) { #entra aqui se for igual ao maior episódio
        indexMaior = c(indexMaior, i)
      }
      if(got[i,3] <= menor) { # ifs iguais aos anteriores para os menores da temporada
        indexMenor = i
        menor = got[i,3]
      } else if (got[i,3] == menor) {
        indexMenor = c(indexMenor, i)
      }
  }
  indexEpisodios = c(indexEpisodios, indexMenor, indexMaior) # adiciona os episódios da ultima temporada no vector
  return (indexEpisodios)
}
indexesMM = indexMM(got,linhas)

dadosMM <- function(got, indexes, coluna) { #função que retorna informações relacionados a coluna que foi passada pelo parametro
  dados = c()
  for(i in indexes) { #para cada indice, pega a caracteristica da coluna e põe no vector
    dados = c(dados, as.character(got[i,coluna]))
  }
  return (dados)
}

nomesMM = dadosMM(got,indexesMM, 2)
dataframeMM = data.frame(Titulo = nomesMM,
                         Nota = dadosMM(got,indexesMM,3),
                         Temporada = dadosMM(got,indexesMM,1))
print("Nomes dos episódios com menor e maior nota:")
print(nomesMM)
print("Dataframe da 5ª questão:")
print(dataframeMM)

#questao 6
print("----- 6ª Questão -----")

menorDPTemp <- function(got,linhas){ # retorna qual a temporada com o menor desvio padrão na audiencia
  tempAtual = 1
  DPs = c() # vector que armazena o desvio padrão de cada temporada
  audiencias = c() # vector que armazena a audiencia de cada episodio para calcula o desvio padrao da temporada
  
  for (i in 1:linhas){ # percorre todos os episódios
    if(got[i,1] != tempAtual) { # quando é o primeiro episódio de cada temporada, ele adiciona ao vector o desvio padrão dessa temporada
      DPs = c(DPs, sd(audiencias))
      audiencias = c() # reseta o array para a nova temporada
      tempAtual = tempAtual + 1
    }
      audiencias = c(audiencias, got[i,5]) # adiciona ao vector a audiencia do episodio de index i
  }
  DPs = c(DPs, sd(audiencias)) # adiciona ao DPs o desvio padrão da ultima temporada
  minimum = min(DPs) # pega o menor desvio padrão
  for(i in 1:length(DPs)) {
    if(DPs[i] == minimum) { # entra no if quando o i é o minimo, e retorna ele
      return(i)
    }
  }
}

menorDP = menorDPTemp(got,linhas)
print(paste("Temporada com menor desvio padrão:",menorDP))

#questao 7
print("----- 7ª Questão -----")

brienne <- function(got,linhas) {
  notas = c()
  for(i in 1:linhas) {
    personagens = as.character(got[i,4]) # transforma em character o campo de personagens do episodio de index i
    if(grepl("Brienne",personagens)) { # se encontrar Brienne no episódio, entra no if
      notas = c(notas, got[i,3]) # adiciona a um vector a nota do episódio
    }
    
  }
  return(mean(notas)) # retorna a média do vector de notas
}
mediaBrienne = brienne(got,linhas)
print(paste("Média:",mediaBrienne))

#questao 8
print("----- 8ª Questão -----")

temp4app <- function(got,linhas) {
  characters = c()
  for(i in 1:linhas) { # percorre todos os episódios
    if(got[i,1] == 4) { # só entra no if se for da temporada 4
      listCharacters = strsplit(as.character(got[i,4]), ",")
      characters = c(characters, unlist(listCharacters)) # une todos os personagens em um único vector
    } else if(got[i,1] == 5) { # quando acaba os episódios da 4 temporada, entra aqui
      frequencia = table(characters) # faz uma planilha com os itens no vector, todo personagem que já apareceu antes terá um index que será o seu numero de aparições
      minApp = list(names(frequencia)[frequencia == min(frequencia)]) # cria uma lista com os personagens que apareceram apenas uma vez
      return (minApp) # retorna a lista
    }
  }
}
Resposta = temp4app(got,linhas)
print("Personagens que apareceram apenas em um único episódio durante a 4 temporada")
print(Resposta)


#questao 9
print("----- 9ª Questão -----")

questao9 <- function(got,linhas, personagem) { # função que gera um histograma dado o nome de um personagem
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

personagem <-readline(prompt = "Digite o nome do personagem para qual o histograma será gerado")
questao9(got, linhas, personagem)

