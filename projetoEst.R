# Maria Augusta(mamb2)
# Igor Simões (ibps)
# Tiago Sousa (tsc2)
library(stringr)

got = read.csv("PlanilhaGOT.csv", header = TRUE)
#print (got)

dimensao = dim(got)
#print(dimensao)
linhas = dimensao[1][1] # pegando quantidade de linhas
#print(linhas)
colunas = got[0,] # pegando quantidade de colunas
#print(colunas)
#print(length(colunas))

# questao 1
media = mean(got[,3])
print(media)

dp = sd(got[,3])
print(dp)

freq = table(got$Nota)
moda = names(table(got$Nota))[table(got$Nota) == max(table(got$Nota))]
print(moda)

# questao 2

media2 = mean(got[,5])
print(media2)

dp2 = sd(got[,5])
print(dp2)

freq2 = table(got$Audiencia.Em.milhoes)
moda2 = names(table(got$Audiencia.Em.milhoes))[table(got$Audiencia.Em.milhoes) == max(table(got$Audiencia.Em.milhoes))]
print(moda2)

# questao 4


nomeEp <- function(got,linhas){
  eps = c()
    
  for (i in 1:linhas){
    if(got[i,3] >= 9){
      eps=c(eps, as.character(got[i,2]))
    }
  }
  return (eps)
}

nomeEps = nomeEp(got, linhas)
print (nomeEps)

#questao 5

indexMM <- function(got,linhas){
  maior = 0
  indexMaior = 0
  menor = 10
  indexMenor = 0
  tempAtual = 1
  eps = c()
  
  for (i in 1:linhas){
    if(got[i,1] != tempAtual) {
      eps = c(eps, indexMenor, indexMaior)
      maior = 0
      indexMaior = 0
      menor = 10
      indexMenor = 0
      tempAtual = tempAtual + 1
    }
    if(got[i,1] == tempAtual){
      if(got[i,3] > maior) {
        indexMaior = i
        maior = got[i,3]
      }
      if(got[i,3] < menor) {
        indexMenor = i
        menor = got[i,3]
      }
    }
  }
  eps = c(eps, indexMenor, indexMaior)
  return (eps)
}
indexesMM = indexMM(got,linhas)

nomeMM <- function(got, abc, coluna) {
  eps = c()
  for(i in abc) {
    eps = c(eps, as.character(got[i,coluna]))
  }
  return (eps)
}


nomesMM = nomeMM(got,indexesMM, 2)
matrixA = data.frame(Nota = nomeMM(got,indexesMM,3),
                     Titulo = nomeMM(got,indexesMM,2),
                     Temporada = nomeMM(got,indexesMM,1))

#questao 6


DPTemps <- function(got,linhas){
  tempAtual = 1
  eps = c()
  publicos = c()
  
  for (i in 1:linhas){
    if(got[i,1] != tempAtual) {
      eps = c(eps, sd(publicos))
      publicos = c()
      tempAtual = tempAtual + 1
    }
    if(i != linhas) {
      publicos = c(publicos, got[i,5])
    }
  }
  eps = c(eps, sd(publicos))
  minimum = min(eps)
  index = 0
  for(i in 1:length(eps)) {
    if(eps[i] == minimum) {
      index = i
    }
  }
  return (index)
}

menorDPTemp = DPTemps(got,linhas)

#questao 7
brienne <- function(got,linhas) {
  notas = c()
  for(i in 1:linhas) {
    personagens = as.character(got[i,4])
    if(grepl("Brienne",personagens)) {
      notas = c(notas, got[i,3])
    }
    
  }
  return(mean(notas))
}
AKK = brienne(got,linhas)

#questao 8
temp4app <- function(got,linhas) {
  characters = c()
  for(i in 1:linhas) {
    if(got[i,1] == 4) {
      amor = strsplit(as.character(got[i,4]), ",")
      characters = c(characters, unlist(amor))
    } else if(got[i,1] == 5) {
      frequencia = table(characters)
      minApp = list(names(frequencia)[frequencia == min(frequencia)])
      return (minApp)
    }
  }
}
Resposta = temp4app(got,linhas)


#questao 9
questao9 <- function(got,linhas, personagem) {
  freq = c()
  tempAtual = 1
  for(i in 1:linhas) {
    personagens = as.character(got[i,4])
    if(tempAtual != got[i,1]) {
      tempAtual = tempAtual + 1
    }
    if(grepl(personagem,personagens)) {
      freq = c(freq, tempAtual)
      #freqTemp = freqTemp + 1
    }
    
  }
  #freq = c(freq, freqTemp)
  return (freq)
}
fre = questao9(got, linhas, "Bran Stark")
histChar = hist(questao9(got,linhas,"Bran Stark"), breaks = c(0,1,2,3,4,5,6,7,8), xlim = c(0, 8), ylim = c(0, 10), col = "lightblue")

