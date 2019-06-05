# Maria Augusta(mamb2)
# Igor Simões (ibps)
# Tiago Sousa (tsc2)


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

