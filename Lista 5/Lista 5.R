# Lista de Processos Estocáticos 

# Bibliotecas
library(ggplot2)

# Exercício 1

# Exercício 2

c <- 0 # Perder tudo
d <- 50 # Quanto se quer ganhar
p <- 18/38 # probabilidade de vencer a rodada
q <- 20/38 # 1-p
x0 <- c(10,20,30,40) # Vetor de valores iniciais
Nsim <-1000 # 1000 Numero de simulações

# item a

teo <- function(x,d,q,p,c){ # Função que retorna resultados teóricos
  prob <- (1-(q/p)^(x-c))/(1-(q/p)^(d-c))
  return(prob)
}
Sn <- numeric()
simula <- function(x1,p,q){
    x <- sample(x=c(1,-1),replace = T,size = 10000,prob = c(q,p))
    x0 <- c(x1,x)
    Sn <- cumsum(x0)
    for (i in 1:length(Sn)){
      b <- Sn[i]
      if (b<=c){return((Sn[1:i]))} 
      if (b>=d) {return((Sn[1:i]))}
    }
  return(Sn)
}
# Resultado simulado
simula(20,p,q)
sds <- numeric(0)
for (i in 1:1000){
  sds[i] <- length(simula(x0[1],q,p))/10000
}
mean(sds)

# Resultado teórico

for (i in 1:4) {
  print(teo(x0[i],d,q,p,c))
}

# item b
p <- 1/2

teo1 <- function(x,c,d){ # Função que retorna resultados teóricos
  prob <- (x-c)/(d-c)
  return(prob)
}
s <- numeric()
for (i in 1 :1000){
  s[i] <- length(simula(x0[2],1/2,1/2))/1000
}

# Resultado teórico

for (i in 1:4) {
  print(teo1(x0[i],d,c))
}

# Exercício 3

p0 <- 1/2 # Probabilidade de ter 0 filho
p1 <- 1/4 # Probabilidade de ter 1 filho
p2 <- 1/4 # Probabilidade de ter 2 filhos

# item a

x0 <- 1 # número de indivíduos no início (geração 0)
n <- 1 # número de indivíduos no início (geração 0)
Nsim <- 100 # número de Simulações
pop.size <- numeric(0) # Vetor da população média
sequ <- numeric(0)
filhos <- function(x0,n){ # função que calcula o número de filhos
  for(i in 1:n){
    sequ[i] <- runif(1)
    if(sequ[i]<p0){
      x0 <- c(x0,0)
    }
    if(sequ[i]>=p0 && sequ[i]<(p0+p1)){
      x0 <- c(x0,1)
      x1 <- filhos(1,1)
      x0 <- c(x0,x1)
    }
    if(sequ[i]>=(p0+p2)){
      x0 <- c(x0,2)
      x1 <- filhos(2,1)
      x0 <- c(x0,x1)
    }
  }
  return(x0)
}

for(i in 1:Nsim){ # Resultado simulado
  pop.size[i] <- sum(filhos(x0,n))
}

mean(pop.size) # média da poupulação simualda
(1/(1-(p1+2*p2))) # média da poupulação teoŕica

# item b
x0 <- 2 # número de indivíduos no início (geração 0)
n <- 2 # número de indivíduos no início (geração 0)
p0 <- 1/4 # Probabilidade de ter 0 filho
p1 <- 1/4 # Probabilidade de ter 1 filho
p2 <- 1/2 # Probabilidade de ter 2 filhos

pop.size <- vector() # Vetor da população média

filhos1 <- function(x0,n){ # função que calcula o número de filhos
  for(i in 1:n){
    sequ[i] <- runif(1)
    if(sequ[i]<p0){
      x0 <- c(x0,0)
    }
    if(sequ[i]>=p1 && sequ[i]<p2){
      x0 <- c(x0,1)
      x1 <- filhos(1,1)
      x0 <- c(x0,x1)
    }
    if(sequ[i]>=p2){
      x0 <- c(x0,2)
      x1 <- filhos(2,1)
      x0 <- c(x0,x1)
    }
  }
  return(x0)
}

i <-1
while (i<100){ # Resultado simulado
  pop.size[i] <- sum(filhos1(x0,n))
  if (pop.size[i] >= 100){
    i <- i+1
  }
}
length(pop.size) # média da poupulação simualda
#(n/(1-(p1+2*p2))) # média da poupulação teoŕica

# Exercício 4

a <- 100 # Quantidade de votos do candidato A
b <- 50 # Quantidade de votos do candidato B
Total <- a+b # Total de votos

Nsim <- 1000 # número de simulações
soma <- numeric(0)

lid <- function(emb){ # Verifica se durante a contagem A esteve na liderança
  s <- cumsum(emb)
  for (i in 1:length(s)){
    b <- s[i]
    if (b <=0) return(0)
  }
  return(1)
}

for( j in 1:Nsim){ # SImulações do processo de contagem
  emb <- sample(rep(c(1,-1),c(a,b)),replace=T)
  soma[j] <- lid(emb)
}

sum(soma)/Nsim # Resultado Simulado
(a-b)/Total # Resultado Teórico

  