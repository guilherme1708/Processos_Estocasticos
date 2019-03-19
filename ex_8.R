ic.m <- function(x,sigma2,conf.level=0.95){ # Função para calcular o Intervalo de confiança
  n<-length(x)
  xbarra<-mean(x)
  z<-qnorm((1+conf.level)/2)
  LI <- xbarra - z*sqrt(sigma2)/sqrt(n)
  LS <- xbarra + z*sqrt(sigma2)/sqrt(n)
  return(c(LI,LS))
}

GeraPasseio <- function(n,v.inicial=0){ # Função para gerar vo passeio aleatório
  amostra <- rbinom(n,1,p)
  amostra[amostra==0] <- -1
  v.inicial + cumsum(amostra)
}

p <- 0.6 # 0. ultimo número USP

n <- 1000 # Tamanho da amostra

Sn <- numeric() # vetor com todos Sn

for(i in 1:1000){ # simulações para Sn
  Sn[i]<-mean(GeraPasseio(1000)[1000])
}
Sn.bar <- mean(Sn) # Média dos Sn

hist(Sn,breaks = 20, probability = T, ylab = "Densidade de Frequência", xlim=c(100,300), xlab=expression(S[n]), main=expression(paste('Histograma de ', S[n])),col="Grey")
curve(dnorm(x,Sn.bar,sd(Sn)),ylim=c(0,5),lwd=2,col=1,ylim=c(0,0.09),add = T)

ic.m(Sn,var(Sn),0.683) # Intervalo de confiança para Sn de 0.683 (1 sigma)
ic.m(Sn,var(Sn),0.954) # Intervalo de confiança para Sn de 0.954 (2 sigma)
ic.m(Sn,var(Sn),0.997) # Intervalo de confiança para Sn de 0.997 (3 sigma)

c1 <- ic.m(Sn,var(Sn),0.683)[2] - Sn.bar 
c2 <- ic.m(Sn,var(Sn),0.954) [2] - Sn.bar
c3 <- ic.m(Sn,var(Sn),0.997)[2] - Sn.bar
