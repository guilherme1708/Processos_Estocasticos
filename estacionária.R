invariante = function(trans) {
  # Trans é a matriz que se quer estudar a distribuição estacioária
  # Para determinar pi que satisfaz pi %*% trans = pi resolvemos o
  # sistema (t(trans) - I) %* pi = 0 onde t() e' a matriz transposta e
  # pi agora e' um vetor coluna. No lado direito trocamos a ultima linha
  # por uma linha de 1's, e no lado esquerdo trocamos o vetor nulo por
  # (0,0,...0,1), isso representa a restricao da soma das entradas de pi
  # ser 1. Entao, resolvemos para pi.
  
  ##########################################
  
  n <- nrow(trans)
  a <- t(trans)
  for(i in 1:n)    {    a[i, i] <- a[i, i] - 1  }
  a[n, ] <- 1
  b <- rep(0, n)
  b[n] <- 1
  solve(a, b)
  
}

# Algoritmo retirado de https://anotacoesdeaula.wordpress.com/2011/10/12/bc1414-cadeias-de-markov/