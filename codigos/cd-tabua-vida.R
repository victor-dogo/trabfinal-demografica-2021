# Definindo função para nmx

nmx <- function(vetor_pop, vetor_morte){
  result <- vetor_morte/vetor_pop
  result <- round(result, digits=5)
  return(result)
}

# Definindo função para nqx

nqx <- function(vetor_pop, vetor_morte){
  ob <- nmx(vetor_pop, vetor_morte)
  a <- c(0.1, 0.4, 0.5)
  n_q_x <- c((1*ob[1])/(1+1*(1-a[1])*ob[1]),
           (4*ob[2])/(1+4*(1-a[2])*ob[2]))
  for (ii in 3:17) {
    n_q_x <- c(n_q_x, (5*ob[ii])/(1+5*(1-a[3])*ob[ii]))
  }
  n_q_x <- c(n_q_x, 1)
  n_q_x <- round(n_q_x, digits=5)
  return(n_q_x)
}

# Definindo funcao para ndx e lx

ndx_lx <- function(vetor_pop, vetor_morte, l){
  coort <- c(l)
  nqx <- nqx(vetor_pop, vetor_morte)
  n_d_x <- NULL
  for (ii in 2:18) {
    n_d_x <- c(n_d_x, round(coort[ii-1]*nqx[ii-1]))
    coort <- c(coort, coort[ii-1]-n_d_x[ii-1])
  }
  n_d_x <- c(n_d_x, coort[18])
  result <- data.frame(lx=coort, ndx=n_d_x)
  return(result)
}

# Definindo funcao para Lx

Lx <- function(vetor_pop, vetor_morte, l){
  vec <- ndx_lx(vetor_pop, vetor_morte, l)
  lx <- vec$lx
  ndx <- vec$ndx
  a <- c(0.1, 0.4, 0.5)
  L_x <- c(round(1*(lx[2]+a[1]*ndx[1])),
           round(4*(lx[3]+a[2]*ndx[2])))
  for (ii in 3:17) {
    L_x <- c(L_x, round(5*(lx[ii+1]+a[3]*ndx[ii])))
  }
  L_x <- c(L_x, round(ndx[18]/(vetor_morte[18]/vetor_pop[18])))
  return(L_x)
}

#Definindo funcao para Tx

Tx <- function(vetor_pop, vetor_morte, l){
  Lx <- Lx(vetor_pop, vetor_morte, l)
  T_x <- c(Lx[18])
  for (ii in 17:1) {
    T_x <- c(T_x[1]+Lx[ii],T_x)
  }
  return(T_x)
}

# Definindo funcao para espx

espx <- function(vetor_pop, vetor_morte, l){
  lx <- ndx_lx(vetor_pop, vetor_morte, l)
  lx <- lx$lx
  Tx <- Tx(vetor_pop, vetor_morte, l)
  e_x <- round(Tx/lx, digits=2)
  return(e_x)
}

# Definindo funcao para Px,x+n

npx <- function(vetor_pop, vetor_morte, l){
  Lx <- Lx(vetor_pop, vetor_morte, l)
  n_p_x <- NULL
  for (ii in 1:17) {
    n_p_x <- c(n_p_x, (Lx[ii+1]/Lx[ii]))
  }
  round(n_p_x, digits = 5)
  n_p_x[1] <- n_p_x[2] <- 1
  n_p_x[length(n_p_x)] <- NA
  n_p_x <- c(n_p_x, NA)
  return(n_p_x)
}

# Juntando todas as funcoes numa funcao para calcular a tabela de vida

tabela_vida <- function(vetor_pop, vetor_morte, l){
  x <- c(0, 1, seq(5, 80, 5))
  n <- c(1, 4, rep(5,16))
  nmx <- nmx(vetor_pop, vetor_morte)
  nqx <- nqx(vetor_pop, vetor_morte)
  ndxlx <- ndx_lx(vetor_pop, vetor_morte, l)
  lx <- ndxlx$lx
  ndx <- ndxlx$ndx
  Lx <- Lx(vetor_pop, vetor_morte, l)
  Tx <- Tx(vetor_pop, vetor_morte, l)
  ex <- espx(vetor_pop, vetor_morte, l)
  npx <- npx(vetor_pop, vetor_morte, l)
  #Apresentando resultado
  tabela_result <- data.frame(x, n, nmx, nqx, lx, ndx, Lx, Tx, ex, npx)
  return(tabela_result)
}

##### Usando dados conhecidos

vetor_obitos <- c(15,0,4,1,3,4,5,11,17,21,34,43,52,43,73,75,90,95+129)
vetor_povo <- c(913, 4599-913,5123,6012,3811+2430,6773,6964,6698,
                6264,6203,6162,5566,4685,3628,2787,2283,1724,
                1727+198+2)
faixa_etaria <- c("0", "1", as.character(seq(5, 80, 5)))

tabela_apresentacao <- data.frame(faixa_etaria, vetor_povo, vetor_obitos)

knitr::kable(tabela_apresentacao,
             col.names = c("Faixa Etária", "População", "Óbitos"),
             "latex")
## Calculando tabela de vida e criando arquivo csv para referencia

tabela_de_vida <- tabela_vida(vetor_povo, vetor_obitos, 100000)
library(readr)
write_csv2(tabela_de_vida, file="tabelas/tabela-vida-sjbv.csv")

## Apresentando tabela de vida

knitr::kable(tabela_de_vida, "latex")
