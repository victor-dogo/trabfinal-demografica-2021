saldo_migratorio <- function(i, e){
  saldo <- i - e
  return(saldo)
}

taxa_imigracao <- function(i, pop){
  taxa <- i/pop * 1000
  return(taxa)
}

taxa_emigracao <- function(e, pop){
  taxa <- e/pop * 1000
  return(taxa)
}

taxa_migracao_liquida <- function(i, e, pop){
  taxa <- (i-e)/pop * 1000
  return(taxa)
}

indice_eficacia_migratoria <- function(i, e){
  indice <- (i-e)/(i+e)
  return(indice)
}

# Definindo dados conhecidos:

imigrantes <- 4741
emigrantes <- 5654
pop_total_sjbv <- 83639

#Calculando taxas

SM <- saldo_migratorio(imigrantes, emigrantes)

TI <- taxa_imigracao(imigrantes, pop_total_sjbv)

TE <- taxa_emigracao(emigrantes, pop_total_sjbv)

TML <- taxa_migracao_liquida(imigrantes, emigrantes, pop_total_sjbv)

IEM <- indice_eficacia_migratoria(imigrantes, emigrantes)

# Apresentando resultados

nome_taxa <- c("SM", "TI", "TE", "TML", "IEM")

valor_taxa <- c(SM, TI, TE, TML, IEM)

tabela_migracao <- data.frame(nome_taxa, valor_taxa)

knitr::kable(tabela_migracao, col.names = c("Taxa", "Resultado"),
             "latex")
