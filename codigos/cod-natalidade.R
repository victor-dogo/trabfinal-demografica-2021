taxa_bruta_natalidade <- function(nascidos_vivos, populacao_total){
  # Esta funcao toma um valor correspondendo ao num. total de nascidos
  # vivos e um valor correspondendo a pop. total
  taxa <- nascidos_vivos/populacao_total * 1000
  return(taxa)
}

taxa_fecundidade_geral <- function(nascidos_vivos, populacao_f_15_49_anos){
  # Esta funcao toma um valor correspondendo ao num. total de nascidos
  # vivos e um valor correspondendo ao num. total de mulheres da populacao
  # entre 15 e 49 anos
  taxa <- nascidos_vivos/populacao_f_15_49_anos * 1000
  return(taxa)
}

taxa_especifica_fecundidade <- function(vetor_nascidos_vivos, vetor_pop_f){
  # Esta funcao toma um vetor de nascidos vivos por faixa etaria da mae
  # no momento do parto e um vetor da pop. feminina dessas mesmas faixas.
  # O vetor saida e a taxa especifica de cada faixa etaria.
  vetor_taxa <- vetor_nascidos_vivos/vetor_pop_f * 1000
  return(vetor_taxa)
}