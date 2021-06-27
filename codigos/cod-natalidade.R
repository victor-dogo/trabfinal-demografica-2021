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

taxa_fecundidade_total <- function(vetor_taxa_especifica){
  # Esta funcao toma o vetor resultante da funcao taxa_especifica_fecundidade
  # e retorna a taxa de fecundidade total. Aqui, definiu-se a
  # amplitude dos intervalos igual a 5.
  # Esta funcao tambem é utilizada para calculo da taxa bruta
  # de reproducao se o vetor de entrada for das taxas especificas
  # de fecundidade levando em conta nascimentos de filhas do
  # sexo feminino apenas.
  taxa <- 5*sum(vetor_taxa_especifica) / 1000
  return(taxa)
}


# Valores obtidos no IBGE

nascidos_vivos <- c(115, 212, 263, 215, 102, 25, 1)
nascidos_vivos_fem <- c(56,103,132,96,50,9,1)
  
pop_total_sjbv_2010 <- 83639
pop_total_fem_1549_anos <- c(1897+1143,3367,3495,3403,
                             3183,3214,3267)

## Calculo da TBN

TBN <- taxa_bruta_natalidade(sum(nascidos_vivos), pop_total_sjbv_2010)

## Calculo da TFG

TFG <- taxa_fecundidade_geral(sum(nascidos_vivos), sum(pop_total_fem_1549_anos))

## Calculo das TEF

TEF_vetor <- taxa_especifica_fecundidade(nascidos_vivos, pop_total_fem_1549_anos)

## Calculo da TFT

TFT <- taxa_fecundidade_total(TEF_vetor)

## Calculo da TBR
# Primeiro, definido o vetor TEF para nascimentos de meninas

TEF_vetor_fem <- taxa_especifica_fecundidade(nascidos_vivos_fem, pop_total_fem_1549_anos)

#Agora, calculando a TBR com base na funcao taxa_fecundidade_total

TBR <- taxa_fecundidade_total(TEF_vetor_fem)


### Apresentando resultados
## Primeiramente, apresentando as Taxas Específicas

faixas_etarias <- c("15 a 19 anos", "20 a 24 anos", "25 a 29 anos",
                    "30 a 34 anos", "35 a 39 anos", "40 a 44 anos",
                    "45 a 49 anos")

tabela_taxas_especificas <- data.frame(faixas_etarias,
                                       pop_total_fem_1549_anos,
                                       nascidos_vivos,
                                       nascidos_vivos_fem,
                                       TEF_vetor,
                                       TEF_vetor_fem)

knitr::kable(tabela_taxas_especificas, 
             col.names = c("Faixas Etárias",
                           "População Feminina",
                           "Nascidos Vivos",
                           "Nascidos Vivos (F)",
                           "TEF geral",
                           "TEF f"), "latex")

# Agora, apresentando o restante dos resultados

taxas <- c("TBN", "TFG", "TFT", "TBR")
resultados <- c(TBN, TFG, TFT, TBR)

tabela_resultados_natalidade <- data.frame(taxas, resultados)

knitr::kable(tabela_resultados_natalidade,
             col.names = c("Taxa", "Resultado"), "latex")
