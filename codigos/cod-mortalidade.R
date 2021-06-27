taxa_bruta_mortalidade <- function(total_obitos, pop_total){
  # Esta funcao toma como entrada o valor do total de obitos e 
  # a populacao total.
  taxa <- total_obitos/pop_total * 1000
  return(taxa)
}

taxa_especifica_mortalidade <- function(vetor_obitos, vetor_pop){
  # Esta funcao toma como entrada o vetor com obitos de acordo com
  # cada faixa etaria e a populacao dessas faixas
  vetor_taxa <- vetor_obitos/vetor_pop * 1000
  return(vetor_taxa)
}

taxa_mortalidade_infantil <- function(nascidos_vivos, obitos_de_0a1_anos){
  # Esta funcao toma como entrada o total de nascidos vivos no ano
  # e o numero de obitos de crianças entre 0 e 1 ano no mesmo periodo
  taxa <- obitos_de_0a1_anos/nascidos_vivos * 1000
  return(taxa)
}

#Definindo dados segundo IBGE

vetor_pop_sjbv <- c(4599,5123,6012,3811+2430,6773,6964,6698,
                    6264,6203,6162,5566,4685,3628,2787,5934)

vetor_obitos_sjbv <- c(15,4,1,3,4,5,11,17,21,34,43,52,43,73,
                       75+90+95+129)

nascidos_vivos <- c(115, 212, 263, 215, 102, 25, 1)

obitos_de_0a1_ano_sjbv <- 15

## Calculo da Taxa Bruta de Mortalidade

TBM <- taxa_bruta_mortalidade(sum(vetor_obitos_sjbv),
                              sum(vetor_pop_sjbv))

## Calculo das Taxas Especificas de Mortalidade

TEM_vetor <- taxa_especifica_mortalidade(vetor_obitos_sjbv,
                                         vetor_pop_sjbv)

## Calculo da Mortalidade Infantil

TMI <- taxa_mortalidade_infantil(sum(nascidos_vivos),
                                 obitos_de_0a1_ano_sjbv)


## Organizando dados em tabela

faixas_etarias <- c("0 a 4 anos", "5 a 9 anos", "10 a 14 anos",
                    "15 a 19 anos", "20 a 24 anos", "25 a 29 anos",
                    "30 a 34 anos", "35 a 39 anos", "40 a 44 anos",
                    "45 a 49 anos", "50 a 54 anos", "55 a 59 anos",
                    "60 a 64 anos", "65 a 69 anos", "70 anos ou mais")

tabela_taxas_especificas_mortalidade <- data.frame(faixas_etarias,
                                                   vetor_pop_sjbv,
                                                   vetor_obitos_sjbv,
                                                   TEM_vetor)

knitr::kable(tabela_taxas_especificas_mortalidade,
             col.names = c("Faixas Etárias",
                           "População",
                           "Óbitos",
                           "TEM"), 
             "latex")

taxas <- c("TBM", "TMI")

valores <- c(TBM, TMI)

tabela_taxas_mortalidade <- data.frame(taxas, valores)

knitr::kable(tabela_taxas_mortalidade,
             col.names = c("Taxa", "Resultado"), "latex")

