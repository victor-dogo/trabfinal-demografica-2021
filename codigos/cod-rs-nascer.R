
# Dados obtidos
nascimentos_homem <- 486
nascimentos_mulher <- 447
nascimentos_total <- nascimentos_homem+nascimentos_mulher

nasc <- c(nascimentos_homem, nascimentos_mulher, nascimentos_total)
label <- c("Homem", "Mulher", "Total")
tabela_dados <- data.frame(label, nasc)

knitr::kable(tabela_dados, col.names = c("Sexo ao Nascer", "Quantidade"),
             "latex")

# Definindo função para RSN

razao_sexo_nascer <- function(nasc_m, nasc_f){
  razao <- nasc_m/nasc_f * 100
  return(razao)
}

# Definindo funcao para ic da proporcao de nascimentos masculinos com
# 95% de confiança

ic_proporcao_nasc_m <- function(nasc_m, nasc_tot){
  p_hat <- nasc_m/nasc_tot
  lim_inf <- p_hat - 1.96 * sqrt((p_hat*(1-p_hat))/nasc_tot)
  lim_sup <- p_hat + 1.96 * sqrt((p_hat*(1-p_hat))/nasc_tot)
  tabela_result <- data.frame(lim_inf, lim_sup)
  return(tabela_result)
}

# Definindo funcao para ic da rsn com 95% de confiança

ic_rsn <- function(nasc_m, nasc_tot){
  table <- ic_proporcao_nasc_m(nasc_m, nasc_tot)
  lim_inf <- table$lim_inf/(1-table$lim_inf) * 100
  lim_sup <- table$lim_sup/(1-table$lim_sup) * 100
  tabela_result <- data.frame(lim_inf, lim_sup)
  return(tabela_result)
}

# Calculando RSN

rsn <- razao_sexo_nascer(nascimentos_homem, nascimentos_mulher)
rsn

# Calculando IC para prop. de nascimentos masculinos

ic_prop_masc <- ic_proporcao_nasc_m(nascimentos_homem, nascimentos_total)
ic_prop_masc

# Calculando IC para RSN

ic_razao_sexo_nascer <- ic_rsn(nascimentos_homem, nascimentos_total)
ic_razao_sexo_nascer
