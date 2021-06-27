#Dispondo dados

pop_sjbv_1079 <- c(1178,1220,1191,1183,1240,1250,1275,1286,1207,
                   1223,1318,1340,1347,1378,1390,1317,1298,1408,
                   1508,1433,1458,1323,1368,1281,1268,1313,1218,
                   1282,1214,1237,1317,1213,1278,1162,1233,1331,
                   1265,1242,1162,1162,1251,1094,1081,1080,1060,
                   1019,997,929,892,848,823,742,730,671,662,629,
                   580,527,529,522,472,423,492,452,444,410,357,
                   358,316,283)

idade <- as.character(10:79)

tabela_idades <- data.frame(idade, pop_sjbv_1079)

knitr::kable(tabela_idades,
             col.names = c("Idade", "População"),
             "latex")

# Definindo funcao para cada coeficiente

myers_coef <- function(vetor_pop, i){
  # Essa funcao requere um vetor de tamanho 70 contendo populacao
  # de idades entre 10 e 79 anos
  vetor_pop <- c(1:9, vetor_pop)
  m <- (1+i) * vetor_pop[10+i]
  soma <- NULL
  for (ii in 2:7) {
    soma <- c(soma, vetor_pop[(ii*10)+i])
  }
  vetor_pop <- vetor_pop[10:79]
  m <- m + 10*sum(soma)
  return(m)
}

myers_coef_total <- function(vetor_pop){
  m_vector <- NULL
  # Esta funcao recebe o vetor de populacoes entre 10 e 79 anos
  # e retorna um dataframe com coeficientes m_i e m total
  for (i in 0:9) {
    m_vector <- c(m_vector, myers_coef(vetor_pop, i))
  }
  m_vector <- c(m_vector, sum(m_vector))
  coef_string <- c("m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7",
                   "m8", "m9", "mtot")
  tabela_myers <- data.frame(coef_string, m_vector)
  return(tabela_myers)
}

myers_ratio <- function(vetor_pop){
  # Esta funcao toma como entrada o vetor de populacoes com
  # idades entre 10 e 79 anos e retorna um dataframe
  # com razoes seguindo coeficiente de myers para analise
  # de preferencia por digitos
  table <- myers_coef_total(vetor_pop)
  m_ratio <- NULL
  for (i in 1:10) {
    m_ratio <- c(m_ratio, round(table$m_vector[i]/table$m_vector[11],
                                digits = 3))
  }
  coef_string <- c("m0-mtot", "m1-mtot", "m2-mtot", "m3-mtot", 
                   "m4-mtot", "m5-mtot", "m6-mtot", "m7-mtot",
                   "m8-mtot", "m9-mtot")
  table_ratio <- data.frame(coef_string, m_ratio)
}

# Calculando coeficientes de myers

tabela_resultado_myers <- myers_coef_total(pop_sjbv_1079)

# Calculando razoes do coeficiente de myers

tabela_razoes_myers <- myers_ratio(vetor_pop)

# Apresentando resultados:

knitr::kable(tabela_resultado_myers,
             col.names = c("Coeficiente", "Resultado"),
             "latex")

knitr::kable(tabela_razoes_myers,
             col.names = c("Razão", "Resultado"),
             "latex")
