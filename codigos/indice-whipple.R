indice_whipple <- function(vetor_p25atep60, vetor_p23atep62){
  indice <- sum(vetor_p25atep60)/(0.2 * sum(vetor_p23atep62)) * 100
  return(indice)
}


faixa_etaria <- as.character(23:62)

vetor_p25atep60 <- c(1317,1458,1313,1317,1331,1251,1019,823)

vetor_p23atep62 <- c(1378,1390,1317,1298,1408,1508,1433,1458,1323,
                     1368,1281,1268,1313,1218,1282,1214,1237,1317,
                     1213,1278,1162,1233,1331,1265,1242,1162,1162,
                     1251,1094,1081,1080,1060,1019,997,929,892,848,
                     823,742,730)

tabela_whipple <- data.frame(faixa_etaria,
                             vetor_p23atep62)

knitr::kable(tabela_whipple, col.names = c("Idade", "População"),
             "latex")

indice <- indice_whipple(vetor_p25atep60, vetor_p23atep62)
indice
