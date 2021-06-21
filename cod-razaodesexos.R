library(ggplot2)

label_faixas <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                  "30-34", "35-39", "40-44", "45-49", "50-54",
                  "55-59", "60-64", "65-69", "70+")

# Razao de Sao Joao da Boa Vista

pop_m_sjbv <- c(2369,2560,3054,1914+1287,3406,3469,3295,3081,
                2989,2895,2687,2155,1726,1260,2399)

pop_f_sjbv <- c(2230,2563,2958,1897+1143,3367,3495,3403,3183,
                3214,3267,2879,2530,1902,1527,3535)

pop_tot_sjbv <- pop_m_sjbv+pop_f_sjbv

rs_sjbv <- round(pop_m_sjbv/pop_f_sjbv * 100, digits = 2)

# Razao de Espirito Santo do Pinhal

pop_m_pinhal <- c(1166,1331,1624,1035+657,1767,1707,1647,1573,
                  1497,1439,1302,1030,891,614,1227)

pop_f_pinhal <- c(1148,1269,1658,964+671,1759,1698,1591,1603,
                  1677,1480,1396,1161,950,706,1669)

pop_tot_pinhal <- pop_m_pinhal + pop_f_pinhal

rs_pinhal <- round(pop_m_pinhal/pop_f_pinhal * 100, digits = 2)

# Razao de Aguai

pop_m_aguai <- c(1170,1146,1391,906+565,1533,1540,1342,1095,
                 1081,1040,933,736,620,382,737)

pop_f_aguai <- c(1127,1156,1271,821+548,1370,1441,1197,1145,
                 1090,1003,947,777,645,407,986)

pop_tot_aguai <- pop_m_aguai + pop_f_aguai

rs_aguai <- round(pop_m_aguai/pop_f_aguai * 100, digits = 2)

# Definindo Tabelas
##Sao Joao

tabela_sjbv <- knitr::kable(data.frame(label_faixas,
                                       pop_m_sjbv, pop_f_sjbv,
                                       pop_tot_sjbv, rs_sjbv),
                            col.names= c("Faixa Etária",
                                         "Masculina", "Feminina",
                                         "Total", "Razão de Sexos"),
                            "latex")

##Pinhal

tabela_pinhal <- knitr::kable(data.frame(label_faixas,
                                         pop_m_pinhal, pop_f_pinhal,
                                       pop_tot_pinhal, rs_pinhal),
                            col.names= c("Faixa Etária",
                                         "Masculina", "Feminina",
                                         "Total", "Razão de Sexos"),
                            "latex")

##Aguai

tabela_aguai <- knitr::kable(data.frame(label_faixas, 
                                        pop_m_aguai, pop_f_aguai,
                                       pop_tot_aguai, rs_aguai),
                            col.names= c("Faixa Etária",
                                         "Masculina", "Feminina",
                                         "Total", "Razão de Sexos"),
                            "latex") 


############# grafico ggplot tidy

Cidade <- c(rep("São João da Boa Vista", 15), 
            rep("Espírito Santo do Pinhal", 15),
            rep("Aguaí", 15))

faixas_etarias <- factor(label_faixas,
                         levels = label_faixas)

razoes_sexo <- c(rs_sjbv, rs_pinhal, rs_aguai)

tabela_graph <- data.frame(Cidade, faixas_etarias, razoes_sexo)

ggplot(data=tabela_graph, aes(x=faixas_etarias, y=razoes_sexo,
                              color=Cidade, group=Cidade))+
  geom_line(size=1)+
  geom_hline(yintercept=100, color="red", size=0.5)+
  geom_point(size=2)+
  theme(axis.text.x = element_text(angle=90, hjust=1))+
  labs(x="Faixas Etárias", y="Razão de Sexos",
       title="Razão de Sexo por Faixas Etárias da Região",
       subtitle="São João da Boa Vista demonstrou razão menor em Faixas Etárias avançadas",
       caption="Fonte: Censo IBGE 2010")

ggsave("imagens/razao-linhas.png", dpi=300)
