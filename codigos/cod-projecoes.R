projecao_aritmetica <- function(p0,p2,t0,t2,t_proj){
  K_a <- (p2 - p0)/(t2-t0)
  print(K_a)
  pop_estimada <- p0 + K_a * (t_proj - t0)
  return(pop_estimada)
}

projecao_geometrica <- function(p0,p2,t0,t2,t_proj){
  K_g <- (log(p2) - log(p0))/(t2-t0)
  print(K_g)
  pop_estimada <- p0 * exp(K_g * (t_proj - t0))
  return(pop_estimada)
}

projecao_decrescente <- function(p0,p1,p2,t0,t2,t_proj){
  P_s <- (2*p0*p1*p2 - p1^2 * (p0+p2))/(p0*p2 - p1^2)
  print(P_s)
  K_d <- (-log((P_s - p2)/(P_s - p0)))/(t2 - t0)
  print(K_d)
  pop_estimada <- p0 + ((P_s - p0)*(1 - exp(-K_d*(t_proj - t0))))
  return(pop_estimada)
}

projecao_logistica <- function(p0,p1,p2,t0,t1,t2,t_proj){
  P_s <- (2*p0*p1*p2 - p1^2 * (p0+p2))/(p0*p2 - p1^2)
  print(P_s)
  c <- (P_s - p0)/p0
  print(c)
  K_l <- (1/(t2 - t1))*log((p0*(P_s - p1))/(p1*(P_s - p0)))
  print(K_l)
  pop_estimada <- P_s/(1 + (c*exp(K_l*(t_proj - t0))))
  return(pop_estimada)
}

#Definindo dados observados

pop_2010 <- 83639
t0 <- 2010

pop_2015 <- 89027
t1 <- 2015

pop_2020 <- 91771
t2 <- 2020

vetor_pop <- c(pop_2010, pop_2015, pop_2020)

vetor_anos <- c(seq(2025, 2060, 5))

#Calculando estimativas para cada ano e cada projecao especifica

proj_arit <- proj_geo <- proj_dec <- proj_log <- NULL

for (ii in vetor_anos) {
  proj_arit <- c(proj_arit,
                 projecao_aritmetica(pop_2010, pop_2020, t0, t2, ii))
  proj_geo <- c(proj_geo,
                projecao_geometrica(pop_2010, pop_2020, t0, t2, ii))
  proj_dec <- c(proj_dec,
                projecao_decrescente(pop_2010, pop_2015, pop_2020, t0, t2, ii))
  proj_log <- c(proj_log,
                projecao_logistica(pop_2010, pop_2015, pop_2020, t0, t1, t2, ii))
}

# Apresentando resultados em tabela

tabela_anos <- data.frame(vetor_anos,
                          proj_arit,
                          proj_geo, 
                          proj_dec,
                          proj_log)

knitr::kable(tabela_anos,
             col.names = c("Ano", "Aritmética", "Geométrica",
                           "Taxa Decrescente", "Projeção Logística"),
             "latex")

# Apresentando resultados em grafico

library(ggplot2)

## Arrumando dados em formato tidy

Tipo <- c(rep("Aritmética", 8), rep("Geométrica", 8),
          rep("Taxa Decrescente", 8), rep("Logística", 8))

valores_proj <- c(proj_arit, proj_geo, proj_dec, proj_log)

anos <- rep(vetor_anos, 4)

tabela_grafico_tidy <- data.frame(Tipo,
                                  valores_proj,
                                  anos)

ggplot(data=tabela_grafico_tidy, aes(x=anos, y=valores_proj,
                                     group=Tipo, color=Tipo))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_x_continuous(breaks=anos)+
  labs(x="Ano", y="População Total Projetada",
       title="Projeção de São João da Boa Vista",
       subtitle="Projeções Logística e de Taxa Decrescente tenderam para valor próximo de 95.000 pessoas",
       caption="Fonte: IBGE")+
  theme_bw()

ggsave("imagens/proj-completa.png", dpi=300)

#Grafico para projecao logistica e taxa decrescente apenas

##Arrumando dados

Tipo <- c(rep("Taxa Decrescente", 8), rep("Logística", 8))

valores_proj <- c(proj_dec, proj_log)

anos <- rep(vetor_anos, 2)

tabela_grafico_tidy <- data.frame(Tipo,
                                  valores_proj,
                                  anos)

ggplot(data=tabela_grafico_tidy, aes(x=anos, y=valores_proj,
                                     group=Tipo, color=Tipo))+
  geom_line(size=1)+
  geom_point(size=2)+
  scale_x_continuous(breaks=anos)+
  labs(x="Ano", y="População Total Projetada",
       title="Projeção Logística e de Taxa Decrescente de São João da Boa Vista",
       subtitle="A projeção de Taxa Decrescente se manteve um pouco abaixo da projeção Logística",
       caption="Fonte: IBGE")+
  theme_bw()

ggsave("imagens/proj-dec-log.png", dpi=300)
