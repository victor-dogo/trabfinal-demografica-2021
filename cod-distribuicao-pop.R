library(ggplot2)

# Religião

lab_religiao <- c("Sem Religião", "Católica Apostólica Romana",
                  "Espírita", "Evangélica", "Testemunhas de Jeová",
                  "Outras Crenças")

qtd_religiao <- c(3597, 57818, 3217, 16359, 1021,
                  35+11+46+41+80+163+351+33+9+49+60+15+144+482+167)

tabela_religiao <- data.frame(lab_religiao, qtd_religiao)

ggplot(data=tabela_religiao, aes(x=lab_religiao, y=qtd_religiao,
                                 fill=lab_religiao, label=qtd_religiao))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="", y="Quantidade", 
       title="População de São João da Boa Vista por Religião",
       subtitle="A esmagadora maioria é da Igreja Católica Apostólica Romana",
       caption="Fonte: Censo IBGE 2010")+
  geom_text()+
  guides(fill=FALSE)+
  theme_bw()

ggsave("imagens/tabela-relig.png", dpi=300)

# Genero

lab_genero <- c("Masculino", "Feminino")

qtd_genero <- c(2451+2560+3054+3201+3406+3469+6376+5884+4842+2935+2367,
                2132+2563+2958+3040+3367+3495+6586+6481+5409+3480+3582)

tabela_genero <- data.frame(lab_genero, qtd_genero)

ggplot(data=tabela_genero, aes(x=lab_genero, y=qtd_genero,
                                 fill=lab_genero, label=qtd_genero))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="", y="Quantidade", 
       title="População de São João da Boa Vista por Gênero",
       subtitle="Há pouca diferença entre mulheres e homens",
       caption="Fonte: Censo IBGE 2010")+
  geom_text()+
  guides(fill=FALSE)+
  theme_bw()

ggsave("imagens/tabela-gen.png", dpi=300)

# Rendimento

lab_rend <- c("Sem Rendimento", "< 1/4 de Salário Mínimo",
              "1/4< e <1/2 Salário Mínimo", "1/2< e <1 Salário Mínimo",
              "1< e <2 Salários Mínimos", "2< e <3 Salários Mínimos",
              "3< e <5 Salários Mínimos", "5< e <10 Salários Mínimos",
              "10< e <15 Salários Mínimos", "15< e <20 Salários Mínimos",
              "20< e <30 Salários Mínimos", "Mais de 30 Salários Mínimos")

qtd_rend <- c(18939, 1523, 1431, 11894, 20492, 7990, 5983, 3679,
              928, 469, 285, 319)

tabela_rend <- data.frame(lab_rend, qtd_rend)

tabela_rend$lab_rend <- factor(tabela_rend$lab_rend,
                               levels = lab_rend)

ggplot(data=tabela_rend, aes(x=lab_rend, y=qtd_rend,
                               fill=lab_rend, label=qtd_rend))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="", y="Quantidade", 
       title="São João da Boa Vista por Rendimento Médio Mensal",
       subtitle="Maior parte da população não recebe mais do que 2 Salários Mínimos",
       caption="Fonte: Censo IBGE 2010")+
  geom_text()+
  guides(fill=FALSE)+
  theme_bw()

ggsave("imagens/tabela-rend.png", dpi=300)

# Escolaridade

lab_escola <- c("Sem Instrução/Fundamental Incompleto",
                "Ensino Médio Incompleto", "Superior Incompleto",
                "Superior Completo", "Não Determinado")

qtd_escola <- c(30345, 14191, 19090, 9961, 346)

tabela_escola <- data.frame(lab_escola, qtd_escola)

tabela_escola$lab_escola <- factor(tabela_escola$lab_escola,
                               levels = lab_escola)

ggplot(data=tabela_escola, aes(x=lab_escola, y=qtd_escola,
                             fill=lab_escola, label=qtd_escola))+
  geom_bar(stat="identity")+
  coord_flip()+
  labs(x="", y="Quantidade", 
       title="População de São João da Boa Vista por Escolaridade",
       subtitle="A maioria não completou o Ensino Fundamental",
       caption="Fonte: Censo IBGE 2010")+
  geom_text()+
  guides(fill=FALSE)+
  theme_bw()

ggsave("imagens/tabela-escola.png", dpi=300)
