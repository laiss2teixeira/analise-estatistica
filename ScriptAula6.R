# Aula 06 - 08-02-21- visualiza√ß√£o de Dados (ggplot)
# Visualiza√ß√£o de dados
# Entrar com os dados de COVID - https://covid.saude.gov.br/
dados_covid <- read.csv2("HIST_PAINEL_COVIDBR_04fev2021.csv", 
                         encoding="UTF=8")
#carregando pacotes
library(tidyverse)
library(extrafont)

#visluimbre dos dados, resumo r√°pido
glimpse (dados_covid)

#convertando para data um vetor de caracter

dados_covid <-  dados_covid %>% 
  mutate(
    Data = as.Date(data), 
    dia = trunc (as.numeric (difftime(Data,"2020-01-01", units = "days")))
    
  )

glimpse (dados_covid)


# Gr√°fico do n√∫mero de novos casos ao longo do tempo
  windowsFonts(A = windowsFont("Arial"))
  
  plot_casos_novos + 
    geom_col(color="orange") +
    theme_minimal() +
    ggtitle("Novos Casos di√°rios - Brasil") +
    theme(text = element_text(family="A", face="bold", size=12),
          axis.title = element_text(colour = "red"),
          axis.text = element_text(colour = "blue"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "black"),
          axis.ticks.x = element_line("Black"),
          axis.ticks.y = element_line("Black"),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90) # mudar a orienta√ß√£o
          
    ) +
  #scale_y_continuous(breaks = seq(0,85000,5000)) + # Alterar a escala
  # scale_x_date(limits = as.Date(c("2020-06-06","2021-06-06")))
 scale_y_continuous(breaks = c(0,36547,55000,90000)) # Alterar a escala

  
# Construir um gr·fico de colunas do total de n˙mero de casos por regi„o
  dados_covid %>%
    filter(regiao != "Brasil", estado !="" ,municipio != "") %>%
    group_by(regiao) %>%
    summarise(total_casos = sum(casosNovos)) %>%
    ggplot(aes(x=regiao, y=total_casos)) +
    geom_col(color="Black",fill= "gray") +
    labs(x="Regi„o do PaÌs", y="N˙mero total de casos")

# Construir um gr·fico de colunas do total de n˙mero de casos, mapeado
# por cada estado
  dados_covid %>%
    filter(regiao != "Brasil", estado !="" ,municipio != "") %>%
    group_by(regiao, estado) %>%
    summarise(total_casos = sum(casosNovos)) %>%
    ggplot(aes(x=regiao, y=total_casos, fill=estado)) +
    geom_col(color="Black") +
    labs(x="Regi„o do PaÌs", y="N˙mero total de casos",
         fill = "Estados" ) +
    coord_flip()
  
# Construir um gr·fico de colunas do total de n˙mero de casos, mapeado
# por cada estado
  dados_covid %>%
    filter(regiao != "Brasil", estado !="" ,municipio != "") %>%
    group_by(regiao, estado) %>%
    summarise(total_casos = sum(casosNovos)) %>%
    ggplot(aes(x=regiao, y=total_casos, fill=estado)) +
    geom_col(color="Black",position = "dodge") +
    labs(x="Regi„o do PaÌs", y="N˙mero total de casos",
         fill = "Estados" ) 
  
  # Construir um gr·fico de colunas do total de n˙mero de casos, mapeado
  # por cada estado
  dados_covid %>%
    filter(regiao != "Brasil", estado !="" ,municipio != "") %>%
    group_by(regiao, estado) %>%
    summarise(total_casos = sum(casosNovos)) %>%
    ggplot(aes(x=regiao, y=total_casos, fill=estado)) +
    geom_col(color="Black",position = "dodge") +
    labs(x="Regi„o do PaÌs", y="N˙mero total de casos",
         fill = "Estados" ) +
    facet_wrap(~regiao, nrow = 2, scales = "free") +
    theme_minimal()
    
  # Para a capital do seu estado, gr·fico de linhas e pontos
  # do total de casos acumlados
dados_covid %>%
  filter(regiao == "Norte", municipio=="") %>% 
  ggplot(aes(x=Data, y=casosAcumulado,color=estado)) +
  geom_point()
 
#Somente para BelÈm
dados_covid %>%
  filter(estado == "PA" | estado == "SP",
          municipio=="S„o Paulo" | municipio=="Bel√©m" ) %>%
  ggplot(aes(x=Data, y=casosAcumulado, color=estado)) +
  geom_point()
  

# Entrada de dados txt (internet)
URL <- "https://raw.githubusercontent.com/arpanosso/r_data_science_fcav/master/dados/geomorfologia.txt"
geomorfologia <- read.table(URL, h= TRUE)
View(geomorfologia)

# Ler o banco de dados de geomorfologia
# Fazer um gr·fico do n˙mero de pontos amostrais (observaÁıes)
# usar o group_by
# funÁ„o de contagem  `n()``.
# mapeado por superfÌcie geomÛrfico
geomorfologia %>%
  group_by(SUP) %>%
  summarise(n = n()) %>%
  ggplot(aes(SUP,y=n,fill=SUP)) +
  geom_col(color="black")

# Fazer um gr·fico de dispers„o do teor de Argila do solo
# em funÁ„o da coordenada x. Mapear o tipo de solo
# com a cor dos pontos.
geomorfologia %>%
  group_by(Solo) %>%
  ggplot(aes(x=X,y=ARGILA,color=Solo))+
  geom_point(size=3)

# Fazer um gr·fico de dispers„o do teor de P do solo
# em funÁ„o da coordenada x para cada superfÌcie (faceta)
geomorfologia %>%
  ggplot(aes(y=P,x=X, color = SUP, shape=SUP))+
  geom_point(size=3)+
  facet_wrap(~SUP, scales = "free")

#Boxplot da vari·vel ARGILA
geomorfologia %>%
  ggplot(aes(y=ARGILA))+
  geom_boxplot(color="black",fill="lightblue") +
  theme_bw() +
  coord_cartesian(xlim = c(-0.75,0.75))

#Boxplot para argila em cada superfÌcie geomÛrfica
geomorfologia %>%
  ggplot(aes(y=ARGILA,fill=Solo))+
  geom_boxplot(color="black") +
  scale_y_continuous(breaks = seq(0,30,2)) +
  theme_bw() 

# Vamos retirar os outliers de LVp, PV1 e PV2
geomorfologia %>%
  filter(!(Solo == "LVP" & ARGILA > 24),
         !(Solo == "PV1" & ARGILA < 16),
         !(Solo == "PV2" & ARGILA > 24),
         !(Solo == "PV4" & ARGILA > 19),
         !(Solo == "PV5" & ARGILA > 7)) %>%
  ggplot(aes(y=ARGILA,fill=Solo))+
  geom_boxplot(color="black") +
  scale_y_continuous(breaks = seq(0,30,2)) +
  theme_bw() 

# Histograma da vari·vel argila
geomorfologia %>%
  ggplot(aes(x=AG)) +
  geom_histogram()

# Histograma da vari·vel argila em densidade
geomorfologia %>%
  ggplot(aes(x=AG, y=..density..,fill=SUP)) +
  geom_histogram(color="black",fill="white",bins=15) +
  geom_density(alpha=0.1) +
  theme_classic() +
  facet_wrap(~SUP,scales = "free")
  
 

  