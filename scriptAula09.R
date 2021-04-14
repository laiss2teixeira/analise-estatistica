# Aula 09
# Carregar as pacotes
library(tidyverse)
library(nortest)
library(lawstat)
library(MASS)
library(ExpDes.pt)

# Entrada de dados
dados <- read.table("dados/racoes.txt",h=TRUE)

# resumo do banco de dados
glimpse(dados)

# Estatística Descritiva do Banco de dados
# Por Tratamento (Rações)
# Tabela de Estatística Descritiva
# coef de varia, coef de assim e curtosi
# normalidade
cont_na <- function(x) sum(is.na(x))
p_shapiro <- function(x) round(shapiro.test(x)$p.value,5)
# alpha = 5%, ou seja, p<0.05, rejeitamos H0
# H0: Dados tem distribuição normal
# H1: Dados não tem ditribuição normal

dados %>% 
  group_by(racoes) %>% 
  summarise(N = n(),
            Nna=cont_na(a_peso),
            Média=mean(a_peso,na.rm=TRUE),
            Mediana=median(a_peso,na.rm=TRUE),
            Variância=var(a_peso,na.rm=TRUE),
            Desv_Pad=sd(a_peso,na.rm=TRUE),
            Err_Pad=Desv_Pad/sqrt(N),
            Mínimo=min(a_peso,na.rm=TRUE),
            Máximo=max(a_peso,na.rm=TRUE),
            Q1=quantile(a_peso,0.25,na.rm=TRUE),
            Q3=quantile(a_peso,0.75,na.rm=TRUE),
            DIQ = IQR(a_peso,na.rm=TRUE),
            Coef_Var = 100*Desv_Pad/Média,
            Coef_Assim = agricolae::skewness(a_peso),
            Coef_Curt = agricolae::kurtosis(a_peso),
            Normalidade = p_shapiro(a_peso)
  ) %>% 
  View()

# Distribuição
## Histograma
dados %>% 
  ggplot(aes(x=a_peso,y=..density..)) +
  geom_histogram(bins=7,color="black",fill="lightgray") +
  labs(x="Peso (g)",y="Densidade de frequência")+
  theme_classic()+
  geom_density(color="red",fill="blue",alpha=0.1)

# Plotando as densidades para cada tratamentos
dados %>% 
  ggplot(aes(x=a_peso,y=..density..,color=racoes)) +
  geom_freqpoly(bins=5)

# Fazer o boxplot da variável a peso.
dados %>% 
  ggplot(aes(y=a_peso))+
  geom_boxplot(fill="orange") +
  coord_cartesian(xlim=c(-1,1))

# Boxplot por tratamento 
dados %>% 
  ggplot(aes(y=a_peso,x=racoes,fill=as.factor(racoes)))+
  geom_boxplot()

# reordenado
dados %>% 
  ggplot(aes(y=a_peso,x=reorder(racoes,a_peso,mean),
             fill=as.factor(racoes)))+
  geom_boxplot() +
  coord_flip()

# Violinplot
dados %>% 
  ggplot(aes(y=a_peso,x=reorder(racoes,a_peso,mean),
             fill=racoes))+
  geom_violin(trim = FALSE) +
  stat_summary(fun=mean, geom="point", size=4,
               shape=21,fill="gray")+
  theme(legend.position = "none")

#dotplot
dados %>% 
  ggplot(aes(y=a_peso,x=reorder(racoes,a_peso,mean)))+
  geom_violin(trim = FALSE) +
  theme(legend.position = "none")+
  geom_dotplot(binaxis = "y",stackdir = "center",bins=2,
               dotsize = 2)

# Violin e Boxplot
dados %>% 
  ggplot(aes(y=a_peso,x=racoes,fill=racoes))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width=0.2,fill="lightgray") +
  stat_summary(fun=mean, geom="point", size=2,
               shape=21,fill="blue")+
  theme_bw()+
  theme(legend.position = "none")

#QQ plot de a_peso
dados %>% 
  ggplot(aes(sample=a_peso)) +
  stat_qq(color="red")+
  stat_qq_line(color="blue")

# Teste de Normalidade dos desvios (erros)
## Criar o modelo da anova
trat <- as.factor(dados$racoes)
y <- dados$a_peso
mod <- aov(y~trat)

# Extrair os resíduos para normalide e diagnóstico de 
# outliers
dados<-dados %>% 
          mutate(
            rs_peso = rstudent(mod),
            pred_peso = predict(mod)
          )
## Histograma dos Resíduos
dados %>% 
  ggplot(aes(x=rs_peso,y=..density..))+
  geom_histogram(fill="gray",color="black",bins=10)

## QQ plot dos resíduos
dados %>% 
  ggplot(aes(sample=rs_peso)) +
  stat_qq(color="red")+
  stat_qq_line(color="blue")

## Teste de normalidade dos resíduos
shapiro.test(dados$rs_peso) #Shapiro-Wilk
lillie.test(dados$rs_peso) #Kolmogorov-Smirnov
ad.test(dados$rs_peso) #Anderson-Darling
cvm.test(dados$rs_peso) #Cramer-von Mises
### Concluímos que os resíduos tem distribuição normal.

## Estudo dos outliers
# Quanto temos os resíduos padronizados (studentizados)
# se os erros possuem distribuição normal, ou próxima à normal,
# 95% dos resíduos estarão entre -3 e 3, ou seja tudo o 
# que estiver abaixo de -3 e acima de 3 pode ser considerado
# um outlier. O gráfico de ser dos resíduos studentizados vs
# os valore preditos.
# Construir o gráfico dos resíduos em função dos preditos
# (dispersão).
dados %>% 
  ggplot(aes(x=pred_peso,y=rs_peso)) +
  geom_point(shape=21,color="black",fill="gray",size=4)+
  theme_classic()+
  coord_cartesian(ylim=c(-4,4))+
  geom_hline(yintercept = c(-3,3),color="red",
             linetype=2)

# Ordernar os dados por rs_peso
dados %>% 
  arrange(desc(rs_peso)) %>% 
  View()

# ou
dados %>% 
  filter(rs_peso > 3 | rs_peso< -3) %>% 
  View()

# Homogeneidade das Variâncias (homocedasticidade)
## 4 testes são empregados, teste de Levene, Brown-Forsythe
## Bartlett e Box-Cox
## Teste Clássio de Levene
levene.test(y,trat,location = "mean") ## pacote lawstat

## Teste de Brown-Forsythe
levene.test(y,trat) ## pacote lawstat

## Teste de Bartlett
bartlett.test(y,trat)

## Teste de Box-Cox
boxcox(mod,seq(-5,5,1))
## Critério
### se lambda não difere de 1 => Homocedasticidade
### se lambda difere de 1 (transformação nos dados)
  ### se lambda difere de zero y_t = (y^lambda-1)/lambda
  ### se lambda não difere de zero y_t =log(y)

# Realizar a análise de variância
trat <- as.factor(dados$racoes)
y <- dados$a_peso
dic(trat,y,quali=TRUE,mcomp="tukey",sigF=0.05,sigT=0.05)









