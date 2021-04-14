# Aula 09 - 2
# Carregar as pacotes
library(tidyverse)
library(nortest)
library(lawstat)
library(MASS)
library(ExpDes.pt)

# Entrada de dados
dados <- read.table("dados/prod.txt",h=TRUE)

# resumo do banco de dados
glimpse(dados)

# Estatística Descritiva do Banco de dados
# Por Tratamento (VARIEDADES)
# Tabela de Estatística Descritiva
cont_na <- function(x) sum(is.na(x))
p_shapiro <- function(x) round(shapiro.test(x)$p.value,5)
# alpha = 5%, ou seja, p<0.05, rejeitamos H0
# H0: Dados tem distribuição normal
# H1: Dados não tem ditribuição normal

dados %>% 
  group_by(Trat) %>% 
  summarise(N = n(),
            Nna=cont_na(Y),
            Média=mean(Y,na.rm=TRUE),
            Mediana=median(Y,na.rm=TRUE),
            Variância=var(Y,na.rm=TRUE),
            Desv_Pad=sd(Y,na.rm=TRUE),
            Err_Pad=Desv_Pad/sqrt(N),
            Mínimo=min(Y,na.rm=TRUE),
            Máximo=max(Y,na.rm=TRUE),
            Q1=quantile(Y,0.25,na.rm=TRUE),
            Q3=quantile(Y,0.75,na.rm=TRUE),
            DIQ = IQR(Y,na.rm=TRUE),
            Coef_Var = 100*Desv_Pad/Média,
            Coef_Assim = agricolae::skewness(Y),
            Coef_Curt = agricolae::kurtosis(Y),
            Normalidade = p_shapiro(Y)
  ) %>% 
  View()

# Distribuição
## Histograma
dados %>% 
  ggplot(aes(x=Y,y=..density..)) +
  geom_histogram(bins=7,color="black",fill="lightgray") +
  labs(x="Peso (g)",y="Densidade de frequência")+
  theme_classic()+
  geom_density(color="red",fill="blue",alpha=0.1)

# Plotando as densidades para cada tratamentos
dados %>% 
  ggplot(aes(x=Y,y=..density..,color=as.factor(Trat))) +
  geom_freqpoly(bins=5)

# Fazer o boxplot da variável produção.
dados %>% 
  ggplot(aes(y=Y))+
  geom_boxplot(fill="orange") +
  coord_cartesian(xlim=c(-1,1))

# Boxplot por tratamento 
dados %>% 
  ggplot(aes(y=Y,x=Trat,fill=as.factor(Trat)))+
  geom_boxplot()

# reordenado
dados %>% 
  ggplot(aes(y=Y,x=reorder(Trat,Y,mean),
             fill=as.factor(Trat)))+
  geom_boxplot() +
  coord_flip()

# Violinplot
dados %>% 
  ggplot(aes(y=Y,x=reorder(Trat,Y,mean),
             fill=as.factor(Trat)))+
  geom_violin(trim = FALSE) +
  stat_summary(fun=mean, geom="point", size=4,
               shape=21,fill="gray")+
  theme(legend.position = "none")

#dotplot
dados %>% 
  ggplot(aes(y=Y,x=reorder(Trat,Y,mean)))+
  geom_violin(trim = FALSE) +
  theme(legend.position = "none")+
  geom_dotplot(binaxis = "y",stackdir = "center",bins=2,
               dotsize = .5)

# Violin e Boxplot
dados %>% 
  ggplot(aes(y=Y,x=as.factor(Trat),fill=as.factor(Trat)))+
  geom_violin(trim = FALSE)+
  geom_boxplot(width=0.05,fill="lightgray") +
  stat_summary(fun=mean, geom="point", size=2,
               shape=21,fill="blue")+
  theme_bw()+
  theme(legend.position = "none")

#QQ plot de produção
dados %>% 
  ggplot(aes(sample=Y)) +
  stat_qq(color="red")+
  stat_qq_line(color="blue")

# Teste de Normalidade dos desvios (erros)
## Criar o modelo da anova
trat <- as.factor(dados$Trat)
y <- dados$y_boxcox
mod <- aov(y~trat)

# Extrair os resíduos para normalide e diagnóstico de 
# outliers
dados<-dados %>% 
  mutate(
    rs_Y = rstudent(mod),
    pred_Y = predict(mod)
  )

## Histograma dos Resíduos
dados %>% 
  ggplot(aes(x=rs_Y,y=..density..))+
  geom_histogram(fill="gray",color="black",bins=10)

## QQ plot dos resíduos
dados %>% 
  ggplot(aes(sample=rs_Y)) +
  stat_qq(color="red",size=4)+
  stat_qq_line(color="blue")

## Teste de normalidade dos resíduos
shapiro.test(dados$rs_Y) #Shapiro-Wilk
lillie.test(dados$rs_Y) #Kolmogorov-Smirnov
ad.test(dados$rs_Y) #Anderson-Darling
cvm.test(dados$rs_Y) #Cramer-von Mises
### Concluímos que os resíduos não tem distribuição normal.

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
  ggplot(aes(x=pred_Y,y=rs_Y)) +
  geom_point(shape=21,color="black",fill="red",size=4)+
  theme_classic()+
  coord_cartesian(ylim=c(-4,4))+
  geom_hline(yintercept = c(-3,3),color="blue",
             linetype=2)

# Ordernar os dados por rs_Y
dados %>% 
  arrange(rs_Y) %>% 
  View()

# Substituir o valor
dados[2,3] <-2477.00000 # 2 é a linha e 3 a coluna

# Homogeneidade das Variâncias (homocedasticidade)
## 4 testes são empregados, teste de Levene, Brown-Forsythe
## Bartlett e Box-Cox
## Teste Clássio de Levene
levene.test(y,trat,location = "mean") ## pacote lawstat

## Teste de Brown-Forsythe
levene.test(y,trat) ## pacote lawstat

## Teste de Bartlett (TRANSFORMAÇÃO)
bartlett.test(y,trat)
# Classificar a falta de homocedasticidade, ou seja, a
# HETEROCEDASTICIDADE 
### Heterocedasticidade Regular: existe uma relação entre
### a média e a variância podemos utilizar a transformação
### y_t <- y^(1-b/2), onde b é o coeficiente angular do 
### modelo de regressão linear entre o log da variância e 
### o log da média, para cada tratamento.
dados %>% 
  group_by(Trat) %>% 
  summarise(
    log_media = log(mean(Y)),
    log_var = log(var(Y))
  ) %>% 
  ggplot(aes(x=log_media,y=log_var))+
  geom_point()+
  geom_smooth(method = "lm")

da <- dados %>% 
  group_by(Trat) %>% 
  summarise(
    log_media = log(mean(Y)),
    log_var = log(var(Y))
  )
reg<-lm(da$log_var~da$log_media)
(resumo<-summary.lm(reg))
# Rejeitamos H0 ao nível de 5% de probabilidade, e concluímos
# que existe uma relação linear entre o log_media e o 
# log_var, portanto, a HETEROCEDASTICIDADE É REGULAR, podemos
# fazer a transformação y_t=y^(1-b/2).
# SOMENTE NO CASO DE HETEROCEDASTICIDADE REGULAR
# SE A HETEROCIDASTICIDADE FOR IRREGULAR recomenda-se testes
# não paramétricos.
b <- resumo$coefficients[2]
dados <- dados %>% 
            mutate(
              y_tb = Y^(1-b/2)
            )

## Teste de Box-Cox (TRASFORMAÇÃO)
BOXCOX<-boxcox(mod,seq(-1,5,1))
(BOXCOX<-data.frame(BOXCOX))
## Critério
### se lambda não difere de 1 => Homocedasticidade
### se lambda difere de 1 (transformação nos dados)
### se lambda difere de zero y_t = (y^lambda-1)/lambda
### se lambda não difere de zero y_t =log(y)

# Buscar o maior valor de lambda
BOXCOX %>% 
  arrange(desc(y)) %>% 
  View()
## Ou
lambda <- BOXCOX$x[BOXCOX$y == max(BOXCOX$y)]
dados <- dados %>% 
          mutate(
            y_boxcox = (Y^lambda-1)/lambda
          )

# Realizar a análise de variância
trat <- as.factor(dados$Trat)
y <- dados$y_boxcox
dic(trat,y,quali=TRUE,mcomp="tukey",sigF=0.05,sigT=0.05)