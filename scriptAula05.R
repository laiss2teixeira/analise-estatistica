# Aula 05 - SEXTA FEIRA
# 5 tratamentos e 4 blocos = 4 X 5 = 20 parcelas
# Como fazer um sorteio aleatório dessas 20 parcelas.
library(agricolae)
trt <-c("T1","T2","T3","T4","T5")
r <- c(4,4,4,4,4)
sorteio <-design.crd(trt,r,serie=2,seed=2543)
saida1<-sorteio
saida1

# Ou
sample(1:20,20,replace = FALSE)

# Ou
TRAT <- gl(5,4)
REP <- gl(4,1,20)
set.seed(1235)
rnd<-sample(1:20,20,replace = FALSE)
da<-data.frame(TRAT,REP,rnd)
filtro <- order(da$rnd)
da[filtro,]

# FATORIAL em DIC
# FATOR 1 (T, PD, PC, ILPF)
# FATOR 2 (P1, P2, P3)
# TRAT (T_P1, T_P2, T_P3,...,ILPF_P3)
# REP (5)
# número de parcelas experimentais 4 x 3 x 5 = 60
fator1 <- gl(4,15,labels = c("T", "PD", "PC", "ILPF"))
fator2 <- gl(3,5,60, labels = c("P1","P2","P3"))
trat <- as.factor(paste(fator1,fator2,sep = "_"))
rep <- gl(5,1,60)
set.seed(1235)
rnd<-sample(1:60,60, replace = FALSE)

da<-data.frame(fator1,fator2,trat,rep,rnd)
filtro <- order(da$rnd)
da<-da[filtro,]
filtro2 <- order(da$rep)
da<-da[filtro2,]

matrix(da$trat,byrow=FALSE,ncol=5)


# Exercício CAP 6
# 1)
ex1 <- function(x,y){
  cat("digite o primeiro número\n")
  x1 <- readLines(n=1)
  cat("digite o sgundo número\n")
  x2 <- readLines(n=1)
  return(ifelse(x1>x2,sprintf("Maior valor é %s",x1),
                sprintf("Maior valor é %s",x2)))
}
ex1()

# ou
compara_numero <- function(x,y){
  if(x>y) return(x)
  else return(y)
}
compara_numero(8,17)

# 5)
maior_menor <- function(x,y,z){
  maior <- max(c(x,y,z))
  menor <- min(c(x,y,z))
#  c(MIN=menor, MAX=maior) 
  return( paste(menor,maior) )
}
maior_menor(7000,50000,100)

range(c(5,8,7))

# 13) Construa um algoritmo que verifique se o número fornecido pelo 
# usuário (inteiro maior que 1) é primo ou não (números primos são 
#os números naturais que têm apenas dois divisores o 1 e ele mesmo, 
#exemplo (2, 3, 5, 7, 11, 13, 17…).

primo <- function(x){
  if(sum(x %% 1:x == 0) == 2) {
    print("Número primo")
  }else{
    print("Número não é primo")
  }
}
primo(17)
primo(11)
# 7)
f1 <- function(a=1,b=2,c=3){
  delta <- b^2-4*a*c
  if(delta > 0){
    x1 = (-b+sqrt(delta))/(2*a)
    x2 = (-b-sqrt(delta))/(2*a)
    print(paste("x1=",x1,"; x2=",x2))
  }else{
    if(delta == 0) {
      x = -b/(2*a)
      print(paste("x=",x))
    }else{
      print("As raízes são imaginárias")
    }
  }
}
f1(2,0,-1)
f1(1,2,3)
f1(1,5,-6)


# 12
i <- 1:10
i2 <- i^2
coef <- rep(c(1,-1),5)
sum(i*coef/i2)

# ou
soma <- 0
for(i in 1:10){
  if(i%%2 == 0){
    soma = soma - i/(i^2)
  }else{
    soma = soma + i/(i^2)
  }
};soma

# ou
SOMA <- 0
for(i in 1:10){
  SOMA <- SOMA + (-1)^(i+1)*i/(i^2)
  print(SOMA)
};SOMA


# 11) Escreva um programa no qual o usuário digite dois números e o 
# programa deve apresentar todos os números inteiros entre esses 
# dois números. Se os números forem iguais, o programa deve exibir 
#uma mensagem dizendo para o usuário digitar dois números inteiros 
#diferentes. Se o usuário digitar núemros reais, o programa deve
# retornar somente os inteiros...
cria_valor_entre <- function(x,y){
  x<-trunc(x)
  y<-y%/%1
  if(x != y){
      resposta <- x:y
      return(resposta)
  }else{
    print("Digitar números inteiros diferentes")
  }
}
cria_valor_entre(-11.5,8.66)
  
require(tidyverse)  
dados<-read.csv2("dados/cvd_04022021.csv")
str(dados)

# Visualização de dados
# entrar com os dados de COVID19 - https://covid.saude.gov.br/
dados_covid <- read.csv2("dados/HIST_PAINEL_COVIDBR_04fev2021.csv")
str(dados_covid)

# Observa a coluna regição
levels(as.factor(dados_covid$regiao))

# Manipular datas...
# "2021-02-05"
dados_covid$DATA <- as.POSIXlt(dados_covid$data)
str(dados_covid)

library(tidyverse)
# Filtrando apenas a região BRASIL, ou seja todos os casos do país
dados_covid %>% 
  filter(regiao == "Brasil") %>% 
  View()

# A partir dos dados do Brasil, criar a variável porcentagem
dados_covid %>% 
  mutate(porcentagem = casosAcumulado/210147125*100) %>% 
  filter(regiao=="Brasil") %>% 
  View()

# Fazer um gráfico de pontos entre o número de novos casos e o tempo
dados_covid <- dados_covid %>% 
  mutate(tempo = as.POSIXlt(data),
         dia = difftime(tempo,"2020-01-01"), units ="days")

# Gráfico de pontos e linhas
dados_covid %>% 
  filter(regiao == "Brasil") %>% 
  ggplot( aes(x=dia,y=casosNovos) )  + 
  geom_point(color="red",size=3,shape=21,fill="green") +
  geom_line(color="blue")+
  geom_smooth(color="orange")

# Gráfico de Barras
dados_covid %>% 
  filter(estado=="PA", municipio=="BelÃ©m") %>% 
  ggplot( aes(x=dia,y=casosNovos) ) +
  geom_col(color="orange",fill="blue")+
  coord_cartesian(xlim = c(150,180))

dados_covid %>% 
  filter(estado=="PA") %>% 
  filter(str_detect(municipio,"^Be")) %>% 
  select(municipio) %>% 
  View()


