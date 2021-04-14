# Composição de Fatores
vetor_1 <- c(1,1,1,2,2,2,3,3,3,4,4,4)
rep(1,3)
vetor_2 <- c(rep(1,3),rep(2,3),rep(3,3),rep(4,3))  
vetor_1 == vetor_2

typeof(vetor_1)
class(vetor_1)
mode(vetor_1)

vetor_2 <- as.factor(vetor_2)
typeof(vetor_2)
class(vetor_2)
mode(vetor_2)
vetor_2
levels(vetor_2)

# Tratamentos de um experimentos X(5) trat e Y(10) rep
# Níveis do Fator - Qualitativos
# 1 - Testemunha (Mata)
# 2 - Plantio Direto - (PD)
# 3 - Convencional - (PC)
# 4 - Integração Lavou Pecuária (ILP)
# 5 - Integração Lavou Pecuária Floresta (ILPF)
trat <- gl(5, 10, labels= c("Mata","PD","PC","ILP","ILPF"))
trat

# A partir da função gl, criem o fator Blocos
# I, II, III, IV, V, VI, VII, VIII, IX, X
bloco <- gl(10,1,(5*10))
data.frame(trat,bloco)

# Vamos simular um experimento fatorial.
# FATOR 1: Espaçamento  (20,45,60)
# FATOR 2: Profundidade (1,2,3,4,5,6)
# BLOCOS: 5 blocos
3*6*5

fator1 <- gl(3,30,labels = c("20","45","60"))
fator2 <- gl(6,5,90,labels = c("0-10","10-20","20-30","30-40","40-50","50-60"))
bloco <- gl(5,1,90,labels = c("I","II","III","IV","V"))
data.frame(fator1,fator2,bloco)

# Para 
for(i in 1:10) {
  print(i)
}

# Imprimir de 20 a 50 de 2 em 2
for(i in seq(20,50,2)){
  print(i)
}

# ou
for(i in 20:50) if(i%%2 == 0) print(i)

# Carregar a base de dados geomorfologia
# nome da variável + 4 estatisticas
# média - mean()
# mediana - median()
# desvio padrão - sd()
# cv - CV=100*DP/media

# Bom trabalho...
# Entrada de dados txt (internet)
URL <- "https://raw.githubusercontent.com/arpanosso/r_data_science_fcav/master/dados/geomorfologia.txt"
geomorfologia <- read.table(URL, h=TRUE)
View(geomorfologia)

# Resolvendo com o FOR
for( i in 5:22) {
  # entrando com os dados
  vya <- geomorfologia[,i]
  
  # processamento
  media <- mean(vya)
  mediana <- median(vya)
  dp <- sd(vya)
  cv <- 100*dp/media
  
  #saída
  print(names(geomorfologia[i]))
  print(paste("Média = ", round(media,4) ))
  print(paste("Mediana = ", round(mediana,4) ))
  print(paste("Desvio Padrão = ", round(dp,4) ))
  print(paste("Coef. Var. = ", round(cv,4) ))
}


# Resolvendo sem o for
cv <- function(x) 100*sd(x)/mean(x)
apply(geomorfologia[5:22], 2, mean)
apply(geomorfologia[5:22], 2, median)
apply(geomorfologia[5:22], 2, sd)
apply(geomorfologia[5:22], 2, cv)

# Usando uma função e estatística MIN, MAX, q1, q3, cofe assim e coef kurt
estat_desc <- function(x){
  media <- mean(x)
  mediana <- median(x)
  dp <- sd(x)
  cv <- 100*dp/media
  MIN <- min(x)
  MAX <- max(x)
  q1 <- quantile(x,0.25)
  q3 <- quantile(x,0.75)
  assi <- agricolae::skewness(x)
  curt <- agricolae::kurtosis(x)
  p_w <- round(shapiro.test(x)$p,4)
  c(Mínimo = MIN, Q1=q1, Média=media, Med=mediana, Q3=q3, Máximo=MAX,
    DesvPad = dp, CV = cv,Assimetria=assi,Curtose=curt, Normalidade=p_w)
}
da <- apply(geomorfologia[5:22], 2, estat_desc)
class(da)
da <- as.data.frame(da)
class(da)

writexl::write_xlsx(da,"dados/estat_desc.xlsx")


# Mais alguns teste de normalidade
nortest::lillie.test(geomorfologia$ARGILA) # Kolmogorov-Smirnov
nortest::ad.test(geomorfologia$ARGILA) # Anderson-Darling
nortest::cvm.test(geomorfologia$ARGILA) # Cramer-von Mises













