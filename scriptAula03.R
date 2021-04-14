# Script Aula 03
# Entrada de dados txt (internet)
URL <- "https://raw.githubusercontent.com/arpanosso/r_data_science_fcav/master/dados/geomorfologia.txt"
geomorfologia <- read.table(URL, h=TRUE)
View(geomorfologia)

# Visualização de dados
# Boxplot da variável ARGILA
boxplot(geomorfologia$ARGILA,
        col="orange",
        ylab="Teor de Argila do solo (%)",
        ylim=c(0,30),
        las=1,
        main="Boxplot")
y<-summary(geomorfologia$ARGILA)
y<-y[-4]
text(1+.28,y,round(y,2))

# Separando o conjunto de dados, acima e abaixo da mediana
limite <- y[3]
geomorfologia$CLASS <- ifelse(geomorfologia$ARGILA <= limite,
                              "Classe 1","Classe 2")


str(geomorfologia)

# FATOR - dados categóricos, tem o atributo de níveis.
geomorfologia$SUP <- as.factor(geomorfologia$SUP)
geomorfologia$Solo <- as.factor(geomorfologia$Solo)
str(geomorfologia)

plot(geomorfologia$ARGILA ~ geomorfologia$SUP )
plot(geomorfologia$P ~ geomorfologia$SUP )

plot(geomorfologia$P ~ geomorfologia$Solo )

# Modelagem de geomorfologia.
# Correlação
matriz_corre <- cor(geomorfologia[5:22])

# Mapa
library(corrplot)
corrplot(matriz_corre, method = "number")

# Estudar a correlação entre V e SB
plot(geomorfologia$V,geomorfologia$SB)

cor.test(geomorfologia$V,geomorfologia$SB)
mod <- lm(geomorfologia$SB ~ geomorfologia$V)
summary.lm(mod)
plot(geomorfologia$V,geomorfologia$SB,pch=16,col="blue")
abline(mod,col="red",lwd=2,lty=2)
text(40,23, "R² = 0,38",cex=1.3)
text(40,25, "SB = -3,303 + 0,1534 V",cex=1.3)
text(20,15, expression(paste(R^2,"=0,38")),cex=1.3)

# Exemplo de um curva de distribuição normal padronizada
curve(dnorm(x),-3,3,col="red",lwd=2)
text(2,0.3,expression(paste("N(",mu,"=0, ",sigma,"=1)")))
abline(v=0,col="darkgreen")

# histograma de ARGILA
hist(geomorfologia$ARGILA, prob=TRUE, xlim=c(0,30))
media <- mean(geomorfologia$ARGILA)
dp <- sd(geomorfologia$ARGILA)
curve(dnorm(x,media,dp),col="red",add=TRUE,lwd=2)


# Teste de normalidade para superficie I
filtro <- geomorfologia$SUP == "I"
da<-geomorfologia[filtro,]
hist(da$ARGILA, prob=TRUE)
media <- mean(da$ARGILA)
dp <- sd(da$ARGILA)
curve(dnorm(x,media,dp),col="red",add=TRUE,lwd=2)
shapiro.test(da$ARGILA)

# Teste de normalidade para superficie II
filtro <- geomorfologia$SUP == "II"
da<-geomorfologia[filtro,]
hist(da$ARGILA, prob=TRUE)
media <- mean(da$ARGILA)
dp <- sd(da$ARGILA)
curve(dnorm(x,media,dp),col="blue",add=TRUE,lwd=2)
shapiro.test(da$ARGILA)

# Teste de normalidade para superficie III
filtro <- geomorfologia$SUP == "III"
da<-geomorfologia[filtro,]
hist(da$ARGILA, prob=TRUE)
media <- mean(da$ARGILA)
dp <- sd(da$ARGILA)
curve(dnorm(x,media,dp),col="orange",add=TRUE,lwd=2)
shapiro.test(da$ARGILA)



