# Entrada de dados txt (internet)
URL <- "https://raw.githubusercontent.com/arpanosso/r_data_science_fcav/master/dados/geomorfologia.txt"
geomorfologia <- read.table(URL, h=TRUE)
View(geomorfologia)

# Tipos de objetos
# vetor
x <- 34
y <- c(34, 45, 28, 27)

is.vector(geomorfologia)
is.vector(x)
is.vector(y)
length(y)
y[2]
is.vector(y[2])
y[2]/5
plot(y)

# Data frame - 
is.data.frame(geomorfologia)
length(geomorfologia)
geomorfologia[11]
is.vector(geomorfologia[11])
geomorfologia[11] * 4
plot(geomorfologia[11])
is.vector(geomorfologia$ARGILA)
plot(geomorfologia$ARGILA) # Acesso à variável
plot(geomorfologia$ARGILA,geomorfologia$P)

# Funções para coletar informação
class(geomorfologia)
class(y)

mode(geomorfologia)
mode(y)

typeof(geomorfologia)
typeof(y)



# Tipos de dados
# String ou Caracteres
"FCAV - UNESP"
'Pode usar aspas simples'

# Numérico
-5
6
7L
pi
6.976

# Lógico
TRUE
FALSE

vl<-c(TRUE,TRUE,FALSE,TRUE)
sum(vl)
mean(vl)

length(vl) - sum(vl)

# Tabela verdade da conjunção (E)
TRUE & TRUE
TRUE & FALSE
FALSE & TRUE
FALSE & FALSE

# Tabela verdade da disjunção (OU)
TRUE | TRUE
TRUE | FALSE
FALSE | TRUE
FALSE | FALSE

# Tabela da negação (NÃO)
!TRUE
!FALSE

sum(vl) # soma dos verdadeiros
sum(!vl)

# Coletando informações de um base de dados
str(geomorfologia)
skimr::skim(geomorfologia)
dplyr::glimpse(geomorfologia)

# Valores Faltantes NA
NA # valor faltante
4*4
NA * 4
NA + 1
NA / 7

y[3] <- NA
y
sum(y, na.rm = TRUE)
mean(y, na.rm = TRUE)
sd(y, na.rm = TRUE)

idade1 <- NA
idade2 <- NA

is.na(idade1)
is.na(idade2)
is.na(y)

# Como encontro o número de NAs em um vetor?
sum(is.na(y))
mean(is.na(y))

# Como encontro o número de não NAs em um vetor?
sum(!is.na(y))

# Média para cada coluna de um data frame
dim(y) 
dim(geomorfologia)
apply(geomorfologia[5:22], 2, mean) # 1 para linhas e 2 para colunas

# vamos criar um banco de dados auxiliar
da <- geomorfologia
da[c(17,58,100),11] <- NA

# Contar o número de NAs e cana coluna do data frame auxiliar
sum(is.na(da)) # tenho 3 mas não identifico a coluna...
sum(is.na.data.frame(da)) # quais a coluna??

# vamos criar uma função para contar NAs
cont_na <- function( vetor ){
  sum(is.na( vetor ))
}
cont_na(y)
w <- c(NA, 4, 8, NA, 7)
cont_na(w)

# vamos aplicar a função ao data frame
apply(da[5:22], 2, cont_na)

# Entrada de dados valores faltantes
caminho <- "dados/exemploNA.txt"
exemploNA <- read.table(caminho,
                        h = TRUE,
                        sep = ";",
                        na.strings = ".")
exemploNA
str(exemploNA)

# Operadores aritméticos
1+1
2-3
4/2
2*1
2^10
2**10
3 %/% 2
23 %/% 5
23 %% 5
23/5 - 23 %/%5


# Operadores Relacionais
5 > 4
5 >= 25/5
4 < 2
4 <= 8/2
5 == 5
5 != 5

TRUE == FALSE
FALSE == FALSE

"A MAIS BONITA" == "a mais bonita"
"A MAIS BONITA" == "A MAIS BONITA"

# Todos os valores de argila maiores q 10%
filtro <- geomorfologia$ARGILA > 10
str(filtro)
geomorfologia[filtro, 11 ]

# Todas as variáveis para a SUP == "I"
filtro_1 <- geomorfologia$SUP == "I"
geomorfologia[filtro_1, ]

# Vamos selecionar somente os solos do tipo LV
filtro_2 <- geomorfologia$Solo == "LV"
geomorfologia[filtro_2, c(2,11,20) ]

# Vamos selecionar somente os solos do tipo LV com teor de 
# argila maiores ou iguais a 20%
filtro_3 <- geomorfologia$ARGILA >= 20 & geomorfologia$Solo == "LV"
geomorfologia[filtro_3,]


# Criar a variável ARGILA+SILTE
geomorfologia$ARG_SIL <- geomorfologia$ARGILA + geomorfologia$SILTE

# Passar a T (CTC) para a escala logarítmica
geomorfologia$CTC_log <- log10(geomorfologia$T)

# Classificar a CTC média (>=15) e baixa (<15)
geomorfologia$Class_T <- ifelse(geomorfologia$T < 15, "baixa", "média")

# Classificar a CTC média (>=15) e baixa (<15 e >=6) e muito baixa (<6)
geomorfologia$Class_T2 <- ifelse(geomorfologia$T < 6, "muito baixa", 
                                ifelse(geomorfologia$T <=15,"baixa","média"))

# buscar os nomes da colunas de um Data Frame
names(geomorfologia)

# vamos eliminar as 4 últimas colunas
geomorfologia<-geomorfologia[ -c(23,24,25,26) ]

# criando um novo banco de dados
da <- geomorfologia[c(1:2,14:22)]

# carregar o pacote para escrever em excel
library(writexl)
write_xlsx(da, "dados/da.xlsx")

# escrever com a write.table
write.table(da,"dados/da.xls",quote = FALSE,
            sep="\t")

# Ordenar o banco de dados por teor de aargila do menor para o
# maior
filtro_4 <- order(geomorfologia$ARGILA)
geomorfologia[filtro_4, ]

# Ordenar o banco de dados por teor de argila do maior para o
# menor

filtro_5 <- order(geomorfologia$ARGILA, decreasing = TRUE)
da <- geomorfologia[filtro_5, ]





