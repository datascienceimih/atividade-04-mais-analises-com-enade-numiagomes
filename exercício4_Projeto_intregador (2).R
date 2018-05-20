###########################Atividade 04 do Projeto Integrador#############################

######Cursos de Ciências de Dados do Centro Universitário Metodista Izabela Hendrix.######
####Numiá Freitas Gomes######
####Prof. Neylson Crepalde###

##########################################################################################
#carregando outros pacootes necessários
library(readr)
library(dplyr)
library(descr)
###################Exercício 1

enade14 <- read_csv2("https://raw.githubusercontent.com/neylsoncrepalde/introducao_ao_r/master/dados/enade_2014_amostra.csv")

summary(enade14$nu_idade)
média <- mean(enade14$nu_idade)
média 
mediana <- median(enade14$nu_idade)
mediana
variância <- var(enade14$nu_idade)
variância
desv.p <- sd(enade14$nu_idade)
desv.p
maximo <- max(enade14$nu_idade)
maximo
minimo <- min(enade14$nu_idade)
minimo

#############Exercício 2

freq(enade14$tp_sexo)


enade14$tp_sexo1 = enade14$tp_sexo

enade14$tp_sexo1[enade14$tp_sexo1 == "N"]<- NA

freq(enade14$tp_sexo1)

############Exercício 3

idadeF <- (enade14$nu_idade[enade14$tp_sexo1 == "F"])
idadeM <- (enade14$nu_idade[enade14$tp_sexo1 == "M"])

médiaF <- mean(idadeF, na.rm = T)
médiaF

médiaM <- mean(idadeM, na.rm = T)
médiaM


medianaF <- median(idadeF, na.rm = T)
medianaF

medianaM <- median(idadeM, na.rm = T)
medianaM

maxF <- max(idadeF, na.rm = T)
maxF

maxM <- max(idadeM, na.rm = T)
maxM

minF <- min(idadeF, na.rm = T)
minF

minM <- min(idadeM, na.rm = T)
minM

varF <- var(idadeF, na.rm = T)
varF

varM <- var(idadeM, na.rm = T)
varM

desv.pF <- sd(idadeF, na.rm = T)
desv.pF

desv.pM <- sd(idadeM, na.rm = T)
desv.pM

hist(idadeF,main = "Idade dos participantes do sexo Feminino - Enade 2014",
     xlab = "Idade", ylab = "Quantidade")
boxplot(idadeF,main = "Idade dos participantess do sexo Feminino - Enade 2014")

hist(idadeM,main =  "Idade dos participantes do sexo Masculino - Enade 2014",
     xlab = "Idade", ylab = "Quantidade")
boxplot(idadeM, main =  "Idade dos participantes do sexo Masculino - Enade 2014")

############Exercício 4

branco <- (enade14$nu_idade[enade14$qe_i2 == "a"])
negro <- (enade14$nu_idade[enade14$qe_i2 == "b"])
pardo <- (enade14$nu_idade[enade14$qe_i2 == "c"])
amarelo <- (enade14$nu_idade[enade14$qe_i2 == "d"])
indigena <- (enade14$nu_idade[enade14$qe_i2 == "e"])

médiaBranco <- mean(branco, na.rm = T)
médiaBranco
freq(branco,main = "Frequência de idade para alunos brancos.  - Enade 2014",
     xlab = "Idade", ylab = "Frequência")

médiaNegro <- mean(negro, na.rm = T)
médiaNegro
freq(negro,main = "Frequência de idade para alunos negros.  - Enade 2014",
     xlab = "Idade", ylab = "Frequência")

médiaPardo <- mean(pardo, na.rm = T)
médiaPardo
freq(pardo,main = "Frequência de idade para alunos pardos.  - Enade 2014",
     xlab = "Idade", ylab = "Frequência")

médiaAmarelo <- mean(amarelo, na.rm = T)
médiaAmarelo
freq(amarelo, main = "Frequência de idade para alunos amarelos.  - Enade 2014",
     xlab = "Idade", ylab = "Frequência")

médiaIndigena <- mean(indigena, na.rm = T)
médiaIndigena
freq(indigena,main = "Frequência de idade para alunos indígenas.  - Enade 2014",
     xlab = "Idade", ylab = "Frequência")

medianaBranco <- median(branco, na.rm = T)
medianaBranco

medianaNegro <- median(negro, na.rm = T)
medianaNegro

medianaPardo <- median(pardo, na.rm = T)
medianaPardo

medianaAmarelo <- median(amarelo, na.rm = T)
medianaAmarelo

medianaIndigena <- median(indigena, na.rm = T)
medianaIndigena

maxBranco <- max(branco, na.rm = T)
maxBranco

maxNegro <- max(negro, na.rm = T)
maxNegro

maxPardo <- max(pardo, na.rm = T)
maxPardo

maxAmarelo <- max(amarelo, na.rm = T)
maxAmarelo

maxIndigena <- max(indigena, na.rm = T)
maxIndigena

minBranco <- min(branco, na.rm = T)
minBranco

minNegro <- min(negro, na.rm = T)
minNegro

minPardo <- min(pardo, na.rm = T)
minPardo

minAmerelo <- min(amarelo, na.rm = T)
minAmerelo

minIndigena <- min(indigena, na.rm = T)
minIndigena

varBranco <- var(branco, na.rm = T)
varBranco

varNegro <- var(negro, na.rm = T)
varNegro

varPardo <- var(pardo, na.rm = T)
varPardo

varAmerelo <- var(amarelo, na.rm = T)
varAmerelo

varIndigena <- var(indigena, na.rm = T)
varIndigena

desv.pBranco <- sd(branco, na.rm = T)
desv.pBranco

desv.pNegro <- sd(negro, na.rm = T)
desv.pNegro

desv.pPardo <- sd(pardo, na.rm = T)
desv.pPardo

desv.pAmarelo <- sd(amarelo, na.rm = T)
desv.pAmarelo

desv.pIndigena <- sd(indigena, na.rm = T)
desv.pIndigena



################Exercício 5

freq(enade14$co_regiao_curso)

enade14$co_regiao_curso1 <- enade14$co_regiao_curso
enade14$co_regiao_curso1[enade14$co_regiao_curso1 == 1] = "Norte" 
enade14$co_regiao_curso1[enade14$co_regiao_curso1 == 2] = "Nordeste"
enade14$co_regiao_curso1[enade14$co_regiao_curso1 == 3] = "Sudeste"
enade14$co_regiao_curso1[enade14$co_regiao_curso1 == 4] = "Sul"
enade14$co_regiao_curso1[enade14$co_regiao_curso1 == 5] = "CentroOeste"

freq(enade14$co_regiao_curso1, main = "Frequência de alunos por região do Brasil - Enade 2014",
     xlab = "Região",  ylab = "Frequência")

#############Exercício 6

enade14$qe_i2_1 <- enade14$qe_i2
enade14$qe_i2_1[enade14$qe_i2_1 == "a"] = "Branco" 
enade14$qe_i2_1[enade14$qe_i2_1 == "b"] = "Negro"
enade14$qe_i2_1[enade14$qe_i2_1 == "c"] = "Pardo"
enade14$qe_i2_1[enade14$qe_i2_1 == "d"] = "Amarelo"
enade14$qe_i2_1[enade14$qe_i2_1 == "e"] = "Indígena"

enade14$qe_i8_1 <- enade14$qe_i8
enade14$qe_i8_1[enade14$qe_i8_1 == "a"] = "Até 1,5" 
enade14$qe_i8_1[enade14$qe_i8_1 == "b"] = "De 1,5 a 3"
enade14$qe_i8_1[enade14$qe_i8_1 == "c"] = "De 3 a 4,5"
enade14$qe_i8_1[enade14$qe_i8_1 == "d"] = "De 4,5 a 6"
enade14$qe_i8_1[enade14$qe_i8_1 == "e"] = "De 6 a 10"
enade14$qe_i8_1[enade14$qe_i8_1 == "f"] = "De 10 a 30"
enade14$qe_i8_1[enade14$qe_i8_1 == "g"] = "Maior de 30"


table(enade14$qe_i8_1, enade14$qe_i2_1)

###############################################################################################
###################################### Desafios ###############################################

######Desafio 1

obj <- table(enade14$qe_i8_1, enade14$qe_i2_1)
chisq.test(obj)

##### Desafio 2 
###A hipótese nula é de que as variáveis não estão associadas, em outras palavras, eles são
###independentes. A hipótese alternativa é de que as variáveis estão associadas, ou dependentes.

#Como temos o p-valor menor que o nível de significancia rejeitamos a hipotese nula de que as 
# variáveis não estão associadas, assim podemos dizer que existe associação entre a renda e a cor
#Enade 2014

##### Desafio 3

enade14$pp = enade14$co_catad

enade14$pp[enade14$pp == 93] = "Público"
enade14$pp[enade14$pp == 116] = "Público"
enade14$pp[enade14$pp == 118] = "Privado"
enade14$pp[enade14$pp == 121] = "Privado"
enade14$pp[enade14$pp == 10001] = "Público"
enade14$pp[enade14$pp == 10002] = "Público"
enade14$pp[enade14$pp == 10003] = "Público"
enade14$pp[enade14$pp == 10004] = "Privado"
enade14$pp[enade14$pp == 10005] = "Privado"
enade14$pp[enade14$pp == 10006] = "Privado"
enade14$pp[enade14$pp == 10007] = "Privado" 
enade14$pp[enade14$pp == 10008] = "Privado"
enade14$pp[enade14$pp == 10009] = "Privado"

head(enade14$pp)

PubliPriv <- enade14$pp
head(PubliPriv)

##### Desafio 4
IMIH <- (enade14$nt_ger[enade14$co_ies == "206"])
mean(IMIH, na.rm = T)

