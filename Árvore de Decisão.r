# -------------------- Essenciais -------------------- #
base = read.csv('GSSvocab.csv')

base$X = NULL
base$year = NULL

base$vocab = ifelse(base$vocab < mean(base$vocab, na.rm = TRUE), 1, 0) # USAR ESSA LINHA PARA OBTER "sem pré-processamento"
# Tomando como partido esse experimento uma prova, se a nota for maior que a media entao ela foi aprovada (1),
# se nao foi maior q 5, reprovada (0).
#table(base$vocab)
base$vocab = factor(base$vocab , levels = unique(base$vocab), labels = c(0,1) ) # USAR ESSA LINHA PARA OBTER "sem pré-processamento"
# -------------------- Biblioteca -------------------- #
#install.packages("caTools")
library(caTools)
#install.packages("rpart")
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#install.packages("lattice")
library(lattice)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("caret")
library(caret)
# -------------------- Biblioteca -------------------- #

# -------------------- Essenciais -------------------- #

# ---------------- Pré-processamento ---------------- #

base$vocab = ifelse(is.na(base$vocab), mean(base$vocab, na.rm = TRUE), base$vocab) 
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)
base$educ = ifelse(is.na(base$educ), mean(base$educ, na.rm = TRUE), base$educ)

base[, 6] = scale(base[,6])
base[, 7] = scale(base[,7])

base$vocab = ifelse(base$vocab < mean(base$vocab), 1, 0) 
# Tomando como partido esse experimento uma prova, se a nota for maior que a media entao ela foi aprovada (1),
# se nao foi maior q 5, reprovada (0).
#table(base$vocab)
base$vocab = factor(base$vocab , levels = unique(base$vocab), labels = c(0,1) )

#table(base$gender)
base$gender = factor(base$gender, levels = c('female', 'male'), labels = c(0,1) )
# 0 == female, 1 == male

#table(base$nativeBorn)
base$nativeBorn = factor(base$nativeBorn, levels = c('no', 'yes'), labels = c(0,1) )
# 0 == no, 1 == yes

#table(base$ageGroup)
base$ageGroup = factor(base$ageGroup, levels =unique(base$ageGroup), labels = c(1,2,3,4,5))

# ---------------- Pré-processamento ---------------- #

# ---------------- Árvore de Decisão ---------------- #
set.seed(1)
divisao = sample.split(base$vocab, SplitRatio = 0.85)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = rpart(formula = vocab ~., data = base_treinamento)
classificador = rpart(formula = vocab ~., data = base_treinamento, control = rpart.control(cp = 0))
#print(classificador)
plot(classificador)
text(classificador)

rpart.plot(classificador)

previsao = predict(classificador, newdata = base_teste[,-5])
#print(previsao)

#printcp(classificador)
#print(classificador$cptable)

classificador$cptable[which.min(classificador$cptable[, "xerror"]),"CP"]

poda = classificador$cptable[which.min(classificador$cptable[, "xerror"]),"CP"]

plotcp(classificador)

#prune(classificador,poda)
classificador = prune(classificador, poda)

rpart.plot(classificador)
previsao = predict(classificador, newdata = base_teste[, -5], type = 'class')
matriz_confusao = table(base_teste[, 5], previsao)
#print(matriz_confusao)

confusionMatrix(matriz_confusao)

# Accuracy : 0.6942 - Árvore de Decisão – inconsistentes + faltantes + escalonamento
# Accuracy : 0.6935 - Árvore de Decisão – inconsistentes + faltantes
# Accuracy : 0.7056 - Árvore de Decisão – sem pré-processamento (Apenas na coluna "vocab")
# ---------------- Árvore de Decisão ---------------- #