base = read.csv('GSSvocab.csv')

summary(base)

base$X = NULL
base$year = NULL

base = na.omit(base)

base[, 6] = scale(base[,6])
base[, 7] = scale(base[,7])

library(caTools)
library(e1071)

base$vocab = ifelse(base$vocab < mean(base$vocab), 1, 0)
# Tomando como partido esse experimento uma prova, se a nota for maior que a media entao ela foi aprovada (1),
# se nao foi maior q 5, reprovada (0).
table(base$vocab)
base$vocab = factor(base$vocab , levels = c(0,1), labels = c(0,1) )

#table(base$gender)
base$gender = factor(base$gender, levels = c('female', 'male'), labels = c(0,1) )
# 0 == female, 1 == male

#table(base$nativeBorn)
base$nativeBorn = factor(base$nativeBorn, levels = c('no', 'yes'), labels = c(0,1) )
# 0 == no, 1 == yes

#table(base$ageGroup)
base$ageGroup = factor(base$ageGroup, levels =unique(base$ageGroup), labels = c(1,2,3,4,5))


set.seed(1)
divisao = sample.split(base$vocab, SplitRatio = 0.98)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

classificador = naiveBayes(x = base_treinamento[-5], y = base_treinamento$vocab)
print(classificador)


previsao = predict(classificador, newdata = base_teste[-5])
print(previsao)

matriz_confusao = table(base_teste[, 5],previsao)

print(matriz_confusao)

library(caret)
confusionMatrix(matriz_confusao)
# ----------------------------------------------------------

base = read.csv('GSSvocab.csv')
library(rpart)

classificador = rpart(formula = vocab ~., data = base)
classificador = rpart(formula = vocab ~., data = base, control = rpart.control(minbucket = 1))
print(classificador)
plot(classificador)
text(classificador)

library(rpart.plot)
rpart.plot(classificador)


previsao = predict(classificador, newdata = base[,-5])
print(previsao)

printcp(classificador)
print(classificador$cptable)

classificador$cptable[which.min(classificador$cptable[, "xerror"]),"CP"]

poda = classificador$cptable[which.min(classificador$cptable[, "xerror"]),"CP"]

plotcp(classificador)


prune(classificador,poda)
classificador = prune(classificador, 0.01)

library(RColorBrewer)
rpart.plot(classificador)
previsao = predict(classificador, newdata = base_teste[, -5], type = 'class')
matriz_confusao = table(base_teste[, 5], previsao)
print(matriz_confusao)

library(lattice)
library(ggplot2)
library(caret)
confusionMatrix(matriz_confusao)