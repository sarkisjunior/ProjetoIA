base = read.csv('GSSvocab.csv')

summary(base)

base$X = NULL
base$year = NULL

base$vocab = ifelse(is.na(base$vocab), mean(base$vocab, na.rm = TRUE), base$vocab) 
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)
base$educ = ifelse(is.na(base$educ), mean(base$educ, na.rm = TRUE), base$educ)

base[, 6] = scale(base[,6])
base[, 7] = scale(base[,7])

library(caTools)


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
divisao = sample.split(base$vocab, SplitRatio = 0.95)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)


library(h2o)

h2o.init(nthreads = -1)

classificador = h2o.deeplearning(y = 'vocab', training_frame = as.h2o(base_treinamento), activation = 'Rectifier', hidden = c(160, 160), epochs = 1000)

previsao = h2o.predict(classificador, newdata = as.h2o(base_teste[-5]))

previsao = (previsao > 0.5)

previsao = as.vector(previsao)

library(caret)
confusionMatrix(matriz_confusao)

# ZeroR 
table(base_teste$vocab)