# -------------------- Essenciais -------------------- #
base = read.csv('GSSvocab.csv')

summary(base)
table(base$ageGroup)
base$X = NULL
base$year = NULL

#base$vocab = ifelse(base$vocab <= mean(base$vocab, na.rm = TRUE), 1, 0) # USAR ESSA LINHA PARA OBTER "sem pré-processamento"
# Tomando como partido esse experimento uma prova, se a nota for maior que a media entao ela foi aprovada (1),
# se nao foi maior q 5, reprovada (0).
#base$vocab = factor(base$vocab , levels = c(0,1), labels = c(0,1) ) # USAR ESSA LINHA PARA OBTER "sem pré-processamento"
#base = na.omit(base) # USAR ESSA LINHA PARA OBTER "sem pré-processamento"
# -------------------- Biblioteca -------------------- #
#install.packages("caTools")
library(caTools)
#install.packages("randomForest")
library(randomForest)
#install.packages("caret")
library(caret)
# -------------------- Biblioteca -------------------- #

# -------------------- Essenciais -------------------- #

# ---------------- Pré-processamento ---------------- #
base$vocab = ifelse(is.na(base$vocab), mean(base$vocab, na.rm = TRUE), base$vocab) 
base$age = ifelse(is.na(base$age), mean(base$age, na.rm = TRUE), base$age)
base$educ = ifelse(is.na(base$educ), mean(base$educ, na.rm = TRUE), base$educ)
base$nativeBorn = ifelse(is.na(base$nativeBorn), 'yes', base$nativeBorn)
base$ageGroup = ifelse(is.na(base$ageGroup), '60+', base$ageGroup)
base$educGroup = ifelse(is.na(base$educGroup),'12 yrs', base$educGroup)

base[, 6] = scale(base[,6])
base[, 7] = scale(base[,7])

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
# 0 == no, 1 == yes, 1== NA

#table(base$ageGroup)
base$ageGroup = factor(base$ageGroup, levels =unique(base$ageGroup), labels = c(1,2,3,4,5))

#table(base$educGroup)
base$educGroup = factor(base$educGroup, levels = unique(base$educGroup), labels = c(1,2,3,4,5))

# ---------------- Pré-processamento ---------------- #

# ------------------ RandomForest ------------------ #
set.seed(1)
divisao = sample.split(base$vocab, SplitRatio = 0.85)
base_treinamento = subset(base, divisao == TRUE)
base_teste = subset(base, divisao == FALSE)

#set.seed(1)
c = rep(0,200)

for(i in 1:200) {
    classificador = randomForest(x = base_treinamento[-5], y = base_treinamento$vocab, ntree = i)
    previsao = predict(classificador, newdata = base_teste[-5], type = 'class')
    matriz_confusao = table(base_teste[,5], previsao)
    print(matriz_confusao)

    cm = confusionMatrix(matriz_confusao)
    print(confusionMatrix(matriz_confusao))
    conf_Matrix <- confusionMatrix(matriz_confusao)
    print(conf_Matrix)
    c[i] = cm$overall['Accuracy']
}

plot(c, type = 'l')

# Accuracy : 0.6968 - Randon Forest– inconsistentes + faltantes + escalonamento
# Accuracy : 0.6972 - Randon Forest – inconsistentes + faltantes
# Accuracy : 0.6950 - Randon Forest – faltantes (não é possivel fazer sem retirar os faltantes)
# ------------------ RandomForest ------------------ #