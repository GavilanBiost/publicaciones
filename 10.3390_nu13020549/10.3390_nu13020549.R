#Sintaxis final Satin-VAS-Metabolomica
#Packages:

library(readxl)
library(xlsx)
library(missForest)
library(RNOmni)
library(caTools)
library(glmnet)
library(pROC)
library(rio)
library(haven)
library(agricolae)
library(tidyverse)
library(caret)
library(magrittr)
library(dplyr)

#Formacion de las bases de datos:

#Apertura BBDD bq:
bioquimica_SATIN = read_excel("/Volumes/JES GAVI/ARTICULOS/BBDD/Satin/BBDD limpias/bioquimica SATIN.xlsx")
bioquimica_SATIN = read_excel("D:/ARTICULOS/BBDD/Satin/BBDD limpias/bioquimica SATIN.xlsx")
colnames(bioquimica_SATIN)

#Apertura BBDD general:
Satin_BBDD_completa = read_excel("/Volumes/JES GAVI/ARTICULOS/BBDD/Satin/BBDD limpias/Satin_BBDD_completa.xlsx")
Satin_BBDD_completa = read_excel("D:/ARTICULOS/BBDD/Satin/BBDD limpias/Satin_BBDD_completa.xlsx")
colnames(Satin_BBDD_completa)

#Creacion de la BBDD con met v8, v14:
SATIN1=bioquimica_SATIN[,196:328]
SATIN1 = data.frame(cbind(bioquimica_SATIN[1], SATIN1))
str(SATIN1$allocation_no)

#Creacion de la BBDD con variables de ajuste v8:
SATIN2 = Satin_BBDD_completa[,c("allocation_no", "v8.1_avas_3_.5", "v8.1_avas_3_60", "v8.1_avas_3_120", "v8.1_avas_3_180",
                                "v2_4.3", "v2_4.4", "v8_4.1", "v8_4.2", "v7_10.1")]
str(SATIN2$allocation_no)

#Se estima el AIC del SATIN considerando los tiempos -5, 60, 120, 180 min.

#Trapezoidal Integration

SATIN2[2:5] = SATIN2[2:5]/10

SATIN2 = subset(SATIN2, v8.1_avas_3_.5 > 0) #Se eliminan valores NA. 151

##Se imputan los valores perdidos para no perder pacientes:

set.seed(1)

S2 = missForest(data.frame(SATIN2[2:5]), verbose = T)

S2$OOBerror

SATIN2[2:5] = data.frame(S2$ximp)

colnames(SATIN2)

tiempo = c(0,60,120,180)

SATIN21 = data.frame(t(SATIN2[2:5]))

AICs = 9999

for (i in 1:length(SATIN21)){
  AICs[[i]] =  trapz(tiempo, SATIN21[[i]])
}

SATIN2 = cbind(SATIN2[1], AICs, SATIN2[6:10])

rm(SATIN21, S2, AICs, i, tiempo) #Se eliminan porque ya no son útiles

#Ajuste del AICs en v8 por peso y grupo de intervencion:

####Generacion de variables de perdida de peso:

SATIN2$v2_peso = (SATIN2$v2_4.3 + SATIN2$v2_4.4)/2
SATIN2$v8_peso = (SATIN2$v8_4.1 + SATIN2$v8_4.2)/2

SATIN2$dpeso_8_2 = SATIN2$v8_peso - SATIN2$v2_peso

####Ajuste por minimos cuadrados:

media<-mean(SATIN2$AICs, na.rm=T) #1061.666

# Linear model
modelo<-lm (SATIN2$AICs ~ SATIN2$dpeso_8_2 + as.factor(SATIN2$v7_10.1), na.action=na.exclude)

# Residuals
resid(modelo)

# Adjusted value
cambios <- resid(modelo, na.action=na.exclude) + (media)

#Los participantes 92 y 120 presentan NA en el valor del peso. Se utiliza su valor sin ajustar:

SATIN2$AICs[92] #735
SATIN2$AICs[120] #609

SATIN2$AICs = cambios

colnames(SATIN2)

SATIN2$AICs[92] = 735
SATIN2$AICs[120] = 609

rm(cambios, media)

#Imputacion y normalizacion de la BBDD de metabolomica:

S1 = data.frame(SATIN2$allocation_no)

colnames(S1) = c("allocation_no")

#Se añade el VAS a la BBDD:

SATIN_VAS = merge(S1, SATIN1, by = "allocation_no")

#NAs en cada variables
imp1 = SATIN_VAS[2:134]

na1 = sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
na2 = sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))
na =  cbind("NA"=na1, "% NA"=na2)
na

#Metabolites con NA mayor de 20%
A_NA=rownames(na[na[,2]>20 ,])

#Removemos > 20%
drop = A_NA
imp1 = imp1[,!(names(imp1) %in% drop)]
sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))

#Imputacion con randoforest:

colnames(imp1)

set.seed(1)

imp1 = missForest(imp1, verbose = T)

imp1$OOBerror

imp1 = data.frame(imp1$ximp)

colnames(imp1)

#Normalización Ranknorm.

VAS1 = data.frame(cbind(S1, imp1, SATIN2[2]))

imp1.rankNorm = apply(imp1, 2, rankNorm)

VAS.rankNorm = data.frame(cbind(S1, imp1.rankNorm, SATIN2[2]))

save(VAS.rankNorm, file = "VAS.rankNorm.11032020.rda")

rm(imp1, imp1.rankNorm, na, S1, na1, na2, drop)

#####Modelos de elastic net v8:

#Preparacion de la BBDD:
VAS.rankNorm11 = VAS.rankNorm[-1]

#Split t-v

set.seed(2222)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = VAS.rankNorm11$AICs %>% createDataPartition(p = 0.8, list = F)
  
  train.data[[i]] = VAS.rankNorm11[training,]
  test.data[[i]] = VAS.rankNorm11[-training,]
  
  #Modelo
  
  cv[[i]] = train(AICs ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(AICs~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$AICs #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$AICs), Rsquare = R2(predictions[[i]], test.data[[i]]$AICs), Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$AICs, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$AICs)$estimate)
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha

train_rows = c()
training = c()
validation = c()
modelito = c()
X = c()
Y = c()
X1 = c()
Y1 = c()
pred = c()
met = c()

for (i in 1:10) {
  
  #Particion
  
  train_rows[[i]] = VAS.rankNorm11$AICs %>% createDataPartition(p = 0.80, list = F)
  training[[i]] = VAS.rankNorm11[train_rows[[i]],]
  validation[[i]] = VAS.rankNorm11[-train_rows[[i]],]
  
  a=as.data.frame(training[[i]])
  X[[i]] = as.matrix(a[,2:(dim(a)[2]-1)])
  Y[[i]] = a$AICs
  
  b=as.data.frame(validation[[i]])
  X1[[i]] = as.matrix(b[,2:(dim(a)[2]-1)])
  Y1[[i]] = b$AICs
  
  #Modelo
  
  modelito[[i]] = glmnet(X[[i]], Y[[i]], alpha = alp, lambda = l)
  
  #Predicciones:
  
  pred[[i]] = modelito[[i]] %>% predict(X1[[i]])
  
  met[[i]] = data.frame(RMSE = RMSE(pred[[i]], Y1[[i]]), Rcuadrado = R2(pred[[i]], Y1[[i]]), Pearson.IC = cor.test(pred[[i]], Y1[[i]], conf.level = 0.95)$conf.int,
                        Pearson.cor = cor.test(pred[[i]], Y1[[i]])$estimate)
}

coef_met_min = c()

for (i in 1:10) {
  
  metabolitos.min = coef(modelito[[i]], s = l)
  
  coef.min = metabolitos.min@x
  coef.min = coef.min[-1]
  
  seleccion.min = c(metabolitos.min@i)
  nombres = as.data.frame(colnames(X[[i]]))
  seleccion.min1 = nombres[seleccion.min,]
  seleccion.min1 = as.character(seleccion.min1)
  coef_met_min[[i]] = cbind(seleccion.min1, coef.min)
  
}

(coef.min.ajusta.AUC = print(coef_met_min))

BBDD.met.min = data.frame(colnames(VAS.rankNorm11))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.VAS.rankNorm11."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "11032020.Validacion.ajust.AUC.xlsx")

###############################--------- Validacion interna 
metabolitos = c("v8_Glycine", "v8_SM_32.2", "v8_SM_38.1")
media = c(31.52510626, -6.442208066, -16.95936031)
model.met = data.frame(metabolitos, media)
str(model.met)
model.met$metabolitos = as.character(model.met$metabolitos)

X2 = data.frame(VAS.rankNorm11[,1:(dim(VAS.rankNorm11)[2]-1)])
Y2 = VAS.rankNorm$AICs
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"," +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
  
  correr = cor.test(Y2,modelo, conf.level = .95)
  
} 


####Coef con la BBDD completa y cv:

set.seed(123)

cvfit = c()
coef_met_min = c()

X = data.frame(VAS.rankNorm11[,1:(dim(VAS.rankNorm11)[2]-1)])
Y = VAS.rankNorm$AICs

for (i in 1:10) {
  
  cvfit[[i]] = cv.glmnet(as.matrix(X), as.matrix(Y), family="gaussian", type.measure = "mse", alpha = alp)
  
  metabolitos.min = coef(cvfit[[i]], s = "lambda.min")
  
  coef.min = metabolitos.min@x
  coef.min = coef.min[-1]
  
  seleccion.min = c(metabolitos.min@i)
  nombres = as.data.frame(colnames(X))
  seleccion.min1 = nombres[seleccion.min,]
  seleccion.min1 = as.character(seleccion.min1)
  coef_met_min[[i]] = cbind(seleccion.min1, coef.min)
  
}

BBDD.met.min = data.frame(colnames(VAS.rankNorm11))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.VAS.rankNorm11."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "11032020.coef.full.ajus.xlsx")

save.image(file = "11032020.RData")

####################------Analisis en la visita 14:

SATIN1=bioquimica_SATIN[,345:477]
SATIN1 = data.frame(cbind(bioquimica_SATIN[1], SATIN1))

SATIN2 = Satin_BBDD_completa[,c("allocation_no", "v14.1_avas_3_.5", "v14.1_avas_3_60", "v14.1_avas_3_120", "v14.1_avas_3_180",
                                "v8_4.1", "v8_4.2", "v14_4.1", "v14_4.2", "v7_10.1")]

#Se estima el AIC del SATIN considerando los tiempos -5, 60, 120, 180 min.

#Trapezoidal Integration

SATIN2[2:5] = SATIN2[2:5]/10

SATIN2 = subset(SATIN2, v14.1_avas_3_.5 > 0)
SATIN2 = subset(SATIN2, v8_4.1 > 0)

##Se imputan los valores perdidos para no perder pacientes:

set.seed(1)

S2 = missForest(data.frame(SATIN2[2:5]), verbose = T)

S2$OOBerror

SATIN2[2:5] = data.frame(S2$ximp)

colnames(SATIN2)

tiempo = c(0,60,120,180)

SATIN21 = data.frame(t(SATIN2[2:5]))

AICs = 9999

for (i in 1:length(SATIN21)){
  AICs[[i]] =  trapz(tiempo, SATIN21[[i]])
}

SATIN2 = cbind(SATIN2[1], AICs, SATIN2[6:10])

rm(SATIN21, S2, i, tiempo, AICs)

#Ajuste del AICs por peso y grupo de intervencion:

####Generacion de variables de perdida de peso:

SATIN2$v14_peso = (SATIN2$v14_4.1 + SATIN2$v14_4.2)/2
SATIN2$v8_peso = (SATIN2$v8_4.1 + SATIN2$v8_4.2)/2

SATIN2$dpeso_14_8 = SATIN2$v14_peso - SATIN2$v8_peso

####Ajuste por minimos cuadrados:

media<-mean(SATIN2$AICs, na.rm=T) #1082.756

# Linear model
modelo<-lm (SATIN2$AICs ~ SATIN2$dpeso_14_8 + as.factor(SATIN2$v7_10.1), na.action=na.exclude)

# Residuals
resid(modelo)

# Adjusted value
cambios <- resid(modelo, na.action=na.exclude) + (media)

SATIN2$AICs = cambios

colnames(SATIN2)

#Imputacion y normalizacion de la BBDD de metabolomica:

S1 = data.frame(SATIN2$allocation_no)

colnames(S1) = c("allocation_no")

#Se añade el VAS a la BBDD:

SATIN_VAS = merge(S1, SATIN1, by = "allocation_no")

#NAs en cada variables
imp1 = SATIN_VAS[2:134]

na1 = sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
na2 = sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))
na =  cbind("NA"=na1, "% NA"=na2)
na

#Removemos > 20% (marcado previamente)
drop = c("v14_PC_34.1","v14_PC_36.2","v14_PC_36.4","v14_TG_48.1","v14_TG_48.2","v14_TG_50.4","v14_TG_51.2","v14_TG_52.1",
         "v14_TG_52.5")
imp1 = imp1[,!(names(imp1) %in% drop)]
sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))

#Imputacion con randoforest:

colnames(imp1)

set.seed(1)

imp1 = missForest(imp1, verbose = T)

imp1$OOBerror

imp1 = data.frame(imp1$ximp)

colnames(imp1)

#Normalización Ranknorm.

VAS2 = data.frame(cbind(S1, imp1, SATIN2[2]))

imp1.rankNorm = apply(imp1, 2, rankNorm)

VAS.rankNorm.v14 = data.frame(cbind(S1, imp1.rankNorm, SATIN2[2]))

save(VAS.rankNorm.v14, file = "VAS.rankNorm.v14.16032020.rda")

rm(A_NA, cambios, drop, media, na1, na2, SATIN1, SATIN2, SATIN_VAS, S1, na, imp1, imp1.rankNorm)

###########Analisis en v14:

#Preparacion de la BBDD:
VAS.rankNorm.v14.11 = VAS.rankNorm.v14[-1]

#Split t-v

set.seed(2223)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = VAS.rankNorm.v14.11$AICs %>% createDataPartition(p = 0.8, list = F)
  
  train.data[[i]] = VAS.rankNorm.v14.11[training,]
  test.data[[i]] = VAS.rankNorm.v14.11[-training,]
  
  #Modelo
  
  cv[[i]] = train(AICs ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(AICs~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$AICs #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$AICs), Rsquare = R2(predictions[[i]], test.data[[i]]$AICs), Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$AICs, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$AICs)$estimate)
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha

train_rows = c()
training = c()
validation = c()
modelito = c()
X = c()
Y = c()
X1 = c()
Y1 = c()
pred = c()
met = c()

for (i in 1:10) {
  
  #Particion
  
  train_rows[[i]] = VAS.rankNorm.v14.11$AICs %>% createDataPartition(p = 0.80, list = F)
  training[[i]] = VAS.rankNorm.v14.11[train_rows[[i]],]
  validation[[i]] = VAS.rankNorm.v14.11[-train_rows[[i]],]
  
  a=as.data.frame(training[[i]])
  X[[i]] = as.matrix(a[,2:(dim(a)[2]-1)])
  Y[[i]] = a$AICs
  
  b=as.data.frame(validation[[i]])
  X1[[i]] = as.matrix(b[,2:(dim(a)[2]-1)])
  Y1[[i]] = b$AICs
  
  #Modelo
  
  modelito[[i]] = glmnet(X[[i]], Y[[i]], alpha = alp, lambda = l)
  
  #Predicciones:
  
  pred[[i]] = modelito[[i]] %>% predict(X1[[i]])
  
  met[[i]] = data.frame(RMSE = RMSE(pred[[i]], Y1[[i]]), Rsquare = R2(pred[[i]], Y1[[i]]), Pearson.IC = cor.test(pred[[i]], Y1[[i]], conf.level = 0.95)$conf.int,
                        Pearson.cor = cor.test(pred[[i]], Y1[[i]])$estimate)
}

coef_met_min = c()

for (i in 1:10) {
  
  metabolitos.min = coef(modelito[[i]], s = l)
  
  coef.min = metabolitos.min@x
  coef.min = coef.min[-1]
  
  seleccion.min = c(metabolitos.min@i)
  nombres = as.data.frame(colnames(X[[i]]))
  seleccion.min1 = nombres[seleccion.min,]
  seleccion.min1 = as.character(seleccion.min1)
  coef_met_min[[i]] = cbind(seleccion.min1, coef.min)
  
}

(coef.min.ajusta.AUC = print(coef_met_min))

BBDD.met.min = data.frame(colnames(VAS.rankNorm.v14.11))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.VAS.rankNorm.v14.11."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "11032020.Validacion.ajust.AUC.v14.xlsx")

###############################--------- Parte 3: Validacion interna 
metabolitos = c("v14_Glycine")
media = c(22.0064482)
model.met = data.frame(metabolitos, media)
str(model.met)
model.met$metabolitos = as.character(model.met$metabolitos)

X2 = data.frame(VAS.rankNorm.v14.11[,1:(dim(VAS.rankNorm.v14.11)[2]-1)])
Y2 = VAS.rankNorm.v14$AICs
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"," +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
  
  correr = cor.test(Y2,modelo, conf.level = .95)
  
} 


####Coef con la BBDD completa y cv:

set.seed(1231)

cvfit = c()
coef_met_min = c()

X = data.frame(VAS.rankNorm.v14.11[,1:(dim(VAS.rankNorm.v14.11)[2]-1)])
Y = VAS.rankNorm.v14$AICs

for (i in 1:10) {
  
  cvfit[[i]] = cv.glmnet(as.matrix(X), as.matrix(Y), family="gaussian", type.measure = "mse", alpha = alp)
  
  metabolitos.min = coef(cvfit[[i]], s = "lambda.min")
  
  coef.min = metabolitos.min@x
  coef.min = coef.min[-1]
  
  seleccion.min = c(metabolitos.min@i)
  nombres = as.data.frame(colnames(X))
  seleccion.min1 = nombres[seleccion.min,]
  seleccion.min1 = as.character(seleccion.min1)
  coef_met_min[[i]] = cbind(seleccion.min1, coef.min)
  
}

BBDD.met.min = data.frame(colnames(VAS.rankNorm.v14.11))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.VAS.rankNorm.v14.11."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "16032020.coef.full.ajus.v14.xlsx")

save.image(file = "16032020.v14.RData")

######Participantes por nodo:

Copen = subset(VAS.rankNorm, allocation_no < 2000)
Reus = subset(VAS.rankNorm, allocation_no > 2000)

Copen.v14 = subset(VAS.rankNorm.v14, allocation_no < 2000)
Reus.v14 = subset(VAS.rankNorm.v14, allocation_no > 2000)

####Tabla características de los participantes:

####Generacion de la BBDD con los datos:

###Valores AICs

mean(VAS.rankNorm$AICs)
sd(VAS.rankNorm$AICs)

mean(VAS.rankNorm.v14$AICs)
sd(VAS.rankNorm.v14$AICs)

AUCS1 = VAS.rankNorm[,c("allocation_no", "AICs")]
AUCS2 = VAS.rankNorm.v14[,c("allocation_no", "AICs")]

AUCS = merge(AUCS1, AUCS2, by = "allocation_no", all.x = T)

t.test(AUCS$AICs.x, AUCS$AICs.y, paired = T)

T1 = VAS.rankNorm[1]

T1 = merge(T1, Satin_BBDD_completa, by = "allocation_no")

T1 = T1[,c("allocation_no", "v1_gq_11", "v1_gq_1", "v2_4.1", "v2_4.2", "v2_4.3", "v2_4.4","v8_4.1", "v8_4.2", "v8_4.3", "v8_4.4", 
           "v8_5.1", "v8_5.2", "v8_5.3", "v8_5.4","v14_4.1", "v14_4.2", "v14_4.3", "v14_4.4", 
           "v14_5.1", "v14_5.2", "v14_5.3", "v14_5.4")]
T2 = bioquimica_SATIN[,c("allocation_no", "v8_Glucose", "v8_Cholesterol", "v8_HDL-C", "v8_LDL-C", "v8_TG",
                         "v14_Glucose", "v14_Cholesterol", "v14_HDL-C", "v14_LDL-C", "v14_TG")]

Table1 = merge(T1, T2, by = "allocation_no")  

T3 = VAS.rankNorm.v14[1]


rm(T1, T2)

colnames(Table1) = c("allocation_no", "edad", "sexo", "v2_altura1", "v2_altura2", "v2_peso1", "v2_peso2","v8_peso1", "v8_peso2", 
                     "v8_cintura1", "v8_cintura2", "v8_PAS1", "v8_PAS2", "v8_PAD1", "v8_PAD2", "v14_peso1", "v14_peso2", 
                     "v14_cintura1", "v14_cintura2", 
                     "v14_PAS1", "v14_PAS2", "v14_PAD1", "v14_PAD2","v8_Glucose", "v8_Cholesterol", 
                     "v8_HDL", "v8_LDL", "v8_TG","v14_Glucose", "v14_Cholesterol", 
                     "v14_HDL", "v14_LDL", "v14_TG")

Table2 = merge(T3, Table1, by = "allocation_no") 

Table1$sexo = factor(Table1$sexo, labels = c("F", "M"))
Table2$sexo = factor(Table2$sexo, labels = c("F", "M"))

##Calculos de medias

mean(Table1$edad, na.rm = T)
sd(Table1$edad, na.rm = T)

table(Table1$sexo)

mean(((Table1$v8_peso1 + Table1$v8_peso2)/2), na.rm = T)
sd(((Table1$v8_peso1 + Table1$v8_peso2)/2), na.rm = T)

mean((((Table1$v2_peso1 + Table1$v2_peso2)/2) - ((Table1$v8_peso1 + Table1$v8_peso2)/2))/((Table1$v2_peso1 + Table1$v2_peso2)/2), na.rm = T)
mean((((Table1$v2_peso1 + Table1$v2_peso2)/2) - ((Table1$v8_peso1 + Table1$v8_peso2)/2)), na.rm = T)
sd((((Table1$v2_peso1 + Table1$v2_peso2)/2) - ((Table1$v8_peso1 + Table1$v8_peso2)/2)), na.rm = T)

mean((((Table1$v8_peso1 + Table1$v8_peso2)/2)/((Table1$v2_altura1 + Table1$v2_altura2)/2)^2), na.rm = T)
sd((((Table1$v8_peso1 + Table1$v8_peso2)/2)/((Table1$v2_altura1 + Table1$v2_altura2)/2)^2), na.rm = T)

mean(((Table1$v8_cintura1 + Table1$v8_cintura2)/2), na.rm = T)
sd(((Table1$v8_cintura1 + Table1$v8_cintura2)/2), na.rm = T)

mean(((Table1$v8_PAS1 + Table1$v8_PAS2)/2), na.rm = T)
sd(((Table1$v8_PAS1 + Table1$v8_PAS2)/2), na.rm = T)

mean(((Table1$v8_PAD1 + Table1$v8_PAD2)/2), na.rm = T)
sd(((Table1$v8_PAD1 + Table1$v8_PAD2)/2), na.rm = T)

mean(Table1$v8_Glucose, na.rm = T)
sd(Table1$v8_Glucose, na.rm = T)

mean(Table1$v8_Cholesterol, na.rm = T)
sd(Table1$v8_Cholesterol, na.rm = T)

mean(Table1$v8_HDL, na.rm = T)
sd(Table1$v8_HDL, na.rm = T)

mean(Table1$v8_LDL, na.rm = T)
sd(Table1$v8_LDL, na.rm = T)

mean(Table1$v8_TG, na.rm = T)
sd(Table1$v8_TG, na.rm = T)

#####Table 2:

mean(Table2$edad, na.rm = T)
sd(Table2$edad, na.rm = T)

table(Table2$sexo)

mean(((Table2$v14_peso1 + Table2$v14_peso2)/2), na.rm = T)
sd(((Table2$v14_peso1 + Table2$v14_peso2)/2), na.rm = T)

mean((((Table2$v8_peso1 + Table2$v8_peso2)/2) - ((Table2$v14_peso1 + Table2$v14_peso2)/2))/((Table2$v8_peso1 + Table2$v8_peso2)/2), na.rm = T)
mean((((Table2$v8_peso1 + Table2$v8_peso2)/2) - ((Table2$v14_peso1 + Table2$v14_peso2)/2)), na.rm = T)
sd((((Table2$v8_peso1 + Table2$v8_peso2)/2) - ((Table2$v14_peso1 + Table2$v14_peso2)/2)), na.rm = T)

mean((((Table2$v14_peso1 + Table2$v14_peso2)/2)/((Table2$v2_altura1 + Table2$v2_altura2)/2)^2), na.rm = T)
sd((((Table2$v14_peso1 + Table2$v14_peso2)/2)/((Table2$v2_altura1 + Table2$v2_altura2)/2)^2), na.rm = T)

mean(((Table2$v14_cintura1 + Table2$v14_cintura2)/2), na.rm = T)
sd(((Table2$v14_cintura1 + Table2$v14_cintura2)/2), na.rm = T)

mean(((Table2$v14_PAS1 + Table2$v14_PAS2)/2), na.rm = T)
sd(((Table2$v14_PAS1 + Table2$v14_PAS2)/2), na.rm = T)

mean(((Table2$v14_PAD1 + Table2$v14_PAD2)/2), na.rm = T)
sd(((Table2$v14_PAD1 + Table2$v14_PAD2)/2), na.rm = T)

mean(Table2$v14_Glucose, na.rm = T)
sd(Table2$v14_Glucose, na.rm = T)

mean(Table2$v14_Cholesterol, na.rm = T)
sd(Table2$v14_Cholesterol, na.rm = T)

mean(Table2$v14_HDL, na.rm = T)
sd(Table2$v14_HDL, na.rm = T)

mean(Table2$v14_LDL, na.rm = T)
sd(Table2$v14_LDL, na.rm = T)

mean(Table2$v14_TG, na.rm = T)
sd(Table2$v14_TG, na.rm = T)

#P-values:

Table3 = merge(Table1, Table2, by = "allocation_no", all.x = T)

colnames(Table3)
t.test(Table3$edad.x, Table3$edad.y)
t.test(((Table3$v8_peso1.x + Table3$v8_peso2.x)/2), ((Table3$v14_peso1.x + Table3$v14_peso2.x)/2))
t.test((((Table3$v8_peso1.x + Table3$v8_peso2.x)/2)/((Table3$v2_altura1.x + Table3$v2_altura2.x)/2)^2), (((Table3$v14_peso1.y + Table3$v14_peso2.y)/2)/((Table3$v2_altura1.y + Table3$v2_altura2.y)/2)^2))
t.test(((Table3$v8_cintura1.x + Table3$v8_cintura2.x)/2), ((Table3$v14_cintura1.y + Table3$v14_cintura2.y)/2))
t.test(((Table3$v8_PAS1.x + Table3$v8_PAS2.x)/2), ((Table3$v14_PAS1.y + Table3$v14_PAS2.y)/2))
t.test(((Table3$v8_PAD1.x + Table3$v8_PAD2.x)/2), ((Table3$v14_PAD1.y + Table3$v14_PAD2.y)/2))
t.test(Table3$v8_Glucose.x, Table3$v14_Glucose.y)
mean(Table3$v8_Glucose.x - Table3$v14_Glucose.y, na.rm = T)
sd(Table3$v8_Glucose.x - Table3$v14_Glucose.y, na.rm = T)
t.test(Table3$v8_Cholesterol.x, Table3$v14_Cholesterol.y)
mean(Table3$v8_Cholesterol.x - Table3$v14_Cholesterol.y, na.rm = T)
sd(Table3$v8_Cholesterol.x - Table3$v14_Cholesterol.y, na.rm = T)
t.test(Table3$v8_HDL.x, Table3$v14_HDL.y)
mean(Table3$v8_HDL.x - Table3$v14_HDL.y, na.rm =T)
sd(Table3$v8_HDL.x - Table3$v14_HDL.y, na.rm = T)
t.test(Table3$v8_LDL.x, Table3$v14_LDL.y)
mean(Table3$v8_LDL.x - Table3$v14_LDL.y, na.rm = T)
sd(Table3$v8_LDL.x - Table3$v14_LDL.y, na.rm = T)
t.test(Table3$v8_TG.x, Table3$v14_TG.y)
mean(Table3$v8_TG.x - Table3$v14_TG.y, na.rm = T)
sd(Table3$v8_TG.x - Table3$v14_TG.y, na.rm = T)
t.test((((Table3$v8_peso1.y + Table3$v8_peso2.y)/2) - ((Table3$v14_peso1.y + Table3$v14_peso2.y)/2))/((Table3$v8_peso1.y + Table3$v8_peso2.y)/2), (((Table3$v2_peso1.x + Table3$v2_peso2.x)/2) - ((Table3$v8_peso1.x + Table3$v8_peso2.x)/2))/((Table3$v2_peso1.x + Table3$v2_peso2.x)/2))
chisq.test(Table3$sexo.x, Table3$sexo.y)

###Tabla suplementaria: medias/medianas/SD de los metabolitos (VAS1 = V8 y VAS2 = V14):

VAS1 = VAS1[-1]
VAS1 = VAS1[-125]
VAS2 = VAS2[-1]
VAS2 = VAS2[-125]

medias_v8 = apply(VAS1,2,mean)
medianas_v8 = apply(VAS1,2,median)
SD_v8 = apply(VAS1,2,sd)
nombres = colnames(VAS1)

medias_v14 = apply(VAS2,2,mean)
medianas_v14 = apply(VAS2,2,median)
SD_v14 = apply(VAS2,2,sd)

VAS.des = data.frame(cbind(nombres, medias_v8, medianas_v8, SD_v8, medias_v14, medianas_v14, SD_v14))

export(VAS.des, "metabolitos_descriptivos.xlsx")

###Tabla suplementaria2: medias/SD/deltas de peso, IMC, cintura, fat mass, lean mass, energia y macros (VAS1 = V8 y VAS2 = V14):

T1 = VAS.rankNorm[1]

T1 = merge(T1, Satin_BBDD_completa, by = "allocation_no")

T1 = T1[,c("allocation_no", "v2_4.1", "v2_4.2", "v2_4.3", "v2_4.4", "v2_4.5", "v2_4.6","v8_4.1", "v8_4.2", "v8_4.3", "v8_4.4", 
           "v14_4.1", "v14_4.2", "v14_4.3", "v14_4.4", "v2_lean_mass", "v8_lean_mass", "v14_lean_mass",
           "v2_fat_mass", "v8_fat_mass", "v14_fat_mass")]

Nutri_Satin = read_excel("D:/ARTICULOS/BBDD/Satin/BBDD limpias/Nutri_Satin.xlsx")

colnames(Nutri_Satin)

T2 = Nutri_Satin[,c("allocation_no", "V2_KCAL", "V8_KCAL", "V14_KCAL", "V2_PROT", "V8_PROT",
                         "V14_PROT", "V2_HC", "V8_HC", "V14_HC", "V2_LIP", "V8_LIP", "V14_LIP",
                         "V2_FIBRA", "V8_FIBRA", "V14_FIBRA")]

Tab1 = merge(T1, T2, by = "allocation_no")  

colnames(Tab1) = c("allocation_no", "v2_altura1", "v2_altura2", "v2_peso1", "v2_peso2", "v2_cintura1", "v2_cintura2", "v8_peso1", "v8_peso2", 
                     "v8_cintura1", "v8_cintura2", "v14_peso1", "v14_peso2", 
                     "v14_cintura1", "v14_cintura2", "v2_lean_mass", "v8_lean_mass", "v14_lean_mass",
                     "v2_fat_mass", "v8_fat_mass", "v14_fat_mass", "V2_KCAL", "V8_KCAL", "V14_KCAL", "V2_PROT", "V8_PROT",
                     "V14_PROT", "V2_HC", "V8_HC", "V14_HC", "V2_LIP", "V8_LIP", "V14_LIP",
                     "V2_FIBRA", "V8_FIBRA", "V14_FIBRA")

Tab1$del_pes_v82 = ((Tab1$v8_peso1 + Tab1$v8_peso2)/2) - ((Tab1$v2_peso1 + Tab1$v2_peso2)/2)
Tab1$del_pes_v148 = ((Tab1$v14_peso1 + Tab1$v14_peso2)/2) - ((Tab1$v8_peso1 + Tab1$v8_peso2)/2)
Tab1$imc_v2 = (((Tab1$v2_peso1 + Tab1$v2_peso2)/2)/((Tab1$v2_altura1 + Tab1$v2_altura2)/2)^2)
Tab1$imc_v8 = (((Tab1$v8_peso1 + Tab1$v8_peso2)/2)/((Tab1$v2_altura1 + Tab1$v2_altura2)/2)^2)
Tab1$imc_v14 = (((Tab1$v14_peso1 + Tab1$v14_peso2)/2)/((Tab1$v2_altura1 + Tab1$v2_altura2)/2)^2)
Tab1$del_imc_v82 = Tab1$imc_v8 - Tab1$imc_v2
Tab1$del_imc_v148 = Tab1$imc_v14 - Tab1$imc_v8
Tab1$del_cint_v82 = ((Tab1$v8_cintura1 + Tab1$v8_cintura2)/2) - ((Tab1$v2_cintura1 + Tab1$v2_cintura2)/2)
Tab1$del_cint_v148 = ((Tab1$v14_cintura1 + Tab1$v14_cintura2)/2) - ((Tab1$v8_cintura1 + Tab1$v8_cintura2)/2)
Tab1$del_fat_v82 = (Tab1$v8_fat_mass) - (Tab1$v2_fat_mass)
Tab1$del_fatt_v148 = (Tab1$v14_fat_mass) - (Tab1$v8_fat_mass)
Tab1$del_lean_v82 = (Tab1$v8_lean_mass) - (Tab1$v2_lean_mass)
Tab1$del_lean_v148 = (Tab1$v14_lean_mass) - (Tab1$v8_lean_mass)
Tab1$del_kcal_v82 = (Tab1$V8_KCAL) - (Tab1$V2_KCAL)
Tab1$del_kcal_v148 = (Tab1$V14_KCAL) - (Tab1$V8_KCAL)
Tab1$del_prot_v82 = (Tab1$V8_PROT) - (Tab1$V2_PROT)
Tab1$del_prot_v148 = (Tab1$V14_PROT*4/Tab1$V14_KCAL) - (Tab1$V8_PROT*4/Tab1$V8_KCAL)
Tab1$del_hc_v82 = (Tab1$V8_HC) - (Tab1$V2_HC)
Tab1$del_hc_v148 = (Tab1$V14_HC*4/Tab1$V14_KCAL) - (Tab1$V8_HC*4/Tab1$V8_KCAL)
Tab1$del_lip_v82 = (Tab1$V8_LIP) - (Tab1$V2_LIP)
Tab1$del_lip_v148 = (Tab1$V14_LIP*9/Tab1$V14_KCAL) - (Tab1$V8_LIP*9/Tab1$V8_KCAL)
Tab1$del_FIBRA_v82 = (Tab1$V8_FIBRA) - (Tab1$V2_FIBRA)
Tab1$del_FIBRA_v148 = (Tab1$V14_FIBRA) - (Tab1$V8_FIBRA)

medias = c()
sds = c()

for (i in 1:length(Tab1)){
  medias[[i]] =  mean(Tab1[[i]], na.rm = T)
  sds[[i]] = sd(Tab1[[i]], na.rm = T)
}

nombres = colnames(Tab1)

Tab1 = data.frame(cbind(nombres, medias, sds))

export(Tab1, "Tab1.xlsx")

mean((Tab1$del_prot_v148), na.rm = T)
sd((Tab1$del_prot_v148), na.rm = T)
mean((Tab1$del_hc_v148), na.rm = T)
sd((Tab1$del_hc_v148), na.rm = T)
mean((Tab1$del_lip_v148), na.rm = T)
sd((Tab1$del_lip_v148), na.rm = T)
mean((Tab1$v8_peso1 + Tab1$v8_peso2)/2, na.rm = T)
sd((Tab1$v8_peso1 + Tab1$v8_peso2)/2, na.rm = T)
mean((Tab1$v14_peso1 + Tab1$v14_peso2)/2, na.rm = T)
sd((Tab1$v14_peso1 + Tab1$v14_peso2)/2, na.rm = T)
mean(Tab1$del_imc_v82, na.rm = T)
sd(Tab1$del_imc_v82, na.rm = T)
mean(Tab1$del_imc_v148, na.rm = T)
sd(Tab1$del_imc_v148, na.rm = T)
mean((Tab1$v8_cintura1 + Tab1$v8_cintura2)/2, na.rm = T)
sd((Tab1$v8_cintura1 + Tab1$v8_cintura2)/2, na.rm = T)
mean((Tab1$v14_cintura1 + Tab1$v14_cintura2)/2, na.rm = T)
sd((Tab1$v14_cintura1 + Tab1$v14_cintura2)/2, na.rm = T)

t.test(((Tab1$v8_peso1 + Tab1$v8_peso2)/2), ((Tab1$v14_peso1 + Tab1$v14_peso2)/2))
t.test((((Tab1$v8_peso1 + Tab1$v8_peso2)/2)/((Tab1$v2_altura1 + Tab1$v2_altura2)/2)^2), (((Tab1$v14_peso1 + Tab1$v14_peso2)/2)/((Tab1$v2_altura1 + Tab1$v2_altura2)/2)^2))
t.test(((Tab1$v8_cintura1 + Tab1$v8_cintura2)/2), ((Tab1$v14_cintura1 + Tab1$v14_cintura2)/2))
t.test(Tab1$v8_fat_mass, Tab1$v14_fat_mass)
t.test(Tab1$v8_lean_mass, Tab1$v14_lean_mass)
t.test(Tab1$V8_KCAL, Tab1$V14_KCAL)
t.test(Tab1$V8_HC*4/Tab1$V8_KCAL, Tab1$V14_HC*4/Tab1$V14_KCAL)
t.test(Tab1$V8_PROT*4/Tab1$V8_KCAL, Tab1$V14_PROT*4/Tab1$V14_KCAL)
t.test(Tab1$V8_LIP*9/Tab1$V8_KCAL, Tab1$V14_LIP*9/Tab1$V14_KCAL)
t.test(Tab1$V8_FIBRA, Tab1$V14_FIBRA)

Tab1$v8_peso1
Tab1$v14_peso1
Tab1$del_cint_v148

## Calculo de C Pearson

test = c()

for (i in 2:125) {
  test[[i]] = cor(VAS.rankNorm$AICs, VAS.rankNorm[[i]])
}

names = colnames(VAS.rankNorm)
names = names[-126]
names = names[-1]

test = test[-1]

HM1 = data.frame(cbind(names, test))

## Scatter plots

Conf3x2 = matrix(c(1:6), nrow=2, byrow=TRUE)
Conf3x2

layout(Conf3x2)
layout.show(6)

x <- VAS.rankNorm$v8_Glycine
y <- VAS.rankNorm$AICs

cor.test(VAS.rankNorm$v8_Glycine, VAS.rankNorm$AICs)

plot(x, y, xlab = "Glycine", ylab = "AICs", main = "P-value <0.001",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = VAS.rankNorm), col = "blue")

x <- VAS.rankNorm$v8_LinoA
y <- VAS.rankNorm$AICs

cor.test(VAS.rankNorm$v8_LinoA, VAS.rankNorm$AICs)

plot(x, y, xlab = "Linoleic Acid", ylab = "AICs", main = "P-value 0.025",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = VAS.rankNorm), col = "blue")

x <- VAS.rankNorm$v8_Sucrose
y <- VAS.rankNorm$AICs

cor.test(VAS.rankNorm$v8_Sucrose, VAS.rankNorm$AICs)

plot(x, y, xlab = "Sucrose", ylab = "AICs", main = "P-value 0.027",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = VAS.rankNorm), col = "blue")

x <- VAS.rankNorm$v8_SM_32.2
y <- VAS.rankNorm$AICs

cor.test(VAS.rankNorm$v8_SM_32.2, VAS.rankNorm$AICs)

plot(x, y, xlab = "C32:2 SM", ylab = "AICs", main = "P-value 0.009",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = VAS.rankNorm), col = "blue")

x <- VAS.rankNorm$v8_SM_38.1
y <- VAS.rankNorm$AICs

cor.test(VAS.rankNorm$v8_SM_38.1, VAS.rankNorm$AICs)

plot(x, y, xlab = "C38:1 SM", ylab = "AICs", main = "P-value 0.001",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = VAS.rankNorm), col = "blue")

dev.off()

layout(Conf3x2)

x <- VAS.rankNorm.v14$v14_Glycine
y <- VAS.rankNorm.v14$AICs

cor.test(VAS.rankNorm.v14$v14_Glycine, VAS.rankNorm.v14$AICs)

plot(x, y, xlab = "Glycine", ylab = "AICs", main = "P-value 0.001",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = VAS.rankNorm.v14), col = "blue")

x <- VAS.rankNorm.v14$v14_LinoA
y <- VAS.rankNorm.v14$AICs

cor.test(VAS.rankNorm.v14$v14_LinoA, VAS.rankNorm.v14$AICs)

plot(x, y, xlab = "Linoleic Acid", ylab = "AICs", main = "P-value 0.022",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = VAS.rankNorm.v14), col = "blue")

x <- VAS.rankNorm.v14$v14_PC_38.4.e
y <- VAS.rankNorm.v14$AICs

cor.test(VAS.rankNorm.v14$v14_PC_38.4.e, VAS.rankNorm.v14$AICs)

plot(x, y, xlab = "C38:4 ePC", ylab = "AICs", main = "P-value 0.017",
     pch = 19, frame = FALSE)
abline(lm(y ~ x, data = VAS.rankNorm.v14), col = "blue")

dev.off()

### Peticiones de los revisores: Nutrients 29/01/2021

## Correlacion probe day energy intake - Huella

PDay <- read_excel("D:/ARTICULOS/BBDD/Satin/BBDD limpias/maintenance_data_withoutTFEQ_THA.xlsx")
View(PDay)

### Generacion de la puntuacion:

model.met <- read_excel("modelos.xlsx", sheet = "V8")
View(model.met)

model.met$metabolitos = as.character(model.met$metabolitos)

X2 = data.frame(VAS.rankNorm[,2:(dim(VAS.rankNorm)[2])])
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"," +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
  
  #correr = cor.test(Y2,modelo, conf.level = .95)
  
} 

BBDD = data.frame(cbind(VAS.rankNorm$allocation_no, X2$model))
colnames(BBDD) = c("allocation_no", "V8")

model.met <- read_excel("modelos.xlsx", sheet = "V14")
View(model.met)

model.met$metabolitos = as.character(model.met$metabolitos)

X2 = data.frame(VAS.rankNorm.v14[,2:(dim(VAS.rankNorm.v14.11)[2])])
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"," +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
  
  #correr = cor.test(Y2,modelo, conf.level = .95)
  
} 

BBDD1 = data.frame(cbind(VAS.rankNorm.v14$allocation_no, X2$model))
colnames(BBDD1) = c("allocation_no", "V14")

BBDD = merge(BBDD, BBDD1, by = "allocation_no", all.x = T)

PDay1 = data.frame(cbind(PDay$id, PDay$lun_kj8, PDay$din_kj8, PDay$lun_kj14, PDay$din_kj14))
colnames(PDay1) = c("allocation_no", "lun_kj8", "din_kj8", "lun_kj14", "din_kj14")

BBDD = merge(BBDD, PDay1, by = "allocation_no")

BBDD$kcal_v8 = (BBDD$lun_kj8/4.18) + (BBDD$din_kj8/4.18)
BBDD$kcal_v14 = (BBDD$lun_kj14/4.18) + (BBDD$din_kj14/4.18)

cor.test(BBDD$V8, BBDD$kcal_v8)
cor.test(BBDD$V14, BBDD$kcal_v14)

SATIN2 = Satin_BBDD_completa[,c("allocation_no", "v8.1_avas_3_.5")]

BBDD = merge(BBDD, SATIN2, by = "allocation_no", all.x = T)

cor.test(BBDD$V8, BBDD$v8.1_avas_3_.5)

SATIN2 = Satin_BBDD_completa[,c("allocation_no", "v14.1_avas_3_.5")]

BBDD = merge(BBDD, SATIN2, by = "allocation_no", all.x = T)

cor.test(BBDD$V14, BBDD$v14.1_avas_3_.5)

B1 = data.frame(cbind(VAS.rankNorm$allocation_no, VAS.rankNorm$v8_Glycine, VAS.rankNorm$v8_LinoA, VAS.rankNorm$v8_Sucrose, VAS.rankNorm$v8_SM_32.2,
                      VAS.rankNorm$v8_SM_38.1))
colnames(B1) = c("allocation_no", "v8_Glycine", "v8_LinoA", "v8_Sucrose", "v8_SM_32.2", "v8_SM_38.1")
BBDD = merge(BBDD, B1, by = "allocation_no", all.x = T)

cor.test(BBDD$kcal_v8, BBDD$v8.1_avas_3_.5)
cor.test(BBDD$kcal_v14, BBDD$v14.1_avas_3_.5)

B2 = data.frame(cbind(VAS.rankNorm$allocation_no, VAS.rankNorm$AICs))
colnames(B2) = c("allocation_no", "AIC_v8")

BBDD = merge(BBDD, B2, by = "allocation_no", all.x = T)

cor.test(BBDD$kcal_v8, BBDD$AIC_v8)

B3 = data.frame(cbind(VAS.rankNorm.v14$allocation_no, VAS.rankNorm.v14$AICs))
colnames(B3) = c("allocation_no", "AIC_v14")

BBDD = merge(BBDD, B3, by = "allocation_no", all.x = T)

cor.test(BBDD$kcal_v14, BBDD$AIC_v14)

cor.test((BBDD$lun_kj8/4.18), BBDD$AIC_v8)
cor.test((BBDD$lun_kj14/4.18), BBDD$AIC_v14)

cor.test((BBDD$lun_kj8/4.18), BBDD$V8)
cor.test((BBDD$lun_kj14/4.18), BBDD$V14)
