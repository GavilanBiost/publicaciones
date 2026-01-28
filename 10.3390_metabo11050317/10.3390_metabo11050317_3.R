##Generacion de deltas y posterior normalización:

##V0, V8, V14

colnames(V0)

bb1 = V0[2:137]
bb0 = V8[2:137]

V8.2 = data.frame(cbind(V0[1], (bb0-bb1))) ##BBDD con los deltas de V8-V0 (met y CC)
MMV8.2 = data.frame(cbind(V8.2[2], V8.2[5:137]))
MMV8.2 = subset(MMV8.2, v8_lean_mass >= -100) #74 NAs
MOV8.2 = data.frame(cbind(V8.2[3], V8.2[5:137]))
MOV8.2 = subset(MOV8.2, v8_bone_mass >= -100) #74 NAs
MGV8.2 = data.frame(cbind(V8.2[4], V8.2[5:137]))
MGV8.2 = subset(MGV8.2, v8_fat_mass >= -100) #74 NAs

export(MMV8.2, "MMV8.2.xlsx")
export(MOV8.2, "MOV8.2.xlsx")
export(MGV8.2, "MGV8.2.xlsx")

MMV8.2 <- read_excel("MMV8.2.xlsx")
MOV8.2 <- read_excel("MOV8.2.xlsx")
MGV8.2 <- read_excel("MGV8.2.xlsx")

bb1 = V14[2:137]
bb0 = V8[2:137]

V14.8 = data.frame(cbind(V14[1], (bb1-bb0))) ##BBDD con los deltas de V14-V8 (met y CC)
MMV14.8 = data.frame(cbind(V14.8[2], V14.8[5:137]))
MMV14.8 = subset(MMV14.8, v14_lean_mass >= -100) #96 NAs
MOV14.8 = data.frame(cbind(V14.8[3], V14.8[5:137]))
MOV14.8 = subset(MOV14.8, v14_bone_mass >= -100) #96 NAs
MGV14.8 = data.frame(cbind(V14.8[4], V14.8[5:137]))
MGV14.8 = subset(MGV14.8, v14_fat_mass >= -100) #96 NAs

export(MMV14.8, "MMV14.8.xlsx")
export(MOV14.8, "MOV14.8.xlsx")
export(MGV14.8, "MGV14.8.xlsx")

MMV14.8 <- read_excel("MMV14.8.xlsx")
MOV14.8 <- read_excel("MOV14.8.xlsx")
MGV14.8 <- read_excel("MGV14.8.xlsx")

#MM8.2

###Deteccion de NAs y normalizacion

imp1 = MMV8.2[2:134]

na1 = sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
na2 = sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))
na =  cbind("NA"=na1, "% NA"=na2)
na

#Metabolites con NA mayor de 20%
A_NA=rownames(na[na[,2]>20 ,])

#ReMGveMGs > 20%
drop = A_NA #-13 met
imp1 = imp1[,!(names(imp1) %in% drop)] #10
sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))

#Imputacion con randoforest:

colnames(imp1)

set.seed(1)

imp1 = missForest(as.matrix(imp1), verbose = T)

imp1$OOBerror

imp1 = data.frame(imp1$ximp)

colnames(imp1)

#Normalización Ranknorm.

imp1.rankNorm = apply(imp1, 2, rankNorm)

MMV8.2.rankNorm = data.frame(cbind(MMV8.2[1], imp1.rankNorm))

save(MMV8.2.rankNorm, file = "MMV8.2.rankNorm.06052020.rda")

rm(imp1, imp1.rankNorm, na, na1, na2, drop)

#MOV8.2

imp1 = MOV8.2[2:134]

#ReMGveMGs > 20%
drop = A_NA #-13 met
imp1 = imp1[,!(names(imp1) %in% drop)] #10
sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))

#Imputacion con randoforest:

colnames(imp1)

set.seed(1)

imp1 = missForest(as.matrix(imp1), verbose = T)

imp1$OOBerror

imp1 = data.frame(imp1$ximp)

colnames(imp1)

#Normalización Ranknorm.

imp1.rankNorm = apply(imp1, 2, rankNorm)

MOV8.2.rankNorm = data.frame(cbind(MOV8.2[1], imp1.rankNorm))

save(MOV8.2.rankNorm, file = "MOV8.2.rankNorm.06052020.rda")

rm(imp1, imp1.rankNorm, drop)

#MGV8.2

imp1 = MGV8.2[2:134]

#ReMGveMGs > 20%
drop = A_NA #-13 met
imp1 = imp1[,!(names(imp1) %in% drop)] #10
sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))

#Imputacion con randoforest:

colnames(imp1)

set.seed(1)

imp1 = missForest(as.matrix(imp1), verbose = T)

imp1$OOBerror

imp1 = data.frame(imp1$ximp)

colnames(imp1)

#Normalización Ranknorm.

imp1.rankNorm = apply(imp1, 2, rankNorm)

MGV8.2.rankNorm = data.frame(cbind(MGV8.2[1], imp1.rankNorm))

save(MGV8.2.rankNorm, file = "MGV8.2.rankNorm.06052020.rda")

rm(imp1, imp1.rankNorm, drop)

#MMV14.8

imp1 = MMV14.8[2:134]

#ReMGveMGs > 20%
drop = A_NA #-13 met
imp1 = imp1[,!(names(imp1) %in% drop)] #10
sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))

#Imputacion con randoforest:

colnames(imp1)

set.seed(1)

imp1 = missForest(as.matrix(imp1), verbose = T)

imp1$OOBerror

imp1 = data.frame(imp1$ximp)

colnames(imp1)

#Normalización Ranknorm.

imp1.rankNorm = apply(imp1, 2, rankNorm)

MMV14.8.rankNorm = data.frame(cbind(MMV14.8[1], imp1.rankNorm))

save(MMV14.8.rankNorm, file = "MMV14.8.rankNorm.06052020.rda")

rm(imp1, imp1.rankNorm, drop)

#MOV14.8

imp1 = MOV14.8[2:134]

#ReMGveMGs > 20%
drop = A_NA #-13 met
imp1 = imp1[,!(names(imp1) %in% drop)] #10
sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))

#Imputacion con randoforest:

colnames(imp1)

set.seed(1)

imp1 = missForest(as.matrix(imp1), verbose = T)

imp1$OOBerror

imp1 = data.frame(imp1$ximp)

colnames(imp1)

#Normalización Ranknorm.

imp1.rankNorm = apply(imp1, 2, rankNorm)

MOV14.8.rankNorm = data.frame(cbind(MOV14.8[1], imp1.rankNorm))

save(MOV14.8.rankNorm, file = "MOV14.8.rankNorm.06052020.rda")

rm(imp1, imp1.rankNorm, drop)

#MGV14.8

imp1 = MGV14.8[2:134]

#ReMGveMGs > 20%
drop = A_NA #-13 met
imp1 = imp1[,!(names(imp1) %in% drop)] #10
sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))

#Imputacion con randoforest:

colnames(imp1)

set.seed(1)

imp1 = missForest(as.matrix(imp1), verbose = T)

imp1$OOBerror

imp1 = data.frame(imp1$ximp)

colnames(imp1)

#Normalización Ranknorm.

imp1.rankNorm = apply(imp1, 2, rankNorm)

MGV14.8.rankNorm = data.frame(cbind(MGV14.8[1], imp1.rankNorm))

save(MGV14.8.rankNorm, file = "MGV14.8.rankNorm.06052020.rda")

rm(imp1, imp1.rankNorm, drop)


#####-----Modelos Continuos --> NO CONVERGEN LOS MODELOS MAS ALLA DE MASA MAGRA. Se decide no continuar
##### con este objetivo.

###MMV8.2.rankNorm; MOV8.2.rankNorm; MGV8.2.rankNorm
###MMV14.8.rankNorm; MOV14.8.rankNorm; MGV14.8.rankNorm

#Split t-v

set.seed(1000)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = MOV8.2.rankNorm$bone_mass %>% createDataPartition(p = 0.8, list = F)
  
  train.data[[i]] = MOV8.2.rankNorm[training,]
  test.data[[i]] = MOV8.2.rankNorm[-training,]
  
  #modelo
  
  cv[[i]] = train(bone_mass ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(bone_mass~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$bone_mass #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$bone_mass), Rsquare = R2(predictions[[i]], test.data[[i]]$bone_mass), Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$bone_mass, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$bone_mass)$estimate)
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
  
  train_rows[[i]] = MOV8.2.rankNorm$bone_mass %>% createDataPartition(p = 0.80, list = F)
  training[[i]] = MOV8.2.rankNorm[train_rows[[i]],]
  validation[[i]] = MOV8.2.rankNorm[-train_rows[[i]],]
  
  a=as.data.frame(training[[i]])
  X[[i]] = as.matrix(a[,2:(dim(a)[2]-1)])
  Y[[i]] = a$bone_mass
  
  b=as.data.frame(validation[[i]])
  X1[[i]] = as.matrix(b[,2:(dim(a)[2]-1)])
  Y1[[i]] = b$bone_mass
  
  #modelo
  
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

(coef.min.ajust = print(coef_met_min))

BBDD.met.min = data.frame(colnames(MOV8.2.rankNorm))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.MOV8.2.rankNorm."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","V2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "06052020.ValMOV8.2.xlsx")

###############################--------- Parte 3: Validacion interna 
met <- read_excel("06052020.ValMOV8.2.xlsx")

model.met = data.frame(met$metabolitos, met$media)
str(model.met)
model.met$met.metabolitos = as.character(model.met$met.metabolitos)
model.met = subset(model.met, met.media != 0)

X2 = data.frame(MOV8.2.rankNorm[,2:(dim(MOV8.2.rankNorm)[2])])
Y2 = MOV8.2.rankNorm$bone_mass
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

set.seed(1001)

cvfit = c()
coef_met_min = c()

X = data.frame(MOV8.2.rankNorm[,2:(dim(MOV8.2.rankNorm)[2])])
Y = MOV8.2.rankNorm$bone_mass

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

BBDD.met.min = data.frame(colnames(MOV8.2.rankNorm))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.MOV8.2.rankNorm."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","V2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "06052020.CoefMOV8.2.xlsx")

save.image(file = "06052020.CoefMOV8.2.xlsx.RData")
