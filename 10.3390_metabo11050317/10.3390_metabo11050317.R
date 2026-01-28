#Apertura de las BBDD:
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
library(gridExtra)

bq <- read_excel("/Volumes/JES GAVI/ARTICULOS/BBDD/Satin/BBDD limpias/bioquimica SATIN.xlsx")
BBDD <- read_excel("/Volumes/JES GAVI/ARTICULOS/BBDD/Satin/BBDD limpias/Satin_BBDD_completa.xlsx")

#####Analisis transversal con todos los metabolitos (v0):

colnames(bq)
bq1 = bq[,c("allocation_no","v2_C0","v2_C2.0","v2_C3.0","v2_C4.1","v2_C3.0.2M","v2_C4.0","v2_C4.OH",  
            "v2_C5.OH","v2_C6.OH","v2_C4.1.2M","v2_C4.0.2M","v2_C4.0.3M","v2_C6.0","v2_C7.0","v2_C4.DC",    
            "v2_C3.DC","v2_C3.DC.M","v2_C5.DC","v2_C6.DC","v2_C8.1","v2_C5.M.DC","v2_C8.0","v2_C9.0",      
            "v2_C10.2","v2_C10.1","v2_C10.0","v2_C11.0","v2_C12.1","v2_C12.0","v2_C7.DC","v2_C14.2",      
            "v2_C13.0","v2_C14.1","v2_C14.0","v2_C15.0","v2_C16.OH","v2_C16.2","v2_C16.1","v2_C18.2",       
            "v2_C16.0","v2_C18.1","v2_C18.0","v2_5-HT","v2_5-HIAA","v2_TMAO","v2_FChr","v2_Echr",        
            "v2_TChr","v2_TG_RMN","v2_PhCh","v2_LCh","v2_Sph","v2_FAC","v2_w3","v2_ARA+EPA",
            "v2_DHA","v2_LINOLEIC","v2_PUFA","v2_MUFA","v2_LPC_14.0","v2_LPC_15.0","v2_LPC_16.0","v2_LPC_16.0.e", 
            "v2_LPC_16.1","v2_LPC_16.1.e","v2_LPC_17.0","v2_LPC_18.0","v2_LPC_18.0.e","v2_LPC_18.1","v2_LPC_18.2","v2_LPC_20.0",    
            "v2_LPC_20.1","v2_LPC_20.3","v2_LPC_20.4","v2_LPC_22.6","v2_PC_30.0","v2_PC_32.0","v2_PC_32.1","v2_PC_32.1.e",   
            "v2_PC_32.2","v2_PC_33.1","v2_PC_34.0","v2_PC_34.1","v2_PC_34.1.e","v2_PC_34.2","v2_PC_34.2.e","v2_PC_34.3.e",   
            "v2_PC_34.4","v2_PC_35.1","v2_PC_35.2","v2_PC_36.1","v2_PC_36.2","v2_PC_36.2.e","v2_PC_36.3","v2_PC_36.4",  
            "v2_PC_36.4.e","v2_PC_36.5","v2_PC_36.5.e","v2_PC_37.4","v2_PC_38.3","v2_PC_38.4","v2_PC_38.4.e","v2_PC_38.5",    
            "v2_PC_38.5.e","v2_PC_38.6","v2_PC_40.4","v2_PC_40.4.e","v2_PC_40.5.e","v2_PC_40.6","v2_PC_42.5.e","v2_PE_36.5.e",   
            "v2_PE_38.5.e","v2_PE_38.6.e","v2_SM_32.1","v2_SM_32.2","v2_SM_33.1","v2_SM_34.1","v2_SM_34.2","v2_SM_35.1",    
            "v2_SM_36.0","v2_SM_36.1","v2_SM_36.2","v2_SM_38.1","v2_SM_38.2","v2_SM_40.1","v2_SM_40.2","v2_SM_41.1",    
            "v2_SM_41.2","v2_SM_42.1","v2_SM_42.2","v2_SM_42.3","v2_TG_48.1","v2_TG_48.2","v2_TG_50.1","v2_TG_50.2",    
            "v2_TG_50.3","v2_TG_50.4","v2_TG_51.2","v2_TG_52.1","v2_TG_52.2","v2_TG_52.3","v2_TG_52.4","v2_TG_52.5",    
            "v2_TG_54.2","v2_TG_54.3","v2_TG_54.4","v2_TG_54.5","v2_LA","v2_GlyA","v2_Ala","v2_Glycine",
            "v2_2-HbutA","v2_3-HbutA","v2_Val","v2_Leu","v2_Glycerol","v2_Isoleu","v2_Proline","v2_GliA",        
            "v2_Ser","v2_Thr","v2_Meth","v2_Orn","v2_GlutA","v2_Phe","v2_Lys","v2_CitA",        
            "v2_Fruc","v2_Glu","v2_Tyr","v2_PalA","v2_LinoA","v2_OleicA","v2_StearicA","v2_Tryp", 
            "v2_Sucrose","v2_alphaToco","v2_Choles")]
colnames(BBDD[4000:4236])
BBDD1 = BBDD[,c("allocation_no","v2_lean_mass","v8_lean_mass","v14_lean_mass","v2_bone_mass","v8_bone_mass",
                "v14_bone_mass","v2_fat_mass","v8_fat_mass","v14_fat_mass","sex", "v1_gq_11")]

BBDD2 = merge(BBDD1, bq1, by = "allocation_no")

###Masa magra: Modelos continuos:

MM = cbind(BBDD2[13], BBDD2[15:147])
MO = cbind(BBDD2[5], BBDD2[15:147])
MG = cbind(BBDD2[14], BBDD2[15:147])

MM = subset(MM, v2_lean_mass_a > 0) #Se eliminan valores NA. 10
MO = subset(MO, v2_bone_mass > 0) #Se eliminan valores NA. 9
MG = subset(MG, v2_fat_mass_a > 0) #Se eliminan valores NA. 9

#NAs en cada variables
imp1 = MG[2:134]

na1 = sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
na2 = sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))
na =  cbind("NA"=na1, "% NA"=na2)
na

#Metabolites con NA mayor de 20%
A_NA=rownames(na[na[,2]>20 ,])

#ReMGveMGs > 20%
drop = A_NA
imp1 = imp1[,!(names(imp1) %in% drop)] #10
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

imp1.rankNorm = apply(imp1, 2, rankNorm)

MG.rankNorm = data.frame(cbind(MG[1], imp1.rankNorm))

save(MG.rankNorm, file = "MG.rankNorm.29042020.rda")

rm(imp1, imp1.rankNorm, na, na1, na2, drop)

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
  
  training = MG.rankNorm$v2_fat_mass %>% createDataPartition(p = 0.8, list = F)
  
  train.data[[i]] = MG.rankNorm[training,]
  test.data[[i]] = MG.rankNorm[-training,]
  
  #modelo
  
  cv[[i]] = train(v2_fat_mass ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(v2_fat_mass~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$v2_fat_mass #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$v2_fat_mass), Rsquare = R2(predictions[[i]], test.data[[i]]$v2_fat_mass), Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$v2_fat_mass, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$v2_fat_mass)$estimate)
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.1

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
  
  train_rows[[i]] = MG.rankNorm$v2_fat_mass %>% createDataPartition(p = 0.80, list = F)
  training[[i]] = MG.rankNorm[train_rows[[i]],]
  validation[[i]] = MG.rankNorm[-train_rows[[i]],]
  
  a=as.data.frame(training[[i]])
  X[[i]] = as.matrix(a[,2:(dim(a)[2]-1)])
  Y[[i]] = a$v2_fat_mass
  
  b=as.data.frame(validation[[i]])
  X1[[i]] = as.matrix(b[,2:(dim(a)[2]-1)])
  Y1[[i]] = b$v2_fat_mass
  
  #modelo
  
  modelito[[i]] = glmnet(X[[i]], Y[[i]], alpha = 0.1, lambda = l)
  
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

BBDD.met.min = data.frame(colnames(MG.rankNorm))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.MG.rankNorm."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "29042020.ValMGv0.all.xlsx")

###############################--------- Parte 3: Validacion interna 
met <- read_excel("validaciones/29042020.ValMGv0.all.xlsx")

model.met = data.frame(met$metabolitos, met$media)
str(model.met)
model.met$met.metabolitos = as.character(model.met$met.metabolitos)
model.met = subset(model.met, met.media != 0)

X2 = data.frame(MG.rankNorm[,2:(dim(MG.rankNorm)[2])])
Y2 = MG.rankNorm$v2_fat_mass
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

X = data.frame(MG.rankNorm[,2:(dim(MG.rankNorm)[2])])
Y = MG.rankNorm$v2_fat_mass

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

BBDD.met.min = data.frame(colnames(MG.rankNorm))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.MG.rankNorm."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "29042020.CoefMGv0.all.xlsx")

save.image(file = "29042020.CoefMGv0.all.xlsx.RData")


#######----Tabla de caracteristicas basales:

V0.1 = V0[,c("allocation_no", "v2_lean_mass",  "v2_bone_mass",  "v2_fat_mass")]

V0.1 = merge(V0.1, V8, by = "allocation_no")

colnames(V0.1)

V0.1 = V0.1[,c("allocation_no", "v2_lean_mass",  "v2_bone_mass",  "v2_fat_mass",
               "v8_lean_mass",  "v8_bone_mass",  "v8_fat_mass")]

V0.1 = merge(V0.1, V14, by = "allocation_no")

V0.1 = V0.1[,c("allocation_no", "v2_lean_mass",  "v2_bone_mass",  "v2_fat_mass",
             "v8_lean_mass",  "v8_bone_mass", "v8_fat_mass", "v14_lean_mass",
             "v14_bone_mass",  "v14_fat_mass")]

colnames(V0.1)

T1 = BBDD[,c("allocation_no", "v1_gq_11", "v1_gq_1", "v2_4.1", "v2_4.2", "v2_4.3", "v2_4.4","v8_4.1", "v8_4.2", "v8_4.3", "v8_4.4", 
           "v8_5.1", "v8_5.2", "v8_5.3", "v8_5.4","v14_4.1", "v14_4.2", "v14_4.3", "v14_4.4", 
           "v14_5.1", "v14_5.2", "v14_5.3", "v14_5.4")]

T1 = merge(V0.1, T1, by = "allocation_no")

T2 = bq[,c("allocation_no", "v2_Glucose", "v2_Cholesterol", "v2_HDL-C", "v2_LDL-C", "v2_TG",
           "v8_Glucose", "v8_Cholesterol", "v8_HDL-C", "v8_LDL-C", "v8_TG",
           "v14_Glucose", "v14_Cholesterol", "v14_HDL-C", "v14_LDL-C", "v14_TG")]

Table1 = merge(T1, T2, by = "allocation_no")  

rm(T1, T2)

colnames(Table1) = c("allocation_no","v2_lean_mass","v2_bone_mass","v2_fat_mass","v8_lean_mass","v8_bone_mass",   
                     "v8_fat_mass", "v14_lean_mass",
                     "v14_bone_mass",  "v14_fat_mass", "edad","sexo","v2_altura1","v2_altura2","v2_peso1",
                     "v2_peso2","v8_peso1","v8_peso2","v8_circunferencia1","v8_circunferencia2","v8_5.1",
                     "v8_5.2","v8_5.3","v8_5.4","v14_peso1","v14_peso2","v14_4.3",        
                     "v14_4.4","v14_5.1","v14_5.2","v14_5.3","v14_5.4","v2_Glucose",     
                     "v2_Cholesterol","v2_HDL-C","v2_LDL-C","v2_TG","v8_Glucose","v8_Cholesterol", 
                     "v8_HDL-C","v8_LDL-C","v8_TG","v14_Glucose","v14_Cholesterol","v14_HDL-C",  
                     "v14_LDL-C","v14_TG")

Table1$sexo = factor(Table1$sexo, labels = c("F", "M"))

export(Table1, "Table1.xlsx")

##Calculos de medias

mean(Table1$edad, na.rm = T)
sd(Table1$edad, na.rm = T)

table(Table1$sexo)

mean(((Table1$v2_peso1 + Table1$v2_peso2)/2), na.rm = T)
sd(((Table1$v2_peso1 + Table1$v2_peso2)/2), na.rm = T)

mean((((Table1$v2_peso1 + Table1$v2_peso2)/2)/((Table1$v2_altura1 + Table1$v2_altura2)/2)^2), na.rm = T)
sd((((Table1$v2_peso1 + Table1$v2_peso2)/2)/((Table1$v2_altura1 + Table1$v2_altura2)/2)^2), na.rm = T)

mean(((Table1$v8_peso1 + Table1$v8_peso2)/2), na.rm = T)
sd(((Table1$v8_peso1 + Table1$v8_peso2)/2), na.rm = T)

mean((((Table1$v8_peso1 + Table1$v8_peso2)/2)/((Table1$v2_altura1 + Table1$v2_altura2)/2)^2), na.rm = T)
sd((((Table1$v8_peso1 + Table1$v8_peso2)/2)/((Table1$v2_altura1 + Table1$v2_altura2)/2)^2), na.rm = T)

mean(((Table1$v14_peso1 + Table1$v14_peso2)/2), na.rm = T)
sd(((Table1$v14_peso1 + Table1$v14_peso2)/2), na.rm = T)

mean((((Table1$v14_peso1 + Table1$v14_peso2)/2)/((Table1$v2_altura1 + Table1$v2_altura2)/2)^2), na.rm = T)
sd((((Table1$v14_peso1 + Table1$v14_peso2)/2)/((Table1$v2_altura1 + Table1$v2_altura2)/2)^2), na.rm = T)

mean(Table1$v2_lean_mass, na.rm = T)
sd(Table1$v2_lean_mass, na.rm = T)

mean(Table1$v2_bone_mass, na.rm = T)
sd(Table1$v2_bone_mass, na.rm = T)

mean(Table1$v2_fat_mass, na.rm = T)
sd(Table1$v2_fat_mass, na.rm = T)

mean(Table1$v8_lean_mass, na.rm = T)
sd(Table1$v8_lean_mass, na.rm = T)

mean(Table1$v8_bone_mass, na.rm = T)
sd(Table1$v8_bone_mass, na.rm = T)

mean(Table1$v8_fat_mass, na.rm = T)
sd(Table1$v8_fat_mass, na.rm = T)

mean(Table1$v14_lean_mass, na.rm = T)
sd(Table1$v14_lean_mass, na.rm = T)

mean(Table1$v14_bone_mass, na.rm = T)
sd(Table1$v14_bone_mass, na.rm = T)

mean(Table1$v14_fat_mass, na.rm = T)
sd(Table1$v14_fat_mass, na.rm = T)

mean(Table1$v2_Glucose, na.rm = T)
sd(Table1$v2_Glucose, na.rm = T)

mean(Table1$v2_Cholesterol, na.rm = T)
sd(Table1$v2_Cholesterol, na.rm = T)

mean(Table1$v2_HDL, na.rm = T)
sd(Table1$v2_HDL, na.rm = T)

mean(Table1$v2_LDL, na.rm = T)
sd(Table1$v2_LDL, na.rm = T)

mean(Table1$v2_TG, na.rm = T)
sd(Table1$v2_TG, na.rm = T)

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

mean(Table1$v14_Glucose, na.rm = T)
sd(Table1$v14_Glucose, na.rm = T)

mean(Table1$v14_Cholesterol, na.rm = T)
sd(Table1$v14_Cholesterol, na.rm = T)

mean(Table1$v14_HDL, na.rm = T)
sd(Table1$v14_HDL, na.rm = T)

mean(Table1$v14_LDL, na.rm = T)
sd(Table1$v14_LDL, na.rm = T)

mean(Table1$v14_TG, na.rm = T)
sd(Table1$v14_TG, na.rm = T)

#####Table 2:

Table2 = subset(Table1, v8_lean_mass >0)

table(Table2$sexo)

Table2.1 = subset(Table1, v14_lean_mass >0)

table(Table2.1$sexo)

export(Table1, "Table1.xlsx")

#P-values:

Table3 <- read_excel("Table1.xlsx")

colnames(Table3)
anova(lm(v2_lean_mass ~ visita, Table3))
anova(lm(v2_bone_mass ~ visita, Table3))
anova(lm(v2_fat_mass ~ visita, Table3))

Table3$peso = (Table3$v2_peso1 + Table3$v2_peso2)/2
anova(lm(peso ~ visita, Table3))

Table3$imc = (((Table3$v2_peso1 + Table3$v2_peso2)/2)/((Table3$v2_altura1 + Table3$v2_altura2)/2)^2)
anova(lm(imc ~ visita, Table3))

anova(lm(v2_Glucose ~ visita, Table3))
anova(lm(v2_Cholesterol ~ visita, Table3))
anova(lm(Table3$v2_HDL ~ Table3$visita))
anova(lm(v2_LDL  ~ visita, Table3))
anova(lm(v2_TG ~ visita, Table3))


####--- Figuras:

Resultados <- read_excel("/Volumes/JES GAVI/ARTICULOS/Autor/Satin/ART huella composicion corporal/Resultados.xlsx", 
                         sheet = "Full2")

Resultados <- read_excel("D:/ARTICULOS/Autor/Satin/ART huella composicion corporal/Resultados.xlsx", 
                         sheet = "Figura v3")

##Lean mass

seleccion = subset(Resultados, media.lm > -100, select = c("metabolitos","media.lm", "sd.lm"))

seleccion = seleccion[order(seleccion$media.lm),]

ggplot(seleccion, aes(x = media.lm, y = metabolitos, xmin = media.lm-sd.lm, xmax = media.lm+sd.lm)) +
  geom_point(size = 1) + 
  xlab("Coefficient value") + ylab("Metabolites") +
  geom_errorbarh(size = 0.5, height = 0.5)

##Se generan dos gráficos para una mejor visualizacion de los resultados:
seleccion_neg = seleccion[(seleccion$media.lm<0), ]
seleccion_pos = seleccion[(seleccion$media.lm>0), ]

#Negative

nombres_neg = seleccion_neg$metabolitos

seleccion_neg = seleccion_neg[order(seleccion_neg$media.lm),]

seleccion_neg$metabolitos = factor(seleccion_neg$metabolitos)

neg = ggplot(seleccion_neg, aes(x = media.lm, y = reorder(metabolitos, -media.lm), xmin = media.lm-sd.lm, xmax = media.lm+sd.lm)) +
  geom_point(size = 2) + 
  xlab("Coefficient value") + ylab("Metabolites") +
  geom_errorbarh(size = 0.5, height = 0.1) +
  scale_x_continuous(limits=c(-1, 0)) +
  theme_bw()

#Positive

nombres_pos = seleccion_pos$metabolitos

pos = ggplot(seleccion_pos, aes(x = media.lm, y = metabolitos, xmin = media.lm-sd.lm, xmax = media.lm+sd.lm)) +
  geom_point(size = 2) + 
  xlab("Coefficient value") + ylab("Metabolites") +
  geom_errorbarh(size = 0.5, height = 0.1) +
  scale_x_continuous(limits=c(0, 1.5)) +
  scale_y_discrete(limits= nombres_pos, position = "right") +
  theme_bw() 

ambos = grid.arrange(neg, pos, ncol=2)

##Bone mass

#seleccion = subset(Resultados, media.bm > -100, select = c("metabolitos","media.bm", "sd.bm"))

#seleccion = seleccion[order(seleccion$media.bm),]

#ggplot(seleccion, aes(x = media.bm, y = metabolitos, xmin = media.bm-sd.bm, xmax = media.bm+sd.bm)) +
#  geom_point(size = 1) + 
#  xlab("Coefficient value") + ylab("Metabolites") +
#  geom_errorbarh(size = 0.5, height = 0.5)

##Se generan dos gráficos para una mejor visualizacion de los resultados:
#seleccion_neg = seleccion[(seleccion$media.bm<0), ]
#seleccion_pos = seleccion[(seleccion$media.bm>0), ]

#Negative

#nombres_neg = seleccion_neg$metabolitos

#seleccion_neg = seleccion_neg[order(seleccion_neg$media.bm),]

#seleccion_neg$metabolitos = factor(seleccion_neg$metabolitos)

#neg = ggplot(seleccion_neg, aes(x = media.bm, y = reorder(metabolitos, -media.bm), xmin = media.bm-sd.bm, xmax = media.bm+sd.bm)) +
#  geom_point(size = 2) + 
#  xlab("Coefficient value") + ylab("Metabolites") +
#  geom_errorbarh(size = 0.5, height = 0.1) +
#  scale_x_continuous(limits=c(-0.08, 0)) +
#  theme_bw()

#Positive

#nombres_pos = seleccion_pos$metabolitos

#pos = ggplot(seleccion_pos, aes(x = media.bm, y = metabolitos, xmin = media.bm-sd.bm, xmax = media.bm+sd.bm)) +
#  geom_point(size = 2) + 
#  xlab("Coefficient value") + ylab("Metabolites") +
#  geom_errorbarh(size = 0.5, height = 0.1) +
#  scale_x_continuous(limits=c(0, 0.15)) +
#  scale_y_discrete(limits= nombres_pos, position = "right") +
#  theme_bw() 

#ambos = grid.arrange(neg, pos, ncol=2)

##Fat mass

seleccion = subset(Resultados, media.fm > -100, select = c("metabolitos","media.fm", "sd.fm"))

seleccion = seleccion[order(seleccion$media.fm),]

ggplot(seleccion, aes(x = media.fm, y = metabolitos, xmin = media.fm-sd.fm, xmax = media.fm+sd.fm)) +
  geom_point(size = 1) + 
  xlab("Coefficient value") + ylab("Metabolites") +
  geom_errorbarh(size = 0.5, height = 0.5)

##Se generan dos gráficos para una mejor visualizacion de los resultados:
seleccion_neg = seleccion[(seleccion$media.fm<0), ]
seleccion_pos = seleccion[(seleccion$media.fm>0), ]

#Negative

nombres_neg = seleccion_neg$metabolitos

seleccion_neg = seleccion_neg[order(seleccion_neg$media.fm),]

seleccion_neg$metabolitos = factor(seleccion_neg$metabolitos)

neg = ggplot(seleccion_neg, aes(x = media.fm, y = reorder(metabolitos, -media.fm), xmin = media.fm-sd.fm, xmax = media.fm+sd.fm)) +
  geom_point(size = 2) + 
  xlab("Coefficient value") + ylab("Metabolites") +
  geom_errorbarh(size = 0.5, height = 0.1) +
  scale_x_continuous(limits=c(-1.2, 0)) +
  theme_bw()

#Positive

nombres_pos = seleccion_pos$metabolitos

pos = ggplot(seleccion_pos, aes(x = media.fm, y = metabolitos, xmin = media.fm-sd.fm, xmax = media.fm+sd.fm)) +
  geom_point(size = 2) + 
  xlab("Coefficient value") + ylab("Metabolites") +
  geom_errorbarh(size = 0.5, height = 0.1) +
  scale_x_continuous(limits=c(0, 1.3)) +
  scale_y_discrete(limits= nombres_pos, position = "right") +
  theme_bw() 

ambos = grid.arrange(neg, pos, ncol=2)



