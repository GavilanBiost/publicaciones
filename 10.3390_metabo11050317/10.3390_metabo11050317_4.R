###Elastic en V0, V8 y v14.

##Creación de las bbdd:
##Solo se incluyen aquellas variables que están en todas las visitas:

bq1 = bq[,c("allocation_no","v2_FChr","v2_Echr",        
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
BBDD1 = BBDD[,c("allocation_no","v2_lean_mass","v2_bone_mass","v2_fat_mass")]

V0 = merge(BBDD1, bq1, by = "allocation_no")

bq1 = bq[,c("allocation_no","v8_FChr","v8_Echr",        
            "v8_TChr","v8_TG_RMN","v8_PhCh","v8_LCh","v8_Sph","v8_FAC","v8_w3","v8_ARA+EPA",
            "v8_DHA","v8_LINOLEIC","v8_PUFA","v8_MUFA","v8_LPC_14.0","v8_LPC_15.0","v8_LPC_16.0","v8_LPC_16.0.e", 
            "v8_LPC_16.1","v8_LPC_16.1.e","v8_LPC_17.0","v8_LPC_18.0","v8_LPC_18.0.e","v8_LPC_18.1","v8_LPC_18.2","v8_LPC_20.0",    
            "v8_LPC_20.1","v8_LPC_20.3","v8_LPC_20.4","v8_LPC_22.6","v8_PC_30.0","v8_PC_32.0","v8_PC_32.1","v8_PC_32.1.e",   
            "v8_PC_32.2","v8_PC_33.1","v8_PC_34.0","v8_PC_34.1","v8_PC_34.1.e","v8_PC_34.2","v8_PC_34.2.e","v8_PC_34.3.e",   
            "v8_PC_34.4","v8_PC_35.1","v8_PC_35.2","v8_PC_36.1","v8_PC_36.2","v8_PC_36.2.e","v8_PC_36.3","v8_PC_36.4",  
            "v8_PC_36.4.e","v8_PC_36.5","v8_PC_36.5.e","v8_PC_37.4","v8_PC_38.3","v8_PC_38.4","v8_PC_38.4.e","v8_PC_38.5",    
            "v8_PC_38.5.e","v8_PC_38.6","v8_PC_40.4","v8_PC_40.4.e","v8_PC_40.5.e","v8_PC_40.6","v8_PC_42.5.e","v8_PE_36.5.e",   
            "v8_PE_38.5.e","v8_PE_38.6.e","v8_SM_32.1","v8_SM_32.2","v8_SM_33.1","v8_SM_34.1","v8_SM_34.2","v8_SM_35.1",    
            "v8_SM_36.0","v8_SM_36.1","v8_SM_36.2","v8_SM_38.1","v8_SM_38.2","v8_SM_40.1","v8_SM_40.2","v8_SM_41.1",    
            "v8_SM_41.2","v8_SM_42.1","v8_SM_42.2","v8_SM_42.3","v8_TG_48.1","v8_TG_48.2","v8_TG_50.1","v8_TG_50.2",    
            "v8_TG_50.3","v8_TG_50.4","v8_TG_51.2","v8_TG_52.1","v8_TG_52.2","v8_TG_52.3","v8_TG_52.4","v8_TG_52.5",    
            "v8_TG_54.2","v8_TG_54.3","v8_TG_54.4","v8_TG_54.5","v8_LA","v8_GlyA","v8_Ala","v8_Glycine",
            "v8_2-HbutA","v8_3-HbutA","v8_Val","v8_Leu","v8_Glycerol","v8_Isoleu","v8_Proline","v8_GliA",        
            "v8_Ser","v8_Thr","v8_Meth","v8_Orn","v8_GlutA","v8_Phe","v8_Lys","v8_CitA",        
            "v8_Fruc","v8_Glu","v8_Tyr","v8_PalA","v8_LinoA","v8_OleicA","v8_StearicA","v8_Tryp", 
            "v8_Sucrose","v8_alphaToco","v8_Choles")]
BBDD1 = BBDD[,c("allocation_no","v8_lean_mass","v8_bone_mass","v8_fat_mass")]

V8 = merge(BBDD1, bq1, by = "allocation_no")

bq1 = bq[,c("allocation_no","v14_FChr","v14_Echr",        
            "v14_TChr","v14_TG_RMN","v14_PhCh","v14_LCh","v14_Sph","v14_FAC","v14_w3","v14_ARA+EPA",
            "v14_DHA","v14_LINOLEIC","v14_PUFA","v14_MUFA","v14_LPC_14.0","v14_LPC_15.0","v14_LPC_16.0","v14_LPC_16.0.e", 
            "v14_LPC_16.1","v14_LPC_16.1.e","v14_LPC_17.0","v14_LPC_18.0","v14_LPC_18.0.e","v14_LPC_18.1","v14_LPC_18.2","v14_LPC_20.0",    
            "v14_LPC_20.1","v14_LPC_20.3","v14_LPC_20.4","v14_LPC_22.6","v14_PC_30.0","v14_PC_32.0","v14_PC_32.1","v14_PC_32.1.e",   
            "v14_PC_32.2","v14_PC_33.1","v14_PC_34.0","v14_PC_34.1","v14_PC_34.1.e","v14_PC_34.2","v14_PC_34.2.e","v14_PC_34.3.e",   
            "v14_PC_34.4","v14_PC_35.1","v14_PC_35.2","v14_PC_36.1","v14_PC_36.2","v14_PC_36.2.e","v14_PC_36.3","v14_PC_36.4",  
            "v14_PC_36.4.e","v14_PC_36.5","v14_PC_36.5.e","v14_PC_37.4","v14_PC_38.3","v14_PC_38.4","v14_PC_38.4.e","v14_PC_38.5",    
            "v14_PC_38.5.e","v14_PC_38.6","v14_PC_40.4","v14_PC_40.4.e","v14_PC_40.5.e","v14_PC_40.6","v14_PC_42.5.e","v14_PE_36.5.e",   
            "v14_PE_38.5.e","v14_PE_38.6.e","v14_SM_32.1","v14_SM_32.2","v14_SM_33.1","v14_SM_34.1","v14_SM_34.2","v14_SM_35.1",    
            "v14_SM_36.0","v14_SM_36.1","v14_SM_36.2","v14_SM_38.1","v14_SM_38.2","v14_SM_40.1","v14_SM_40.2","v14_SM_41.1",    
            "v14_SM_41.2","v14_SM_42.1","v14_SM_42.2","v14_SM_42.3","v14_TG_48.1","v14_TG_48.2","v14_TG_50.1","v14_TG_50.2",    
            "v14_TG_50.3","v14_TG_50.4","v14_TG_51.2","v14_TG_52.1","v14_TG_52.2","v14_TG_52.3","v14_TG_52.4","v14_TG_52.5",    
            "v14_TG_54.2","v14_TG_54.3","v14_TG_54.4","v14_TG_54.5","v14_LA","v14_GlyA","v14_Ala","v14_Glycine",
            "v14_2-HbutA","v14_3-HbutA","v14_Val","v14_Leu","v14_Glycerol","v14_Isoleu","v14_Proline","v14_GliA",        
            "v14_Ser","v14_Thr","v14_Meth","v14_Orn","v14_GlutA","v14_Phe","v14_Lys","v14_CitA",        
            "v14_Fruc","v14_Glu","v14_Tyr","v14_PalA","v14_LinoA","v14_OleicA","v14_StearicA","v14_Tryp", 
            "v14_Sucrose","v14_alphaToco","v14_Choles")]
BBDD1 = BBDD[,c("allocation_no","v14_lean_mass","v14_bone_mass","v14_fat_mass")]

V14 = merge(BBDD1, bq1, by = "allocation_no")

#####--------------V0

##MM
###Masa magra: Modelos continuos:
colnames(V14)

MM = cbind(V14[2], V14[5:137])
MO = cbind(V14[3], V14[5:137])
MG = cbind(V14[4], V14[5:137])

MM = subset(MM, v14_lean_mass > 0) #Se eliminan valores NA. 91
MO = subset(MO, v14_bone_mass > 0) #Se eliminan valores NA. 91
MG = subset(MG, v14_fat_mass > 0) #Se eliminan valores NA. 91

#NAs en cada variables
imp1 = MO[2:134]

na1 = sapply(imp1, function(imp1) sum(length(which(is.na(imp1)))))
na2 = sapply(imp1, function(imp1) (100*sum(length(which(is.na(imp1))))/sum(length((imp1)))))
na =  cbind("NA"=na1, "% NA"=na2)
na

#Metabolites con NA mayor de 20%
A_NA=rownames(na[na[,2]>20 ,])

#ReMOveMOs > 20%
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

MO.rankNorm = data.frame(cbind(MO[1], imp1.rankNorm))

save(MO.rankNorm, file = "MOV14.rankNorm.06052020.rda")

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
  
  training = MO.rankNorm$v14_bone_mass %>% createDataPartition(p = 0.8, list = F)
  
  train.data[[i]] = MO.rankNorm[training,]
  test.data[[i]] = MO.rankNorm[-training,]
  
  #modelo
  
  cv[[i]] = train(v14_bone_mass ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(v14_bone_mass~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$v14_bone_mass #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$v14_bone_mass), Rsquare = R2(predictions[[i]], test.data[[i]]$v14_bone_mass), Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$v14_bone_mass, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$v14_bone_mass)$estimate)
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
  
  train_rows[[i]] = MO.rankNorm$v14_bone_mass %>% createDataPartition(p = 0.80, list = F)
  training[[i]] = MO.rankNorm[train_rows[[i]],]
  validation[[i]] = MO.rankNorm[-train_rows[[i]],]
  
  a=as.data.frame(training[[i]])
  X[[i]] = as.matrix(a[,2:(dim(a)[2]-1)])
  Y[[i]] = a$v14_bone_mass
  
  b=as.data.frame(validation[[i]])
  X1[[i]] = as.matrix(b[,2:(dim(a)[2]-1)])
  Y1[[i]] = b$v14_bone_mass
  
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

BBDD.met.min = data.frame(colnames(MO.rankNorm))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.MO.rankNorm."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","V2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "06052020.ValMOV14.xlsx")

###############################--------- Parte 3: Validacion interna 
met <- read_excel("06052020.ValMOV14.xlsx")

model.met = data.frame(met$metabolitos, met$media)
str(model.met)
model.met$met.metabolitos = as.character(model.met$met.metabolitos)
model.met = subset(model.met, met.media != 0)

X2 = data.frame(MO.rankNorm[,2:(dim(MO.rankNorm)[2])])
Y2 = MO.rankNorm$v14_bone_mass
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

X = data.frame(MO.rankNorm[,2:(dim(MO.rankNorm)[2])])
Y = MO.rankNorm$v14_bone_mass

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

BBDD.met.min = data.frame(colnames(MO.rankNorm))
colnames(BBDD.met.min)[colnames(BBDD.met.min)=="colnames.MO.rankNorm."] = "seleccion.min1"

for (i in 1:10) {
  BBDD.met.min= merge(BBDD.met.min, coef_met_min[[i]], by = "seleccion.min1", all.x = T)
}
colnames(BBDD.met.min) = c("metabolitos","v1","V2","v3","v4","v5","v6","v7","v8","v9","v10")

export(BBDD.met.min, "06052020.CoefMOV14.xlsx")

save.image(file = "06052020.CoefMOV14.xlsx.RData")





