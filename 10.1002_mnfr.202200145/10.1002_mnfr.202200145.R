# Librerías: ####
library(readr)
library(haven)
library(compare)
library(rio)
library(Hmisc)
library(caret)
library(dplyr)
library(pROC)
library(glmnet)
library(gtools)
library(readr)
library(shiny)
library(MASS)
library(lattice)
library(colorspace)
library(rJava)
library(xlsxjars)
library(iterators)
library(foreach)
library(mixOmics)
library(xlsx)
library(missForest)
library(randomForest)
library(dendextend)
library(ggrepel)
library(abind)
library(factoextra)
library(pls)
library(ropls)
library(knitr)
library(dplyr)
library(agricolae)
library(QuantPsyc)
library(Rcpp)
library(tibble)
library(rgl)
library(tidyr)
library(cluster)
library(mclust)
library(pvclust)
library(Amelia)
library(foreign)
library(chillR)
library(tictoc)
library(cvAUC)
library(gmodels)
library(foreign)
library(reshape)
library(ggplot2)
library(nortest)
library(car)
library(Hmisc)
library(xlsx)
library(rJava)
library(varhandle)
library(plyr)
library(rpart)
library(Publish)
library(Rmisc)
library(readxl)
library(survival)
library(PerformanceAnalytics)
library(lmtest)
library(tis)

# Bases de datos: ####
NEW_Combined_metabolites_phenotypes_230218 <- read_csv("E:/ARTICUlOS/BBDD/PREDIMED/NEW_Combined_metabolites_phenotypes_230218.csv")
phenotypes_predimed_20190903 <- read_csv("E:/ARTICUlOS/BBDD/PREDIMED/phenotypes_predimed_20190903.csv")
FFQ_PREDIMED_2012 <- read_sav("E:/ARTICUlOS/BBDD/PREDIMED/FFQ_PREDIMED_2012.SAV")

###Fusion de las BBDD.
BBDD = merge(phenotypes_predimed_20190903, FFQ_PREDIMED_2012, by = "id")

BBDD_basal = BBDD_basal[1]

BBDD = merge(BBDD_basal, BBDD, by = "id")

# Variables: carnicos, pescados, carnes blancas, carnes rojas, carnes procesadas.
# Carnicos y pescados ya existe. Genero las variables de carnes blancas, rojas y procesadas segun el articulo: 10.1016/j.clnu.2016.03.017
# Carne roja: cerdo, ternera y cordero
# Carne procesada: despojos, jamón, embutidos, paté, hamburguesas y tocino.

BBDD$RM = BBDD$c_ternera + BBDD$c_cerdo + BBDD$c_cordero
summary(BBDD$RM)
BBDD$PRM = BBDD$higad + BBDD$visceras + BBDD$j_serrano + BBDD$j_cocido + BBDD$embutidos + BBDD$pates + BBDD$hamburguesa + BBDD$bacon
summary(BBDD$PRM)
summary(BBDD$pescados)
summary(BBDD$carnicos)

BBDD$RM3 = BBDD$c_ternera3 + BBDD$c_cerdo3 + BBDD$c_cordero3
summary(BBDD$RM3)
BBDD$PRM3 = BBDD$higad3 + BBDD$visceras3 + BBDD$j_serrano3 + BBDD$j_cocido3 + BBDD$embutidos3 + BBDD$pates3 + BBDD$hamburguesa3 + BBDD$bacon3
summary(BBDD$PRM3)
summary(BBDD$pescados3)
summary(BBDD$carnicos3)

# Filtraje: ####

# Participantes sin FFQ: 11 participantes (baseline).
# Participantes sin FFQ: 269 participantes (1 year).

BBDD = subset (BBDD, RM >= 0)

# Extremos de energia: (modificar variable o variable3 segun sea visita basal o anual)

BBDD$sub1 = 2 #Se genera la variable que marcará que participantes cumplen el requisito.

for (i in 1:length(BBDD$sub1)){
  if (BBDD$energiat[i]>=4000 & BBDD$sex[i]== "man") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat[i]<=800 & BBDD$sex[i]== "man") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat[i]>=3500 & BBDD$sex[i]== "woman") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat[i]<=500 & BBDD$sex[i]== "woman") {BBDD$sub1[i]='1'} 
  else {BBDD$sub1[i]='0'}}
table(BBDD$sub1)

#Se elimina 34 participantes (extremos de energia) (baseline)
#Se elimina 22 participantes (extremos de energia) (1 year)

BBDD = subset(BBDD, sub1 == 0)

# Falta el filtraje por participantes sub >20% metabolitos.

BBDD$ter_RM = cut2(BBDD$RM, g = 3)
table(BBDD$ter_RM)
tapply(BBDD$RM, BBDD$ter_RM, mean, na.rm = T)

BBDD$ter_PRM = cut2(BBDD$PRM, g = 3)
table(BBDD$ter_PRM)
tapply(BBDD$PRM, BBDD$ter_PRM, mean, na.rm = T)

BBDD$ter_M = cut2(BBDD$carnicos, g = 3)
table(BBDD$ter_M)
tapply(BBDD$carnicos, BBDD$ter_M, mean, na.rm = T)

BBDD$ter_F = cut2(BBDD$pescados, g = 3)
table(BBDD$ter_F)
tapply(BBDD$pescados, BBDD$ter_F, mean, na.rm = T)

# Café:

tapply(BBDD$cafes, BBDD$ter_M, mean, na.rm = T)
tapply(BBDD$cafes, BBDD$ter_M, quantile, na.rm = T)

#Tablas descriptivas: ####
##Según terciles pescados: ####
colnames(BBDD)[1000:1258]
colnames(BBDD)[1:999]

#BBDD1 = data.frame(cbind(BBDD[1258], BBDD[1257], BBDD[905:916], BBDD[889:897], BBDD[1246:1248]))#Baseline
BBDD1 = data.frame(cbind(BBDD[1258], BBDD[1257], BBDD[1083:1094], BBDD[1068:1076], BBDD[1250:1252]))

colnames(BBDD1)

colnames(BBDD)

BBDD2 = data.frame(cbind(BBDD[1254], BBDD[3:9], BBDD[11:12], BBDD[58:60], BBDD[70]))

colnames(BBDD2)

medianas = c()
iqrs = c()
cuantiles = c()
medianas.tot = c()
iqrs.tot = c()
cuantiles.tot = c()

for (i in 3:26) {
  medianas[[i]] = tapply(BBDD1[[i]], BBDD1[[2]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD1[[i]], BBDD1[[2]], IQR, na.rm = T)
  cuantiles [[i]] = tapply(BBDD1[[i]], BBDD1[[2]], quantile, na.rm = T)
  medianas.tot[[i]] = median(BBDD1[[i]],na.rm = T)
  iqrs.tot[[i]] = IQR(BBDD1[[i]], na.rm = T)
  cuantiles.tot [[i]] = quantile(BBDD1[[i]], na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)
medianas.tot = data.frame(matrix(unlist(medianas.tot), ncol=1, byrow=T),stringsAsFactors=FALSE)
iqrs.tot = data.frame(matrix(unlist(iqrs.tot), ncol=1, byrow=T),stringsAsFactors=FALSE)
cuantiles = data.frame(matrix(unlist(cuantiles), ncol=15, byrow=T),stringsAsFactors=FALSE)
cuantiles.tot = data.frame(matrix(unlist(cuantiles.tot), ncol=5, byrow=T),stringsAsFactors=FALSE)

iqrs.tot = data.frame(cbind(medianas.tot, iqrs.tot))
iqrs.tot = iqrs.tot[-2,]
iqrs.tot = iqrs.tot[-1,]
iqrs.tot = data.frame(cbind(iqrs.tot, cuantiles.tot))

resumen = data.frame(cbind(medianas, iqrs, cuantiles, iqrs.tot))

resumen$nombres = colnames(BBDD1)[3:26]

colnames(resumen) = c("T1", "T2", "T3", "IQR1", "IQR2", "IQR3", "0", "25", "50", "75", "100", "0", "25", "50", "75", "100", "0", "25", "50", "75", "100", "tot", "IQR", "0", "25", "50", "75", "100","names")

export(resumen, "alimentacion_IQR_carne3.xlsx")

colnames(BBDD2)

(table(as.factor(BBDD2$sex), BBDD2$ter_F)) 
(prop.table(table(as.factor(BBDD2$sex), BBDD2$ter_F),2)*100) 

(table(as.factor(BBDD2$diabetes0), BBDD2$ter_F)) 
(prop.table(table(as.factor(BBDD2$diabetes0), BBDD2$ter_F),2)*100) 

(table(as.factor(BBDD2$dyslip0), BBDD2$ter_F)) 
(prop.table(table(as.factor(BBDD2$dyslip0), BBDD2$ter_F),2)*100) 

(table(as.factor(BBDD2$hyperten0), BBDD2$ter_F)) 
(prop.table(table(as.factor(BBDD2$hyperten0), BBDD2$ter_F),2)*100) 

(table(as.factor(BBDD2$fam_history), BBDD2$ter_F)) 
(prop.table(table(as.factor(BBDD2$fam_history), BBDD2$ter_F),2)*100) 

(table(as.factor(BBDD2$smoking0), BBDD2$ter_F)) 
(prop.table(table(as.factor(BBDD2$smoking0), BBDD2$ter_F),2)*100) 

tapply(BBDD2$age, BBDD2$ter_F, median, na.rm = T)
tapply(BBDD2$age, BBDD2$ter_F, IQR, na.rm = T)

tapply(BBDD2$bmi0, BBDD2$ter_F, median, na.rm = T)
tapply(BBDD2$bmi0, BBDD2$ter_F, IQR, na.rm = T)

tapply(BBDD2$waist_0, BBDD2$ter_F, median, na.rm = T)
tapply(BBDD2$waist_0, BBDD2$ter_F, IQR, na.rm = T)

tapply(BBDD2$cholesterol0, BBDD2$ter_F, median, na.rm = T)
tapply(BBDD2$cholesterol0, BBDD2$ter_F, IQR, na.rm = T)

tapply(BBDD2$triglycerol0, BBDD2$ter_F, median, na.rm = T)
tapply(BBDD2$triglycerol0, BBDD2$ter_F, IQR, na.rm = T)

tapply(BBDD2$hdl0, BBDD2$ter_F, median, na.rm = T)
tapply(BBDD2$hdl0, BBDD2$ter_F, IQR, na.rm = T)

##Según carnes: ####

colnames(BBDD1)

medianas = c()
iqrs = c()
medianas.tot = c()
iqrs.tot = c()

for (i in 3:27) {
  medianas[[i]] = tapply(BBDD1[[i]], BBDD1[[2]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD1[[i]], BBDD1[[2]], IQR, na.rm = T)
  medianas.tot[[i]] = median(BBDD1[[i]],na.rm = T)
  iqrs.tot[[i]] = IQR(BBDD1[[i]], na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)
medianas.tot = data.frame(matrix(unlist(medianas.tot), ncol=1, byrow=T),stringsAsFactors=FALSE)
iqrs.tot = data.frame(matrix(unlist(iqrs.tot), ncol=1, byrow=T),stringsAsFactors=FALSE)

iqrs.tot = data.frame(cbind(medianas.tot, iqrs.tot))
iqrs.tot$X2 = colnames(BBDD1)[1:27]
iqrs.tot = iqrs.tot[-2,]
iqrs.tot = iqrs.tot[-1,]

resumen = data.frame(cbind(medianas, iqrs, iqrs.tot))

colnames(resumen) = c("T1", "T2", "T3", "IQR1", "IQR2", "IQR3", "tot", "IQR", "names")

export(resumen, "alimentacion2.xlsx")

(table(as.factor(BBDD2$sex), BBDD2$ter_M)) 
(prop.table(table(as.factor(BBDD2$sex), BBDD2$ter_M),2)*100) 

(table(as.factor(BBDD2$diabetes0), BBDD2$ter_M)) 
(prop.table(table(as.factor(BBDD2$diabetes0), BBDD2$ter_M),2)*100) 

(table(as.factor(BBDD2$dyslip0), BBDD2$ter_M)) 
(prop.table(table(as.factor(BBDD2$dyslip0), BBDD2$ter_M),2)*100) 

(table(as.factor(BBDD2$hyperten0), BBDD2$ter_M)) 
(prop.table(table(as.factor(BBDD2$hyperten0), BBDD2$ter_M),2)*100) 

(table(as.factor(BBDD2$fam_history), BBDD2$ter_M)) 
(prop.table(table(as.factor(BBDD2$fam_history), BBDD2$ter_M),2)*100) 

(table(as.factor(BBDD2$smoking0), BBDD2$ter_M)) 
(prop.table(table(as.factor(BBDD2$smoking0), BBDD2$ter_M),2)*100) 

tapply(BBDD2$age, BBDD2$ter_M, median, na.rm = T)
tapply(BBDD2$age, BBDD2$ter_M, IQR, na.rm = T)
tapply(BBDD2$age, BBDD2$ter_M, quantile, na.rm = T)

tapply(BBDD2$bmi0, BBDD2$ter_M, median, na.rm = T)
tapply(BBDD2$bmi0, BBDD2$ter_M, IQR, na.rm = T)
tapply(BBDD2$bmi0, BBDD2$ter_M, quantile, na.rm = T)

tapply(BBDD2$waist_0, BBDD2$ter_M, median, na.rm = T)
tapply(BBDD2$waist_0, BBDD2$ter_M, IQR, na.rm = T)
tapply(BBDD2$waist_0, BBDD2$ter_M, quantile, na.rm = T)

tapply(BBDD2$cholesterol0, BBDD2$ter_M, median, na.rm = T)
tapply(BBDD2$cholesterol0, BBDD2$ter_M, IQR, na.rm = T)

tapply(BBDD2$triglycerol0, BBDD2$ter_M, median, na.rm = T)
tapply(BBDD2$triglycerol0, BBDD2$ter_M, IQR, na.rm = T)

tapply(BBDD2$hdl0, BBDD2$ter_M, median, na.rm = T)
tapply(BBDD2$hdl0, BBDD2$ter_M, IQR, na.rm = T)

##Total ####

(table(as.factor(BBDD2$sex))) 
(prop.table(table(as.factor(BBDD2$sex)))*100) 

(table(as.factor(BBDD2$diabetes0))) 
(prop.table(table(as.factor(BBDD2$diabetes0)))*100) 

(table(as.factor(BBDD2$dyslip0))) 
(prop.table(table(as.factor(BBDD2$dyslip0)))*100) 

(table(as.factor(BBDD2$hyperten0))) 
(prop.table(table(as.factor(BBDD2$hyperten0)))*100) 

(table(as.factor(BBDD2$fam_history))) 
(prop.table(table(as.factor(BBDD2$fam_history)))*100) 

(table(as.factor(BBDD2$smoking0))) 
(prop.table(table(as.factor(BBDD2$smoking0)))*100) 

median(BBDD2$age, na.rm = T)
IQR(BBDD2$age, na.rm = T)
quantile(BBDD2$age, na.rm = T)

median(BBDD2$bmi0, na.rm = T)
IQR(BBDD2$bmi0, na.rm = T)
quantile(BBDD2$bmi0, na.rm = T)

median(BBDD2$waist_0, na.rm = T)
IQR(BBDD2$waist_0, na.rm = T)
quantile(BBDD2$waist_0, na.rm = T)

median(BBDD2$cholesterol0, na.rm = T)
IQR(BBDD2$cholesterol0, na.rm = T)

median(BBDD2$triglycerol0, na.rm = T)
IQR(BBDD2$triglycerol0, na.rm = T)

median(BBDD2$hdl0, na.rm = T)
IQR(BBDD2$hdl0, na.rm = T)

save.image(file = "23112020.v1.RData")

#Inicio del análisis:####
## Fase 1: Preparación de datos. ####

### Fase 1.1: Selección de los metabolitos y la/s variable/s dependientes BASALES ####
merged_dataset_filt = BBDD

colnames(BBDD)[1000:1254]

short_db<-cbind(id=merged_dataset_filt[,1],
                merged_dataset_filt[,83:230],
                merged_dataset_filt[,382:584],
                merged_dataset_filt[,790:838],
                merged_dataset_filt[,911:912],
                merged_dataset_filt[,1246:1247]) # aquí puedes ir poniendo las otras variables de ingesta
# la variable ntile es la que tiene el consumo del alimento que queremos explorar. En lugar de 1 variable, podrías incluir todas de las que quieres evaluar un perfil de metabolitos (carne, pescado, etc.)

### Fase 1.2: Eliminar los metabolitos que son estándares: ####
#C240PC, glycocholated4, thymined4, inosine15N4
drop <- c("c240pc", "glycocholated4", "thymined4", "inosine15n4")
short_db = short_db[,!(names(short_db) %in% drop)]

### Fase 1.3: Determinar missing values en los sujetos y eliminar aquellos con más de un 20% ####
# Check Subjects with NAs
subjects_NA<-as.data.frame(rowSums(is.na(short_db)))
otra<-cbind(short_db$id, subjects_NA)
df <-  short_db
subjects_NA<-as.data.frame(rowSums(is.na(df)))
otra<-cbind(df$id, subjects_NA)
rows_remove<-otra$`df$id`[otra$`rowSums(is.na(df))`>78]  # threshold based on this databases 0.2*390
newdata <- subset(df, !id %in% rows_remove)
short_db<-newdata

### Fase 1.4: Determinar missing values en metabolitos ####
#Generate database with only metabolites
quantitative_variables<-short_db[,2:(length(short_db)-4)]

#NAs in each variable
na_count1 <-sapply(quantitative_variables, function(quantitative_variables) sum(length(which(is.na(quantitative_variables)))))
na_count2 <-sapply(quantitative_variables, function(quantitative_variables) (100*sum(length(which(is.na(quantitative_variables))))/sum(length((quantitative_variables)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

#Metabolites with NA higher than 20%
metabo_high_NA<-rownames(na_count[na_count[,2]>20 ,])

#Remove variables with high number of missing values (>20)
drop <- metabo_high_NA
quantitative_variables_filtered = quantitative_variables[,!(names(quantitative_variables) %in% drop)]
na_count1 <-sapply(quantitative_variables_filtered, function(quantitative_variables_filtered) sum(length(which(is.na(quantitative_variables_filtered)))))
na_count2 <-sapply(quantitative_variables_filtered, function(quantitative_variables_filtered) (100*sum(length(which(is.na(quantitative_variables_filtered))))/sum(length((quantitative_variables_filtered)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

na_count = data.frame(na_count)
na_count$names = rownames(na_count)

export(na_count, "na_count.xlsx")

### Fase 1.5: Tratamiento de missing values --> valor mitad del mínimo. ####
# Antes de imputarlos, conviene guardar el porcentaje de missings que había para cada metabolito.
inter_qvf <- quantitative_variables_filtered
for(i in 1:ncol(inter_qvf)){
  inter_qvf[is.na(inter_qvf[,i]), i] <- min(inter_qvf[,i], na.rm = TRUE)/2
}

### Fase 1.6: Escalar y centrar los datos ####
x<-inter_qvf
#Scale and center variables
scaledcentered_treatment <- apply(x, 2, function(x) ((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)))

BBDDmet = cbind(short_db[1], scaledcentered_treatment,
                                 short_db[,398:401])

export(BBDDmet, "BBDDmet.xlsx")

## Fase 2: modelos de elastic net: ####

### Fase 2.1: modelos binomiales: ####

BBDDmet$ter_RM = cut2(BBDDmet$RM, g = 3)
table(BBDDmet$ter_RM)
tapply(BBDDmet$RM, BBDDmet$ter_RM, mean, na.rm = T)

BBDDmet$ter_PRM = cut2(BBDDmet$PRM, g = 3)
table(BBDDmet$ter_PRM)
tapply(BBDDmet$PRM, BBDDmet$ter_PRM, mean, na.rm = T)

BBDDmet$ter_M = cut2(BBDDmet$carnicos, g = 3)
table(BBDDmet$ter_M)
tapply(BBDDmet$carnicos, BBDDmet$ter_M, mean, na.rm = T)

BBDDmet$ter_F = cut2(BBDDmet$pescados, g = 3)
table(BBDDmet$ter_F)
tapply(BBDDmet$pescados, BBDDmet$ter_F, mean, na.rm = T)

### ter_M = total meat ####

#### Estimación del alpha

ST = data.frame(cbind(BBDDmet[2:386], BBDDmet[393]))

ST$sub1 = 3

for (i in 1:length(ST$sub1)){
  if (ST$ter_M[i]=="[  0,109)") {ST$sub1[i]='0'}
  else if (ST$ter_M[i]=="[152,533]") {ST$sub1[i]='1'}  
  else {ST$sub1[i]='2'}}
table(ST$sub1)

ST_M = subset(ST, sub1 <= 1)

table(ST_M$sub1)

ST_M$ter_M = NULL

ST_seg <-  ST_M

rows <- sample(nrow(ST_M))
ST_M <- ST_M [rows,]
folds <- cut(seq(1,nrow(ST_M)),breaks=10,labels=FALSE)

set.seed(1)

train.data = c()
test.data = c()
cv = c()
bT = c()
pred = c()
pred1 = c()
roc.min = c()
roc.min1 = c()

for (i in 1:10) { #Seleccion de lambda y alpha
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = ST_M [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = ST_M [training, ] # testeas en el 10% (que es 1 fold)
  
  #Modelo
  
  cv[[i]] = train(sub1 ~ . , data = train.data[[i]],  method = "glmnet", metric = "Accuracy", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(sub1~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$sub1 #Outcome
  
  pred[[i]] = cv[[i]] %>% predict(x.test, type = "raw", s = "lambda.min")
  pred1[[i]] = cv[[i]] %>% predict(x.test, type = "raw", s = "lambda.1se")
  
  roc.min[[i]] = roc(as.numeric(y.test), as.numeric(pred[[i]]), ci=TRUE)
  roc.min1[[i]] = roc(as.numeric(y.test), as.numeric(pred1[[i]]), ci=TRUE)
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.5

### Fase 2.2: T-V y calculo de la AUC ####

match_names<-read.table("names_vars_280218.csv", 
                        sep = ";", header=TRUE)
match_names<-match_names[,1:2]

set.seed(3)

X.train_saved = c()
Y.train_saved = c()
X.test_saved = c()
Y.test_saved = c()
fit = c()
cvfit = c()
variables_values = c()

for (i in 1:10)
{
    cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = ST_M [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = ST_M [training, ] # testeas en el 10% (que es 1 fold)
  
  #TRAIN
  a<-as.data.frame(train.data[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$sub1
  
  #TEST
  a<-as.data.frame(test.data[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$sub1
  
  #save databases
  
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="binomial", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="binomial", type.measure = "class", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

#### AUC values from each metabolite model in the TRAIN-TESTING

type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

values_roc_model<-matrix(NA,10,3)
roc_obj<-list()
for (j in 1:10){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
        #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    #roc_obj <- roc(Y.test, modelo, ci=TRUE)
    
    roc_obj[[j]] <- roc(Y.test, modelo, ci=TRUE)
    values_roc_model[j,1]<-roc_obj[[j]]$auc
    values_roc_model[j,2]<-roc_obj[[j]]$ci[1]
    values_roc_model[j,3]<-roc_obj[[j]]$ci[3]
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==1)
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>1)
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
  
}
colnames(values_roc_model)<-c("AUC","LCI","HCI")

mean<-mean(values_roc_model[,"AUC"], na.rm=T)
n<-sum(!is.na(values_roc_model[,1]))
sd<-sd(values_roc_model[,"AUC"], na.rm=T)

error <- qnorm(0.975)*sd/sqrt(n)
left <- mean-error
right <- mean+error

paste(mean, " (", left, "; ", right, ")", sep="")

mean_95_AUC<-t(c(mean, left, right))

copy.table(mean_95_AUC)
copy.table(values_roc_model)

#### Estimacion de los coef con toda la BBDD

set.seed(2)

tic("WHOLE")

# Using lambda.min
type.lambda<-c("lambda.min")

fit_full<-list()
cvfit_full<-list()
roc_results_full<-list()
variables_values_full<-list()
standard_roc_full<-list()

full_data<-as.data.frame(ST_M)
X.full<- as.matrix(ST_M[,1:(dim(ST_M)[2]-1)])
Y.full <- ST_M$sub1

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="binomial", alpha=alp)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="binomial", type.measure = "class", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
  
}

toc()

#### FULL

type.lambda<-c("lambda.1se")

variables_values_full_1se<-list()

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full_1se[[i]]<-cbind(variables_get_full, values_metabo_full)
  
}

#### FULL DATASET
#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(ST_M)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(ST_M))
colnames(abc_set)[colnames(abc_set)=="colnames.ST_M."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES VOLCANO 

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")
copy.table(new_mean_values_metabo)

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#All metabolites
dotchart(new_mean_values_metabo_sorted$mean,labels=new_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<=-0.2) | (new_mean_values_metabo_sorted$mean>=0.2), ]
dotchart(selected_mean_values_metabo_sorted$mean,labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))

#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<0), ]

dotchart(selected_mean_values_metabo_sorted_negative$mean,labels=selected_mean_values_metabo_sorted_negative$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean>=0), ]

dotchart(selected_mean_values_metabo_sorted_positive$mean,
         labels=selected_mean_values_metabo_sorted_positive$Metabolites,
         cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-0.20, 0))

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 0.15))

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
grid.arrange(plot1, plot2, ncol=2)

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

#If need to be further used as matrix
#selected_mean_values_metabo_sorted_matrix<-as.matrix(selected_mean_values_metabo_sorted)

save.image(file = "09122020.v1.log_ter_M.RData")

###ter_F ####
colnames(scaledcentered_treatment)

SF = data.frame(cbind(BBDDmet[2:386], BBDDmet[394]))

SF$sub1 = 3

for (i in 1:length(SF$sub1)){
  if (SF$ter_F[i]=="[  0.0,  78.6)") {SF$sub1[i]='0'}
  else if (SF$ter_F[i]=="[117.5,1112.3]") {SF$sub1[i]='1'}  
  else {SF$sub1[i]='2'}}
table(SF$sub1)

SF_F = subset(SF, sub1 <= 1)

table(SF_F$sub1)

SF_F$ter_F = NULL

SF_seg <-  SF_F

rows <- sample(nrow(SF_F))
SF_F <- SF_F [rows,]
folds <- cut(seq(1,nrow(SF_F)),breaks=10,labels=FALSE)

set.seed(11)

train.data = c()
test.data = c()
cv = c()
bT = c()
pred = c()
pred1 = c()
roc.min = c()
roc.min1 = c()

for (i in 1:10) { #Seleccion de lambda y alpha
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = SF_F [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = SF_F [training, ] # testeas en el 10% (que es 1 fold)
  
  #Modelo
  
  cv[[i]] = train(sub1 ~ . , data = train.data[[i]],  method = "glmnet", metric = "Accuracy", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(sub1~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$sub1 #Outcome
  
  pred[[i]] = cv[[i]] %>% predict(x.test, type = "raw", s = "lambda.min")
  pred1[[i]] = cv[[i]] %>% predict(x.test, type = "raw", s = "lambda.1se")
  
  roc.min[[i]] = roc(as.numeric(y.test), as.numeric(pred[[i]]), ci=TRUE)
  roc.min1[[i]] = roc(as.numeric(y.test), as.numeric(pred1[[i]]), ci=TRUE)
}

l = cv[[i]]$bestTune$lambda
alp = 0.6 #cv[[i]]$bestTune$alpha #0.6

### Fase 2.2: T-V y calculo de la AUC ####

match_names<-read.table("names_vars_280218.csv", 
                        sep = ";", header=TRUE)
match_names<-match_names[,1:2]

set.seed(31)

X.train_saved = c()
Y.train_saved = c()
X.test_saved = c()
Y.test_saved = c()
fit = c()
cvfit = c()
variables_values = c()

for (i in 1:10)
{
  
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = SF_F [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = SF_F [training, ] # testeas en el 10% (que es 1 fold)
  
  #TRAIN
  a<-as.data.frame(train.data[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$sub1
  
  #TEST
  a<-as.data.frame(test.data[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$sub1
  
  #save databases
  
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="binomial", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="binomial", type.measure = "class", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  liSF_Fetabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-liSF_Fetabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(liSF_Fetabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!
#### AUC values from each metabolite model in the TRAIN-TESTING

type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

values_roc_model<-matrix(NA,10,3)
roc_obj<-list()
for (j in 1:10){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    #roc_obj <- roc(Y.test, modelo, ci=TRUE)
    
    roc_obj[[j]] <- roc(Y.test, modelo, ci=TRUE)
    values_roc_model[j,1]<-roc_obj[[j]]$auc
    values_roc_model[j,2]<-roc_obj[[j]]$ci[1]
    values_roc_model[j,3]<-roc_obj[[j]]$ci[3]
    
    nuevo_yteSF_Fodel<-cbind(Y.test, modelo)
    if (j==1)
    {
      nuevo_yteSF_Fodel2<-nuevo_yteSF_Fodel
    }
    if (j>1)
    {
      nuevo_yteSF_Fodel2<-rbind(nuevo_yteSF_Fodel2, nuevo_yteSF_Fodel)
    }
    
  } else {} # If there are no metabolites, SKIP!
  
}
colnames(values_roc_model)<-c("AUC","LCI","HCI")

mean<-mean(values_roc_model[,"AUC"], na.rm=T)
n<-sum(!is.na(values_roc_model[,1]))
sd<-sd(values_roc_model[,"AUC"], na.rm=T)

error <- qnorm(0.975)*sd/sqrt(n)
left <- mean-error
right <- mean+error

paste(mean, " (", left, "; ", right, ")", sep="")

mean_95_AUC<-t(c(mean, left, right))

copy.table(mean_95_AUC)
copy.table(values_roc_model)

### Fase 2.3: Estimacion de los coef con toda la BBDD ####

set.seed(21)

tic("WHOLE")

# Using lambda.min
type.lambda<-c("lambda.min")

fit_full<-list()
cvfit_full<-list()
roc_results_full<-list()
variables_values_full<-list()
standard_roc_full<-list()

full_data<-as.data.frame(SF_F)
X.full<- as.matrix(SF_F[,1:(dim(SF_F)[2]-1)])
Y.full <- SF_F$sub1

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="binomial", alpha=alp)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="binomial", type.measure = "class", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  liSF_Fetabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-liSF_Fetabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(liSF_Fetabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
  
}

toc()

# Con el siguiente código puedes cambiar el tipo de lambda sin tener que volver a ejecutar el loop

#### FULL

type.lambda<-c("lambda.1se")

variables_values_full_1se<-list()

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full_1se[[i]]<-cbind(variables_get_full, values_metabo_full)
  
}

#### COMPENDIA OF METABOLITES
#### FULL DATASET
#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(SF_F)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(SF_F))
colnames(abc_set)[colnames(abc_set)=="colnames.SF_F."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

copy.table(abc_set)

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")
 
### FIGURES
# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")
copy.table(new_mean_values_metabo)

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#All metabolites
dotchart(new_mean_values_metabo_sorted$mean,labels=new_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<=-0.2) | (new_mean_values_metabo_sorted$mean>=0.2), ]
dotchart(selected_mean_values_metabo_sorted$mean,labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<0), ]

dotchart(selected_mean_values_metabo_sorted_negative$mean,labels=selected_mean_values_metabo_sorted_negative$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean>=0), ]

dotchart(selected_mean_values_metabo_sorted_positive$mean,
         labels=selected_mean_values_metabo_sorted_positive$Metabolites,
         cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-0.30, 0))

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 0.40))

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
grid.arrange(plot1, plot2, ncol=2)

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

#If need to be further used as matrix
#selected_mean_values_metabo_sorted_matrix<-as.matrix(selected_mean_values_metabo_sorted)

save.image(file = "14122020.v1.log_ter_F.RData")

###ter_RM ####
colnames(scaledcentered_treatment)

ST = data.frame(cbind(BBDDmet[2:386], BBDDmet[391]))

ST$sub1 = 3

for (i in 1:length(ST$sub1)){
  if (ST$ter_RM[i]=="[ 0.0, 41.4)") {ST$sub1[i]='0'}
  else if (ST$ter_RM[i]=="[74.3,460.7]") {ST$sub1[i]='1'}  
  else {ST$sub1[i]='2'}}
table(ST$sub1)

ST_RM = subset(ST, sub1 <= 1)

table(ST_RM$sub1)

ST_RM$ter_RM = NULL

ST_seg <-  ST_RM

rows <- sample(nrow(ST_RM))
ST_RM <- ST_RM [rows,]
folds <- cut(seq(1,nrow(ST_RM)),breaks=10,labels=FALSE)

set.seed(112)

train.data = c()
test.data = c()
cv = c()
bT = c()
pred = c()
pred1 = c()
roc.min = c()
roc.min1 = c()

for (i in 1:10) { #Seleccion de lambda y alpha
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = ST_RM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = ST_RM [training, ] # testeas en el 10% (que es 1 fold)
  
  #Modelo
  
  cv[[i]] = train(sub1 ~ . , data = train.data[[i]],  method = "glmnet", metric = "Accuracy", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(sub1~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$sub1 #Outcome
  
  pred[[i]] = cv[[i]] %>% predict(x.test, type = "raw", s = "lambda.min")
  pred1[[i]] = cv[[i]] %>% predict(x.test, type = "raw", s = "lambda.1se")
  
  roc.min[[i]] = roc(as.numeric(y.test), as.numeric(pred[[i]]), ci=TRUE)
  roc.min1[[i]] = roc(as.numeric(y.test), as.numeric(pred1[[i]]), ci=TRUE)
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.2

### Fase 2.4: T-V y calculo de la AUC ####

match_names<-read.table("names_vars_280218.csv", 
                        sep = ";", header=TRUE)
match_names<-match_names[,1:2]

set.seed(312)

X.train_saved = c()
Y.train_saved = c()
X.test_saved = c()
Y.test_saved = c()
fit = c()
cvfit = c()
variables_values = c()

for (i in 1:10)
{
  
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = ST_RM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = ST_RM [training, ] # testeas en el 10% (que es 1 fold)
  
  #TRAIN
  a<-as.data.frame(train.data[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$sub1
  
  #TEST
  a<-as.data.frame(test.data[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$sub1
  
  #save databases
  
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="binomial", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="binomial", type.measure = "class", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!
#### AUC values from each metabolite model in the TRAIN-TESTING

type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

values_roc_model<-matrix(NA,10,3)
roc_obj<-list()
for (j in 1:10){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    #roc_obj <- roc(Y.test, modelo, ci=TRUE)
    
    roc_obj[[j]] <- roc(Y.test, modelo, ci=TRUE)
    values_roc_model[j,1]<-roc_obj[[j]]$auc
    values_roc_model[j,2]<-roc_obj[[j]]$ci[1]
    values_roc_model[j,3]<-roc_obj[[j]]$ci[3]
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==1)
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>1)
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
  
}
colnames(values_roc_model)<-c("AUC","LCI","HCI")

mean<-mean(values_roc_model[,"AUC"], na.rm=T)
n<-sum(!is.na(values_roc_model[,1]))
sd<-sd(values_roc_model[,"AUC"], na.rm=T)

error <- qnorm(0.975)*sd/sqrt(n)
left <- mean-error
right <- mean+error

paste(mean, " (", left, "; ", right, ")", sep="")

mean_95_AUC<-t(c(mean, left, right))

copy.table(mean_95_AUC)
copy.table(values_roc_model)

### Fase 2.5: Estimacion de los coef con toda la BBDD ####

set.seed(212)

tic("WHOLE")

# Using lambda.min
type.lambda<-c("lambda.min")

fit_full<-list()
cvfit_full<-list()
roc_results_full<-list()
variables_values_full<-list()
standard_roc_full<-list()

full_data<-as.data.frame(ST_RM)
X.full<- as.matrix(ST_RM[,1:(dim(ST_RM)[2]-1)])
Y.full <- ST_RM$sub1

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="binomial", alpha=alp)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="binomial", type.measure = "class", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
  
}

toc()

#### FULL

type.lambda<-c("lambda.1se")

variables_values_full_1se<-list()

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full_1se[[i]]<-cbind(variables_get_full, values_metabo_full)
  
}

#### COMPENDIA OF METABOLITES
#### FULL DATASET
#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(ST_RM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(ST_RM))
colnames(abc_set)[colnames(abc_set)=="colnames.ST_RM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")
 
### FIGURES
# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")
copy.table(new_mean_values_metabo)

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#All metabolites
dotchart(new_mean_values_metabo_sorted$mean,labels=new_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<=-0.2) | (new_mean_values_metabo_sorted$mean>=0.2), ]
dotchart(selected_mean_values_metabo_sorted$mean,labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<0), ]

dotchart(selected_mean_values_metabo_sorted_negative$mean,labels=selected_mean_values_metabo_sorted_negative$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean>=0), ]

dotchart(selected_mean_values_metabo_sorted_positive$mean,
         labels=selected_mean_values_metabo_sorted_positive$Metabolites,
         cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-0.25, 0))

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 0.25))

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
grid.arrange(plot1, plot2, ncol=2)

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

#If need to be further used as matrix
#selected_mean_values_metabo_sorted_matrix<-as.matrix(selected_mean_values_metabo_sorted)

save.image(file = "16122020.v1.log_ter_RM.RData")

###ter_PRM####

ST = data.frame(cbind(BBDDmet[2:386], BBDDmet[392]))

ST$sub1 = 3
table(ST$ter_PRM)

for (i in 1:length(ST$sub1)){
  if (ST$ter_PRM[i]=="[ 0.0, 17.3)") {ST$sub1[i]='0'}
  else if (ST$ter_PRM[i]=="[32.5,174.8]") {ST$sub1[i]='1'}  
  else {ST$sub1[i]='2'}}
table(ST$sub1)

ST_PRM = subset(ST, sub1 <= 1)

table(ST_PRM$sub1)

ST_PRM$ter_PRM = NULL

ST_seg <-  ST_PRM

rows <- sample(nrow(ST_PRM))
ST_PRM <- ST_PRM [rows,]
folds <- cut(seq(1,nrow(ST_PRM)),breaks=10,labels=FALSE)

set.seed(1111)

train.data = c()
test.data = c()
cv = c()
bT = c()
pred = c()
pred1 = c()
roc.min = c()
roc.min1 = c()

for (i in 1:10) { #Seleccion de lambda y alpha
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = ST_PRM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = ST_PRM [training, ] # testeas en el 10% (que es 1 fold)
  
  #Modelo
  
  cv[[i]] = train(sub1 ~ . , data = train.data[[i]],  method = "glmnet", metric = "Accuracy", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(sub1~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$sub1 #Outcome
  
  pred[[i]] = cv[[i]] %>% predict(x.test, type = "raw", s = "lambda.min")
  pred1[[i]] = cv[[i]] %>% predict(x.test, type = "raw", s = "lambda.1se")
  
  roc.min[[i]] = roc(as.numeric(y.test), as.numeric(pred[[i]]), ci=TRUE)
  roc.min1[[i]] = roc(as.numeric(y.test), as.numeric(pred1[[i]]), ci=TRUE)
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.3

### Fase 2.6: T-V y calculo de la AUC ####

match_names<-read.table("names_vars_280218.csv", 
                        sep = ";", header=TRUE)
match_names<-match_names[,1:2]

set.seed(3121)

X.train_saved = c()
Y.train_saved = c()
X.test_saved = c()
Y.test_saved = c()
fit = c()
cvfit = c()
variables_values = c()

for (i in 1:10)
{
  
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = ST_PRM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = ST_PRM [training, ] # testeas en el 10% (que es 1 fold)
  
  #TRAIN
  a<-as.data.frame(train.data[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$sub1
  
  #TEST
  a<-as.data.frame(test.data[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$sub1
  
  #save databases
  
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="binomial", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="binomial", type.measure = "class", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

#### AUC values from each metabolite model in the TRAIN-TESTING

type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

values_roc_model<-matrix(NA,10,3)
roc_obj<-list()
for (j in 1:10){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    #roc_obj <- roc(Y.test, modelo, ci=TRUE)
    
    roc_obj[[j]] <- roc(Y.test, modelo, ci=TRUE)
    values_roc_model[j,1]<-roc_obj[[j]]$auc
    values_roc_model[j,2]<-roc_obj[[j]]$ci[1]
    values_roc_model[j,3]<-roc_obj[[j]]$ci[3]
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==1)
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>1)
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
  
}
colnames(values_roc_model)<-c("AUC","LCI","HCI")

mean<-mean(values_roc_model[,"AUC"], na.rm=T)
n<-sum(!is.na(values_roc_model[,1]))
sd<-sd(values_roc_model[,"AUC"], na.rm=T)

error <- qnorm(0.975)*sd/sqrt(n)
left <- mean-error
right <- mean+error

paste(mean, " (", left, "; ", right, ")", sep="")

mean_95_AUC<-t(c(mean, left, right))

copy.table(mean_95_AUC)
copy.table(values_roc_model)

### Fase 2.7: Estimacion de los coef con toda la BBDD ####

set.seed(2111)

tic("WHOLE")

# Using lambda.min
type.lambda<-c("lambda.min")

fit_full<-list()
cvfit_full<-list()
roc_results_full<-list()
variables_values_full<-list()
standard_roc_full<-list()

full_data<-as.data.frame(ST_PRM)
X.full<- as.matrix(ST_PRM[,1:(dim(ST_PRM)[2]-1)])
Y.full <- ST_PRM$sub1

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="binomial", alpha=alp)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="binomial", type.measure = "class", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

type.lambda<-c("lambda.1se")

variables_values_full_1se<-list()

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full_1se[[i]]<-cbind(variables_get_full, values_metabo_full)
  
}

##################### COMPENDIA OF METABOLITES
#### FULL DATASET
# Ignore warning!

#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(ST_PRM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(ST_PRM))
colnames(abc_set)[colnames(abc_set)=="colnames.ST_PRM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES
# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")
copy.table(new_mean_values_metabo)

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#All metabolites
dotchart(new_mean_values_metabo_sorted$mean,labels=new_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<=-0.2) | (new_mean_values_metabo_sorted$mean>=0.2), ]
dotchart(selected_mean_values_metabo_sorted$mean,labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<0), ]

dotchart(selected_mean_values_metabo_sorted_negative$mean,labels=selected_mean_values_metabo_sorted_negative$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean>=0), ]

dotchart(selected_mean_values_metabo_sorted_positive$mean,
         labels=selected_mean_values_metabo_sorted_positive$Metabolites,
         cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-0.20, 0))

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 0.20))

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
grid.arrange(plot1, plot2, ncol=2)

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

#If need to be further used as matrix
#selected_mean_values_metabo_sorted_matrix<-as.matrix(selected_mean_values_metabo_sorted)

save.image(file = "16122020.v1.log_ter_PRM.RData")

## Fase 3: Modelos ENR lineales: (modelos para los scores) ####

### Carnicos: ####

#### Estimacion del valor de alpha:

#Split t-v

TM = data.frame(cbind(BBDDmet[2:386], BBDDmet[387]))

set.seed(001)

tic("Alpha")

rows <- sample(nrow(TM))
TM <- TM [rows,]
folds <- cut(seq(1,nrow(TM)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = TM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = TM [training, ] # testeas en el 10% (que es 1 fold)
  
  #modelo
  
  cv[[i]] = train(carnicos ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(carnicos~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$carnicos #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$carnicos), #Rsquare = R2(predictions[[i]], test.data[[i]]$carnicos), 
                             Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$carnicos, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$carnicos)$estimate)
}

toc()

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.2

tic("WHOLE")

set.seed(002)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(TM)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$carnicos

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", alpha=.5)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

##################### COMPENDIA OF METABOLITES
#### FULL DATASET
#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(TM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(TM))
colnames(abc_set)[colnames(abc_set)=="colnames.TM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")
copy.table(new_mean_values_metabo)

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#All metabolites
dotchart(new_mean_values_metabo_sorted$mean,labels=new_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<=-0.2) | (new_mean_values_metabo_sorted$mean>=0.2), ]
dotchart(selected_mean_values_metabo_sorted$mean,labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<0), ]

dotchart(selected_mean_values_metabo_sorted_negative$mean,labels=selected_mean_values_metabo_sorted_negative$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean>=0), ]

dotchart(selected_mean_values_metabo_sorted_positive$mean,
         labels=selected_mean_values_metabo_sorted_positive$Metabolites,
         cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-5, 0))

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 4))

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

######### FIGURES

#Change scale
plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-5, 0)) + geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 4)) + geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
grid.arrange(plot1, plot2, ncol=2)

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

#If need to be further used as matrix
selected_mean_values_metabo_sorted_matrix<-as.matrix(selected_mean_values_metabo_sorted)

# To run training-testing

# en la cv.glmnet tengo puesto nfolds=10 para simplificar, se podría poner igual al valor de sujetos para que
# hiciera leave-one-out CV

# Si quieres puedes probar qué obtenemos tirándolo de dos formas:
# 1. Loop 10-fold cross validation con 10-CV dentro de cada iteración (código de abajo)
# 2. Solo un loop usando el whole dataset, y poniendo en nfolds=dim(TM)[1] (obviamente, tendrás que cambiar código)

rows <- sample(nrow(TM))
TM2 <- TM[rows, ]
TM <- TM2
folds <- cut(seq(1,nrow(TM)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

tic("AUC: Training-Testing")

#### EXECUTION TRAINING-TESTING

#Variables
set_train<-list()
set_test<-list()
X.train_saved<-list()
Y.train_saved<-list()
X.test_saved<-list()
Y.test_saved<-list()    
fit<-list()
cvfit<-list()
roc_results<-list()
variables_values<-list()
standard_roc<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
reg_sum<-list()
reg_sum_no_intercept<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #sample <- sample.int(n = nrow(TM), size = floor(.90*nrow(TM)), replace = F)
  #set_train[[i]] <- TM[sample, ]
  #set_test[[i]]  <- TM[-sample, ]
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- TM[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- TM[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$carnicos
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$carnicos
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}
toc()
# Con el siguiente código puedes cambiar el tipo de lambda sin tener que volver a ejecutar el loop

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0 # variable that includes the iterations in which we found a model (i.e. at least one variable associated)

for (i in 1:10)
{
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
  
  if (dim(variables_values[[i]])[1] == 0)
  {
    cat("No metabolites found for iteration", i, '\n')
  }
  
  if (dim(variables_values[[i]])[1] != 0)
  {
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    #plot(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))
    #linearMod <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda))), col="red")
    #reg_sum[[i]]<-summary(linearMod)
    
    #linearMod_no_intercept <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1)  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1), col="red")
    #reg_sum_no_intercept[[i]]<-summary(linearMod_no_intercept)
    
    si_model <- cbind(si_model, i)
  }
}

si_model <- si_model[,-1]

##################### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
#### CORRELATION values from each metabolite model in the TRAIN-TESTING

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    
    corr_obj[[j]] <- cor.test(Y.test, modelo, method="pearson")
    values_corr_model[j,1]<-corr_obj[[j]]$estimate
    values_corr_model[j,2]<-corr_obj[[j]]$conf.int[1]
    values_corr_model[j,3]<-corr_obj[[j]]$conf.int[2]
    values_corr_model[j,4]<-signif(corr_obj[[j]]$p.value,4)
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==si_model[1])
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>si_model[1])
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F) # By default, the confidence interval is calculated based on the alpha/2-quantile of the t-distribution, where alpha = 0.05. This is motivated if the data are normally distributed. It can be changed to the alpha/2-quantile of the normal distribution by

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

copy.table(ci.mean)
copy.table(values_corr_model)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
nuevo_ytest_model2
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

save.image(file = "17122020.v1.TM.RData")

### Pescado: ####

#### Estimacion del valor de alpha:

#Split t-v

FM = data.frame(cbind(BBDDmet[2:386], BBDDmet[388]))

set.seed(0011)

tic("Alpha")

rows <- sample(nrow(FM))
FM <- FM [rows,]
folds <- cut(seq(1,nrow(FM)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = FM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = FM [training, ] # testeas en el 10% (que es 1 fold)
  
  #modelo
  
  cv[[i]] = train(pescados ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(pescados~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$pescados #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$pescados), #Rsquare = R2(predictions[[i]], test.data[[i]]$pescados), 
                             Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$pescados, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$pescados)$estimate)
}

toc()

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.1

tic("WHOLE")

set.seed(0021)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(FM)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$pescados

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", alpha=.5)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

##################### COMPENDIA OF METABOLITES
#### FULL DATASET
#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(FM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(FM))
colnames(abc_set)[colnames(abc_set)=="colnames.FM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")
copy.table(new_mean_values_metabo)

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#All metabolites
dotchart(new_mean_values_metabo_sorted$mean,labels=new_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<=-0.2) | (new_mean_values_metabo_sorted$mean>=0.2), ]
dotchart(selected_mean_values_metabo_sorted$mean,labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<0), ]

dotchart(selected_mean_values_metabo_sorted_negative$mean,labels=selected_mean_values_metabo_sorted_negative$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean>=0), ]

dotchart(selected_mean_values_metabo_sorted_positive$mean,
         labels=selected_mean_values_metabo_sorted_positive$Metabolites,
         cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-5, 0))

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 4))

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

######### FIGURES

#Change scale
plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-2.5, 0)) + geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 7.5)) + geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
grid.arrange(plot1, plot2, ncol=2)

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

#If need to be further used as matrix
selected_mean_values_metabo_sorted_matrix<-as.matrix(selected_mean_values_metabo_sorted)

# To run training-testing

# en la cv.glmnet tengo puesto nfolds=10 para simplificar, se podría poner igual al valor de sujetos para que
# hiciera leave-one-out CV

# Si quieres puedes probar qué obtenemos tirándolo de dos formas:
# 1. Loop 10-fold cross validation con 10-CV dentro de cada iteración (código de abajo)
# 2. Solo un loop usando el whole dataset, y poniendo en nfolds=dim(FM)[1] (obviamente, tendrás que cambiar código)

rows <- sample(nrow(FM))
FM2 <- FM[rows, ]
FM <- FM2
folds <- cut(seq(1,nrow(FM)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

tic("AUC: Training-Testing")

#### EXECUTION TRAINING-TESTING

#Variables
set_train<-list()
set_test<-list()
X.train_saved<-list()
Y.train_saved<-list()
X.test_saved<-list()
Y.test_saved<-list()    
fit<-list()
cvfit<-list()
roc_results<-list()
variables_values<-list()
standard_roc<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
reg_sum<-list()
reg_sum_no_intercept<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #sample <- sample.int(n = nrow(FM), size = floor(.90*nrow(FM)), replace = F)
  #set_train[[i]] <- FM[sample, ]
  #set_test[[i]]  <- FM[-sample, ]
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- FM[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- FM[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$pescados
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$pescados
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}
toc()
# Con el siguiente código puedes cambiar el tipo de lambda sin tener que volver a ejecutar el loop

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0 # variable that includes the iterations in which we found a model (i.e. at least one variable associated)

for (i in 1:10)
{
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
  
  if (dim(variables_values[[i]])[1] == 0)
  {
    cat("No metabolites found for iteration", i, '\n')
  }
  
  if (dim(variables_values[[i]])[1] != 0)
  {
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    #plot(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))
    #linearMod <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda))), col="red")
    #reg_sum[[i]]<-summary(linearMod)
    
    #linearMod_no_intercept <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1)  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1), col="red")
    #reg_sum_no_intercept[[i]]<-summary(linearMod_no_intercept)
    
    si_model <- cbind(si_model, i)
  }
}

si_model <- si_model[,-1]

##################### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
#### CORRELATION values from each metabolite model in the TRAIN-TESTING

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    
    corr_obj[[j]] <- cor.test(Y.test, modelo, method="pearson")
    values_corr_model[j,1]<-corr_obj[[j]]$estimate
    values_corr_model[j,2]<-corr_obj[[j]]$conf.int[1]
    values_corr_model[j,3]<-corr_obj[[j]]$conf.int[2]
    values_corr_model[j,4]<-signif(corr_obj[[j]]$p.value,4)
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==si_model[1])
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>si_model[1])
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
  } else {} # If there are no metabolites, SKIP!
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F) # By default, the confidence interval is calculated based on the alpha/2-quantile of the t-distribution, where alpha = 0.05. This is motivated if the data are normally distributed. It can be changed to the alpha/2-quantile of the normal distribution by

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
nuevo_ytest_model2
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

save.image(file = "21122020.v1.FM.RData")

### RM: ####

#### Estimacion del valor de alpha:

#Split t-v

RM = data.frame(cbind(BBDDmet[2:386], BBDDmet[389]))

set.seed(00111)

tic("Alpha")

rows <- sample(nrow(RM))
RM <- RM [rows,]
folds <- cut(seq(1,nrow(RM)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = RM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = RM [training, ] # testeas en el 10% (que es 1 fold)
  
  #modelo
  
  cv[[i]] = train(RM ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(RM~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$RM #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$RM), #Rsquare = R2(predictions[[i]], test.data[[i]]$RM), 
                             Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$RM, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$RM)$estimate)
}

toc()

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.3




tic("WHOLE")

set.seed(00211)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(RM)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$RM

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", alpha=.5)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

#### COMPENDIA OF METABOLITES
#### FULL DATASET

#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(RM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(RM))
colnames(abc_set)[colnames(abc_set)=="colnames.RM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#All metabolites
dotchart(new_mean_values_metabo_sorted$mean,labels=new_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<=-0.2) | (new_mean_values_metabo_sorted$mean>=0.2), ]
dotchart(selected_mean_values_metabo_sorted$mean,labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<0), ]

dotchart(selected_mean_values_metabo_sorted_negative$mean,labels=selected_mean_values_metabo_sorted_negative$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean>=0), ]

dotchart(selected_mean_values_metabo_sorted_positive$mean,
         labels=selected_mean_values_metabo_sorted_positive$Metabolites,
         cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-5, 0))

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 4))

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

#### FIGURES

#Change scale
plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-2.5, 0)) + geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 3)) + geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
grid.arrange(plot1, plot2, ncol=2)

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

#If need to be further used as matrix
selected_mean_values_metabo_sorted_matrix<-as.matrix(selected_mean_values_metabo_sorted)

# To run training-testing

rows <- sample(nrow(RM))
RM2 <- RM[rows, ]
RM <- RM2
folds <- cut(seq(1,nrow(RM)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

tic("AUC: Training-Testing")

#### EXECUTION TRAINING-TESTING

#Variables
set_train<-list()
set_test<-list()
X.train_saved<-list()
Y.train_saved<-list()
X.test_saved<-list()
Y.test_saved<-list()    
fit<-list()
cvfit<-list()
roc_results<-list()
variables_values<-list()
standard_roc<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
reg_sum<-list()
reg_sum_no_intercept<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #sample <- sample.int(n = nrow(RM), size = floor(.90*nrow(RM)), replace = F)
  #set_train[[i]] <- RM[sample, ]
  #set_test[[i]]  <- RM[-sample, ]
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- RM[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- RM[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$RM
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$RM
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}
toc()

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0 # variable that includes the iterations in which we found a model (i.e. at least one variable associated)

for (i in 1:10)
{
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
  
  if (dim(variables_values[[i]])[1] == 0)
  {
    cat("No metabolites found for iteration", i, '\n')
  }
  
  if (dim(variables_values[[i]])[1] != 0)
  {
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    #plot(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))
    #linearMod <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda))), col="red")
    #reg_sum[[i]]<-summary(linearMod)
    
    #linearMod_no_intercept <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1)  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1), col="red")
    #reg_sum_no_intercept[[i]]<-summary(linearMod_no_intercept)
    
    si_model <- cbind(si_model, i)
  }
  
}

si_model <- si_model[,-1]

### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
### CORRELATION values from each metabolite model in the TRAIN-TESTING

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    
    corr_obj[[j]] <- cor.test(Y.test, modelo, method="pearson")
    values_corr_model[j,1]<-corr_obj[[j]]$estimate
    values_corr_model[j,2]<-corr_obj[[j]]$conf.int[1]
    values_corr_model[j,3]<-corr_obj[[j]]$conf.int[2]
    values_corr_model[j,4]<-signif(corr_obj[[j]]$p.value,4)
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==si_model[1])
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>si_model[1])
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
  } else {} # If there are no metabolites, SKIP!
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F) # By default, the confidence interval is calculated based on the alpha/2-quantile of the t-distribution, where alpha = 0.05. This is motivated if the data are normally distributed. It can be changed to the alpha/2-quantile of the normal distribution by

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
nuevo_ytest_model2
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

save.image(file = "22122020.v1.RM.RData")

### PRM: ####

#### Estimacion del valor de alpha:

#Split t-v

PRM = data.frame(cbind(BBDDmet[2:386], BBDDmet[390]))

set.seed(001123)

tic("Alpha")

rows <- sample(nrow(PRM))
PRM <- PRM [rows,]
folds <- cut(seq(1,nrow(PRM)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = PRM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = PRM [training, ] # testeas en el 10% (que es 1 fold)
  
  #modelo
  
  cv[[i]] = train(PRM ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(PRM~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$PRM #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$PRM), #Rsquare = R2(predictions[[i]], test.data[[i]]$PRM), 
                             Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$PRM, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$PRM)$estimate)
}

toc()

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.4

tic("WHOLE")

set.seed(002)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(PRM)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$PRM

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", alpha=.5)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

#### COMPENDIA OF METABOLITES
#### FULL DATASET

#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(PRM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(PRM))
colnames(abc_set)[colnames(abc_set)=="colnames.PRM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")
copy.table(new_mean_values_metabo)

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#All metabolites
dotchart(new_mean_values_metabo_sorted$mean,labels=new_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<=-0.2) | (new_mean_values_metabo_sorted$mean>=0.2), ]
dotchart(selected_mean_values_metabo_sorted$mean,labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean<0), ]

dotchart(selected_mean_values_metabo_sorted_negative$mean,labels=selected_mean_values_metabo_sorted_negative$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(new_mean_values_metabo_sorted$mean>=0), ]

dotchart(selected_mean_values_metabo_sorted_positive$mean,
         labels=selected_mean_values_metabo_sorted_positive$Metabolites,
         cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-5, 0))

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 4))

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

#### FIGURES

#Change scale
plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-1.5, 0)) + geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 2)) + geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
grid.arrange(plot1, plot2, ncol=2)

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

#If need to be further used as matrix
selected_mean_values_metabo_sorted_matrix<-as.matrix(selected_mean_values_metabo_sorted)

# To run training-testing

rows <- sample(nrow(PRM))
PRM2 <- PRM[rows, ]
PRM <- PRM2
folds <- cut(seq(1,nrow(PRM)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

tic("AUC: Training-Testing")

#### EXECUTION TRAINING-TESTING

#Variables
set_train<-list()
set_test<-list()
X.train_saved<-list()
Y.train_saved<-list()
X.test_saved<-list()
Y.test_saved<-list()    
fit<-list()
cvfit<-list()
roc_results<-list()
variables_values<-list()
standard_roc<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
reg_sum<-list()
reg_sum_no_intercept<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #sample <- sample.int(n = nrow(PRM), size = floor(.90*nrow(PRM)), replace = F)
  #set_train[[i]] <- PRM[sample, ]
  #set_test[[i]]  <- PRM[-sample, ]
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- PRM[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- PRM[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$PRM
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$PRM
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}
toc()

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0 # variable that includes the iterations in which we found a model (i.e. at least one variable associated)

for (i in 1:10)
{
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
  
  if (dim(variables_values[[i]])[1] == 0)
  {
    cat("No metabolites found for iteration", i, '\n')
  }
  
  if (dim(variables_values[[i]])[1] != 0)
  {
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    #plot(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))
    #linearMod <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda))), col="red")
    #reg_sum[[i]]<-summary(linearMod)
    
    #linearMod_no_intercept <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1)  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1), col="red")
    #reg_sum_no_intercept[[i]]<-summary(linearMod_no_intercept)
    
    si_model <- cbind(si_model, i)
  }
}

si_model <- si_model[,-1]

### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
### CORRELATION values from each metabolite model in the TRAIN-TESTING

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    
    corr_obj[[j]] <- cor.test(Y.test, modelo, method="pearson")
    values_corr_model[j,1]<-corr_obj[[j]]$estimate
    values_corr_model[j,2]<-corr_obj[[j]]$conf.int[1]
    values_corr_model[j,3]<-corr_obj[[j]]$conf.int[2]
    values_corr_model[j,4]<-signif(corr_obj[[j]]$p.value,4)
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==si_model[1])
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>si_model[1])
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
  
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F) # By default, the confidence interval is calculated based on the alpha/2-quantile of the t-distribution, where alpha = 0.05. This is motivated if the data are normally distributed. It can be changed to the alpha/2-quantile of the normal distribution by

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
nuevo_ytest_model2
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

save.image(file = "22122020.v1.PRM.RData")

rm(BBDD, df, inter_qvf, merged_dataset_filt, na_count, NEW_Combined_metabolites_phenotypes_230218, newdata, otra, quantitative_variables, 
   quantitative_variables_filtered, quantitative_variables_filtered1, scaledcentered_treatment, short_db, subjects_NA, x, drop, i,
   metabo_high_NA, na_count1, na_count2, rows_remove)

## Fase 4: cálculo de los scores: ####

###Calculo de los scores para cada variable de interés (baseline): Se utiliza un excel realizado externamente: Only coef_Gauss, que unicamente
###contiene los coeficientes de los metabolitos de cada modelo selecionados 10 veces.
### Carnicos####

met <- read_excel("Only coef_Gauss.xlsx", sheet = "TM")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet[,2:386])
Y2 = BBDDmet$carnicos
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

Scores_base = data.frame(cbind(BBDDmet[1],BBDDmet[387:390],modelo))
colnames(Scores_base) = c("id","carnicos","pescados","RM","PRM","m.TM" )

###Pescados####

met <- read_excel("Only coef_Gauss.xlsx", sheet = "Fish")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet[,2:386])
Y2 = BBDDmet$pescados
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

Scores_base = data.frame(cbind(Scores_base,modelo))
colnames(Scores_base) = c("id","carnicos","pescados","RM","PRM","m.TM","m.Fish")

###RM####

met <- read_excel("Only coef_Gauss.xlsx", sheet = "RM")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet[,2:386])
Y2 = BBDDmet$RM
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

Scores_base = data.frame(cbind(Scores_base,modelo))
colnames(Scores_base) = c("id","carnicos","pescados","RM","PRM","m.TM","m.Fish","m.RM")

###PRM####

met <- read_excel("Only coef_Gauss.xlsx", sheet = "PRM")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet[,2:386])
Y2 = BBDDmet$PRM
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

Scores_base = data.frame(cbind(Scores_base,modelo))
colnames(Scores_base) = c("id","carnicos","pescados","RM","PRM","m.TM","m.Fish","m.RM","m.PRM")

export(Scores_base, "Scores_base.xlsx")

rm(correr, met, model.met, X2, con, i, modelo, stdout, stdout_full, Y2, b)

###Calculo de los scores para cada variable de interés (1 año):

# Creacion de la BBDDmet_1a:
BBDD = merge(phenotypes_predimed_20190903, FFQ_PREDIMED_2012, by = "id")

BBDD$RM3 = BBDD$c_ternera3 + BBDD$c_cerdo3 + BBDD$c_cordero3
summary(BBDD$RM3)
BBDD$PRM3 = BBDD$higad3 + BBDD$visceras3 + BBDD$j_serrano3 + BBDD$j_cocido3 + BBDD$embutidos3 + BBDD$pates3 + BBDD$hamburguesa3 + BBDD$bacon3
summary(BBDD$PRM3)

summary(BBDD$pescados3)
summary(BBDD$carnicos3)

# Modelos 1 año: ####

## Fase 1: Preparación de datos.####

## Fase 1.1: Selección de los metabolitos y la/s variable/s dependientes BASALES####
BBDD$carne = BBDD$carnicos3
BBDD$peces = BBDD$pescados3

merged_dataset_filt = BBDD

colnames(BBDD)[1000:1250]

short_db1<-cbind(id=merged_dataset_filt[,1],
                 merged_dataset_filt[,234:381],
                 merged_dataset_filt[,587:789],
                 merged_dataset_filt[,839:887],
                 merged_dataset_filt[,1246:1249]) # aquí puedes ir poniendo las otras variables de ingesta
colnames(short_db1)

colnames(short_db1) = c("id","glycine","alanine","serine","threonine","methionine","glutamate","asparagine",
                        "glutamine","histidine","arginine","lysine","valine","leucine","isoleucine","phenylalanine",
                        "tyrosine","tryptophan","proline","hydroxyproline","ornithine","citrulline","taurine","gaba",
                        "dimethylglycine","adma","sdma","nmma","allantoin","aminoisobutyricacid","kynurenicacid","methylhistamine",
                        "hydroxyanthranilicacid","ncarbamoylbetaalanine","thiamine","niacinamide",
                        "betaine","choline","phosphocholine","alphaglycerophosphocholine","acetylcholine","creatine","creatinine","thyroxine",
                        "trimethylaminenoxide","adenosine","cytosine","xanthosine","cotinine","pipecolicacid","pyroglutamicacid","methylnicotinamide",
                        "methioninesulfoxide","sarcosine","betaalanine","anserine","carnitine","c2carnitine","c3carnitine","c3dcch3carnitine",
                        "c4carnitine","c4ohcarnitine","c5carnitine","c51carnitine","c5dccarnitine","c6carnitine","c7carnitine","c8carnitine",
                        "c9carnitine","c10carnitine","c102carnitine","c12carnitine","c121carnitine","c14carnitine","c141carnitine","c142carnitine",
                        "c16carnitine","c18carnitine","c181carnitine","c181ohcarnitine","c182carnitine","c20carnitine","c204carnitine","c26carnitine",
                        "methyladenosine","acetaminophen","bilirubin","cortisol","cortisone","cyclohexylamine","ectoine","guanidoaceticacid",
                        "hypoxanthine","inosine","metronidazole","myristoleicacid","n6acetyllysine","nacetylornithine","pseudouridine","sphinganine",
                        "trimethylbenzene","uricacid","urocanicacid","xanthine","butyrobetaine","dz","methylguanosine","methylhistidine",
                        "nalphaacetyarginine","ed","methylguanine","nacetylcysteinylacetaminoph",
                        "acetamidobutanoate","guanidinobutanoicacid","hydroxy3methylacetophenone","hydroxyhippurate",
                        "acetylamino6amino3methylur","el","atenolol","biliverdin","caffeine","deoxycortisol","dmgv","gabapentin",
                        "guanine","homoarginine","hydroxycotinine","linoleoylethanolamide",
                        "metformin","n1methyl2pyridone5carboxami","n4acetylcytidine","nacetylasparticacid","nacetylputrescine",
                        "nacetylspermidine","nmethylproline","oleoylglycine","pantothenol","phenylacetylglutamine","piperine","prolinebetaine",
                        "quinine","ribothymidine","sphingosine","sulfamethoxazole","trigonellinenmethylnicotinat","trimethyllysine","valsartan","verapamil",
                        "warfarin","c240pc","c140lpc","c161lpc","c160lpc","c182lpc","c181lpc","c180lpc","c205lpc","c204lpc","c203lpc","c226lpc",
                        "c160lpe","c182lpe","c181lpe","c180lpe","c204lpe","c226lpe","c301pc","c300pc","c322pc","c321pc","c320pc","c344pc",
                        "c343pc","c342pc","c341pc","c340pc","c364pca","c364pcb","c363pc","c362pc","c361pc","c360pc","c386pc","c384pc",
                        "c383pc","c382pc","c4010pc","c409pc","c406pc","c345pcplasmalogen","c343pcplasmalogen","c342pcplasmalogen",
                        "c341pcplasmalogena","c341pcplasmalogenb","c365pcplasmalogena","c365pcplasmalogenb",
                        "c364pcplasmalogen","c363pcplasmalogen","c362pcplasmalogen","c361pcplasmalogen","c387pcplasmalogen","c386pcplasmalogen",
                        "c384pcplasmalogen","c407pcplasmalogen","c320pe","c342pe","c340pe","c364pe","c363pe","c362pe","c361pe","c386pe",
                        "c385pe","c384pe","c382pe","c406pe","c343peplasmalogen","c342peplasmalogen","c365peplasmalogen","c364peplasmalogen",
                        "c363peplasmalogen","c362peplasmalogen","c361peplasmalogen","c387peplasmalogen","c386peplasmalogen","c385peplasmalogen",
                        "c383peplasmalogen","c407peplasmalogen","c4211peplasmalogen","c340pi","c384pi","c340ps","c406ps","c363psplasmalogen",
                        "c362psplasmalogen","c361psplasmalogen","c160ceramided181","c220ceramided181","c240ceramided181","c241ceramided181",
                        "c140sm","c161sm","c160sm","c182sm","c181sm","c180sm","c200sm","c221sm","c220sm","c241sm","c240sm","c140ce",
                        "c161ce","c160ce","c183ce","c182ce","c181ce","c180ce","c205ce","c204ce","c203ce","c226ce","c225ce","c224ce",
                        "c141mag","c161mag","c180mag","c221mag","c300dag","c321dag","c320dag","c343dag","c342dag","c341dag","c340dag","c363dag",
                        "c362dag","c361dag","c360dag","c364dag","c385dag","c384dag","c420tag","c442tag","c441tag","c440tag","c463tag","c462tag",
                        "c461tag","c460tag","c484tag","c483tag","c482tag","c481tag","c480tag","c505tag","c504tag","c503tag","c502tag","c501tag",
                        "c500tag","c527tag","c526tag","c525tag","c524tag","c523tag","c522tag","c521tag","c520tag","c5410tag","c549tag","c548tag",
                        "c547tag","c546tag","c545tag","c544tag","c543tag","c542tag","c541tag","c569tag","c568tag","c567tag","c566tag","c565tag",
                        "c564tag","c563tag","c562tag","c561tag","c5811tag","c5810tag","c589tag","c588tag","c587tag","c586tag","c6012tag","c200lpe",
                        "c220lpe","c342hydroxypc","c451tag","c471tag","c491tag","c492tag","c493tag","c510tag","c511tag","c512tag","c513tag","c532tag",
                        "c533tag","c552tag","c553tag","cholesterol","glycocholated4","thymined4","inosine15n4","aminoadipate","alphaglycerophosphate",
                        "pyridoxate","aconitate","adipate","amp","adp","citrate","isocitrate","hexosemonophosphate","fructoseglucosegalactose","fumaratemaleate","gdp",
                        "glucuronate","gmp","hippurate","kynurenine","lactate","lactose","malate","oxalate","pantothenate","pyruvate","quinolinate","salicylurate",
                        "sorbitol","succinate","sucrose","udp","uracil","urate","uridine","glycocholate","glycodeoxychenodeox","suberate","indoxylsulfate","indole3propionate",
                        "gentisate","phosphocreatine","alphahydroxybutyrate","betahydroxybutyrate","hydroxyglutarate","inositol","methyladipatepimelate","phosphoglycerate",
                        "taurodeoxychenodeox","RM","PRM","carnicos","pescados")

## Fase 1.2: Eliminar los metabolitos que son estándares:####
#C240PC, glycocholated4, thymined4, inosine15N4
drop <- c("c240pc", "glycocholated4", "thymined4", "inosine15n4")
short_db1 = short_db1[,!(names(short_db1) %in% drop)]

## Fase 1.3: Determinar missing values en los sujetos y eliminar aquellos con más de un 20% ####
# Check Subjects with NAs
subjects_NA<-as.data.frame(rowSums(is.na(short_db1)))
otra<-cbind(short_db1$id, subjects_NA)
df <-  short_db1
subjects_NA<-as.data.frame(rowSums(is.na(df)))
otra<-cbind(df$id, subjects_NA)
rows_remove<-otra$`df$id`[otra$`rowSums(is.na(df))`>78]  # threshold based on this databases 0.2*390
newdata <- subset(df, !id %in% rows_remove)
short_db1<-newdata

## Fase 1.4: Determinar missing values en metabolitos ####
#Generate database with only metabolites
quantitative_variables1<-short_db1[,2:(length(short_db1)-4)]
#names(quantitative_variables1)

#NAs in each variable
na_count1 <-sapply(quantitative_variables1, function(quantitative_variables1) sum(length(which(is.na(quantitative_variables1)))))
na_count2 <-sapply(quantitative_variables1, function(quantitative_variables1) (100*sum(length(which(is.na(quantitative_variables1))))/sum(length((quantitative_variables1)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)
#na_count

#Metabolites with NA higher than 20%
metabo_high_NA<-rownames(na_count[na_count[,2]>20 ,])

#Remove variables with high number of missing values (>20)
drop <- metabo_high_NA
quantitative_variables1_filtered = quantitative_variables1[,!(names(quantitative_variables1) %in% drop)]
na_count1 <-sapply(quantitative_variables1_filtered, function(quantitative_variables1_filtered) sum(length(which(is.na(quantitative_variables1_filtered)))))
na_count2 <-sapply(quantitative_variables1_filtered, function(quantitative_variables1_filtered) (100*sum(length(which(is.na(quantitative_variables1_filtered))))/sum(length((quantitative_variables1_filtered)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

na_count = data.frame(na_count)
na_count$names = rownames(na_count)

export(na_count, "na_count1a.xlsx")

## Fase 1.5: Tratamiento de missing values --> valor mitad del mínimo. ####
quantitative_variables1_filtered1 = quantitative_variables1_filtered

inter_qvf <- quantitative_variables1_filtered1
for(i in 1:ncol(inter_qvf)){
  inter_qvf[is.na(inter_qvf[,i]), i] <- min(inter_qvf[,i], na.rm = TRUE)/2
}

## Fase 1.6: Escalar y centrar los datos ####
x<-inter_qvf
#Scale and center variables
scaledcentered_treatment <- apply(x, 2, function(x) ((x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)))

BBDDmet_1a = cbind(short_db1[1],scaledcentered_treatment, short_db1[398:401])

BBDDmet_1a = subset(BBDDmet_1a, RM >= 0)

export(BBDDmet_1a, "BBDDmet_1a.xlsx")

###Calculo de los scores para cada variable de interés (1 año):

###----Carnicos

met <- read_excel("Only coef_Gauss.xlsx", sheet = "TM")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet_1a[,2:385])
Y2 = BBDDmet_1a$carnicos
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

Scores_1a = data.frame(cbind(BBDDmet_1a[1],BBDDmet_1a[388:389], BBDDmet_1a[386:387],modelo))
colnames(Scores_1a) = c("id","carnicos","pescados","RM","PRM","m.TM" )

###----Pescados

met <- read_excel("Only coef_Gauss.xlsx", sheet = "Fish")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet_1a[,2:385])
Y2 = BBDDmet_1a$pescados
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

Scores_1a = data.frame(cbind(Scores_1a, modelo))
colnames(Scores_1a) = c("id","carnicos","pescados","RM","PRM","m.TM","m.Fish" )

###----RM

met <- read_excel("Only coef_Gauss.xlsx", sheet = "RM")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet_1a[,2:385])
Y2 = BBDDmet_1a$RM
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

Scores_1a = data.frame(cbind(Scores_1a, modelo))
colnames(Scores_1a) = c("id","carnicos","pescados","RM","PRM","m.TM","m.Fish","m.RM")

###----PRM

met <- read_excel("Only coef_Gauss.xlsx", sheet = "PRM")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet_1a[,2:385])
Y2 = BBDDmet_1a$PRM
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

Scores_1a = data.frame(cbind(Scores_1a, modelo))
colnames(Scores_1a) = c("id","carnicos","pescados","RM","PRM","m.TM","m.Fish","m.RM","m.PRM")

export(Scores_1a, "Scores_1a.xlsx")

rm(b, BBDD, correr, df, inter_qvf, merged_dataset_filt, met, model.met, na_count, newdata, otra, quantitative_variables_filtered,
   quantitative_variables1, quantitative_variables1_filtered, quantitative_variables1_filtered1, scaledcentered_treatment, short_db1,
   subjects_NA, x, X2, con, drop, i, metabo_high_NA, modelo, na_count1, na_count2, stdout, stdout_full, Y2, rows_remove)

# Fase 5: modelos de Cox ####

T2D <- read_csv("E:/ARTICULOS/BBDD/PREDIMED/T2Dprojectweighted.csv")
BBDD_T2D = merge(T2D, BBDDmet, by = "id")
propensity <- read_csv("E:/ARTICULOS/Autor/PREDIMED/ART huella met - carne (estancia)/Estadistica/propensity.csv")
BBDD_T2D = merge(BBDD_T2D, propensity, by = "id")
Alimentos <- read_sav("E:/ARTICULOS/BBDD/PREDIMED/FFQ_PREDIMED_2012.SAV")
BBDD_T2D = merge(BBDD_T2D, Alimentos, by = "id")
load("E:/ARTICULOS/Autor/PREDIMED/ART huella met - carne (estancia)/Estadistica/vars_necesarias_updated.Rdata")
BBDD_T2D = merge(BBDD_T2D, sub_base, by = "id")
BBDD_T2D = merge(BBDD_T2D, Scores_base, by = "id")
colnames(BBDD_T2D)

### Transformacion de las puntuaciones a SD

sd(BBDD_T2D$m.TM)
BBDD_T2D$m.TM = BBDD_T2D$m.TM/15.27858
sd(BBDD_T2D$m.Fish)
BBDD_T2D$m.Fish = BBDD_T2D$m.Fish/17.69554
sd(BBDD_T2D$m.RM)
BBDD_T2D$m.RM = BBDD_T2D$m.RM/8.651162
sd(BBDD_T2D$m.PRM)
BBDD_T2D$m.PRM = BBDD_T2D$m.PRM/4.988975

sd(BBDD_T2D$carnicos)
BBDD_T2D$TM_SD = BBDD_T2D$carnicos/51.99251
sd(BBDD_T2D$pescados)
BBDD_T2D$Fish_SD = BBDD_T2D$pescados/46.90789
sd(BBDD_T2D$RM.y)
BBDD_T2D$RM_SD = BBDD_T2D$RM.y/35.43112
sd(BBDD_T2D$PRM.y)
BBDD_T2D$PRM_SD = BBDD_T2D$PRM.y/19.28976

# Quintile of food groups for ADJUSTMENTS

# FRUITS
BBDD_T2D$ntile_frutatot<-quantcut(BBDD_T2D$frutatot, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D$ntile_frutatot)
levels(BBDD_T2D$ntile_frutatot) <- c("1","2","3","4","5") #Recode levels

# VEGETABLES
BBDD_T2D$ntile_verdutot<-quantcut(BBDD_T2D$verdutot, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D$ntile_verdutot)
levels(BBDD_T2D$ntile_verdutot) <- c("1","2","3","4","5") #Recode levels

# CEREALS
BBDD_T2D$ntile_cerealdes<-quantcut(BBDD_T2D$grupocer, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D$ntile_cerealdes)
levels(BBDD_T2D$ntile_cerealdes) <- c("1","2","3","4","5") #Recode levels

# OLIVE OIL
BBDD_T2D$ntile_ac_oliva<-quantcut(BBDD_T2D$ac_oliva, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D$ntile_ac_oliva)
levels(BBDD_T2D$ntile_ac_oliva) <- c("1","2","3","4","5") #Recode levels

# EGGS
BBDD_T2D$ntile_huevos<-quantcut(BBDD_T2D$huevos, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D$ntile_huevos)
levels(BBDD_T2D$ntile_huevos) <- c("1","2","3","4","5") #Recode levels

# LEGUMES
BBDD_T2D$ntile_legumbre<-quantcut(BBDD_T2D$legumbre, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D$ntile_legumbre)
levels(BBDD_T2D$ntile_legumbre) <- c("1","2","3","4","5") #Recode levels

# NUTS
BBDD_T2D$ntile_fsecos<-quantcut(BBDD_T2D$fsecos, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D$ntile_fsecos)
levels(BBDD_T2D$ntile_fsecos) <- c("1","2","3","4","5") #Recode levels

# DAIRY
BBDD_T2D$ntile_lacteos<-quantcut(BBDD_T2D$lacteos, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D$ntile_lacteos)
levels(BBDD_T2D$ntile_lacteos) <- c("1","2","3","4","5") #Recode levels

colnames(BBDD_T2D)

approaches <- c("m.TM",
                "m.Fish",
                "m.RM",
                "m.PRM")

foods <- c("carnicos",
           "pescados",
           "RM.y",
           "PRM.y")

#approaches_foods <- c("m.TM + TM_SD",
#                      "m.Fish + Fish_SD",
#                      "m.RM + RM_SD",
#                      "m.PRM + PRM_SD")

names(BBDD_T2D)[names(BBDD_T2D) == 'edad0'] <- 'age'
names(BBDD_T2D)[names(BBDD_T2D) == 'sexo'] <- 'sex'
names(BBDD_T2D)[names(BBDD_T2D) == 'diabetes0'] <- 'diabetes'
names(BBDD_T2D)[names(BBDD_T2D) == 'ps1.x'] <- 'ps1'
names(BBDD_T2D)[names(BBDD_T2D) == 'ps2.x'] <- 'ps2'
names(BBDD_T2D)[names(BBDD_T2D) == 'centro'] <- 'center'
names(BBDD_T2D)[names(BBDD_T2D) == 'fum'] <- 'smoking'
names(BBDD_T2D)[names(BBDD_T2D) == 'grup_int'] <- 'interv_g'
names(BBDD_T2D)[names(BBDD_T2D) == 'getota_1.x'] <- 'getota_1'
names(BBDD_T2D)[names(BBDD_T2D) == 'ant_fam'] <- 'fam_history'
names(BBDD_T2D)[names(BBDD_T2D) == 'imc1'] <- 'bmi'
names(BBDD_T2D)[names(BBDD_T2D) == 'tra_col0.x'] <- 'tra_col0'
names(BBDD_T2D)[names(BBDD_T2D) == 'hta0.x'] <- 'hta0'
names(BBDD_T2D)[names(BBDD_T2D) == 'trathta0.x'] <- 'trathta0'
names(BBDD_T2D)[names(BBDD_T2D) == 'hipercol0.x'] <- 'hipercol0'

## T2D

formu_model1 <- list()
fit_model1 <- list()

formu_model2 <- list()
fit_model2 <- list()

formu_model3 <- list()
fit_model3 <- list()

formu_model4 <- list()
fit_model4 <- list()


for (i in 1:4){
  
  formu_model1[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + as.factor(nodo) + as.factor(interv_g)"))
  
  fit_model1[[i]] <- coxph(formu_model1[[i]], weights=w, data = BBDD_T2D)
  
  formu_model2[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         educat + bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) +
                                         as.factor(nodo) + as.factor(interv_g)"))
  
  fit_model2[[i]] <- coxph(formu_model2[[i]], weights=w, data = BBDD_T2D)
  
  formu_model3[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         educat + bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) +
                                         energiat + frutatot + verdutot + grupocer +  
                                         ac_oliva + huevos + legumbre +    
                                         fsecos +
                                         as.factor(nodo) + as.factor(interv_g)"))
  
  fit_model3[[i]] <- coxph(formu_model3[[i]], weights=w, data = BBDD_T2D)
  
  formu_model4[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         educat + bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) +
                                         energiat + frutatot + verdutot + grupocer +  
                                         ac_oliva + huevos + legumbre +    
                                         fsecos + ", foods[i],
                                         "+ as.factor(nodo)+ as.factor(interv_g)"))
  
  fit_model4[[i]] <- coxph(formu_model4[[i]], weights=w, data = BBDD_T2D)
}

# MODEL 1

results_model1 <- matrix(NA, 4, 4)
results_model2 <- matrix(NA, 4, 4)
results_model3 <- matrix(NA, 4, 4)
results_model4 <- matrix(NA, 4, 4)

rownames(results_model1) <- c("model_m.TM",
                              "model_m.Fish",
                              "model_m.RM",
                              "model_m.PRM")
rownames(results_model2)<-rownames(results_model1)
rownames(results_model3)<-rownames(results_model1)
rownames(results_model4)<-rownames(results_model1)

colnames(results_model1)<-c("HR", "lower 95% CI", "upper 95% CI", "P_value")
colnames(results_model2)<-colnames(results_model1)
colnames(results_model3)<-colnames(results_model1)
colnames(results_model4)<-colnames(results_model1)

for (i in 1:4){
  get_data_sum <- summary(fit_model1[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  
  results_model1[i,1] <- round(hr_data[1], 3)
  results_model1[i,2] <- round(hr_data[2], 3)
  results_model1[i,3] <- round(hr_data[3], 3)
  results_model1[i,4] <- round(get_data_sum$coefficients[1,5], 3)  # Pvalue

  get_data_sum <- summary(fit_model2[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]

  results_model2[i,1] <- round(hr_data[1], 3)
  results_model2[i,2] <- round(hr_data[2], 3)
  results_model2[i,3] <- round(hr_data[3], 3)
  results_model2[i,4] <- round(get_data_sum$coefficients[1,5], 3)  # Pvalue

  get_data_sum <- summary(fit_model3[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]

  results_model3[i,1] <- round(hr_data[1], 3)
  results_model3[i,2] <- round(hr_data[2], 3)
  results_model3[i,3] <- round(hr_data[3], 3)
  results_model3[i,4] <- round(get_data_sum$coefficients[1,5], 3)  # Pvalue

  get_data_sum <- summary(fit_model4[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]

  results_model4[i,1] <- round(hr_data[1], 3)
  results_model4[i,2] <- round(hr_data[2], 3)
  results_model4[i,3] <- round(hr_data[3], 3)
  results_model4[i,4] <- round(get_data_sum$coefficients[1,5], 3)  # Pvalue
}

full_results <- rbind(c("Model 1"),results_model1, 
                      c("Model 2"), results_model2,
                      c("Model 3"), results_model3,
                      c("Model 4"), results_model4)

modelos = data.frame(full_results)
modelos$names = rownames(modelos)
export(modelos, "modelos_grupo_23022022.xlsx")
export(BBDD_T2D, "BBDD_T2D.csv")

table(BBDD_T2D$cens)

#### Modelos 1 año:

#### Creacion de la BBDD:

BBDD_T2D1a = merge(ID, T2D, by = "id", all.x = T) # Fusion con los IDs (704, 161 eventos) con nuevos eventos al año
export(BBDD_T2D1a, "BBDD_T2D1a.csv")
BBDD_T2D1a = read_delim("BBDD_T2D1a.csv", ";", escape_double = FALSE, trim_ws = TRUE)
BBDD_T2D1a = merge(BBDD_T2D1a, BBDDmet_1a, by = "id")
BBDD_T2D1a = merge(BBDD_T2D1a, propensity, by = "id")
BBDD_T2D1a = merge(BBDD_T2D1a, Alimentos, by = "id")
BBDD_T2D1a = merge(BBDD_T2D1a, sub_base, by = "id")

BBDD_T2D1a$RM3 = BBDD_T2D1a$c_ternera3 + BBDD_T2D1a$c_cerdo3 + BBDD_T2D1a$c_cordero3
BBDD_T2D1a$PRM3 = BBDD_T2D1a$higad3 + BBDD_T2D1a$visceras3 + BBDD_T2D1a$j_serrano3 + BBDD_T2D1a$j_cocido3 + BBDD_T2D1a$embutidos3 + BBDD_T2D1a$pates3 + BBDD_T2D1a$hamburguesa3 + BBDD_T2D1a$bacon3

table(BBDD_T2D1a$cens)

# Calculo de los Scores al año:

###----Carnicos

met <- read_excel("Only coef_Gauss.xlsx", sheet = "TM")

model.met = met
str(model.met)

X2 = data.frame(BBDD_T2D1a[,116:499])
Y2 = BBDD_T2D1a$carnicos3
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

BBDD_T2D1a$m.TM = modelo

###----Pescados

met <- read_excel("Only coef_Gauss.xlsx", sheet = "Fish")

model.met = met
str(model.met)

X2 = data.frame(BBDD_T2D1a[,116:499])
Y2 = BBDD_T2D1a$pescados3
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

BBDD_T2D1a$m.Fish = modelo

###----RM

met <- read_excel("Only coef_Gauss.xlsx", sheet = "RM")

model.met = met
str(model.met)

X2 = data.frame(BBDD_T2D1a[,116:499])
Y2 = BBDD_T2D1a$RM3
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

BBDD_T2D1a$m.RM = modelo

###----PRM

met <- read_excel("Only coef_Gauss.xlsx", sheet = "PRM")

model.met = met
str(model.met)

X2 = data.frame(BBDD_T2D1a[,116:499])
Y2 = BBDD_T2D1a$PRM3
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

BBDD_T2D1a$m.PRM = modelo

export(BBDD_T2D1a, "BBDD_T2D1a.csv")

rm(b, BBDD, correr, df, inter_qvf, merged_dataset_filt, met, model.met, na_count, newdata, otra, quantitative_variables_filtered,
   quantitative_variables1, quantitative_variables1_filtered, quantitative_variables1_filtered1, scaledcentered_treatment, short_db1,
   subjects_NA, x, X2, con, drop, i, metabo_high_NA, modelo, na_count1, na_count2, stdout, stdout_full, Y2, rows_remove)

sd(BBDD_T2D1a$m.TM)
BBDD_T2D1a$m.TM = BBDD_T2D1a$m.TM/16.0219
sd(BBDD_T2D1a$m.Fish)
BBDD_T2D1a$m.Fish = BBDD_T2D1a$m.Fish/15.89966
sd(BBDD_T2D1a$m.RM)
BBDD_T2D1a$m.RM = BBDD_T2D1a$m.RM/8.714293
sd(BBDD_T2D1a$m.PRM)
BBDD_T2D1a$m.PRM = BBDD_T2D1a$m.PRM/5.175869

sd(BBDD_T2D1a$carnicos3, na.rm = T)
BBDD_T2D1a$TM_SD = BBDD_T2D1a$carnicos3/48.76466
sd(BBDD_T2D1a$pescados3, na.rm = T)
BBDD_T2D1a$Fish_SD = BBDD_T2D1a$pescados3/42.63011
sd(BBDD_T2D1a$RM3, na.rm = T)
BBDD_T2D1a$RM_SD = BBDD_T2D1a$RM3/30.72171
sd(BBDD_T2D1a$PRM3, na.rm = T)
BBDD_T2D1a$PRM_SD = BBDD_T2D1a$PRM3/16.75079

# Quintile of food groups for ADJUSTMENTS

# FRUITS
BBDD_T2D1a$ntile_frutatot<-quantcut(BBDD_T2D1a$frutatot3, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D1a$ntile_frutatot)
levels(BBDD_T2D1a$ntile_frutatot) <- c("1","2","3","4","5") #Recode levels

# VEGETABLES
BBDD_T2D1a$ntile_verdutot<-quantcut(BBDD_T2D1a$verdutot3, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D1a$ntile_verdutot)
levels(BBDD_T2D1a$ntile_verdutot) <- c("1","2","3","4","5") #Recode levels

# CEREALS
BBDD_T2D1a$ntile_cereales<-quantcut(BBDD_T2D1a$grupocer3, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D1a$ntile_cereales)
levels(BBDD_T2D1a$ntile_cereales) <- c("1","2","3","4","5") #Recode levels

# OLIVE OIL
BBDD_T2D1a$ntile_ac_oliva<-Hmisc::cut2(BBDD_T2D1a$ac_oliva3, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D1a$ntile_ac_oliva)
levels(BBDD_T2D1a$ntile_ac_oliva) <- c("1","2","3","4","5") #Recode levels

# EGGS
BBDD_T2D1a$ntile_huevos<-Hmisc::cut2(BBDD_T2D1a$huevos3, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D1a$ntile_huevos)
levels(BBDD_T2D1a$ntile_huevos) <- c("1","2","3","4","5") #Recode levels

# LEGUMES
BBDD_T2D1a$ntile_legumbre<-quantcut(BBDD_T2D1a$legumbre3, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D1a$ntile_legumbre)
levels(BBDD_T2D1a$ntile_legumbre) <- c("1","2","3","4","5") #Recode levels

# NUTS
BBDD_T2D1a$ntile_fsecos<-Hmisc::cut2(BBDD_T2D1a$fsecos3, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D1a$ntile_fsecos)
levels(BBDD_T2D1a$ntile_fsecos) <- c("1","2","3","4","5") #Recode levels

# DAIRY
BBDD_T2D1a$ntile_lacteos<-quantcut(BBDD_T2D1a$lacteos3, q=5, na.rm=TRUE) #based on median
is.factor(BBDD_T2D1a$ntile_lacteos)
levels(BBDD_T2D1a$ntile_lacteos) <- c("1","2","3","4","5") #Recode levels

approaches <- c("m.TM",
                "m.Fish",
                "m.RM",
                "m.PRM")

foods <- c("carnicos3",
           "pescados3",
           "RM3",
           "PRM3")

#approaches_foods <- c("m.TM + TM_SD",
#                      "m.Fish + Fish_SD",
#                      "m.RM + RM_SD",
#                      "m.PRM + PRM_SD")

names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'edad0'] <- 'age'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'sexo'] <- 'sex'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'diabetes0'] <- 'diabetes'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'ps1.x'] <- 'ps1'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'ps2.x'] <- 'ps2'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'centro'] <- 'center'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'fum'] <- 'smoking'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'grup_int'] <- 'interv_g'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'getota_1.x'] <- 'getota_1'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'ant_fam'] <- 'fam_history'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'imc1'] <- 'bmi'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'tra_col0.x'] <- 'tra_col0'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'hta0.x'] <- 'hta0'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'trathta0.x'] <- 'trathta0'
names(BBDD_T2D1a)[names(BBDD_T2D1a) == 'hipercol0.x'] <- 'hipercol0'

## T2D

formu_model1 <- list()
fit_model1 <- list()

formu_model2 <- list()
fit_model2 <- list()

formu_model3 <- list()
fit_model3 <- list()

formu_model4 <- list()
fit_model4 <- list()

for (i in 1:4){
  
  formu_model1[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + as.factor(interv_g) + as.factor(nodo)"))
  
  fit_model1[[i]] <- coxph(formu_model1[[i]], weights=w, data = BBDD_T2D1a)
  
  formu_model2[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         educat + bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) +
                                         strata(nodo)"))
  
  fit_model2[[i]] <- coxph(formu_model2[[i]], weights=w, data = BBDD_T2D1a)
  
  formu_model3[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         educat + bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) +
                                         energiat3 + frutatot3 + verdutot3 + grupocer3 + lacteos3 +  
                                         olivatot3 + huevos3 + legumbre3 +  
                                         fsecos3 +
                                         as.factor(interv_g) + as.factor(nodo)"))

  fit_model3[[i]] <- coxph(formu_model3[[i]], weights=w, data = BBDD_T2D1a)
  
  formu_model4[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         educat + bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) +
                                         energiat3 + frutatot3 + verdutot3 + grupocer3 + lacteos3 +  
                                         olivatot3 + huevos3 + legumbre3 +  
                                         fsecos3 +", foods[i],
                                         "+ as.factor(interv_g) + as.factor(nodo)"))
  fit_model4[[i]] <- coxph(formu_model4[[i]], weights=w, data = BBDD_T2D1a)
}

# MODEL 1

results_model1 <- matrix(NA, 4, 4)
results_model2 <- matrix(NA, 4, 4)
results_model3 <- matrix(NA, 4, 4)
results_model4 <- matrix(NA, 4, 4)

rownames(results_model1) <- c("model_m.TM",
                              "model_m.Fish",
                              "model_m.RM",
                              "model_m.PRM")
rownames(results_model2)<-rownames(results_model1)
rownames(results_model3)<-rownames(results_model1)
rownames(results_model4)<-rownames(results_model1)

colnames(results_model1)<-c("HR", "lower 95% CI", "upper 95% CI", "P_value")
colnames(results_model2)<-colnames(results_model1)
colnames(results_model3)<-colnames(results_model1)
colnames(results_model4)<-colnames(results_model1)

for (i in 1:4){
  get_data_sum <- summary(fit_model1[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  
  results_model1[i,1] <- round(hr_data[1], 3)
  results_model1[i,2] <- round(hr_data[2], 3)
  results_model1[i,3] <- round(hr_data[3], 3)
  results_model1[i,4] <- round(get_data_sum$coefficients[1,5], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model2[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model2[i,1] <- round(hr_data[1], 3)
  results_model2[i,2] <- round(hr_data[2], 3)
  results_model2[i,3] <- round(hr_data[3], 3)
  results_model2[i,4] <- round(get_data_sum$coefficients[1,5], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model3[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model3[i,1] <- round(hr_data[1], 3)
  results_model3[i,2] <- round(hr_data[2], 3)
  results_model3[i,3] <- round(hr_data[3], 3)
  results_model3[i,4] <- round(get_data_sum$coefficients[1,5], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model4[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model4[i,1] <- round(hr_data[1], 3)
  results_model4[i,2] <- round(hr_data[2], 3)
  results_model4[i,3] <- round(hr_data[3], 3)
  results_model4[i,4] <- round(get_data_sum$coefficients[1,5], 3)  # Pvalue
}

full_results <- rbind(c("Model 1"),results_model1, 
                      c("Model 2"), results_model2,
                      c("Model 3"), results_model3,
                      c("Model 4"), results_model4)

modelos = data.frame(full_results)
modelos$names = rownames(modelos)
export(modelos, "modelos1aT2D_23022022.xlsx")

rm(data_sup, fit_model1, fit_model2, fit_model3, fit_model4, formu_model1, formu_model2, formu_model3, formu_model4, full_results, 
   get_data_sum, ID, modelos, results_model1, results_model2, results_model3, results_model4, approaches, foods, hr_data, i)

# Fase 6: heatmaps ####

O1 <- read_excel("Only coef_Gauss.xlsx", sheet = "TM")
O2 <- read_excel("Only coef_Gauss.xlsx", sheet = "Fish")
O3 <- read_excel("Only coef_Gauss.xlsx", sheet = "RM")
O4 <- read_excel("Only coef_Gauss.xlsx", sheet = "PRM")

Final_scores = merge(O1, O2, by = "Metabolites", all = T)
Final_scores = merge(Final_scores, O3, by = "Metabolites", all = T)
Final_scores = merge(Final_scores, O4, by = "Metabolites", all = T)
Final_scores = merge(Final_scores, O5, by = "Metabolites", all = T)

export(Final_scores, "Final_scores.xlsx")
Final_scores <- read_excel("Final_scores.xlsx")

#S1
Final_scores <- read_excel("Final_scores.xlsx", sheet = "S1")
Final_scores = data.frame(Final_scores)

Final_scores1 = melt(Final_scores)
head(Final_scores1)

ggplot(data = data.frame(Final_scores1), aes(x =variable , y = names, fill= value)) + geom_tile() + 
  scale_fill_gradient(low = "Red", high = "Blue") + scale_x_discrete(expand = c(0, 0)) + scale_fill_gradient2() + xlab(" ") + 
  ylab(" ")

#S2
Final_scores <- read_excel("Final_scores.xlsx", sheet = "S2")
Final_scores = data.frame(Final_scores)

Final_scores1 = melt(Final_scores)
head(Final_scores1)

ggplot(data = data.frame(Final_scores1), aes(x =variable , y = names , fill= value)) + geom_tile() + 
  scale_fill_gradient(low = "Red", high = "Blue") + scale_x_discrete(expand = c(0, 0)) + scale_fill_gradient2() + xlab(" ") + 
  ylab(" ")

#S3
Final_scores <- read_excel("Final_scores.xlsx", sheet = "S3")
Final_scores = data.frame(Final_scores)

Final_scores1 = melt(Final_scores)
head(Final_scores1)

ggplot(data = data.frame(Final_scores1), aes(x =variable , y = names , fill= value)) + geom_tile() + 
  scale_fill_gradient(low = "Red", high = "Blue") + scale_x_discrete(expand = c(0, 0)) + scale_fill_gradient2() + xlab(" ") + 
  ylab(" ")

#Figura Pablo
Final_scores <- read_excel("Final_scores.xlsx", sheet = "Hoja1")
Final_scores = data.frame(Final_scores)

Final_scores1 = melt(Final_scores)
head(Final_scores1)

ggplot(data = data.frame(Final_scores1), aes(x =variable , y = names , fill= value)) + geom_tile() + 
  scale_fill_gradient(low = "Red", high = "Blue") + scale_x_discrete(expand = c(0, 0)) + scale_fill_gradient2() + xlab(" ") + 
  ylab(" ")

# Fase 7: Descriptivos con toda la muestra por terciles de fish, RM y PRM (T1 vs T3); y al año por BBDD caso control(BBDD T2D) ####

BBDDter_basal = merge(BBDDmet[1], FFQ_PREDIMED_2012, by = "id")
BBDDter_basal = merge(BBDDter_basal, phenotypes_predimed_20190903, by = "id")

BBDDter_basal$RM = BBDDter_basal$c_ternera + BBDDter_basal$c_cerdo + BBDDter_basal$c_cordero
summary(BBDDter_basal$RM)
BBDDter_basal$PRM = BBDDter_basal$higad + BBDDter_basal$visceras + BBDDter_basal$j_serrano + BBDDter_basal$j_cocido + BBDDter_basal$embutidos + BBDDter_basal$pates + BBDDter_basal$hamburguesa + BBDDter_basal$bacon
summary(BBDDter_basal$PRM)

BBDDter_basal$ter_RM = cut2(BBDDter_basal$RM, g = 3)
table(BBDDter_basal$ter_RM)
tapply(BBDDter_basal$RM, BBDDter_basal$ter_RM, mean, na.rm = T)

BBDDter_basal$ter_PRM = cut2(BBDDter_basal$PRM, g = 3)
table(BBDDter_basal$ter_PRM)
tapply(BBDDter_basal$PRM, BBDDter_basal$ter_PRM, mean, na.rm = T)

BBDDter_basal$ter_F = cut2(BBDDter_basal$pescados, g = 3)
table(BBDDter_basal$ter_F)
tapply(BBDDter_basal$pescados, BBDDter_basal$ter_F, mean, na.rm = T)

BBDD = data.frame(cbind(BBDDter_basal$ter_F, BBDDter_basal$age, BBDDter_basal$bmi0, BBDDter_basal$waist_1, BBDDter_basal$carnicos, BBDDter_basal$pescados,
                        BBDDter_basal$RM, BBDDter_basal$PRM, BBDDter_basal$verdutot, BBDDter_basal$frutatot, BBDDter_basal$legumbre, BBDDter_basal$grupocer,
                        BBDDter_basal$lacteos, BBDDter_basal$olivatot, BBDDter_basal$fsecos, BBDDter_basal$vino, BBDDter_basal$hc, BBDDter_basal$prot,
                        BBDDter_basal$gratot, BBDDter_basal$mo, BBDDter_basal$sa, BBDDter_basal$po, BBDDter_basal$alcoholg, BBDDter_basal$energiat, BBDDter_basal$fibra))

colnames(BBDD) = c("ter_F", "age", "bmi0", "waist_1", "carnicos", "pescados",
                   "RM", "PRM", "verdutot", "frutatot", "legumbre", "grupocer",
                   "lacteos", "olivatot", "fsecos", "vino", "hc", "prot",
                   "gratot", "mo", "sa", "po", "alcoholg", "energiat", "fibra")

medianas = c()
iqrs = c()

for (i in 2:25) {
  medianas[[i]] = tapply(BBDD[[i]], BBDD[[1]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD[[i]], BBDD[[1]], IQR, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(cbind(medianas, iqrs))
iqrs$names = colnames(BBDD)[2:25]

colnames(iqrs) = c("T1", "T2", "T3", "IQR1", "IQR2", "IQR3", "names")

export(iqrs, "alimentacion_terfish.xlsx")

BBDD = data.frame(cbind(BBDDter_basal$ter_RM, BBDDter_basal$age, BBDDter_basal$bmi0, BBDDter_basal$waist_1, BBDDter_basal$carnicos, BBDDter_basal$pescados,
                        BBDDter_basal$RM, BBDDter_basal$PRM, BBDDter_basal$verdutot, BBDDter_basal$frutatot, BBDDter_basal$legumbre, BBDDter_basal$grupocer,
                        BBDDter_basal$lacteos, BBDDter_basal$olivatot, BBDDter_basal$fsecos, BBDDter_basal$vino, BBDDter_basal$hc, BBDDter_basal$prot,
                        BBDDter_basal$gratot, BBDDter_basal$mo, BBDDter_basal$sa, BBDDter_basal$po, BBDDter_basal$alcoholg, BBDDter_basal$energiat, BBDDter_basal$fibra))

colnames(BBDD) = c("ter_RM", "age", "bmi0", "waist_1", "carnicos", "pescados",
                   "RM", "PRM", "verdutot", "frutatot", "legumbre", "grupocer",
                   "lacteos", "olivatot", "fsecos", "vino", "hc", "prot",
                   "gratot", "mo", "sa", "po", "alcoholg", "energiat", "fibra")

medianas = c()
iqrs = c()

for (i in 2:25) {
  medianas[[i]] = tapply(BBDD[[i]], BBDD[[1]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD[[i]], BBDD[[1]], IQR, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(cbind(medianas, iqrs))
iqrs$names = colnames(BBDD)[2:25]

colnames(iqrs) = c("T1", "T2", "T3", "IQR1", "IQR2", "IQR3", "names")

export(iqrs, "alimentacion_terRM.xlsx")

BBDD = data.frame(cbind(BBDDter_basal$ter_PRM, BBDDter_basal$age, BBDDter_basal$bmi0, BBDDter_basal$waist_1, BBDDter_basal$carnicos, BBDDter_basal$pescados,
                        BBDDter_basal$RM, BBDDter_basal$PRM, BBDDter_basal$verdutot, BBDDter_basal$frutatot, BBDDter_basal$legumbre, BBDDter_basal$grupocer,
                        BBDDter_basal$lacteos, BBDDter_basal$olivatot, BBDDter_basal$fsecos, BBDDter_basal$vino, BBDDter_basal$hc, BBDDter_basal$prot,
                        BBDDter_basal$gratot, BBDDter_basal$mo, BBDDter_basal$sa, BBDDter_basal$po, BBDDter_basal$alcoholg, BBDDter_basal$energiat, BBDDter_basal$fibra))

colnames(BBDD) = c("ter_PRM", "age", "bmi0", "waist_1", "carnicos", "pescados",
                   "RM", "PRM", "verdutot", "frutatot", "legumbre", "grupocer",
                   "lacteos", "olivatot", "fsecos", "vino", "hc", "prot",
                   "gratot", "mo", "sa", "po", "alcoholg", "energiat", "fibra")

medianas = c()
iqrs = c()

for (i in 2:25) {
  medianas[[i]] = tapply(BBDD[[i]], BBDD[[1]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD[[i]], BBDD[[1]], IQR, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(cbind(medianas, iqrs))
iqrs$names = colnames(BBDD)[2:25]

colnames(iqrs) = c("T1", "T2", "T3", "IQR1", "IQR2", "IQR3", "names")

export(iqrs, "alimentacion_terPRM.xlsx")

(table(as.factor(BBDDter_basal$sex), BBDDter_basal$ter_F)) 
(prop.table(table(as.factor(BBDDter_basal$sex), BBDDter_basal$ter_F),2)*100) 

(table(as.factor(BBDDter_basal$diabetes0), BBDDter_basal$ter_F)) 
(prop.table(table(as.factor(BBDDter_basal$diabetes0), BBDDter_basal$ter_F),2)*100) 

(table(as.factor(BBDDter_basal$dyslip0), BBDDter_basal$ter_F)) 
(prop.table(table(as.factor(BBDDter_basal$dyslip0), BBDDter_basal$ter_F),2)*100) 

(table(as.factor(BBDDter_basal$hyperten0), BBDDter_basal$ter_F)) 
(prop.table(table(as.factor(BBDDter_basal$hyperten0), BBDDter_basal$ter_F),2)*100) 

(table(as.factor(BBDDter_basal$fam_history), BBDDter_basal$ter_F)) 
(prop.table(table(as.factor(BBDDter_basal$fam_history), BBDDter_basal$ter_F),2)*100) 

(table(as.factor(BBDDter_basal$smoking0), BBDDter_basal$ter_F)) 
(prop.table(table(as.factor(BBDDter_basal$smoking0), BBDDter_basal$ter_F),2)*100) 

(table(as.factor(BBDDter_basal$sex), BBDDter_basal$ter_RM)) 
(prop.table(table(as.factor(BBDDter_basal$sex), BBDDter_basal$ter_RM),2)*100) 

(table(as.factor(BBDDter_basal$diabetes0), BBDDter_basal$ter_RM)) 
(prop.table(table(as.factor(BBDDter_basal$diabetes0), BBDDter_basal$ter_RM),2)*100) 

(table(as.factor(BBDDter_basal$dyslip0), BBDDter_basal$ter_RM)) 
(prop.table(table(as.factor(BBDDter_basal$dyslip0), BBDDter_basal$ter_RM),2)*100) 

(table(as.factor(BBDDter_basal$hyperten0), BBDDter_basal$ter_RM)) 
(prop.table(table(as.factor(BBDDter_basal$hyperten0), BBDDter_basal$ter_RM),2)*100) 

(table(as.factor(BBDDter_basal$fam_history), BBDDter_basal$ter_RM)) 
(prop.table(table(as.factor(BBDDter_basal$fam_history), BBDDter_basal$ter_RM),2)*100) 

(table(as.factor(BBDDter_basal$smoking0), BBDDter_basal$ter_RM)) 
(prop.table(table(as.factor(BBDDter_basal$smoking0), BBDDter_basal$ter_RM),2)*100) 

(table(as.factor(BBDDter_basal$sex), BBDDter_basal$ter_PRM)) 
(prop.table(table(as.factor(BBDDter_basal$sex), BBDDter_basal$ter_PRM),2)*100) 

(table(as.factor(BBDDter_basal$diabetes0), BBDDter_basal$ter_PRM)) 
(prop.table(table(as.factor(BBDDter_basal$diabetes0), BBDDter_basal$ter_PRM),2)*100) 

(table(as.factor(BBDDter_basal$dyslip0), BBDDter_basal$ter_PRM)) 
(prop.table(table(as.factor(BBDDter_basal$dyslip0), BBDDter_basal$ter_PRM),2)*100) 

(table(as.factor(BBDDter_basal$hyperten0), BBDDter_basal$ter_PRM)) 
(prop.table(table(as.factor(BBDDter_basal$hyperten0), BBDDter_basal$ter_PRM),2)*100) 

(table(as.factor(BBDDter_basal$fam_history), BBDDter_basal$ter_PRM)) 
(prop.table(table(as.factor(BBDDter_basal$fam_history), BBDDter_basal$ter_PRM),2)*100) 

(table(as.factor(BBDDter_basal$smoking0), BBDDter_basal$ter_PRM)) 
(prop.table(table(as.factor(BBDDter_basal$smoking0), BBDDter_basal$ter_PRM),2)*100) 

### Tablas descriptivas BBDD T2D.

## BBDD_T2D:

table(BBDD_T2D$cens)

### Baseline:

BBDD_T2D1 = data.frame(cbind(BBDD_T2D$cens, BBDD_T2D$age, BBDD_T2D$bmi, BBDD_T2D$cint1, BBDD_T2D$carnicos.x, BBDD_T2D$pescados.x,
                             BBDD_T2D$RM.x, BBDD_T2D$PRM.x, BBDD_T2D$verdutot, BBDD_T2D$frutatot, BBDD_T2D$legumbre, BBDD_T2D$grupocer,
                             BBDD_T2D$lacteos, BBDD_T2D$olivatot, BBDD_T2D$fsecos, BBDD_T2D$vino, BBDD_T2D$hc, BBDD_T2D$prot,
                             BBDD_T2D$gratot, BBDD_T2D$mo, BBDD_T2D$sa, BBDD_T2D$po, BBDD_T2D$alcoholg, BBDD_T2D$energiat, BBDD_T2D$fibra))


colnames(BBDD_T2D1) = c("cens", "age", "bmi0", "waist_1", "carnicos", "pescados",
                        "RM", "PRM", "verdutot", "frutatot", "legumbre", "grupocer",
                        "lacteos", "olivatot", "fsecos", "vino", "hc", "prot",
                        "gratot", "mo", "sa", "po", "alcoholg", "energiat", "fibra")

medianas = c()
iqrs = c()

for (i in 2:25) {
  medianas[[i]] = tapply(BBDD_T2D1[[i]], BBDD_T2D1[[1]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD_T2D1[[i]], BBDD_T2D1[[1]], IQR, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=2, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=2, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(cbind(medianas, iqrs))
iqrs$names = colnames(BBDD_T2D1)[2:25]

colnames(iqrs) = c("contro", "caso", "IQR1con", "IQR2caso", "names")

export(iqrs, "alimentacion_T2Dbasal.xlsx")

(table(as.factor(BBDD_T2D$sex), BBDD_T2D$cens)) 
(prop.table(table(as.factor(BBDD_T2D$sex), BBDD_T2D$cens),2)*100) 

(table(as.factor(BBDD_T2D$diabetes0), BBDD_T2D$cens)) 
(prop.table(table(as.factor(BBDD_T2D$diabetes0), BBDD_T2D$cens),2)*100) 

(table(as.factor(BBDD_T2D$dyslip0), BBDD_T2D$cens)) 
(prop.table(table(as.factor(BBDD_T2D$dyslip0), BBDD_T2D$cens),2)*100) 

(table(as.factor(BBDD_T2D$hyperten0), BBDD_T2D$cens)) 
(prop.table(table(as.factor(BBDD_T2D$hyperten0), BBDD_T2D$cens),2)*100) 

(table(as.factor(BBDD_T2D$fam_history), BBDD_T2D$cens)) 
(prop.table(table(as.factor(BBDD_T2D$fam_history), BBDD_T2D$cens),2)*100) 

(table(as.factor(BBDD_T2D$smoking0), BBDD_T2D$cens)) 
(prop.table(table(as.factor(BBDD_T2D$smoking0), BBDD_T2D$cens),2)*100) 

### 1 year:

table(BBDD_T2D1a$cens)

BBDD_T2D1a1 = data.frame(cbind(BBDD_T2D1a$cens, BBDD_T2D1a$age, BBDD_T2D1a$bmi, BBDD_T2D1a$cint1, BBDD_T2D1a$carnicos3, BBDD_T2D1a$pescados3,
                             BBDD_T2D1a$RM3, BBDD_T2D1a$PRM3, BBDD_T2D1a$verdutot3, BBDD_T2D1a$frutatot3, BBDD_T2D1a$legumbre3, BBDD_T2D1a$grupocer3,
                             BBDD_T2D1a$lacteos3, BBDD_T2D1a$olivatot3, BBDD_T2D1a$fsecos3, BBDD_T2D1a$vino3, BBDD_T2D1a$HC3, BBDD_T2D1a$PROT3,
                             BBDD_T2D1a$gratot3, BBDD_T2D1a$MO3, BBDD_T2D1a$SA3, BBDD_T2D1a$PO3, BBDD_T2D1a$alcoholg3, BBDD_T2D1a$energiat3, BBDD_T2D1a$fibra3))


colnames(BBDD_T2D1a1) = c("cens", "age", "bmi0", "waist_1", "carnicos", "pescados",
                        "RM", "PRM", "verdutot", "frutatot", "legumbre", "grupocer",
                        "lacteos", "olivatot", "fsecos", "vino", "hc", "prot",
                        "gratot", "mo", "sa", "po", "alcoholg", "energiat", "fibra")

medianas = c()
iqrs = c()

for (i in 2:25) {
  medianas[[i]] = tapply(BBDD_T2D1a1[[i]], BBDD_T2D1a1[[1]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD_T2D1a1[[i]], BBDD_T2D1a1[[1]], IQR, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=2, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=2, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(cbind(medianas, iqrs))
iqrs$names = colnames(BBDD_T2D1a1)[2:25]

colnames(iqrs) = c("contro", "caso", "IQR1con", "IQR2caso", "names")

export(iqrs, "alimentacion_T2D1a.xlsx")

(table(as.factor(BBDD_T2D1a$sex), BBDD_T2D1a$cens)) 
(prop.table(table(as.factor(BBDD_T2D1a$sex), BBDD_T2D1a$cens),2)*100) 

(table(as.factor(BBDD_T2D1a$hipercol0), BBDD_T2D1a$cens)) 
(prop.table(table(as.factor(BBDD_T2D1a$hipercol0), BBDD_T2D1a$cens),2)*100) 

(table(as.factor(BBDD_T2D1a$hta0), BBDD_T2D1a$cens)) 
(prop.table(table(as.factor(BBDD_T2D1a$hta0), BBDD_T2D1a$cens),2)*100) 

(table(as.factor(BBDD_T2D1a$fam_history), BBDD_T2D1a$cens)) 
(prop.table(table(as.factor(BBDD_T2D1a$fam_history), BBDD_T2D1a$cens),2)*100) 

(table(as.factor(BBDD_T2D1a$smoking0), BBDD_T2D1a$cens)) 
(prop.table(table(as.factor(BBDD_T2D1a$smoking0), BBDD_T2D1a$cens),2)*100) 

# Fase 8: diagramas de Venn ####

library(limma)
library(VennDiagram)

Resultados <- read_excel("E:/ARTICULOS/Autor/PREDIMED/ART huella met - carne (estancia)/Art/Resultados.xlsx", 
                         sheet = "Full2")

set_TM = (O1$Metabolites)
set_Fish = (O2$Metabolites)
set_RM = (O3$Metabolites)
set_PRM = (O4$Metabolites)

venn.diagram(x = list(set_TM,set_Fish,set_RM,set_PRM),
             category.names = c("TM", "Fish", "RM", "PRM"),
             filename = 'venn.png',
             output = T, 
             
             # Output features
             imagetype="png" , 
             resolution = 300,
             
             # Circles
             lwd = 7,
             # Numbers
             cex = 1.5,
             fontface = "bold",
             fontfamily = "sans",
             
             # Set names
             cat.cex = 0
)

set_TM = subset(O1$Metabolites, O1$TM >0)
set_Fish = subset(O2$Metabolites, O2$Fish >0)
set_RM = subset(O3$Metabolites, O3$RM >0)
set_PRM = subset(O4$Metabolites, O4$PRM >0)

venn.diagram(x = list(set_TM,set_Fish,set_RM,set_PRM),
             category.names = c("TM", "Fish", "RM", "PRM"),
             filename = 'venn1.png',
             output = T, 
             
             # Output features
             imagetype="png" , 
             resolution = 300,
             
             # Circles
             lwd = 7,
             # Numbers
             cex = 1.5,
             fontface = "bold",
             fontfamily = "sans",
             
             # Set names
             cat.cex = 0
)

set_TM = subset(O1$Metabolites, O1$TM <0)
set_Fish = subset(O2$Metabolites, O2$Fish <0)
set_RM = subset(O3$Metabolites, O3$RM <0)
set_PRM = subset(O4$Metabolites, O4$PRM <0)

venn.diagram(x = list(set_TM,set_Fish,set_RM,set_PRM),
             category.names = c("TM", "Fish", "RM", "PRM"),
             filename = 'venn2.png',
             output = T, 
             
             # Output features
             imagetype="png" , 
             resolution = 300,
             
             # Circles
             lwd = 7,
             # Numbers
             cex = 1.5,
             fontface = "bold",
             fontfamily = "sans",
             
             # Set names
             cat.cex = 0
)

rm(medianas, iqrs, BBDDter_basal, i, BBDD, BBDD_T2D1, BBDD_T2D1a1, Alimentos, set_TM, set_RM, set_PRM, set_Fish)

# Fase 9: Resto de tablas del suplemental file: ####

# Tabla de espeficidad (añadida como matriz de correlaciones):

library(corrplot)

mat1 = (Scores_base[2:9])
colnames(mat1) = c("TM", "Fish", "RM", "PRM", "TM ms", "F ms", "RM ms", "PRM ms")

mat = cor(mat1)

corrplot(mat, method = 'color', type = "upper", addCoef.col = "black")

rm(mat, mat1)

# Analisis secundarios Cox:

# Estratificiación por sexos:

# Basal:

BBDD_T2D_h = subset(BBDD_T2D, sex == "hombre")
table(BBDD_T2D_h$cens)

m1.1.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_h)

summary(m1.1.TM)

m1.1.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                    (ac_oliva) + (huevos) + (legumbre) + carnicos.x +  
                    (fsecos), weights = w, data = BBDD_T2D_h)

summary(m1.1.Fish)

m1.1.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_h)

summary(m1.1.RM)

m1.1.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                   (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                   (fsecos), weights = w, data = BBDD_T2D_h)

summary(m1.1.PRM)

BBDD_T2D_m = subset(BBDD_T2D, sex == "mujer")
table(BBDD_T2D_m$cens)

m1.1.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_m)

summary(m1.1.TM)

m1.1.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                    (ac_oliva) + (huevos) + (legumbre) + carnicos.x +   
                    (fsecos), weights = w, data = BBDD_T2D_m)

summary(m1.1.Fish)

m1.1.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_m)

summary(m1.1.RM)

m1.1.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                   (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                   (fsecos), weights = w, data = BBDD_T2D_m)

summary(m1.1.PRM)

# 1 year

BBDD_T2D_h_1a = subset(BBDD_T2D1a, sex == "hombre")
table(BBDD_T2D_h_1a$cens)

m1.2.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age + as.factor(interv_g)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_h_1a)

summary(m1.2.TM)

m1.2.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age + as.factor(interv_g)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                    (ac_oliva3) + (huevos3) + (legumbre3) + carnicos3 +   
                    (fsecos3), weights = w, data = BBDD_T2D_h_1a)

summary(m1.2.Fish)

m1.2.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age + as.factor(interv_g)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_h_1a)

summary(m1.2.RM)

m1.2.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(interv_g)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat + (frutatot) + (verdutot) + (grupocer) + 
                   (ac_oliva) + (huevos) + (legumbre) + PRM3 +   
                   (fsecos), weights = w, data = BBDD_T2D_h_1a)

summary(m1.2.PRM)

BBDD_T2D_m_1a = subset(BBDD_T2D1a, sex == "mujer")
table(BBDD_T2D_m_1a$cens)

m1.2.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age + as.factor(interv_g)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_m_1a)

summary(m1.2.TM)

m1.2.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age + as.factor(interv_g)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                    (ac_oliva3) + (huevos3) + (legumbre3) + carnicos3 +   
                    (fsecos3), weights = w, data = BBDD_T2D_m_1a)

summary(m1.2.Fish)

m1.2.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age + as.factor(interv_g)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_m_1a)

summary(m1.2.RM)

m1.2.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age + as.factor(interv_g)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                   (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                   (fsecos3), weights = w, data = BBDD_T2D_m_1a)

summary(m1.2.PRM)

rm(m1.1.RM, m1.1.PRM, m1.1.TM, m1.1.Fish, 
   m1.2.RM, m1.2.PRM, m1.2.TM, m1.2.Fish,
   BBDD_T2D_h, BBDD_T2D_m, BBDD_T2D_h_1a, BBDD_T2D_m_1a)

# Analisis secundario por grupo de intervención:

BBDD_T2D_EVOO = subset(BBDD_T2D, interv_g == "aceite de oliva")
BBDD_T2D_NUTS = subset(BBDD_T2D, interv_g == "frutos secos")
BBDD_T2D_DBG = subset(BBDD_T2D, interv_g == "dieta baja en grasa")

# Basal:

table(BBDD_T2D_EVOO$cens)
table(BBDD_T2D_NUTS$cens)
table(BBDD_T2D_DBG$cens)

mean(BBDD_T2D_EVOO$carnicos.x)
sd(BBDD_T2D_EVOO$carnicos.x)
mean(BBDD_T2D_EVOO$pescados.x)
sd(BBDD_T2D_EVOO$pescados.x)
mean(BBDD_T2D_EVOO$RM)
sd(BBDD_T2D_EVOO$RM)
mean(BBDD_T2D_EVOO$PRM)
sd(BBDD_T2D_EVOO$PRM)

m1.3.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_EVOO)

summary(m1.3.TM)

m1.3.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                    (ac_oliva) + (huevos) + (legumbre) + carnicos.x +   
                    (fsecos), weights = w, data = BBDD_T2D_EVOO)

summary(m1.3.Fish)

m1.3.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_EVOO)

summary(m1.3.RM)

m1.3.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                   (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                   (fsecos), weights = w, data = BBDD_T2D_EVOO)

summary(m1.3.PRM)

mean(BBDD_T2D_NUTS$carnicos.x)
sd(BBDD_T2D_NUTS$carnicos.x)
mean(BBDD_T2D_NUTS$pescados.x)
sd(BBDD_T2D_NUTS$pescados.x)
mean(BBDD_T2D_NUTS$RM)
sd(BBDD_T2D_NUTS$RM)
mean(BBDD_T2D_NUTS$PRM)
sd(BBDD_T2D_NUTS$PRM)

m1.3.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_NUTS)

summary(m1.3.TM)

m1.3.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                    (ac_oliva) + (huevos) + (legumbre) + carnicos.x +   
                    (fsecos), weights = w, data = BBDD_T2D_NUTS)

summary(m1.3.Fish)

m1.3.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_NUTS)

summary(m1.3.RM)

m1.3.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                   (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                   (fsecos), weights = w, data = BBDD_T2D_NUTS)

summary(m1.3.PRM)

mean(BBDD_T2D_DBG$carnicos.x)
sd(BBDD_T2D_DBG$carnicos.x)
mean(BBDD_T2D_DBG$pescados.x)
sd(BBDD_T2D_DBG$pescados.x)
mean(BBDD_T2D_DBG$RM)
sd(BBDD_T2D_DBG$RM)
mean(BBDD_T2D_DBG$PRM)
sd(BBDD_T2D_DBG$PRM)

m1.3.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_DBG)

summary(m1.3.TM)

m1.3.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                    (ac_oliva) + (huevos) + (legumbre) + carnicos.x +   
                    (fsecos), weights = w, data = BBDD_T2D_DBG)

summary(m1.3.Fish)

m1.3.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                  (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                  (fsecos), weights = w, data = BBDD_T2D_DBG)

summary(m1.3.RM)

m1.3.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat + (frutatot) + (verdutot) + (grupocer) + lacteos +
                   (ac_oliva) + (huevos) + (legumbre) + pescados.x +   
                   (fsecos), weights = w, data = BBDD_T2D_DBG)

summary(m1.3.PRM)

# 1 year:
BBDD_T2D_EVOO_1a = subset(BBDD_T2D1a, interv_g == "aceite de oliva")
BBDD_T2D_NUTS_1a = subset(BBDD_T2D1a, interv_g == "frutos secos")
BBDD_T2D_DBG_1a = subset(BBDD_T2D1a, interv_g == "dieta baja en grasa")

table(BBDD_T2D_EVOO_1a$cens)
table(BBDD_T2D_NUTS_1a$cens)
table(BBDD_T2D_DBG_1a$cens)

mean(BBDD_T2D_EVOO_1a$carnicos3, na.rm=T)
sd(BBDD_T2D_EVOO_1a$carnicos3, na.rm=T)
mean(BBDD_T2D_EVOO_1a$pescados3, na.rm=T)
sd(BBDD_T2D_EVOO_1a$pescados3, na.rm=T)
mean(BBDD_T2D_EVOO_1a$RM3, na.rm=T)
sd(BBDD_T2D_EVOO_1a$RM3, na.rm=T)
mean(BBDD_T2D_EVOO_1a$PRM3, na.rm=T)
sd(BBDD_T2D_EVOO_1a$PRM3, na.rm=T)

m1.3.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_EVOO_1a)

summary(m1.3.TM)

m1.3.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                    (ac_oliva3) + (huevos3) + (legumbre3) + carnicos3 +   
                    (fsecos3), weights = w, data = BBDD_T2D_EVOO_1a)

summary(m1.3.Fish)

m1.3.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_EVOO_1a)

summary(m1.3.RM)

m1.3.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                   (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                   (fsecos3), weights = w, data = BBDD_T2D_EVOO_1a)

summary(m1.3.PRM)

mean(BBDD_T2D_NUTS_1a$carnicos3, na.rm=T)
sd(BBDD_T2D_NUTS_1a$carnicos3, na.rm=T)
mean(BBDD_T2D_NUTS_1a$pescados3, na.rm=T)
sd(BBDD_T2D_NUTS_1a$pescados3, na.rm=T)
mean(BBDD_T2D_NUTS_1a$RM3, na.rm=T)
sd(BBDD_T2D_NUTS_1a$RM3, na.rm=T)
mean(BBDD_T2D_NUTS_1a$PRM3, na.rm=T)
sd(BBDD_T2D_NUTS_1a$PRM3, na.rm=T)

m1.3.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_NUTS_1a)

summary(m1.3.TM)

m1.3.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                    (ac_oliva3) + (huevos3) + (legumbre3) + carnicos3 +   
                    (fsecos3), weights = w, data = BBDD_T2D_NUTS_1a)

summary(m1.3.Fish)

m1.3.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_NUTS_1a)

summary(m1.3.RM)

m1.3.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                   (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                   (fsecos3), weights = w, data = BBDD_T2D_NUTS_1a)

summary(m1.3.PRM)

mean(BBDD_T2D_DBG_1a$carnicos3, na.rm=T)
sd(BBDD_T2D_DBG_1a$carnicos3, na.rm=T)
mean(BBDD_T2D_DBG_1a$pescados3, na.rm=T)
sd(BBDD_T2D_DBG_1a$pescados3, na.rm=T)
mean(BBDD_T2D_DBG_1a$RM3, na.rm=T)
sd(BBDD_T2D_DBG_1a$RM3, na.rm=T)
mean(BBDD_T2D_DBG_1a$PRM3, na.rm=T)
sd(BBDD_T2D_DBG_1a$PRM3, na.rm=T)

m1.3.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_DBG_1a)

summary(m1.3.TM)

m1.3.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                    (ac_oliva3) + (huevos3) + (legumbre3) + carnicos3 +   
                    (fsecos3), weights = w, data = BBDD_T2D_DBG_1a)

summary(m1.3.Fish)

m1.3.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_DBG_1a)

summary(m1.3.RM)

m1.3.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                   (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                   (fsecos3), weights = w, data = BBDD_T2D_DBG_1a)

summary(m1.3.PRM)

rm(BBDD_T2D_DBG, BBDD_T2D_DBG_1a, BBDD_T2D_EVOO, BBDD_T2D_EVOO_1a, BBDD_T2D_NUTS, BBDD_T2D_NUTS_1a, 
   m1.3.Fish, m1.3.RM, m1.3.TM, m1.3.PRM)

# Analisis de Cox MD:

# Baseline:
BBDD_T2D_MD = subset(BBDD_T2D, interv_g == c("aceite de oliva"))
BBDD_T2D_MD1 = subset(BBDD_T2D, interv_g == c("frutos secos"))
BBDD_T2D_MD = rbind(BBDD_T2D_MD, BBDD_T2D_MD1)
rm(BBDD_T2D_MD1)
table(BBDD_T2D_MD$cens)

m1.4.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_MD)

summary(m1.4.TM)

m1.4.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                    (ac_oliva3) + (huevos3) + (legumbre3) + carnicos3 +   
                    (fsecos3), weights = w, data = BBDD_T2D_MD)

summary(m1.4.Fish)

m1.4.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_MD)

summary(m1.4.RM)

m1.4.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                   (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                   (fsecos3), weights = w, data = BBDD_T2D_MD)

summary(m1.4.PRM)


# 1 year
BBDD_T2D_MD = subset(BBDD_T2D1a, interv_g == c("aceite de oliva"))
BBDD_T2D_MD1 = subset(BBDD_T2D1a, interv_g == c("frutos secos"))
BBDD_T2D_MD = rbind(BBDD_T2D_MD, BBDD_T2D_MD1)
rm(BBDD_T2D_MD1)
table(BBDD_T2D_MD$cens)

m1.4.TM = coxph(Surv(survtime, cens) ~ m.TM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_MD)

summary(m1.4.TM)

m1.4.Fish = coxph(Surv(survtime, cens) ~ m.Fish + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                    (ac_oliva3) + (huevos3) + (legumbre3) + carnicos3 +   
                    (fsecos3), weights = w, data = BBDD_T2D_MD)

summary(m1.4.Fish)

m1.4.RM = coxph(Surv(survtime, cens) ~ m.RM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                  (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                  (fsecos3), weights = w, data = BBDD_T2D_MD)

summary(m1.4.RM)

m1.4.PRM = coxph(Surv(survtime, cens) ~ m.PRM + ps1.y + ps2.y + age+ as.factor(sex)+ as.factor(nodo) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   energiat3 + (frutatot3) + (verdutot3) + (grupocer3) + lacteos3 +
                   (ac_oliva3) + (huevos3) + (legumbre3) + pescados3 +   
                   (fsecos3), weights = w, data = BBDD_T2D_MD)

summary(m1.4.PRM)

# Fase 10: Peticiones de los revisores MNFR: ####
# Características de la población al año por terciles de ingesta:

BBDD_1a <- read_csv("BBDD_1a.csv")

BBDD_1a$ter_RM = cut2(BBDD_1a$RM, g = 3)
table(BBDD_1a$ter_RM)
tapply(BBDD_1a$RM, BBDD_1a$ter_RM, mean, na.rm = T)

BBDD_1a$ter_PRM = cut2(BBDD_1a$PRM, g = 3)
table(BBDD_1a$ter_PRM)
tapply(BBDD_1a$PRM, BBDD_1a$ter_PRM, mean, na.rm = T)

BBDD_1a$ter_F = cut2(BBDD_1a$pescados3, g = 3)
table(BBDD_1a$ter_F)
tapply(BBDD_1a$pescados3, BBDD_1a$ter_F, mean, na.rm = T)

BBDD = data.frame(cbind(BBDD_1a$ter_F, BBDD_1a$age, BBDD_1a$bmi0, BBDD_1a$waist_1, BBDD_1a$carnicos3, BBDD_1a$pescados3,
                        BBDD_1a$RM, BBDD_1a$PRM, BBDD_1a$verdutot3, BBDD_1a$frutatot3, BBDD_1a$legumbre3, BBDD_1a$grupocer3,
                        BBDD_1a$lacteos3, BBDD_1a$olivatot3, BBDD_1a$fsecos3, BBDD_1a$vino3, BBDD_1a$HC3, BBDD_1a$PROT3,
                        BBDD_1a$gratot3, BBDD_1a$MO3, BBDD_1a$SA3, BBDD_1a$PO3, BBDD_1a$alcoholg3, BBDD_1a$energiat3, BBDD_1a$fibra3))

colnames(BBDD) = c("ter_F", "age", "bmi0", "waist_1", "carnicos", "pescados",
                   "RM", "PRM", "verdutot", "frutatot", "legumbre", "grupocer",
                   "lacteos", "olivatot", "fsecos", "vino", "hc", "prot",
                   "gratot", "mo", "sa", "po", "alcoholg", "energiat", "fibra")

medianas = c()
iqrs = c()

for (i in 2:25) {
  medianas[[i]] = tapply(BBDD[[i]], BBDD[[1]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD[[i]], BBDD[[1]], IQR, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(cbind(medianas, iqrs))
iqrs$names = colnames(BBDD)[2:25]

colnames(iqrs) = c("T1", "T2", "T3", "IQR1", "IQR2", "IQR3", "names")

export(iqrs, "alimentacion_terfish_1a.xlsx")

BBDD = data.frame(cbind(BBDD_1a$ter_RM, BBDD_1a$age, BBDD_1a$bmi0, BBDD_1a$waist_1, BBDD_1a$carnicos3, BBDD_1a$pescados3,
                        BBDD_1a$RM, BBDD_1a$PRM, BBDD_1a$verdutot3, BBDD_1a$frutatot3, BBDD_1a$legumbre3, BBDD_1a$grupocer3,
                        BBDD_1a$lacteos3, BBDD_1a$olivatot3, BBDD_1a$fsecos3, BBDD_1a$vino3, BBDD_1a$HC3, BBDD_1a$PROT3,
                        BBDD_1a$gratot3, BBDD_1a$MO3, BBDD_1a$SA3, BBDD_1a$PO3, BBDD_1a$alcoholg3, BBDD_1a$energiat3, BBDD_1a$fibra3))

colnames(BBDD) = c("ter_RM", "age", "bmi0", "waist_1", "carnicos", "pescados",
                   "RM", "PRM", "verdutot", "frutatot", "legumbre", "grupocer",
                   "lacteos", "olivatot", "fsecos", "vino", "hc", "prot",
                   "gratot", "mo", "sa", "po", "alcoholg", "energiat", "fibra")

medianas = c()
iqrs = c()

for (i in 2:25) {
  medianas[[i]] = tapply(BBDD[[i]], BBDD[[1]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD[[i]], BBDD[[1]], IQR, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(cbind(medianas, iqrs))
iqrs$names = colnames(BBDD)[2:25]

colnames(iqrs) = c("T1", "T2", "T3", "IQR1", "IQR2", "IQR3", "names")

export(iqrs, "alimentacion_terRM_1a.xlsx")

BBDD = data.frame(cbind(BBDD_1a$ter_PRM, BBDD_1a$age, BBDD_1a$bmi0, BBDD_1a$waist_1, BBDD_1a$carnicos3, BBDD_1a$pescados3,
                        BBDD_1a$RM, BBDD_1a$PRM, BBDD_1a$verdutot3, BBDD_1a$frutatot3, BBDD_1a$legumbre3, BBDD_1a$grupocer3,
                        BBDD_1a$lacteos3, BBDD_1a$olivatot3, BBDD_1a$fsecos3, BBDD_1a$vino3, BBDD_1a$HC3, BBDD_1a$PROT3,
                        BBDD_1a$gratot3, BBDD_1a$MO3, BBDD_1a$SA3, BBDD_1a$PO3, BBDD_1a$alcoholg3, BBDD_1a$energiat3, BBDD_1a$fibra3))

colnames(BBDD) = c("ter_PRM", "age", "bmi0", "waist_1", "carnicos", "pescados",
                   "RM", "PRM", "verdutot", "frutatot", "legumbre", "grupocer",
                   "lacteos", "olivatot", "fsecos", "vino", "hc", "prot",
                   "gratot", "mo", "sa", "po", "alcoholg", "energiat", "fibra")

medianas = c()
iqrs = c()

for (i in 2:25) {
  medianas[[i]] = tapply(BBDD[[i]], BBDD[[1]], median, na.rm = T)
  iqrs[[i]] = tapply(BBDD[[i]], BBDD[[1]], IQR, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)
iqrs = data.frame(cbind(medianas, iqrs))
iqrs$names = colnames(BBDD)[2:25]

colnames(iqrs) = c("T1", "T2", "T3", "IQR1", "IQR2", "IQR3", "names")

export(iqrs, "alimentacion_terPRM_1a.xlsx")

(table(as.factor(BBDD_1a$sex), BBDD_1a$ter_F)) 
(prop.table(table(as.factor(BBDD_1a$sex), BBDD_1a$ter_F),2)*100) 

(table(as.factor(BBDD_1a$diabetes0), BBDD_1a$ter_F)) 
(prop.table(table(as.factor(BBDD_1a$diabetes0), BBDD_1a$ter_F),2)*100) 

(table(as.factor(BBDD_1a$dyslip0), BBDD_1a$ter_F)) 
(prop.table(table(as.factor(BBDD_1a$dyslip0), BBDD_1a$ter_F),2)*100) 

(table(as.factor(BBDD_1a$hyperten0), BBDD_1a$ter_F)) 
(prop.table(table(as.factor(BBDD_1a$hyperten0), BBDD_1a$ter_F),2)*100) 

(table(as.factor(BBDD_1a$fam_history), BBDD_1a$ter_F)) 
(prop.table(table(as.factor(BBDD_1a$fam_history), BBDD_1a$ter_F),2)*100) 

(table(as.factor(BBDD_1a$smoking0), BBDD_1a$ter_F)) 
(prop.table(table(as.factor(BBDD_1a$smoking0), BBDD_1a$ter_F),2)*100) 

(table(as.factor(BBDD_1a$sex), BBDD_1a$ter_RM)) 
(prop.table(table(as.factor(BBDD_1a$sex), BBDD_1a$ter_RM),2)*100) 

(table(as.factor(BBDD_1a$diabetes0), BBDD_1a$ter_RM)) 
(prop.table(table(as.factor(BBDD_1a$diabetes0), BBDD_1a$ter_RM),2)*100) 

(table(as.factor(BBDD_1a$dyslip0), BBDD_1a$ter_RM)) 
(prop.table(table(as.factor(BBDD_1a$dyslip0), BBDD_1a$ter_RM),2)*100) 

(table(as.factor(BBDD_1a$hyperten0), BBDD_1a$ter_RM)) 
(prop.table(table(as.factor(BBDD_1a$hyperten0), BBDD_1a$ter_RM),2)*100) 

(table(as.factor(BBDD_1a$fam_history), BBDD_1a$ter_RM)) 
(prop.table(table(as.factor(BBDD_1a$fam_history), BBDD_1a$ter_RM),2)*100) 

(table(as.factor(BBDD_1a$smoking0), BBDD_1a$ter_RM)) 
(prop.table(table(as.factor(BBDD_1a$smoking0), BBDD_1a$ter_RM),2)*100) 

(table(as.factor(BBDD_1a$sex), BBDD_1a$ter_PRM)) 
(prop.table(table(as.factor(BBDD_1a$sex), BBDD_1a$ter_PRM),2)*100) 

(table(as.factor(BBDD_1a$diabetes0), BBDD_1a$ter_PRM)) 
(prop.table(table(as.factor(BBDD_1a$diabetes0), BBDD_1a$ter_PRM),2)*100) 

(table(as.factor(BBDD_1a$dyslip0), BBDD_1a$ter_PRM)) 
(prop.table(table(as.factor(BBDD_1a$dyslip0), BBDD_1a$ter_PRM),2)*100) 

(table(as.factor(BBDD_1a$hyperten0), BBDD_1a$ter_PRM)) 
(prop.table(table(as.factor(BBDD_1a$hyperten0), BBDD_1a$ter_PRM),2)*100) 

(table(as.factor(BBDD_1a$fam_history), BBDD_1a$ter_PRM)) 
(prop.table(table(as.factor(BBDD_1a$fam_history), BBDD_1a$ter_PRM),2)*100) 

(table(as.factor(BBDD_1a$smoking0), BBDD_1a$ter_PRM)) 
(prop.table(table(as.factor(BBDD_1a$smoking0), BBDD_1a$ter_PRM),2)*100) 

# Revisor 2, pregunta 2: analisis de sensibilidad sin casos: ####

NEW_Combined_metabolites_phenotypes_230218 <- read_csv("/Volumes/JES GAVI/ARTICULOS/BBDD/PREDIMED/NEW_Combined_metabolites_phenotypes_230218.csv")
phenotypes_predimed_20190903 <- read_csv("/Volumes/JES GAVI/ARTICULOS/BBDD/PREDIMED/phenotypes_predimed_20190903.csv")
FFQ_PREDIMED_2012 <- read_sav("/Volumes/JES GAVI/ARTICULOS/BBDD/PREDIMED/FFQ_PREDIMED_2012.SAV")

###Fusion de las BBDD.
BBDD_rev = merge(phenotypes_predimed_20190903, FFQ_PREDIMED_2012, by = "id")

BBDD_basal = BBDD_basal[1]

BBDD_rev = merge(BBDD_basal, BBDD_rev, by = "id")

##Variables: carnicos, pescados, carnes blancas, carnes rojas, carnes procesadas.
###Carnicos y pescados ya existe. Genero las variables de carnes blancas, rojas y procesadas segun el articulo: 10.1016/j.clnu.2016.03.017
# Carne roja: cerdo, ternera y cordero
# Carne procesada: despojos, jamón, embutidos, paté, hamburguesas y tocino.

BBDD_rev$RM = BBDD_rev$c_ternera + BBDD_rev$c_cerdo + BBDD_rev$c_cordero
summary(BBDD_rev$RM)
BBDD_rev$PRM = BBDD_rev$higad + BBDD_rev$visceras + BBDD_rev$j_serrano + BBDD_rev$j_cocido + BBDD_rev$embutidos + BBDD_rev$pates + BBDD_rev$hamburguesa + BBDD_rev$bacon
summary(BBDD_rev$PRM)
summary(BBDD_rev$pescados)
summary(BBDD_rev$carnicos)

colnames(BBDD_rev)
table(BBDD_rev$cvd_event)
table(BBDD_rev$t2d_event)

BBDD_rev = subset(BBDD_rev, cvd_event == "No")

BBDD_rev_id = BBDD_rev[1]

BBDD_rev_met = merge(BBDD_rev_id, BBDDmet, by = "id")

#### Carnicos:

#### Estimacion del valor de alpha:

#Split t-v

TM = data.frame(cbind(BBDD_rev_met[2:386], BBDD_rev_met[387]))

set.seed(001)

tic("Alpha")

rows <- sample(nrow(TM))
TM <- TM [rows,]
folds <- cut(seq(1,nrow(TM)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = TM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = TM [training, ] # testeas en el 10% (que es 1 fold)
  
  #modelo
  
  cv[[i]] = train(carnicos ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(carnicos~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$carnicos #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$carnicos), #Rsquare = R2(predictions[[i]], test.data[[i]]$carnicos), 
                             Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$carnicos, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$carnicos)$estimate)
}

toc()

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.6

tic("WHOLE")

set.seed(002)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(TM)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$carnicos

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", alpha=.5)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

#### COMPENDIA OF METABOLITES
#### FULL DATASET

#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(TM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(TM))
colnames(abc_set)[colnames(abc_set)=="colnames.TM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

export(new_mean_values_metabo_sorted, "analisis_sensi_TM.xlsx")

# To run training-testing

# en la cv.glmnet tengo puesto nfolds=10 para simplificar, se podría poner igual al valor de sujetos para que
# hiciera leave-one-out CV

# Si quieres puedes probar qué obtenemos tirándolo de dos formas:
# 1. Loop 10-fold cross validation con 10-CV dentro de cada iteración (código de abajo)
# 2. Solo un loop usando el whole dataset, y poniendo en nfolds=dim(TM)[1] (obviamente, tendrás que cambiar código)

rows <- sample(nrow(TM))
TM2 <- TM[rows, ]
TM <- TM2
folds <- cut(seq(1,nrow(TM)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

tic("AUC: Training-Testing")

#### EXECUTION TRAINING-TESTING

#Variables
set_train<-list()
set_test<-list()
X.train_saved<-list()
Y.train_saved<-list()
X.test_saved<-list()
Y.test_saved<-list()    
fit<-list()
cvfit<-list()
roc_results<-list()
variables_values<-list()
standard_roc<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
reg_sum<-list()
reg_sum_no_intercept<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #sample <- sample.int(n = nrow(TM), size = floor(.90*nrow(TM)), replace = F)
  #set_train[[i]] <- TM[sample, ]
  #set_test[[i]]  <- TM[-sample, ]
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- TM[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- TM[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$carnicos
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$carnicos
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}
toc()
# Con el siguiente código puedes cambiar el tipo de lambda sin tener que volver a ejecutar el loop

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0 # variable that includes the iterations in which we found a model (i.e. at least one variable associated)

for (i in 1:10)
{
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
  
  if (dim(variables_values[[i]])[1] == 0)
  {
    cat("No metabolites found for iteration", i, '\n')
  }
  
  if (dim(variables_values[[i]])[1] != 0)
  {
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    #cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    #corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    #plot(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))
    #linearMod <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda))), col="red")
    #reg_sum[[i]]<-summary(linearMod)
    
    #linearMod_no_intercept <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1)  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1), col="red")
    #reg_sum_no_intercept[[i]]<-summary(linearMod_no_intercept)
    
    si_model <- cbind(si_model, i)
  }
}

si_model <- si_model[,-1]

### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
### CORRELATION values from each metabolite model in the TRAIN-TESTING

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    
    corr_obj[[j]] <- cor.test(Y.test, modelo, method="pearson")
    values_corr_model[j,1]<-corr_obj[[j]]$estimate
    values_corr_model[j,2]<-corr_obj[[j]]$conf.int[1]
    values_corr_model[j,3]<-corr_obj[[j]]$conf.int[2]
    values_corr_model[j,4]<-signif(corr_obj[[j]]$p.value,4)
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==si_model[1])
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>si_model[1])
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F) # By default, the confidence interval is calculated based on the alpha/2-quantile of the t-distribution, where alpha = 0.05. This is motivated if the data are normally distributed. It can be changed to the alpha/2-quantile of the normal distribution by

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
nuevo_ytest_model2
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

save.image(file = "20602022_AS_R2_TM.RData")

rm(rows, TM, folds, train.data, test.data, cv, bT, coef.met, predictions, metricas, l, alp, fit_full, X.full, Y.full,
   cvfit_full, variables_values_full, variables_model_full, values_metabo_full)

#### Pescados:

#### Estimacion del valor de alpha:

#Split t-v

FM = data.frame(cbind(BBDD_rev_met[2:386], BBDD_rev_met[388]))

set.seed(001)

tic("Alpha")

rows <- sample(nrow(FM))
FM <- FM [rows,]
folds <- cut(seq(1,nrow(FM)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = FM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = FM [training, ] # testeas en el 10% (que es 1 fold)
  
  #modelo
  
  cv[[i]] = train(pescados ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(pescados~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$pescados #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$pescados), #Rsquare = R2(predictions[[i]], test.data[[i]]$pescados), 
                             Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$pescados, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$pescados)$estimate)
}

toc()

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.2

tic("WHOLE")

set.seed(002)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(FM)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$pescados

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", alpha=alp)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

#### COMPENDIA OF METABOLITES
#### FULL DATASET

#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(FM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(FM))
colnames(abc_set)[colnames(abc_set)=="colnames.FM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

export(new_mean_values_metabo_sorted, "analisis_sensi_FM.xlsx")

# To run training-testing

# en la cv.glmnet tengo puesto nfolds=10 para simplificar, se podría poner igual al valor de sujetos para que
# hiciera leave-one-out CV

# Si quieres puedes probar qué obtenemos tirándolo de dos formas:
# 1. Loop 10-fold cross validation con 10-CV dentro de cada iteración (código de abajo)
# 2. Solo un loop usando el whole dataset, y poniendo en nfolds=dim(TM)[1] (obviamente, tendrás que cambiar código)

rows <- sample(nrow(FM))
FM2 <- FM[rows, ]
FM <- FM2
folds <- cut(seq(1,nrow(FM)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

tic("AUC: Training-Testing")

#### EXECUTION TRAINING-TESTING

#Variables
set_train<-list()
set_test<-list()
X.train_saved<-list()
Y.train_saved<-list()
X.test_saved<-list()
Y.test_saved<-list()    
fit<-list()
cvfit<-list()
roc_results<-list()
variables_values<-list()
standard_roc<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
reg_sum<-list()
reg_sum_no_intercept<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #sample <- sample.int(n = nrow(FM), size = floor(.90*nrow(FM)), replace = F)
  #set_train[[i]] <- FM[sample, ]
  #set_test[[i]]  <- FM[-sample, ]
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- FM[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- FM[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$pescados
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$pescados
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}
toc()
# Con el siguiente código puedes cambiar el tipo de lambda sin tener que volver a ejecutar el loop

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0 # variable that includes the iterations in which we found a model (i.e. at least one variable associated)

for (i in 1:10)
{
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
  
  if (dim(variables_values[[i]])[1] == 0)
  {
    cat("No metabolites found for iteration", i, '\n')
  }
  
  if (dim(variables_values[[i]])[1] != 0)
  {
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    #cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    #corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    #plot(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))
    #linearMod <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda))), col="red")
    #reg_sum[[i]]<-summary(linearMod)
    
    #linearMod_no_intercept <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1)  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1), col="red")
    #reg_sum_no_intercept[[i]]<-summary(linearMod_no_intercept)
    
    si_model <- cbind(si_model, i)
  }
}

si_model <- si_model[,-1]

### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
### CORRELATION values from each metabolite model in the TRAIN-TESTING

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    
    corr_obj[[j]] <- cor.test(Y.test, modelo, method="pearson")
    values_corr_model[j,1]<-corr_obj[[j]]$estimate
    values_corr_model[j,2]<-corr_obj[[j]]$conf.int[1]
    values_corr_model[j,3]<-corr_obj[[j]]$conf.int[2]
    values_corr_model[j,4]<-signif(corr_obj[[j]]$p.value,4)
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==si_model[1])
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>si_model[1])
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F) # By default, the confidence interval is calculated based on the alpha/2-quantile of the t-distribution, where alpha = 0.05. This is motivated if the data are normally distributed. It can be changed to the alpha/2-quantile of the normal distribution by

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

save.image(file = "20602022_AS_R2_FM.RData")

#### RM:

#### Estimacion del valor de alpha:

#Split t-v

RM = data.frame(cbind(BBDD_rev_met[2:386], BBDD_rev_met[389]))

set.seed(001)

tic("Alpha")

rows <- sample(nrow(RM))
RM <- RM [rows,]
folds <- cut(seq(1,nrow(RM)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = RM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = RM [training, ] # testeas en el 10% (que es 1 fold)
  
  #modelo
  
  cv[[i]] = train(RM ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(RM~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$RM #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$RM), #Rsquare = R2(predictions[[i]], test.data[[i]]$RM), 
                             Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$RM, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$RM)$estimate)
}

toc()

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.3




tic("WHOLE")

set.seed(002)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(RM)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$RM

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", alpha=alp)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

#### COMPENDIA OF METABOLITES
#### FULL DATASET

#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(RM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(RM))
colnames(abc_set)[colnames(abc_set)=="colnames.RM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

export(new_mean_values_metabo_sorted, "analisis_sensi_RM.xlsx")

# To run training-testing

# en la cv.glmnet tengo puesto nfolds=10 para simplificar, se podría poner igual al valor de sujetos para que
# hiciera leave-one-out CV

# Si quieres puedes probar qué obtenemos tirándolo de dos formas:
# 1. Loop 10-fold cross validation con 10-CV dentro de cada iteración (código de abajo)
# 2. Solo un loop usando el whole dataset, y poniendo en nfolds=dim(TM)[1] (obviamente, tendrás que cambiar código)

rows <- sample(nrow(RM))
RM2 <- RM[rows, ]
RM <- RM2
folds <- cut(seq(1,nrow(RM)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

tic("AUC: Training-Testing")

#### EXECUTION TRAINING-TESTING

#Variables
set_train<-list()
set_test<-list()
X.train_saved<-list()
Y.train_saved<-list()
X.test_saved<-list()
Y.test_saved<-list()    
fit<-list()
cvfit<-list()
roc_results<-list()
variables_values<-list()
standard_roc<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
reg_sum<-list()
reg_sum_no_intercept<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #sample <- sample.int(n = nrow(RM), size = floor(.90*nrow(RM)), replace = F)
  #set_train[[i]] <- RM[sample, ]
  #set_test[[i]]  <- RM[-sample, ]
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- RM[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- RM[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$RM
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$RM
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}
toc()
# Con el siguiente código puedes cambiar el tipo de lambda sin tener que volver a ejecutar el loop

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0 # variable that includes the iterations in which we found a model (i.e. at least one variable associated)

for (i in 1:10)
{
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
  
  if (dim(variables_values[[i]])[1] == 0)
  {
    cat("No metabolites found for iteration", i, '\n')
  }
  
  if (dim(variables_values[[i]])[1] != 0)
  {
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    #cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    #corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    #plot(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))
    #linearMod <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda))), col="red")
    #reg_sum[[i]]<-summary(linearMod)
    
    #linearMod_no_intercept <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1)  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1), col="red")
    #reg_sum_no_intercept[[i]]<-summary(linearMod_no_intercept)
    
    si_model <- cbind(si_model, i)
  }
}

si_model <- si_model[,-1]

### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
### CORRELATION values from each metabolite model in the TRAIN-TESTING

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    
    corr_obj[[j]] <- cor.test(Y.test, modelo, method="pearson")
    values_corr_model[j,1]<-corr_obj[[j]]$estimate
    values_corr_model[j,2]<-corr_obj[[j]]$conf.int[1]
    values_corr_model[j,3]<-corr_obj[[j]]$conf.int[2]
    values_corr_model[j,4]<-signif(corr_obj[[j]]$p.value,4)
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==si_model[1])
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>si_model[1])
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F) # By default, the confidence interval is calculated based on the alpha/2-quantile of the t-distribution, where alpha = 0.05. This is motivated if the data are normally distributed. It can be changed to the alpha/2-quantile of the normal distribution by

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

save.image(file = "20602022_AS_R2_RM.RData")

#### PRM:

#### Estimacion del valor de alpha:

#Split t-v

PRM = data.frame(cbind(BBDD_rev_met[2:386], BBDD_rev_met[390]))

set.seed(001)

tic("Alpha")

rows <- sample(nrow(PRM))
PRM <- PRM [rows,]
folds <- cut(seq(1,nrow(PRM)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()
coef.met = c()
predictions = c()
metricas = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = PRM [-training, ] # entrenas en el 90% (que es 10-1 = 9 folds)
  test.data[[i]] = PRM [training, ] # testeas en el 10% (que es 1 fold)
  
  #modelo
  
  cv[[i]] = train(PRM ~ . , data = train.data[[i]],  method = "glmnet", trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
  #Predicciones:
  
  x.test = model.matrix(PRM~., test.data[[i]])[,-1] #Predictores
  y.test = test.data[[i]]$PRM #Outcome
  
  predictions[[i]] = cv[[i]] %>% predict(x.test)
  
  metricas[[i]] = data.frame(RMSE = RMSE(predictions[[i]], test.data[[i]]$PRM), #Rsquare = R2(predictions[[i]], test.data[[i]]$PRM), 
                             Pearson.IC = cor.test(predictions[[i]], test.data[[i]]$PRM, conf.level = 0.95)$conf.int,
                             Pearson.cor = cor.test(predictions[[i]], test.data[[i]]$PRM)$estimate)
}

toc()

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.3

tic("WHOLE")

set.seed(002)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

#### Generate 10 times running elastic regression using FULL SET OF SAMPLES --> TO COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(PRM)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$PRM

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", alpha=alp)
  plot(fit_full[[i]])
  #print(fit_full[[i]])
  #coef(fit_full[[i]],s=0.1) 
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", type.measure = "mse", nfolds = 10, alpha=0.8)
  plot(cvfit_full[[i]])
  #cvfit_full[[i]]$lambda.min
  #coef(cvfit_full[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

toc()

#### FULL

for (i in 1:10)
{
  #Save parameters
  list_metabo_full<-coef(cvfit_full[[i]], s = type.lambda)
  
  values_metabo_full<-list_metabo_full@x
  values_metabo_full<-values_metabo_full[-1]
  
  #Number
  listado_selected_full<-c(list_metabo_full@i)
  variables_model_full<-as.data.frame(colnames(X.full))
  variables_get_full<-variables_model_full[listado_selected_full,]
  variables_get_full<-as.character(variables_get_full)
  
  variables_values_full[[i]]<-cbind(variables_get_full, values_metabo_full)
}

#### COMPENDIA OF METABOLITES
#### FULL DATASET

#Create a table with compendia of metabolites FULL DATASET

inter<-as.data.frame(PRM)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)])) #Save variables

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(PRM))
colnames(abc_set)[colnames(abc_set)=="colnames.PRM."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.factor)
abc_set_values[indx] <- lapply(abc_set_values[indx], function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, by = "Metabolites")

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame("Metabolites"=new_mean_values_metabo_sorted$name, "mean"=new_mean_values_metabo_sorted$mean, "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

export(new_mean_values_metabo_sorted, "analisis_sensi_PRM.xlsx")

# To run training-testing

# en la cv.glmnet tengo puesto nfolds=10 para simplificar, se podría poner igual al valor de sujetos para que
# hiciera leave-one-out CV

# Si quieres puedes probar qué obtenemos tirándolo de dos formas:
# 1. Loop 10-fold cross validation con 10-CV dentro de cada iteración (código de abajo)
# 2. Solo un loop usando el whole dataset, y poniendo en nfolds=dim(TM)[1] (obviamente, tendrás que cambiar código)

rows <- sample(nrow(PRM))
PRM2 <- PRM[rows, ]
PRM <- PRM2
folds <- cut(seq(1,nrow(PRM)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

tic("AUC: Training-Testing")

#### EXECUTION TRAINING-TESTING

#Variables
set_train<-list()
set_test<-list()
X.train_saved<-list()
Y.train_saved<-list()
X.test_saved<-list()
Y.test_saved<-list()    
fit<-list()
cvfit<-list()
roc_results<-list()
variables_values<-list()
standard_roc<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
reg_sum<-list()
reg_sum_no_intercept<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  #sample <- sample.int(n = nrow(PRM), size = floor(.90*nrow(PRM)), replace = F)
  #set_train[[i]] <- PRM[sample, ]
  #set_test[[i]]  <- PRM[-sample, ]
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- PRM[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- PRM[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$PRM
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$PRM
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  #print(fit[[i]])
  #coef(fit[[i]],s=0.1) 
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  #cvfit[[i]]$lambda.min
  #coef(cvfit[[i]], s = type.lambda)
  
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  variables_values[[i]]<-cbind(variables_get, values_metabo)
}
toc()
# Con el siguiente código puedes cambiar el tipo de lambda sin tener que volver a ejecutar el loop

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0 # variable that includes the iterations in which we found a model (i.e. at least one variable associated)

for (i in 1:10)
{
  #Save parameters
  list_metabo<-coef(cvfit[[i]], s = type.lambda)
  
  values_metabo<-list_metabo@x
  values_metabo<-values_metabo[-1]
  
  #Number
  listado_selected<-c(list_metabo@i)
  variables_model<-as.data.frame(colnames(X.train))
  variables_get<-variables_model[listado_selected,]
  variables_get<-as.character(variables_get)
  
  variables_values[[i]]<-cbind(variables_get, values_metabo)
  
  if (dim(variables_values[[i]])[1] == 0)
  {
    cat("No metabolites found for iteration", i, '\n')
  }
  
  if (dim(variables_values[[i]])[1] != 0)
  {
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    #cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="pearson")
    #corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda), method="spearman")
    
    #plot(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))
    #linearMod <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)))  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda))), col="red")
    #reg_sum[[i]]<-summary(linearMod)
    
    #linearMod_no_intercept <- lm(Y.test_saved[[i]] ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1)  # build linear regression model
    #abline(lm(Y.test_saved[[i]]  ~ as.numeric(predict(cvfit[[i]],newx=X.test_saved[[i]], s=type.lambda)) -1), col="red")
    #reg_sum_no_intercept[[i]]<-summary(linearMod_no_intercept)
    
    si_model <- cbind(si_model, i)
  }
}

si_model <- si_model[,-1]

### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
### CORRELATION values from each metabolite model in the TRAIN-TESTING

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)  #Check if there are metabolites in the corresponding model
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"," +", "\n", sep="")} #< i>0 for i>1
      if (i == (dim(b)[1])) {cat("(","data$",b[i,1],"*", b[i,2],")", sep="")}
    }
    sink()
    
    close(con)
    
    stdout_full<-paste(unlist(stdout), collapse =" ")
    stdout_full[1]
    
    #Execute syntax
    eval(parse(text=stdout_full[1]))
    
    modelo<-data$model
    
    corr_obj[[j]] <- cor.test(Y.test, modelo, method="pearson")
    values_corr_model[j,1]<-corr_obj[[j]]$estimate
    values_corr_model[j,2]<-corr_obj[[j]]$conf.int[1]
    values_corr_model[j,3]<-corr_obj[[j]]$conf.int[2]
    values_corr_model[j,4]<-signif(corr_obj[[j]]$p.value,4)
    
    nuevo_ytest_model<-cbind(Y.test, modelo)
    if (j==si_model[1])
    {
      nuevo_ytest_model2<-nuevo_ytest_model
    }
    if (j>si_model[1])
    {
      nuevo_ytest_model2<-rbind(nuevo_ytest_model2, nuevo_ytest_model)
    }
    
  } else {} # If there are no metabolites, SKIP!
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F) # By default, the confidence interval is calculated based on the alpha/2-quantile of the t-distribution, where alpha = 0.05. This is motivated if the data are normally distributed. It can be changed to the alpha/2-quantile of the normal distribution by

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

save.image(file = "20602022_AS_R2_PRM.RData")

###Calculo de los scores para cada variable de interés (basal):

###----Carnicos

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "TM")

colnames(met) = c("names", "TM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "TM")]
model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_met[,2:385])
Y2 = BBDD_rev_met$carnicos
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

Scores_rev = data.frame(cbind(BBDD_rev_met[1],BBDD_rev_met[388:389], BBDD_rev_met[386:387],modelo))
colnames(Scores_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev" )

###----Pescados

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "Fish")

colnames(met) = c("names", "Fish")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "Fish")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_met[,2:385])
Y2 = BBDD_rev_met$pescados
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

Scores_rev = data.frame(cbind(Scores_rev, modelo))
colnames(Scores_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev" )

###----RM

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "RM")

colnames(met) = c("names", "RM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "RM")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_met[,2:385])
Y2 = BBDD_rev_met$RM
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

Scores_rev = data.frame(cbind(Scores_rev, modelo))
colnames(Scores_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev","m.RM_rev")

###----PRM

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "PRM")

colnames(met) = c("names", "PRM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "PRM")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_met[,2:385])
Y2 = BBDD_rev_met$RM
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

Scores_rev = data.frame(cbind(Scores_rev, modelo))
colnames(Scores_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev","m.RM_rev","m.PRM_rev")

export(Scores_rev, "Scores_rev.xlsx")

rm(b, BBDD, correr, df, inter_qvf, merged_dataset_filt, met, model.met, na_count, newdata, otra, quantitative_variables_filtered,
   quantitative_variables1, quantitative_variables1_filtered, quantitative_variables1_filtered1, scaledcentered_treatment, short_db1,
   subjects_NA, x, X2, con, drop, i, metabo_high_NA, modelo, na_count1, na_count2, stdout, stdout_full, Y2, rows_remove)

#### Modelos 1 año:

###Fusion de las BBDD.
BBDD_rev_1a = merge(phenotypes_predimed_20190903, FFQ_PREDIMED_2012, by = "id")

BBDD_basal = BBDDmet_1a[1]

BBDD_rev_1a = merge(BBDD_basal, BBDD_rev_1a, by = "id")

##Variables: carnicos, pescados, carnes blancas, carnes rojas, carnes procesadas.
###Carnicos y pescados ya existe. Genero las variables de carnes blancas, rojas y procesadas segun el articulo: 10.1016/j.clnu.2016.03.017
# Carne roja: cerdo, ternera y cordero
# Carne procesada: despojos, jamón, embutidos, paté, hamburguesas y tocino.

colnames(BBDD_rev_1a)
table(BBDD_rev_1a$cvd_event)
table(BBDD_rev_1a$t2d_event)

BBDD_rev_1a = subset(BBDD_rev_1a, PRM3 >= 0)

BBDD_rev_1a = subset(BBDD_rev_1a, cvd_event == "No")

BBDD_rev_1a_id = BBDD_rev_1a[1]

BBDD_rev_1a_met = merge(BBDD_rev_1a_id, BBDDmet_1a, by = "id")

export(BBDD_rev_1a_met, "BBDD_rev_1a_met.xlsx")

###Calculo de los scores para cada variable de interés (1 año):

###----Carnicos

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "TM")

colnames(met) = c("names", "TM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "TM")]
model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_1a_met[,2:385])
Y2 = BBDD_rev_1a_met$carnicos
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

Scores_1a_rev = data.frame(cbind(BBDD_rev_1a_met[1],BBDD_rev_1a_met[388:389], BBDD_rev_1a_met[386:387],modelo))
colnames(Scores_1a_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev" )

###----Pescados

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "Fish")

colnames(met) = c("names", "Fish")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "Fish")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_1a_met[,2:385])
Y2 = BBDD_rev_1a_met$pescados
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

Scores_1a_rev = data.frame(cbind(Scores_1a_rev, modelo))
colnames(Scores_1a_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev" )

###----RM

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "RM")

colnames(met) = c("names", "RM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "RM")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_1a_met[,2:385])
Y2 = BBDD_rev_1a_met$RM
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

Scores_1a_rev = data.frame(cbind(Scores_1a_rev, modelo))
colnames(Scores_1a_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev","m.RM_rev")

###----PRM

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "PRM")

colnames(met) = c("names", "PRM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "PRM")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_1a_met[,2:385])
Y2 = BBDD_rev_1a_met$RM
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

Scores_1a_rev = data.frame(cbind(Scores_1a_rev, modelo))
colnames(Scores_1a_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev","m.RM_rev","m.PRM_rev")

export(Scores_1a_rev, "Scores_1a_rev.xlsx")

rm(b, BBDD, correr, df, inter_qvf, merged_dataset_filt, met, model.met, na_count, newdata, otra, quantitative_variables_filtered,
   quantitative_variables1, quantitative_variables1_filtered, quantitative_variables1_filtered1, scaledcentered_treatment, short_db1,
   subjects_NA, x, X2, con, drop, i, metabo_high_NA, modelo, na_count1, na_count2, stdout, stdout_full, Y2, rows_remove)

###Calculo de los scores para cada variable de interés (1 año):

###----Carnicos

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "TM")

colnames(met) = c("names", "TM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "TM")]
model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_1a_met[,2:385])
Y2 = BBDD_rev_1a_met$carnicos
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

Scores_1a_rev = data.frame(cbind(BBDD_rev_1a_met[1],BBDD_rev_1a_met[388:389], BBDD_rev_1a_met[386:387],modelo))
colnames(Scores_1a_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev" )

###----Pescados

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "Fish")

colnames(met) = c("names", "Fish")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "Fish")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_1a_met[,2:385])
Y2 = BBDD_rev_1a_met$pescados
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

Scores_1a_rev = data.frame(cbind(Scores_1a_rev, modelo))
colnames(Scores_1a_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev" )

###----RM

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "RM")

colnames(met) = c("names", "RM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "RM")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_1a_met[,2:385])
Y2 = BBDD_rev_1a_met$RM
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

Scores_1a_rev = data.frame(cbind(Scores_1a_rev, modelo))
colnames(Scores_1a_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev","m.RM_rev")

###----PRM

met <- read_excel("Only coef_Gauss_rev2.xlsx", sheet = "PRM")

colnames(met) = c("names", "PRM")

met = merge(met, match_names, by = "names")

model.met = met[c("Metabolites", "PRM")]

model.met$Metabolites = as.character(model.met$Metabolites)
str(model.met)

X2 = data.frame(BBDD_rev_1a_met[,2:385])
Y2 = BBDD_rev_1a_met$RM
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

Scores_1a_rev = data.frame(cbind(Scores_1a_rev, modelo))
colnames(Scores_1a_rev) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev","m.RM_rev","m.PRM_rev")

export(Scores_1a_rev, "Scores_1a_rev.xlsx")

rm(b, BBDD, correr, df, inter_qvf, merged_dataset_filt, met, model.met, na_count, newdata, otra, quantitative_variables_filtered,
   quantitative_variables1, quantitative_variables1_filtered, quantitative_variables1_filtered1, scaledcentered_treatment, short_db1,
   subjects_NA, x, X2, con, drop, i, metabo_high_NA, modelo, na_count1, na_count2, stdout, stdout_full, Y2, rows_remove)

# Pregunta 5, revisor 2: modelos con los 10 metabolítos más asociados. ####

###----Carnicos

met <- read_excel("Only coef_Gauss_rev2_5.xlsx", sheet = "TM")


model.met = met
str(model.met)

X2 = data.frame(BBDDmet[,2:385])
Y2 = BBDDmet$carnicos
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

Scores_rev_5 = data.frame(cbind(BBDDmet[1],BBDDmet[388:389], BBDDmet[386:387],modelo))
colnames(Scores_rev_5) = c("id","carnicos","pescados","RM","PRM","m.TM_rev" )

###----Pescados

met <- read_excel("Only coef_Gauss_rev2_5.xlsx", sheet = "Fish")

model.met = met

X2 = data.frame(BBDDmet[,2:385])
Y2 = BBDDmet$pescados
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

Scores_rev_5 = data.frame(cbind(Scores_rev_5, modelo))
colnames(Scores_rev_5) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev" )

###----RM

met <- read_excel("Only coef_Gauss_rev2_5.xlsx", sheet = "RM")

model.met = met

X2 = data.frame(BBDDmet[,2:385])
Y2 = BBDDmet$RM
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

Scores_rev_5 = data.frame(cbind(Scores_rev_5, modelo))
colnames(Scores_rev_5) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev","m.RM_rev")

###----PRM

met <- read_excel("Only coef_Gauss_rev2_5.xlsx", sheet = "PRM")

model.met = met

X2 = data.frame(BBDDmet[,2:385])
Y2 = BBDDmet$RM
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

Scores_rev_5 = data.frame(cbind(Scores_rev_5, modelo))
colnames(Scores_rev_5) = c("id","carnicos","pescados","RM","PRM","m.TM_rev","m.Fish_rev","m.RM_rev","m.PRM_rev")

export(Scores_rev_5, "Scores_rev_5.xlsx")

rm(b, BBDD, correr, df, inter_qvf, merged_dataset_filt, met, model.met, na_count, newdata, otra, quantitative_variables_filtered,
   quantitative_variables1, quantitative_variables1_filtered, quantitative_variables1_filtered1, scaledcentered_treatment, short_db1,
   subjects_NA, x, X2, con, drop, i, metabo_high_NA, modelo, na_count1, na_count2, stdout, stdout_full, Y2, rows_remove)
