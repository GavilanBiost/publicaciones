# Fase 1: ####
# creación de los PM de cada alimento basal, validación interna basal y 
# estimación de los coeficientes de los metabolitos con toda la muestra (basal)

# Libreries:
library(abind)
library(agricolae)
library(Amelia)
library(car)
library(caret)
library(chillR)
library(cluster)
library(colorspace)
library(compare)
library(cvAUC)
library(dendextend)
library(dplyr)
library(factoextra)
library(foreach)
library(foreign)
library(ggplot2)
library(ggrepel)
library(glmnet)
library(gmodels)
library(gtools)
library(haven)
library(Hmisc)
library(iterators)
library(knitr)
library(lattice)
library(lmtest)
library(MASS)
library(mclust)
library(missForest)
library(mixOmics)
library(nortest)
library(PerformanceAnalytics)
library(pls)
library(plyr)
library(pROC)
library(Publish)
library(pvclust)
library(QuantPsyc)
library(randomForest)
library(Rcpp)
library(readr)
library(readxl)
library(reshape)
library(RNOmni)
library(rgl)
library(rio)
library(rJava)
library(Rmisc)
library(bioc::ropls)
library(rpart)
library(shiny)
library(survival)
library(tibble)
library(tictoc)
library(tidyr)
library(tis)
library(varhandle)
library(xlsx)
library(xlsxjars)
library(doParallel)
library(qvalue)
library(EnhancedVolcano)
library(airway)
library(magrittr)
library(parallel)
library(limma)
library(VennDiagram)
library(corrplot)
library(rio)
library(stats)

no_cores <- detectCores()
registerDoParallel(cores=no_cores) 

## 1.1.	Preparación de los datos generales: ####

# Apertura de las bases de datos de trabajo:

metabolites <- read_csv("BBDD/metabolites.csv")
phenotypes <- read_csv("BBDD/phenotypes.csv")
FFQ <- read_sav("BBDD/FFQ_2012.SAV")

###Fusion de las BBDD.
BBDD = merge(phenotype, FFQ, by = "id")

summary(BBDD$ac_oliva)
summary(BBDD$ac_olivavir)
summary(BBDD$olivatot)

## Variables ajustadas a energía:

BBDD<-BBDD[order(-BBDD$ac_oliva),]

df<- data.frame(y = BBDD$ac_oliva,
                y1= BBDD$ac_oliva,
                x = BBDD$energiat) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 16.514)
BBDD$ac_oliva_ajust <- values

BBDD[,c("ac_oliva", "ac_oliva_ajust")]
rm("df1", "values", "df")

BBDD<-BBDD[order(-BBDD$ac_olivavir),]

df<- data.frame(y = BBDD$ac_olivavir,
                y1= BBDD$ac_olivavir,
                x = BBDD$energiat) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 22.44)
BBDD$ac_olivavir_ajust <- values

BBDD[,c("ac_olivavir", "ac_olivavir_ajust")]
rm("df1", "values", "df")

BBDD<-BBDD[order(-BBDD$olivatot),]

df<- data.frame(y = BBDD$olivatot,
                y1= BBDD$olivatot,
                x = BBDD$energiat) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 39.38)
BBDD$olivatot_ajust <- values

BBDD[,c("olivatot", "olivatot_ajust")]
rm("df1", "values", "df")

## Eliminacion participantes sin FFQ

BBDD = subset (BBDD, olivatot >= 0) 

## Eliminación extremos de energia

BBDD$sub1 = 2 

for (i in 1:length(BBDD$sub1)){
  if (BBDD$energiat[i]>=4000 & BBDD$sex[i]== "man") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat[i]<=800 & BBDD$sex[i]== "man") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat[i]>=3500 & BBDD$sex[i]== "woman") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat[i]<=500 & BBDD$sex[i]== "woman") {BBDD$sub1[i]='1'} 
  else {BBDD$sub1[i]='0'}}
table(BBDD$sub1)

BBDD = subset(BBDD, sub1 == 0)

## Terciles

BBDD$ter_ac_oliva = cut2(BBDD$ac_oliva, g = 3)
table(BBDD$ter_ac_oliva)
tapply(BBDD$ac_oliva, BBDD$ter_ac_oliva, mean, na.rm = T)
tapply(BBDD$ac_oliva, BBDD$ter_ac_oliva, sd, na.rm = T)

BBDD$ter_ac_oliva_ajust = cut2(BBDD$ac_oliva_ajust, g = 3)
table(BBDD$ter_ac_oliva_ajust)
tapply(BBDD$ac_oliva_ajust, BBDD$ter_ac_oliva_ajust, mean, na.rm = T)
tapply(BBDD$ac_oliva_ajust, BBDD$ter_ac_oliva_ajust, sd, na.rm = T)

BBDD$ter_ac_olivavir = cut2(BBDD$ac_olivavir, g = 3)
table(BBDD$ter_ac_olivavir)
tapply(BBDD$ac_olivavir, BBDD$ter_ac_olivavir, mean, na.rm = T)
tapply(BBDD$ac_olivavir, BBDD$ter_ac_olivavir, sd, na.rm = T)

BBDD$ter_ac_olivavir_ajust = cut2(BBDD$ac_olivavir_ajust, g = 3)
table(BBDD$ter_ac_olivavir_ajust)
tapply(BBDD$ac_olivavir_ajust, BBDD$ter_ac_olivavir_ajust, mean, na.rm = T)
tapply(BBDD$ac_olivavir_ajust, BBDD$ter_ac_olivavir_ajust, sd, na.rm = T)

BBDD$ter_olivatot = cut2(BBDD$olivatot, g = 3)
table(BBDD$ter_olivatot)
tapply(BBDD$olivatot, BBDD$ter_olivatot, mean, na.rm = T)
tapply(BBDD$olivatot, BBDD$ter_olivatot, sd, na.rm = T)

BBDD$ter_olivatot_ajust = cut2(BBDD$olivatot_ajust, g = 3)
table(BBDD$ter_olivatot_ajust)
tapply(BBDD$olivatot, BBDD$ter_olivatot_ajust, mean, na.rm = T)
tapply(BBDD$olivatot, BBDD$ter_olivatot_ajust, sd, na.rm = T)

## 1.2. Tablas descriptivas según terciles olivatot (tabla 1 y 2): ####
colnames(BBDD)[1000:1255]
colnames(BBDD)[1:999]

BBDD1 = data.frame(cbind(BBDD[1254], BBDD[1255], BBDD[4], BBDD[11], BBDD[13], 
                         BBDD[70], BBDD[905:916], BBDD[889:897], 
                         BBDD[1017:1018]))

medianas = c()
iqrs = c()

for (i in 3:29) {
  medianas[[i]] = tapply(BBDD1[[i]], BBDD1[[2]], mean, na.rm = T)
  iqrs[[i]] = tapply(BBDD1[[i]], BBDD1[[2]], sd, na.rm = T)
}

medianas = data.frame(matrix(unlist(medianas), ncol=3, byrow=T),
                      stringsAsFactors=FALSE)
iqrs = data.frame(matrix(unlist(iqrs), ncol=3, byrow=T),stringsAsFactors=FALSE)

iqrs = data.frame(cbind(medianas, iqrs))
iqrs$nmobres = colnames(BBDD1[3:29])

colnames(iqrs) = c("T1", "T2", "T3", "SD1", "Sd2", "Sd3", "nombres")

export(iqrs, "alimentacion_basal_olitot.xlsx")

str(BBDD1$ter_olivatot)

(table(as.factor(BBDD$sex), BBDD$ter_olivatot)) 
(prop.table(table(as.factor(BBDD$sex), BBDD$ter_olivatot),2)*100) 

(table(as.factor(BBDD$diabetes0), BBDD$ter_olivatot)) 
(prop.table(table(as.factor(BBDD$diabetes0), BBDD$ter_olivatot),2)*100) 

(table(as.factor(BBDD$dyslip0), BBDD$ter_olivatot)) 
(prop.table(table(as.factor(BBDD$dyslip0), BBDD$ter_olivatot),2)*100) 

(table(as.factor(BBDD$hyperten0), BBDD$ter_olivatot)) 
(prop.table(table(as.factor(BBDD$hyperten0), BBDD$ter_olivatot),2)*100) 

(table(as.factor(BBDD$fam_history), BBDD$ter_olivatot)) 
(prop.table(table(as.factor(BBDD$fam_history), BBDD$ter_olivatot),2)*100) 

(table(as.factor(BBDD$smoking0), BBDD$ter_olivatot)) 
(prop.table(table(as.factor(BBDD$smoking0), BBDD$ter_olivatot),2)*100) 

(table(as.factor(BBDD$sex), BBDD$ter_olivatot_ajust)) 
(prop.table(table(as.factor(BBDD$sex), BBDD$ter_olivatot_ajust),2)*100) 

(table(as.factor(BBDD$diabetes0), BBDD$ter_olivatot_ajust)) 
(prop.table(table(as.factor(BBDD$diabetes0), BBDD$ter_olivatot_ajust),2)*100) 

(table(as.factor(BBDD$dyslip0), BBDD$ter_olivatot_ajust)) 
(prop.table(table(as.factor(BBDD$dyslip0), BBDD$ter_olivatot_ajust),2)*100) 

(table(as.factor(BBDD$hyperten0), BBDD$ter_olivatot_ajust)) 
(prop.table(table(as.factor(BBDD$hyperten0), BBDD$ter_olivatot_ajust),2)*100) 

(table(as.factor(BBDD$fam_history), BBDD$ter_olivatot_ajust)) 
(prop.table(table(as.factor(BBDD$fam_history), BBDD$ter_olivatot_ajust),2)*100) 

(table(as.factor(BBDD$smoking0), BBDD$ter_olivatot_ajust)) 
(prop.table(table(as.factor(BBDD$smoking0), BBDD$ter_olivatot_ajust),2)*100) 

## 1.3.	Preparación de la base de datos de metabolómica. ####

merged_dataset_filt = BBDD

colnames(BBDD)[1200:1255]

short_db<-cbind(id=merged_dataset_filt[,1],
                merged_dataset_filt[,83:230],
                merged_dataset_filt[,382:584],
                merged_dataset_filt[,790:838],
                merged_dataset_filt[914],
                merged_dataset_filt[,1017:1018],
                merged_dataset_filt[,1246:1248]) 

##Eliminar los metabolitos que son estándares: C240PC, glycocholated4, 
# thymined4, inosine15N4

drop <- c("c240pc", "glycocholated4", "thymined4", "inosine15n4", #QC
          "acetaminophen", "cyclohexylamine", "metronidazole") #Drugs
short_db = short_db[,!(names(short_db) %in% drop)]

## Determinar missing values en los sujetos y eliminar aquellos > 20%
# Check Subjects with NAs
subjects_NA<-as.data.frame(rowSums(is.na(short_db)))
otra<-cbind(short_db$id, subjects_NA)
# threshold based on this databases 0.2*390
rows_remove<-otra$`short_db$id`[otra$`rowSums(is.na(short_db))`>78]  
short_db <- subset(short_db, !id %in% rows_remove)

quantitative_variables<-short_db[,2:(length(short_db)-6)]

#NAs in each variable
na_count1 <-sapply(quantitative_variables, function(quantitative_variables) 
  sum(length(which(is.na(quantitative_variables)))))
na_count2 <-sapply(quantitative_variables, function(quantitative_variables) 
  (100*sum(length(which(is.na(quantitative_variables))))/sum(length(
    (quantitative_variables)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

#Metabolites with NA > 20%
drop<-rownames(na_count[na_count[,2]>20 ,])
export(drop, "drop.xlsx")

#Remove variables with high number of missing values (>20)
quantitative_variables_filtered = quantitative_variables[,!(names(
  quantitative_variables) %in% drop)]

## Tratamiento de missing values --> missforest.
set.seed(1)

IMP = missForest(as.matrix(quantitative_variables_filtered), verbose = T)
IMP$OOBerror
IMP = data.frame(IMP$ximp)
colnames(IMP)

## Normalizacion con ranknorm:

IMP = apply(IMP, 2, RankNorm)

BBDDmet = data.frame(cbind(short_db[1], IMP, short_db[395:400]))

export(BBDDmet, "20230601_BBDDmet_rkn.xlsx")

rm(BBDD1, BBDD2, df, FFQ_PREDIMED_2012, IMP, iqrs, medianas, 
   merged_dataset_filt, NEW_Combined_metabolites_phenotypes_230218, otra,
   phenotypes_predimed_20190903, quantitative_variables, 
   quantitative_variables_filtered, short_db, subjects_NA, x, drop, i, 
   rows_remove, na_count, na_count1, na_count2)

# Fase 2: ####
## 2.1.	Modelos de regresión elástica neta continuos (ENR). ####

##### Olivatot_ajust

#### Estimacion del valor de alpha:

#Split t-v

OT = data.frame(cbind(BBDDmet[2:383], BBDDmet[389]))

set.seed(001)

rows <- sample(nrow(OT))
OT <- OT [rows,]
folds <- cut(seq(1,nrow(OT)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  # entrenas en el 90%
  train.data[[i]] = OT [-training,]
  # testeas en el 10% 
  test.data[[i]] = OT [training,] 
  
  #modelo
  cv[[i]] = train(olivatot_ajust ~ . , data = train.data[[i]],  
                  method = "glmnet", trControl = trainControl(
                    "cv", number = 10), tuneLength = 10)
  bT[[i]] = cv[[i]]$bestTune
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.4

set.seed(002)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

# Generate 10 times running elastic regression using FULL SET OF SAMPLES TO 
# COMPUTE model coefficients!

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(OT)
X.full<-full_data[,1:(dim(full_data)[2]-4)]
X.full<-as.matrix(X.full)
Y.full <- full_data$olivatot_ajust

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", 
                         alpha=.5)
  plot(fit_full[[i]])
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", 
                              type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  
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

inter<-as.data.frame(OT)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)]))

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(OT))
colnames(abc_set)[colnames(abc_set)=="colnames.OT."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], 
                 by = "variables_get_full", all.x = T)
}
colnames(abc_set) = 
  c("variables_get_full","v1","v2","v3","v4","v5","v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-
  c("variables_get_full", "It1", "It2", "It3", "It4", "It5", "It6", 
    "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.character)
abc_set_values[indx] <- lapply(abc_set_values[indx], 
                               function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

export(mean_values_metabo, "Only coef_Gauss_AOTOT1.xlsx")

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

match_names <- read_delim("BBDD/names_vars.csv", ";", escape_double = FALSE, 
                          trim_ws = TRUE)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")
  
### FIGURES     

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, 
                              by = "Metabolites")

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(
  mean_values_metabo$mean),c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, 
                                     by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame(
  "Metabolites"=new_mean_values_metabo_sorted$name, 
  "mean"=new_mean_values_metabo_sorted$mean, 
  "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[
  order(new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean<=0) | (
    new_mean_values_metabo_sorted$mean>=0), ]
dotchart(selected_mean_values_metabo_sorted$mean,
         labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean<0), ]

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean>=0), ]

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

######### FIGURES

#Change scale
plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, aes(
  x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + xlab(
    "Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-2, 0)) + geom_errorbarh(aes(
    xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, aes(
  x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab(
    "Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 2)) + geom_errorbarh(aes(
    xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
png(filename = "coef_AOTOT_ajusts.png", width = 30, height = 20, res=300, 
    unit="cm")
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]


export(selected_mean_values_metabo_sorted,
       "20230601_olivatot_ajust_preval.xlsx")

# To run training-testing

rows <- sample(nrow(OT))
OT2 <- OT[rows, ]
OT <- OT2
folds <- cut(seq(1,nrow(OT)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS
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
variables_values<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')

  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- OT[-sample, ]  # note that it is inverted!!!!!
  set_test[[i]]  <- OT[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$olivatot_ajust
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$olivatot_ajust
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", 
                         type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  
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

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0

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
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], 
    X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], 
                                 predict(cvfit[[i]],newx=X.test_saved[[i]], 
                                         s=type.lambda), method="pearson")
    cor_values_spearman[[i]]<-cor(Y.test_saved[[i]], 
                                  predict(cvfit[[i]],newx=X.test_saved[[i]], 
                                          s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], 
                                     predict(cvfit[[i]], newx=X.test_saved[[i]],
                                             s=type.lambda), method="pearson")
    corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], 
                                      predict(cvfit[[i]],newx=X.test_saved[[i]],
                                              s=type.lambda), method="spearman")
    si_model <- cbind(si_model, i)
  }
}

##################### PEARSON CORRELATION BASED ON MODEL

# We remove the intercept
#### CORRELATION values from each metabolite model in the TRAIN-TESTING

si_model <- si_model[,-1]

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
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", 
                                            b[i,2],")"," +", "\n", 
                                            sep="")} #< i>0 for i>1
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
#ci.mean(correlations[,1],normal=F) 
# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
nuevo_ytest_model2
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOTOT")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet[,2:383])
Y2 = BBDDmet$olivatot_ajust
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")",
                                          " +", "\n", sep="")}
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

Scores = data.frame(cbind(BBDDmet[1],BBDDmet[387:389], modelo))
colnames(Scores) = c("id","ac_oliva_ajust","ac_olivavir_ajust","olivatot_ajust",
                     "m.AOTOT")

export(Scores, "20230601_Scores.xlsx")

save.image(file = "20230601.v1.Oliveoiltot_ajust.RData")

rm(a, abc_full, abc_set, abc_set_values, alp, b, bT, con, cor_values_pearson, 
   cor_values_spearman, corr_obj, corr_test_pearson, corr_test_spearman, 
   correr, cv, cvfit, cvfit_full, data, fit, fit_full, folds, full_data, i, 
   indx, inter, j, l, list_metabo, list_metabo_full, listado_selected, 
   listado_selected_full, maximo, mean_values_metabo, mean_values_metabo_matrix,
   mean_values_metabo_sorted, met, metabos_NA_model, method, minimo, model.met,
   modelo, new_abc_full, new_mean_values_metabo, new_mean_values_metabo_sorted, 
   nuevo_ytest_model, nuevo_ytest_model2, OT, OT2, plot_negative, plot_positive,
   plot1, plot2, rows, sample, selected_mean_values_metabo_sorted, 
   selected_mean_values_metabo_sorted_negative, 
   selected_mean_values_metabo_sorted_positive, set_test, set_train, si_model, 
   stdout, stdout_full, test, test.data, train, train.data, training, 
   type.lambda, values_corr_model, values_metabo, values_metabo_full, 
   variables_get, variables_get_full, variables_model, variables_model_full, 
   variables_values, variables_values_full, variables2, X.full, X.test, 
   X.test_saved, X.train, X.train_saved, X2, Y.full, Y.test, Y.test_saved, 
   Y.train, Y2, Y.train_saved)

##### ac_olivavir_ajust

#### Estimacion del valor de alpha:

#Split t-v

AOVE = data.frame(cbind(BBDDmet[2:383], BBDDmet[388]))

set.seed(004)

rows <- sample(nrow(AOVE))
AOVE <- AOVE [rows,]
folds <- cut(seq(1,nrow(AOVE)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = AOVE [-training, ] 
  test.data[[i]] = AOVE [training, ]
  
  #modelo
  
  cv[[i]] = train(ac_olivavir_ajust ~ . , data = train.data[[i]],  
                  method = "glmnet", 
                  trControl = trainControl("cv", number = 10), tuneLength = 10)
  
  bT[[i]] = cv[[i]]$bestTune
  
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.2

set.seed(005)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(AOVE)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$ac_olivavir_ajust

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", 
                         alpha=.5)
  plot(fit_full[[i]])
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", 
                              type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  
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

inter<-as.data.frame(AOVE)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)]))

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(AOVE))
colnames(abc_set)[colnames(abc_set)=="colnames.AOVE."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], 
                 by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5",
                      "v6","v7","v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5",
                     "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.character)
abc_set_values[indx] <- lapply(abc_set_values[indx], 
                               function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

export(mean_values_metabo, "Only coef_Gauss_AOVE1.xlsx")

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficientes and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names,
                              by = "Metabolites")

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),
                                              c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names,
                                     by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame(
  "Metabolites"=new_mean_values_metabo_sorted$name, 
  "mean"=new_mean_values_metabo_sorted$mean, 
  "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(
  new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean<=0) | (
    new_mean_values_metabo_sorted$mean>=0), ]
dotchart(selected_mean_values_metabo_sorted$mean,
         labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean<0), ]

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean>=0), ]

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

######### FIGURES

#Change scale
plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, 
                      aes(x = mean, y = reorder(Metabolites, -mean))) + 
  geom_point() + scale_y_discrete(position = "left") + 
  xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-2, 0)) + 
  geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, 
                      aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + xlab("Coefficient value") + 
  ylab("Metabolites") + scale_x_continuous(limits = c(0, 2)) + 
  geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
png(filename = "coef_AOVE_ajusts.png", width = 30, height = 20, res=300,
    unit="cm")
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

export(selected_mean_values_metabo_sorted, 
       "20230602_ac_olivavir_ajust_preval.xlsx")

# To run training-testing

rows <- sample(nrow(AOVE))
AOVE2 <- AOVE[rows, ]
AOVE <- AOVE2
folds <- cut(seq(1,nrow(AOVE)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS

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
variables_values<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- AOVE[-sample, ]
  set_test[[i]]  <- AOVE[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$ac_olivavir_ajust
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$ac_olivavir_ajust
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian",
                         type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  
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

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0

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
    #standard_roc[[i]] <- roc(Y.test_saved[[i]], as.numeric(predict(cvfit[[i]], 
    #X.test_saved[[i]], type = "response")), ci=TRUE)
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], 
                                 predict(cvfit[[i]],newx=X.test_saved[[i]], 
                                         s=type.lambda), method="pearson")
    cor_values_spearman[[i]]<-cor(Y.test_saved[[i]],
                                  predict(cvfit[[i]],newx=X.test_saved[[i]],
                                          s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], 
                                     predict(cvfit[[i]], newx=X.test_saved[[i]],
                                             s=type.lambda), method="pearson")
    corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]], 
                                      predict(cvfit[[i]],newx=X.test_saved[[i]],
                                              s=type.lambda), method="spearman")
    si_model <- cbind(si_model, i)
  }
}


##################### PEARSON CORRELATION BASED ON MODEL

si_model <- si_model[,-1]

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"
                                            ," +", "\n", sep="")}
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
    
  } else {}
}
colnames(values_corr_model)<-c("r-Pearson","LCI","HCI","P-value")

#install.packages("Publish")
require(Publish)

# CV-Correlation value
ci.mean(values_corr_model[,1],normal=T)
#ci.mean(correlations[,1],normal=F)

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
nuevo_ytest_model2
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOVE")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet[,2:383])
Y2 = BBDDmet$ac_olivavir_ajust
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"
                                          ," +", "\n", sep="")}
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

Scores = data.frame(cbind(Scores, modelo))
colnames(Scores) = c("id","ac_oliva_ajust","ac_olivavir_ajust","olivatot_ajust",
                     "m.AOTOT", "m.AOVE")
export(Scores, "Scores.xlsx")

save.image(file = "20230602.v1.ac_olivarvir_ajust.RData")

rm(a, abc_full, abc_set, abc_set_values, alp, b, bT, con, cor_values_pearson, 
   cor_values_spearman, corr_obj, corr_test_pearson, corr_test_spearman, correr,
   cv, cvfit, cvfit_full, data, fit, fit_full, folds, full_data, i, indx, inter,
   j, l, list_metabo, list_metabo_full, listado_selected, listado_selected_full,
   maximo, mean_values_metabo, mean_values_metabo_matrix, 
   mean_values_metabo_sorted, met, metabos_NA_model, method, minimo, model.met,
   modelo, new_abc_full, new_mean_values_metabo, new_mean_values_metabo_sorted, 
   nuevo_ytest_model, nuevo_ytest_model2, AOVE, AOVE2, plot_negative, 
   plot_positive, plot1, plot2, rows, sample, selected_mean_values_metabo_sorted,
   selected_mean_values_metabo_sorted_negative, 
   selected_mean_values_metabo_sorted_positive, set_test, set_train, si_model, 
   stdout, stdout_full, test, test.data, train, train.data, training, 
   type.lambda, values_corr_model, values_metabo, values_metabo_full, 
   variables_get, variables_get_full, variables_model, variables_model_full, 
   variables_values, variables_values_full, variables2, X.full, X.test, 
   X.test_saved, X.train, X.train_saved, X2, Y.full, Y.test, Y.test_saved, 
   Y.train, Y2, Y.train_saved)

##### ac_oliva_ajust

#### Estimacion del valor de alpha:

#Split t-v

AO = data.frame(cbind(BBDDmet[2:383], BBDDmet[387]))

set.seed(006)

rows <- sample(nrow(AO))
AO <- AO [rows,]
folds <- cut(seq(1,nrow(AO)),breaks=10,labels=FALSE)

train.data = c()
test.data = c()
cv = c()
bT = c()

for (i in 1:10) {
  
  #Particion
  
  training = which(folds==i, arr.ind=TRUE)
  
  train.data[[i]] = AO [-training, ]
  test.data[[i]] = AO [training, ])
  
  #modelo
  
  cv[[i]] = train(ac_oliva_ajust ~ . , data = train.data[[i]],  
                  method = "glmnet", 
                  trControl = trainControl("cv", number = 10), tuneLength = 10)
  bT[[i]] = cv[[i]]$bestTune
  
}

l = cv[[i]]$bestTune$lambda
alp = cv[[i]]$bestTune$alpha #0.3

set.seed(007)

# Using lambda.min
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

fit_full<-list()
cvfit_full<-list()
variables_values_full<-list()

full_data<-as.data.frame(AO)
X.full<-full_data[,1:(dim(full_data)[2]-1)]
X.full<-as.matrix(X.full)
Y.full <- full_data$ac_oliva_ajust

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration WHOLE DATASET ", i, '\n')
  cat("##################################", '\n')
  
  fit_full[[i]] = glmnet(X.full, as.numeric(Y.full), family="gaussian", 
                         alpha=.5)
  plot(fit_full[[i]])
  
  cvfit_full[[i]] = cv.glmnet(X.full, as.numeric(Y.full), family="gaussian", 
                              type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit_full[[i]])
  
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

inter<-as.data.frame(AO)
variables2<-as.data.frame(colnames(inter[,2:(length(inter)-1)]))

colnames(variables2)<-c("variables_get_full")
variables2$check<-c('NA')

abc_set = data.frame(colnames(AO))
colnames(abc_set)[colnames(abc_set)=="colnames.AO."] = "variables_get_full"

for (i in 1:10) {
  abc_set= merge(abc_set, variables_values_full[[i]], 
                 by = "variables_get_full", all.x = T)
}
colnames(abc_set) = c("variables_get_full","v1","v2","v3","v4","v5","v6","v7",
                      "v8","v9","v10")

metabos_NA_model<-as.data.frame(10 - rowSums(is.na(abc_set)))
metabos_NA_model<-cbind(abc_set$variables_get_full, metabos_NA_model)
colnames(metabos_NA_model)<-c("variables_get_full", "Join")
metabos_NA_model<-metabos_NA_model[order(metabos_NA_model$variables_get_full),]

abc_set<-merge(abc_set, metabos_NA_model, by = "variables_get_full", all=T)
colnames(abc_set)<-c("variables_get_full", "It1", "It2", "It3", "It4", "It5", 
                     "It6", "It7", "It8", "It9", "It10", "JOIN")

abc_set_values<-abc_set[,2:11]
#Sintax to detect factor variables
str(abc_set[,2:11], list.len=(ncol(abc_set))-3)
#Sintax to change from factor (categorical) to numerical variables
indx <- sapply(abc_set[,2:11], is.character)
abc_set_values[indx] <- lapply(abc_set_values[indx], 
                               function(x) as.numeric(as.character(x)))

abc_full<-cbind("Metabolites"=abc_set[,1], abc_set_values, "JOIN"=abc_set[,12])

abc_full$mean<-apply(abc_full[,2:11], 1, function(x) { mean(x, na.rm=TRUE) })
abc_full$sd<-apply(abc_full[,2:11], 1, function(x) { sd(x, na.rm=TRUE) })

#Get only those CONSISTENTLY repeated the 9-10 times
# Isn't it now set to only 10 times? ==10?
mean_values_metabo <- abc_full[ which(abc_full$JOIN==10), c(1,13,14)]

export(mean_values_metabo, "Only coef_Gauss_AO1.xlsx")

unlist(variables_values_full[[1]])

mean_values_metabo_matrix<-as.matrix(mean_values_metabo)

# Copy table with all the metabolites, coefficients and mean+SD
new_abc_full<-merge(abc_full, match_names, by = "Metabolites")

### FIGURES

# Copy those ten times repeated (mean value for the coefficient)
new_mean_values_metabo<-merge(mean_values_metabo, match_names, 
                              by = "Metabolites")

# Sort values
mean_values_metabo_sorted<-mean_values_metabo[order(mean_values_metabo$mean),
                                              c(1,2,3)]
dim(mean_values_metabo_sorted)[1]

#### Used correct names for variables
new_mean_values_metabo_sorted<-merge(mean_values_metabo_sorted, match_names, 
                                     by = "Metabolites")
new_mean_values_metabo_sorted<-data.frame(
  "Metabolites"=new_mean_values_metabo_sorted$name, 
  "mean"=new_mean_values_metabo_sorted$mean, 
  "sd"=new_mean_values_metabo_sorted$sd)
new_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[order(
  new_mean_values_metabo_sorted$mean),c(1,2,3)]
dim(new_mean_values_metabo_sorted)[1]

#Only those selected
selected_mean_values_metabo_sorted<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean<=0) | (
    new_mean_values_metabo_sorted$mean>=0), ]
dotchart(selected_mean_values_metabo_sorted$mean,
         labels=selected_mean_values_metabo_sorted$Metabolites,cex=.7,
         main="Predictors", 
         xlab="Coefficient value")
abline(v=0)

par( mfrow = c( 1, 2))
#Negative
selected_mean_values_metabo_sorted_negative<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean<0), ]

#Positive
selected_mean_values_metabo_sorted_positive<-new_mean_values_metabo_sorted[(
  new_mean_values_metabo_sorted$mean>=0), ]

minimo<-min(selected_mean_values_metabo_sorted_negative$mean)
maximo<-max(selected_mean_values_metabo_sorted_positive$mean)

######### FIGURES

#Change scale
plot_negative<-ggplot(selected_mean_values_metabo_sorted_negative, 
                      aes(x = mean, y = reorder(Metabolites, -mean))) +
  geom_point() + scale_y_discrete(position = "left") + 
  xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(-2, 0)) + 
  geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

plot_positive<-ggplot(selected_mean_values_metabo_sorted_positive, 
                      aes(x = mean, y = reorder(Metabolites, +mean))) +
  geom_point() + scale_y_discrete(position = "right") + 
  xlab("Coefficient value") + ylab("Metabolites") +
  scale_x_continuous(limits = c(0, 2)) + geom_errorbarh(aes(
    xmax = mean + sd, xmin = mean - sd, height = .2)) + 
  theme_bw()

require(gridExtra)
plot1 <- plot_negative
plot2 <- plot_positive
png(filename = "coef_AO_ajusts.png", width = 30, height = 20, res=300,
    unit="cm")
grid.arrange(plot1, plot2, ncol=2)
dev.off()

# NEGATIVE/POSITIVE METABOLITES
dim(selected_mean_values_metabo_sorted_negative)[1]
dim(selected_mean_values_metabo_sorted_positive)[1]

export(selected_mean_values_metabo_sorted,
       "20230602_ac_oliva_ajust_preval.xlsx")

# To run training-testing

rows <- sample(nrow(AO))
AO2 <- AO[rows, ]
AO <- AO2
folds <- cut(seq(1,nrow(AO)),breaks=10,labels=FALSE)

####  INTAKE is entered as CONTINOUS
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
variables_values<-list()
cor_values_pearson<-list()
cor_values_spearman<-list()
corr_test_pearson<-list()
corr_test_spearman<-list()

for (i in 1:10)
{
  cat("##################################", '\n')
  cat("Iteration TRAINING-testing ", i, '\n')
  cat("##################################", '\n \n')
  
  sample <- which(folds==i,arr.ind=TRUE)
  set_train[[i]] <- AO[-sample, ]
  set_test[[i]]  <- AO[sample, ]
  
  #TRAIN
  a<-as.data.frame(set_train[[i]])
  train<-a[,2:(dim(a)[2]-1)]
  X.train <- train
  X.train <- as.matrix(X.train)
  Y.train <- a$ac_oliva_ajust
  
  #TEST
  a<-as.data.frame(set_test[[i]])
  test<-a[,2:(dim(a)[2]-1)]
  X.test <- test
  X.test <- as.matrix(X.test)
  Y.test <- a$ac_oliva_ajust
  
  #save databases
  X.train_saved[[i]]<-X.train
  Y.train_saved[[i]]<-Y.train
  X.test_saved[[i]]<-X.test
  Y.test_saved[[i]]<-Y.test    
  
  #Elastic REG    
  fit[[i]] = glmnet(X.train, as.numeric(Y.train), family="gaussian", alpha=alp)
  plot(fit[[i]])
  
  cvfit[[i]] = cv.glmnet(X.train, as.numeric(Y.train), family="gaussian", 
                         type.measure = "mse", nfolds = 10, alpha=alp)
  plot(cvfit[[i]])
  
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

#### FOR THE TRAINING-TESTING
type.lambda<-c("lambda.min")
#type.lambda<-c("lambda.1se")

si_model <- 0

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
    cor_values_pearson[[i]]<-cor(Y.test_saved[[i]], 
                                 predict(cvfit[[i]], newx=X.test_saved[[i]], 
                                         s=type.lambda), method="pearson")
    cor_values_spearman[[i]]<-cor(Y.test_saved[[i]],
                                  predict(cvfit[[i]],newx=X.test_saved[[i]], 
                                          s=type.lambda), method="spearman")
    
    corr_test_pearson[[i]]<-cor.test(Y.test_saved[[i]], 
                                     predict(cvfit[[i]],newx=X.test_saved[[i]],
                                             s=type.lambda), method="pearson")
    corr_test_spearman[[i]]<-cor.test(Y.test_saved[[i]],
                                      predict(cvfit[[i]],newx=X.test_saved[[i]],
                                              s=type.lambda), method="spearman")
    
    si_model <- cbind(si_model, i)
  }
}

##################### PEARSON CORRELATION BASED ON MODEL

si_model <- si_model[,-1]

method <- "pearson"   # Change to "spearman" if you need it
values_corr_model<-matrix(NA,10,4)
corr_obj<-list()
for (j in si_model){   # used to be in 1:10
  data<-as.data.frame(X.test_saved[[j]])
  b<-unlist(variables_values[[j]])
  Y.test<-Y.test_saved[[j]]
  
  if (length(b) != 0)
  {
    #TRY
    stdout <- vector('character')
    con    <- textConnection('stdout', 'wr', local = TRUE)
    
    sink(con)
    #do the stuff
    
    for (i in 1:(dim(b)[1]))
    {
      if (i == 1) {cat("data$model=", sep="")}
      if ((i < (dim(b)[1])) & (i > 0)) {cat("(","data$",b[i,1],"*", b[i,2],")"
                                            ," +", "\n", sep="")}
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
#ci.mean(correlations[,1],normal=F)

# Opción 1. Correlación basada en el promedio de los 10-folds
ci.mean(values_corr_model[,1],normal=T)

# Opción 2. Correlación basada en el dataset re-constituido a partir de cada fold
nuevo_ytest_model2
cor.test(nuevo_ytest_model2[,1], nuevo_ytest_model2[,2], method="pearson")

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AO")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet[,2:386])
Y2 = BBDDmet$ac_oliva_ajust
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"
                                          ," +", "\n", sep="")}
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

Scores = data.frame(cbind(Scores, modelo))
colnames(Scores) = c("id","ac_oliva_ajust","ac_oliva_ajust","olivatot_ajust",
                     "m.AOTOT", "m.AOVE", "m.AO")

export(Scores, "Scores.xlsx")

save.image(file = "20230602.v1.ac_oliva_ajust.RData")

rm(a, abc_full, abc_set, abc_set_values, alp, b, bT, con, cor_values_pearson, 
   cor_values_spearman, corr_obj, corr_test_pearson, corr_test_spearman, correr,
   cv, cvfit, cvfit_full, data, fit, fit_full, folds, full_data, i, indx, inter,
   j, l, list_metabo, list_metabo_full, listado_selected, listado_selected_full,
   maximo, mean_values_metabo, mean_values_metabo_matrix, 
   mean_values_metabo_sorted, met, metabos_NA_model, method, minimo, model.met,
   modelo, new_abc_full, new_mean_values_metabo, new_mean_values_metabo_sorted,
   nuevo_ytest_model, nuevo_ytest_model2, AO, AO2, plot_negative, plot_positive,
   plot1, plot2, rows, sample, selected_mean_values_metabo_sorted,
   selected_mean_values_metabo_sorted_negative, 
   selected_mean_values_metabo_sorted_positive, set_test, set_train, si_model,
   stdout, stdout_full, test, test.data, train, train.data, training,
   type.lambda, values_corr_model, values_metabo, values_metabo_full,
   variables_get, variables_get_full, variables_model, variables_model_full,
   variables_values, variables_values_full, variables2, X.full, X.test,
   X.test_saved, X.train, X.train_saved, X2, Y.full, Y.test, Y.test_saved,
   Y.train, Y2, Y.train_saved)

## 2.2. Validación interna visita anual (1 año). ####

## Preparacion de los datos generales 

###Fusion de las BBDD.
summary(BBDD$ac_oliva3)
summary(BBDD$ac_olivavir3)
summary(BBDD$olivatot3)

## Eliminacion participantes sin FFQ

BBDD = subset (BBDD, olivatot3 >= 0) 

## Eliminación extremos de energia

BBDD$sub1 = 2 

for (i in 1:length(BBDD$sub1)){
  if (BBDD$energiat3[i]>=4000 & BBDD$sex[i]== "man") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat3[i]<=800 & BBDD$sex[i]== "man") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat3[i]>=3500 & BBDD$sex[i]== "woman") {BBDD$sub1[i]='1'} 
  else if (BBDD$energiat3[i]<=500 & BBDD$sex[i]== "woman") {BBDD$sub1[i]='1'} 
  else {BBDD$sub1[i]='0'}}
table(BBDD$sub1)

BBDD = subset(BBDD, sub1 == 0)


## Variables ajustadas a energía:

BBDD<-BBDD[order(-BBDD$ac_oliva3),]

df<- data.frame(y = BBDD$ac_oliva3,
                y1= BBDD$ac_oliva3,
                x = BBDD$energiat3) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 9.63)
BBDD$ac_oliva_ajust3 <- values

BBDD[,c("ac_oliva3", "ac_oliva_ajust3")]
rm("df1", "values", "df")

BBDD<-BBDD[order(-BBDD$ac_olivavir3),]

df<- data.frame(y = BBDD$ac_olivavir3,
                y1= BBDD$ac_olivavir3,
                x = BBDD$energiat3) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 34.9)
BBDD$ac_olivavir_ajust3 <- values

BBDD[,c("ac_olivavir3", "ac_olivavir_ajust3")]
rm("df1", "values", "df")

BBDD<-BBDD[order(-BBDD$olivatot3),]

df<- data.frame(y = BBDD$olivatot3,
                y1= BBDD$olivatot3,
                x = BBDD$energiat3) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 44.54)
BBDD$olivatot_ajust3 <- values

BBDD[,c("olivatot3", "olivatot_ajust3")]
rm("df1", "values", "df")

## Preparación de datos metabolomicos.

## Selección de los metabolitos y la/s variable/s dependientes BASALES

merged_dataset_filt = BBDD

colnames(BBDD)[1000:1252]

colnames(BBDD[,839:887])

short_db1<-cbind(id=merged_dataset_filt[,1],
                 merged_dataset_filt[,234:381],
                 merged_dataset_filt[,587:789],
                 merged_dataset_filt[,839:887],
                 merged_dataset_filt[,1250:1252])
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
                        "taurodeoxychenodeox","ac_oliva_ajust","ac_olivavir_ajust","olivatot_ajust")

## Eliminar los metabolitos que son estándares:
drop <- c("c240pc", "glycocholated4", "thymined4", "inosine15n4", #QC
          "acetaminophen", "cyclohexylamine", "metronidazole", "valsartan",
          "warfarin", "verapamil","metformin", "atenolol") #Drugs
short_db1 = short_db1[,!(names(short_db1) %in% drop)]

## Determinar missing values en los sujetos y eliminar aquellos con > 20%
# Check Subjects with NAs
subjects_NA<-as.data.frame(rowSums(is.na(short_db1)))
otra<-cbind(short_db1$id, subjects_NA)
df <-  short_db1
subjects_NA<-as.data.frame(rowSums(is.na(df)))
otra<-cbind(df$id, subjects_NA)
rows_remove<-otra$`df$id`[otra$`rowSums(is.na(df))`>78]
newdata <- subset(df, !id %in% rows_remove)
short_db1<-newdata

## Determinar missing values en metabolitos
#Generate database with only metabolites
quantitative_variables1<-short_db1[,2:(length(short_db1)-3)]
#names(quantitative_variables1)

#NAs in each variable
na_count1 <-sapply(quantitative_variables1, 
                   function(quantitative_variables1) sum(length(which(
                     is.na(quantitative_variables1)))))
na_count2 <-sapply(quantitative_variables1,
                   function(quantitative_variables1) (100*sum(length(which(
                     is.na(quantitative_variables1))))/sum(length((
                       quantitative_variables1)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)
#na_count

#Metabolites with NA higher than 20%
metabo_high_NA<-rownames(na_count[na_count[,2]>20 ,])

#Remove variables with high number of missing values (>20)
drop <- metabo_high_NA
quantitative_variables1_filtered = quantitative_variables1[,!(
  names(quantitative_variables1) %in% drop)]
na_count1 <-sapply(quantitative_variables1_filtered,
                   function(quantitative_variables1_filtered) sum(length(which(
                     is.na(quantitative_variables1_filtered)))))
na_count2 <-sapply(quantitative_variables1_filtered,
                   function(quantitative_variables1_filtered) (100*sum(length(
                     which(is.na(quantitative_variables1_filtered))))/sum(
                       length((quantitative_variables1_filtered)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

na_count = data.frame(na_count)
na_count$names = rownames(na_count)

export(na_count, "na_count1a.xlsx")

## Tratamiento de missing values --> missforest.
set.seed(1)

IMP = missForest(as.matrix(quantitative_variables1_filtered), verbose = T)

IMP$OOBerror

IMP = data.frame(IMP$ximp)

colnames(IMP)

## Normalizacion con ranknorm:

IMP = apply(IMP, 2, RankNorm)

BBDDmet_1a = data.frame(cbind(short_db1[1], IMP, short_db1[395:397]))

export(BBDDmet_1a, "20230602_BBDDmet_1a_rkn.xlsx")

rm(BBDD1, BBDD2, df, IMP, iqrs, medianas, merged_dataset_filt, otra, na_count,
   na_count1, na_count2, metabo_high_NA, inter_qvf, quantitative_variables1,
   quantitative_variables1_filtered, short_db1, subjects_NA, x, drop, i,
   rows_remove, newdata)

## Validación con correlaciones de Pearson.
# Modelos variables ajustadas:
# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOTOT")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet_1a[,2:385])
Y2 = BBDDmet_1a$olivatot_ajust
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

Scores_1a = data.frame(cbind(BBDDmet_1a[1],BBDDmet_1a[383:385], modelo))
colnames(Scores_1a) = c("id","ac_oliva_ajust","ac_olivavir_ajust","olivatot_ajust",
                     "m.AOTOT")

export(Scores_1a, "Scores_1a.xlsx")

save.image(file = "20230605.v2.olivatot_1a_ajust.RData")

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOVE")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet_1a[,2:385])
Y2 = BBDDmet_1a$ac_olivavir_ajust
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
colnames(Scores_1a) = c("id","ac_oliva_ajust","ac_olivavir_ajust","olivatot_ajust",
                        "m.AOTOT_ajust", "m.AOVE")

export(Scores_1a, "Scores_1a.xlsx")

save.image(file = "20230605.v2.ac_olivavir_1a_ajust.RData")

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AO")

model.met = met
str(model.met)

X2 = data.frame(BBDDmet_1a[,2:385])
Y2 = BBDDmet_1a$ac_oliva_ajust
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
colnames(Scores_1a) = c("id","ac_oliva_ajust","ac_olivavir_ajust","olivatot_ajust",
                        "m.AOTOT", "m.AOVE","m.AO")

export(Scores_1a, "Scores_1a.xlsx")

save.image(file = "20230605.v2.ac_oliva_1a_ajust.RData")

rm(b, correr, met, model.met, X2, con, i, no_cores, stdout, stdout_full, Y2, modelo)

# Fase 3: ####
# inferencia con incidencia de DT2 (caso-cohorte DT2) y con incidencia de ECV 
# (caso-cohorte ECV). 

## 3.1: Modelos de Cox con DT2 basal. ####

T2D <- read_csv("BBDD/T2Dprojectweighted.csv")
BBDD_T2D = merge(T2D, BBDDmet, by = "id")
propensity <- read_csv("BBDD/propensity.csv")
BBDD_T2D = merge(BBDD_T2D, propensity, by = "id")
Alimentos <- read_sav("BBDD/FFQ.SAV")
BBDD_T2D = merge(BBDD_T2D, Alimentos, by = "id")
load("BBDD/vars_necesarias_updated.Rdata")
BBDD_T2D = merge(BBDD_T2D, sub_base, by = "id")
Scores_base = data.frame(cbind(Scores[1], Scores[5:7]))
BBDD_T2D = merge(BBDD_T2D, Scores_base, by = "id")
colnames(BBDD_T2D)

BBDD_T2D$m.AOTOT.1 = BBDD_T2D$m.AOTOT/sd(BBDD_T2D$m.AOTOT)
BBDD_T2D$m.AO.1 = BBDD_T2D$m.AO/sd(BBDD_T2D$m.AO)
BBDD_T2D$m.AOVE.1 = BBDD_T2D$m.AOVE/sd(BBDD_T2D$m.AOVE)

colnames(BBDD_T2D)

approaches <- c("m.AOTOT.1",
                "m.AO.1",
                "m.AOVE.1")

foods <- c("olivatot_ajust",
           "ac_oliva_ajust",
           "ac_olivavir_ajust")

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

formu_model1 <- list()
fit_model1 <- list()
formu_model2 <- list()
fit_model2 <- list()
formu_model3 <- list()
fit_model3 <- list()
formu_model4 <- list()
fit_model4 <- list()

for (i in 1:3){
  
  formu_model1[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         as.factor(nodo) + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(nodo))"))
  fit_model1[[i]] <- coxph(formu_model1[[i]], weights=w, data = BBDD_T2D)
  
  formu_model2[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         as.factor(nodo) + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(nodo)) +
                                         bmi + as.factor(smoking) + alcoholg + 
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0)"))
  fit_model2[[i]] <- coxph(formu_model2[[i]], weights=w, data = BBDD_T2D)
  
  formu_model3[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         as.factor(nodo) + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(nodo)) + bmi +
                                         as.factor(smoking) + alcoholg +
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0) + verdutot + 
                                         frutatot + grupocer + fsecos + huevos +
                                         legumbre + pescados + carnicos +
                                         lacteos"))
  fit_model3[[i]] <- coxph(formu_model3[[i]], weights=w, data = BBDD_T2D)
  
  formu_model4[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         as.factor(nodo) + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(nodo)) + bmi + 
                                         as.factor(smoking) + alcoholg + 
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0) +
                                         verdutot + frutatot + grupocer + fsecos
                                         + huevos + legumbre + pescados + 
                                         carnicos + lacteos + ", foods[i]))
  fit_model4[[i]] <- coxph(formu_model4[[i]], weights=w, data = BBDD_T2D)
}

results_model1 <- matrix(NA, 3, 4)
results_model2 <- matrix(NA, 3, 4)
results_model3 <- matrix(NA, 3, 4)
results_model4 <- matrix(NA, 3, 4)

rownames(results_model1) <- c("model_m.AOTOT_1",
                              "model_m.AO_1",
                              "model_m.AOVE_1")

rownames(results_model2)<-rownames(results_model1)
rownames(results_model3)<-rownames(results_model1)
rownames(results_model4)<-rownames(results_model1)

colnames(results_model1)<-c("HR", "lower 95% CI", "upper 95% CI", "P_value")
colnames(results_model2)<-colnames(results_model1)
colnames(results_model3)<-colnames(results_model1)
colnames(results_model4)<-colnames(results_model1)

for (i in 1:3){
  get_data_sum <- summary(fit_model1[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model1[i,1] <- round(hr_data[1], 3)
  results_model1[i,2] <- round(hr_data[2], 3)
  results_model1[i,3] <- round(hr_data[3], 3)
  results_model1[i,4] <- round(get_data_sum$coefficients[1,6], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model2[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model2[i,1] <- round(hr_data[1], 3)
  results_model2[i,2] <- round(hr_data[2], 3)
  results_model2[i,3] <- round(hr_data[3], 3)
  results_model2[i,4] <- round(get_data_sum$coefficients[1,6], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model3[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model3[i,1] <- round(hr_data[1], 3)
  results_model3[i,2] <- round(hr_data[2], 3)
  results_model3[i,3] <- round(hr_data[3], 3)
  results_model3[i,4] <- round(get_data_sum$coefficients[1,6], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model4[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model4[i,1] <- round(hr_data[1], 3)
  results_model4[i,2] <- round(hr_data[2], 3)
  results_model4[i,3] <- round(hr_data[3], 3)
  results_model4[i,4] <- round(get_data_sum$coefficients[1,6], 3)  # Pvalue
}

full_results <- rbind(c("Model 1"),results_model1, 
                      c("Model 2"),results_model2,
                      c("Model 3"),results_model3,
                      c("Model 4"),results_model4)

modelos = data.frame(full_results)
modelos$names = rownames(modelos)
export(modelos, "modelos_T2D_basal_ajus_wth drugs_20230605.xlsx")
export(BBDD_T2D, "BBDD_T2D_20220803.csv")

table(BBDD_T2D$cens)

rm(approaches, con, fit_model1, fit_model2, fit_model3, fit_model4, foods, 
   formu_model1, formu_model2, formu_model3, formu_model4, full_results, 
   get_data_sum, hr_data, modelos, results_model1, results_model2, 
   results_model3, results_model4, i)

## 3.2: Modelos de Cox con DT2 1 año. ####

# Merge de las BBDD

ID <- read_excel("BBDD/ID.xlsx")
metabolites <- read_csv("BBDD/metabolites.csv")
Met_1a_T2D = merge(ID, metabolites, by = "id")

# Normalización de los metabolitos:

colnames(Met_1a_T2D)

Met_1a_T2D = cbind(id=Met_1a_T2D[,1],
                 Met_1a_T2D[,234:381],
                 Met_1a_T2D[,588:789],
                 Met_1a_T2D[,842:887]) 
colnames(Met_1a_T2D)

colnames(Met_1a_T2D) = c("id","glycine","alanine","serine","threonine","methionine",                 
                         "glutamate","asparagine","glutamine","histidine","arginine","lysine",                     
                         "valine","leucine","isoleucine","phenylalanine","tyrosine","tryptophan",                 
                         "proline","hydroxyproline","ornithine","citrulline","taurine","gaba",                       
                         "dimethylglycine","adma","sdma","nmma","allantoin","aminoisobutyricacid",        
                         "kynurenicacid","methylhistamine","hydroxyanthranilicacid","ncarbamoylbetaalanine","thiamine","niacinamide",                
                         "betaine","choline","phosphocholine","alphaglycerophosphocholine","acetylcholine","creatine",                   
                         "creatinine","thyroxine","trimethylaminenoxide","adenosine","cytosine","xanthosine",                 
                         "cotinine","pipecolicacid","pyroglutamicacid","methylnicotinamide","methioninesulfoxide","sarcosine",                  
                         "betaalanine","anserine","carnitine","c2carnitine","c3carnitine","c3dcch3carnitine",           
                         "c4carnitine","c4Ohcarnitine","c5carnitine","c51carnitine","c5dccarnitine","c6carnitine",                
                         "c7carnitine","c8carnitine","c9carnitine","c10carnitine","c102carnitine","c12carnitine",               
                         "c121carnitine","c14carnitine","c141carnitine","c142carnitine","c16carnitine","c18carnitine",               
                         "c181carnitine","c181Ohcarnitine","c182carnitine","c20carnitine","c204carnitine","c26carnitine",               
                         "methyladenosine","acetaminophen","bilirubin","cortisol","cortisone","cyclohexylamine",            
                         "ectoine","guanidoaceticacid","hypoxanthine","inosine","metronidazole","myristoleicacid",            
                         "n6acetyllysine","nacetylornithine","pseudouridine","sphinganine","trimethylbenzene","uricacid",                   
                         "urocanicacid","xanthine","butyrobetaine","dz","methylguanosine","methylhistidine",            
                         "nalphaacetyarginine","ed","methylguanine","nacetylcysteinylacetaminoph","acetamidobutanoate","guanidinobutanoicacid",      
                         "hydroxy3methylacetophenone","hydroxyhippurate","acetylamino6amino3methylur","el","atenolol","biliverdin",                 
                         "caffeine","deoxycortisol","dmgv","gabapentin","guanine","homoarginine",               
                         "hydroxycotinine","linoleoylethanolamide","metformin","n1methyl2pyridone5carboxami","n4acetylcytidine","nacetylasparticacid",        
                         "nacetylputrescine","nacetylspermidine","nmethylproline","oleoylglycine","pantothenol","phenylacetylglutamine",      
                         "piperine","prolinebetaine","quinine","ribothymidine","sphingosine","sulfamethoxazole",           
                         "trigonellinenmethylnicotinat","trimethyllysine","valsartan","verapamil","warfarin","c140lpc",                    
                         "c161lpc","c160lpc","c182lpc","c181lpc","c180lpc","c205lpc",                    
                         "c204lpc","c203lpc","c226lpc","c160lpe","c182lpe","c181lpe",                    
                         "c180lpe","c204lpe","c226lpe","c301pc","c300pc","c322pc",                     
                         "c321pc","c320pc","c344pc","c343pc","c342pc","c341pc",                     
                         "c340pc","c364pca","c364pcb","c363pc","c362pc","c361pc",                     
                         "c360pc","c386pc","c384pc","c383pc","c382pc","c4010pc",                    
                         "c409pc","c406pc","c345pcplasmalogen","c343pcplasmalogen","c342pcplasmalogen","c341pcplasmalogena",         
                         "c341pcplasmalogenb","c365pcplasmalogena","c365pcplasmalogenb","c364pcplasmalogen","c363pcplasmalogen","c362pcplasmalogen",          
                         "c361pcplasmalogen","c387pcplasmalogen","c386pcplasmalogen","c384pcplasmalogen","c407pcplasmalogen","c320pe",                     
                         "c342pe","c340pe","c364pe","c363pe","c362pe","c361pe",                     
                         "c386pe","c385pe","c384pe","c382pe","c406pe","c343peplasmalogen",          
                         "c342peplasmalogen","c365peplasmalogen","c364peplasmalogen","c363peplasmalogen","c362peplasmalogen","c361peplasmalogen",          
                         "c387peplasmalogen","c386peplasmalogen","c385peplasmalogen","c383peplasmalogen","c407peplasmalogen","c4211peplasmalogen",         
                         "c340pi","c384pi","c340ps","c406ps","c363psplasmalogen","c362psplasmalogen",          
                         "c361psplasmalogen","c160ceramided181","c220ceramided181","c240ceramided181","c241ceramided181","c140sm",                     
                         "c161sm","c160sm","c182sm","c181sm","c180sm","c200sm",                     
                         "c221sm","c220sm","c241sm","c240sm","c140ce","c161ce",                     
                         "c160ce","c183ce","c182ce","c181ce","c180ce","c205ce",                     
                         "c204ce","c203ce","c226ce","c225ce","c224ce","c141mag",                    
                         "c161mag","c180mag","c221mag","c300dag","c321dag","c320dag",                    
                         "c343dag","c342dag","c341dag","c340dag","c363dag","c362dag",                    
                         "c361dag","c360dag","c364dag","c385dag","c384dag","c420tag",                    
                         "c442tag","c441tag","c440tag","c463tag","c462tag","c461tag",                    
                         "c460tag","c484tag","c483tag","c482tag","c481tag","c480tag",                    
                         "c505tag","c504tag","c503tag","c502tag","c501tag","c500tag",                    
                         "c527tag","c526tag","c525tag","c524tag","c523tag","c522tag",                    
                         "c521tag","c520tag","c5410tag","c549tag","c548tag","c547tag",                    
                         "c546tag","c545tag","c544tag","c543tag","c542tag","c541tag",                    
                         "c569tag","c568tag","c567tag","c566tag","c565tag","c564tag",                    
                         "c563tag","c562tag","c561tag","c5811tag","c5810tag","c589tag",                    
                         "c588tag","c587tag","c586tag","c6012tag","c200lpe","c220lpe",                    
                         "c342hydroxypc","c451tag","c471tag","c491tag","c492tag","c493tag",                    
                         "c510tag","c511tag","c512tag","c513tag","c532tag","c533tag",                    
                         "c552tag","c553tag","cholesterol","aminoadipate","alphaglycerophosphate","pyridoxate",                 
                         "aconitate","adipate","amo","adp","citrate","isocitrate",                 
                         "hexosemonophosphate","fructoseglucosegalactose","fumaratemaleate","gdp","glucuronate","gmp",                        
                         "hippurate","kynurenine","lactate","lactose","malate","oxalate",                    
                         "pantothenate","pyruvate","quinolinate","salicylurate","sorbitol","succinate",                  
                         "sucrose","udp","uracil","urate","uridine","glycocholate",               
                         "glycodeoxychenodeox","suberate","indoxylsulfate","indole3propionate","gentisate","phosphocreatine",            
                         "alphahydroxybutyrate","betahydroxybutyrate","hydroxyglutarate","inositol","methyladipatepimelate","phosphoglycerate",           
                         "taurodeoxychenodeox")

## Eliminar los metabolitos que son estándares: C240PC, glycocholated4, thymined4, inosine15N4
drop <- c("c240pc", "glycocholated4", "thymined4", "inosine15n4", #QC
          "acetaminophen", "cyclohexylamine", "metronidazole", "valsartan", 
          "warfarin", "verapamil", "metformin", "atenolol")#drugs
Met_1a_T2D = Met_1a_T2D[,!(names(Met_1a_T2D) %in% drop)]

## Determinar missing values en los sujetos y eliminar aquellos con > 20%
# Check Subjects with NAs
subjects_NA<-as.data.frame(rowSums(is.na(Met_1a_T2D)))
otra<-cbind(Met_1a_T2D$id, subjects_NA)
df <-  Met_1a_T2D
subjects_NA<-as.data.frame(rowSums(is.na(df)))
otra<-cbind(df$id, subjects_NA)
# threshold based on this databases 0.2*390
rows_remove<-otra$`df$id`[otra$`rowSums(is.na(df))`>78]
newdata <- subset(df, !id %in% rows_remove)
Met_1a_T2D<-newdata

## Determinar missing values en metabolitos
#Generate database with only metabolites
quantitative_variables1<-Met_1a_T2D[,2:(length(Met_1a_T2D))]
#names(quantitative_variables1)

#NAs in each variable
na_count1 <-sapply(quantitative_variables1, 
                   function(quantitative_variables1) sum(length(which(
                     is.na(quantitative_variables1)))))
na_count2 <-sapply(quantitative_variables1, 
                   function(quantitative_variables1) (100*sum(length(which(
                     is.na(quantitative_variables1))))/sum(length((
                       quantitative_variables1)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)
#na_count

#Metabolites with NA higher than 20%
metabo_high_NA<-rownames(na_count[na_count[,2]>20 ,])

#Remove variables with high number of missing values (>20)
drop <- metabo_high_NA
quantitative_variables1_filtered = quantitative_variables1[,!(names(
  quantitative_variables1) %in% drop)]
na_count1 <-sapply(quantitative_variables1_filtered,
                   function(quantitative_variables1_filtered) sum(length(which(
                     is.na(quantitative_variables1_filtered)))))
na_count2 <-sapply(quantitative_variables1_filtered,
                   function(quantitative_variables1_filtered) (100*sum(length(
                     which(is.na(quantitative_variables1_filtered))))/sum(
                       length((quantitative_variables1_filtered)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

na_count = data.frame(na_count)
na_count$names = rownames(na_count)

## Tratamiento de missing values --> missforest.
set.seed(1)

IMP = missForest(as.matrix(quantitative_variables1_filtered), verbose = T)
IMP$OOBerror
IMP = data.frame(IMP$ximp)
colnames(IMP)

## Normalizacion con ranknorm:

IMP = apply(IMP, 2, RankNorm)

Met_1a_T2D = data.frame(cbind(Met_1a_T2D[1], IMP))

export(Met_1a_T2D, "Met_1a_T2D_20230605.xlsx")

rm(df, IMP, merged_dataset_filt, otra, na_count, na_count1, na_count2, 
   metabo_high_NA, quantitative_variables1, quantitative_variables1_filtered, 
   subjects_NA, drop, i, rows_remove, newdata)

# Calculo de los Scores:

# Modelos variables ajustadas:

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOTOT1a")

model.met = met
str(model.met)

X2 = data.frame(Met_1a_T2D[,2:382])
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"
                                          ," +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
} 

Scores_1a_T2D = data.frame(cbind(Met_1a_T2D[1], modelo))
colnames(Scores_1a_T2D) = c("id","m.AOTOT_ajust")

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOVE1a")

model.met = met
str(model.met)

b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"
                                          ," +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
} 

Scores_1a_T2D = data.frame(cbind(Scores_1a_T2D, modelo))
colnames(Scores_1a_T2D) = c("id", "m.AOTOT_ajust", "m.AOVE_ajust")

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AO1a")

model.met = met
str(model.met)

b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")"
                                          ," +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
} 

Scores_1a_T2D = data.frame(cbind(Scores_1a_T2D, modelo))
colnames(Scores_1a_T2D) = c("id", "m.AOTOT_ajust", "m.AOVE_ajust","m.AO_ajust")
# Fusion con los IDs (704, 161 eventos) con nuevos eventos al año
BBDD_T2D1a = merge(Scores_1a_T2D, T2D, by = "id", all.x = T)
export(BBDD_T2D1a, "BBDD_T2D1a.csv")
#Se generan duplicados con el merge. Exporto y elimino a mano los duplicados.
BBDD_T2D1a = read_delim("BBDD_T2D1a.csv", ";", escape_double = FALSE, trim_ws = TRUE)
BBDD_T2D1a = merge(BBDD_T2D1a, propensity, by = "id")
BBDD_T2D1a = merge(BBDD_T2D1a, Alimentos, by = "id")
BBDD_T2D1a = merge(BBDD_T2D1a, sub_base, by = "id")

export(BBDD_T2D1a, "BBDD_T2D1a_20230605.csv")

table(BBDD_T2D1a$cens)


BBDD_T2D1a$m.AOTOT_ajust.1 = BBDD_T2D1a$m.AOTOT_ajust/sd(BBDD_T2D1a$m.AOTOT_ajust)
BBDD_T2D1a$m.AO_ajust.1 = BBDD_T2D1a$m.AO_ajust/sd(BBDD_T2D1a$m.AO_ajust)
BBDD_T2D1a$m.AOVE_ajust.1 = BBDD_T2D1a$m.AOVE_ajust/sd(BBDD_T2D1a$m.AOVE_ajust)

# Ajuste de variables a energia:

BBDD_T2D1a<-BBDD_T2D1a[order(-BBDD_T2D1a$olivatot3),]

df<- data.frame(y = BBDD_T2D1a$olivatot3,
                y1= BBDD_T2D1a$olivatot3,
                x = BBDD_T2D1a$energiat3) 

df1 <- lm(y~x, data=df)
values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 
                                              mean(BBDD_T2D1a$olivatot3, 
                                                   na.rm = T))
BBDD_T2D1a$olivatot_ajust3 <- values

BBDD_T2D1a[,c("olivatot3", "olivatot_ajust3")]
rm("df1", "values", "df")

BBDD_T2D1a<-BBDD_T2D1a[order(-BBDD_T2D1a$ac_oliva3),]

df<- data.frame(y = BBDD_T2D1a$ac_oliva3,
                y1= BBDD_T2D1a$ac_oliva3,
                x = BBDD_T2D1a$energiat3) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 
                                              mean(BBDD_T2D1a$ac_oliva3, 
                                                   na.rm = T))
BBDD_T2D1a$ac_oliva_ajust3 <- values

BBDD_T2D1a[,c("ac_oliva3", "ac_oliva_ajust3")]
rm("df1", "values", "df")

BBDD_T2D1a<-BBDD_T2D1a[order(-BBDD_T2D1a$ac_olivavir3),]

df<- data.frame(y = BBDD_T2D1a$ac_olivavir3,
                y1= BBDD_T2D1a$ac_olivavir3,
                x = BBDD_T2D1a$energiat3) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 
                                              mean(BBDD_T2D1a$ac_olivavir3, 
                                                   na.rm = T))
BBDD_T2D1a$ac_olivavir_ajust3 <- values

BBDD_T2D1a[,c("ac_olivavir3", "ac_olivavir_ajust3")]
rm("df1", "values", "df")

# Analisis de Cox a 1 a:

colnames(BBDD_T2D1a)

approaches <- c("m.AOTOT_ajust.1",
                "m.AO_ajust.1",
                "m.AOVE_ajust.1")

foods <- c("olivatot_ajust3",
           "ac_oliva_ajust3",
           "ac_olivavir_ajust3")

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

formu_model1 <- list()
fit_model1 <- list()
formu_model2 <- list()
fit_model2 <- list()
formu_model3 <- list()
fit_model3 <- list()
formu_model4 <- list()
fit_model4 <- list()

for (i in 1:3){
  
  formu_model1[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(nodo))"))
  fit_model1[[i]] <- coxph(formu_model1[[i]], weights=w, data = BBDD_T2D1a)
  
  formu_model2[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(nodo)) +
                                         bmi + as.factor(smoking) + alcoholg + 
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0)"))
  fit_model2[[i]] <- coxph(formu_model2[[i]], weights=w, data = BBDD_T2D1a)
  
  formu_model3[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(nodo)) +
                                         bmi + as.factor(smoking) + alcoholg + 
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0) + verdutot3 +
                                         frutatot3 + grupocer3 + fsecos3 + 
                                         huevos3 + legumbre3 + pescados3 + 
                                         carnicos3 + lacteos3"))
  fit_model3[[i]] <- coxph(formu_model3[[i]], weights=w, data = BBDD_T2D1a)
  
  formu_model4[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(nodo)) + bmi + 
                                         as.factor(smoking) + alcoholg + 
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0) + verdutot3 + 
                                         frutatot3 + grupocer3 + fsecos3 + 
                                         huevos3 + legumbre3 + pescados3 + 
                                         carnicos3 + lacteos3 + ", foods[i]))
  fit_model4[[i]] <- coxph(formu_model4[[i]], weights=w, data = BBDD_T2D1a)
}

results_model1 <- matrix(NA, 3, 4)
results_model2 <- matrix(NA, 3, 4)
results_model3 <- matrix(NA, 3, 4)
results_model4 <- matrix(NA, 3, 4)

rownames(results_model1) <- c("model_m.AOTOT_ajust.1",
                              "model_m.AO_ajust.1",
                              "model_m.AOVE_ajust.1")

rownames(results_model2)<-rownames(results_model1)
rownames(results_model3)<-rownames(results_model1)
rownames(results_model4)<-rownames(results_model1)

colnames(results_model1)<-c("HR", "lower 95% CI", "upper 95% CI", "P_value")
colnames(results_model2)<-colnames(results_model1)
colnames(results_model3)<-colnames(results_model1)
colnames(results_model4)<-colnames(results_model1)

for (i in 1:3){
  get_data_sum <- summary(fit_model1[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model1[i,1] <- round(hr_data[1], 3)
  results_model1[i,2] <- round(hr_data[2], 3)
  results_model1[i,3] <- round(hr_data[3], 3)
  results_model1[i,4] <- round(get_data_sum$coefficients[1,6], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model2[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model2[i,1] <- round(hr_data[1], 3)
  results_model2[i,2] <- round(hr_data[2], 3)
  results_model2[i,3] <- round(hr_data[3], 3)
  results_model2[i,4] <- round(get_data_sum$coefficients[1,6], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model3[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model3[i,1] <- round(hr_data[1], 3)
  results_model3[i,2] <- round(hr_data[2], 3)
  results_model3[i,3] <- round(hr_data[3], 3)
  results_model3[i,4] <- round(get_data_sum$coefficients[1,6], 3)  # Pvalue
  
  get_data_sum <- summary(fit_model4[[i]])
  hr_data <- get_data_sum$conf.int[1,c("exp(coef)", "lower .95", "upper .95")]
  
  results_model4[i,1] <- round(hr_data[1], 3)
  results_model4[i,2] <- round(hr_data[2], 3)
  results_model4[i,3] <- round(hr_data[3], 3)
  results_model4[i,4] <- round(get_data_sum$coefficients[1,6], 3)  # Pvalue
}

full_results <- rbind(c("Model 1"),results_model1, 
                      c("Model 2"),results_model2,
                      c("Model 3"),results_model3,
                      c("Model 4"),results_model4)

modelos = data.frame(full_results)
modelos$names = rownames(modelos)
export(modelos, "modelos_T2D_1a_ajus_wth drugs_202306054.xlsx")
export(BBDD_T2D1a, "BBDD_T2D1a_20230605.csv")

table(BBDD_T2D1a$cens)

rm(approaches, fit_model1, fit_model2, fit_model3, fit_model4, foods, 
   formu_model1, formu_model2, formu_model3, formu_model4, full_results, 
   get_data_sum, hr_data, modelos, results_model1, results_model2, 
   results_model3, results_model4, i, con, modelo, stdout, stdout_full, X2)

# Fase 3.3 Modelos de Cox con ECV basal

CVD <- read_csv("BBDD/CVDprojectweighted.csv")
BBDD_CVD = merge(CVD, BBDDmet, by = "id")
propensity <- read_csv("BBDD/propensity.csv")
BBDD_CVD = merge(BBDD_CVD, propensity, by = "id")
Alimentos <- read_sav("BBDD/FFQ.SAV")
BBDD_CVD = merge(BBDD_CVD, Alimentos, by = "id")
load("BBDD/vars_necesarias_updated.Rdata")
BBDD_CVD = merge(BBDD_CVD, sub_base, by = "id")
BBDD_CVD = merge(BBDD_CVD, Scores_base, by = "id")
colnames(BBDD_CVD)

table(BBDD_CVD$cens)

BBDD_CVD$m.AOTOT.1 = BBDD_CVD$m.AOTOT/sd(BBDD_CVD$m.AOTOT)
BBDD_CVD$m.AO.1 = BBDD_CVD$m.AO/sd(BBDD_CVD$m.AO)
BBDD_CVD$m.AOVE.1 = BBDD_CVD$m.AOVE/sd(BBDD_CVD$m.AOVE)

colnames(BBDD_CVD)

approaches <- c("m.AOTOT.1",
                "m.AO.1",
                "m.AOVE.1")

foods <- c("olivatot_ajust",
           "ac_oliva_ajust",
           "ac_olivavir_ajust")

names(BBDD_CVD)[names(BBDD_CVD) == 'edad0'] <- 'age'
names(BBDD_CVD)[names(BBDD_CVD) == 'sexo'] <- 'sex'
names(BBDD_CVD)[names(BBDD_CVD) == 'diabetes0'] <- 'diabetes'
names(BBDD_CVD)[names(BBDD_CVD) == 'ps1.x'] <- 'ps1'
names(BBDD_CVD)[names(BBDD_CVD) == 'ps2.x'] <- 'ps2'
names(BBDD_CVD)[names(BBDD_CVD) == 'centro'] <- 'center'
names(BBDD_CVD)[names(BBDD_CVD) == 'fum'] <- 'smoking'
names(BBDD_CVD)[names(BBDD_CVD) == 'grup_int'] <- 'interv_g'
names(BBDD_CVD)[names(BBDD_CVD) == 'getota_1.x'] <- 'getota_1'
names(BBDD_CVD)[names(BBDD_CVD) == 'ant_fam'] <- 'fam_history'
names(BBDD_CVD)[names(BBDD_CVD) == 'imc1'] <- 'bmi'
names(BBDD_CVD)[names(BBDD_CVD) == 'tra_col0.x'] <- 'tra_col0'
names(BBDD_CVD)[names(BBDD_CVD) == 'hta0.x'] <- 'hta0'
names(BBDD_CVD)[names(BBDD_CVD) == 'trathta0.x'] <- 'trathta0'
names(BBDD_CVD)[names(BBDD_CVD) == 'hipercol0.x'] <- 'hipercol0'

formu_model1 <- list()
fit_model1 <- list()
formu_model2 <- list()
fit_model2 <- list()
formu_model3 <- list()
fit_model3 <- list()
formu_model4 <- list()
fit_model4 <- list()

for (i in 1:3){
  
  formu_model1[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", 
                                         approaches[i], 
                                         " + age + sex + ps1 + ps2 +
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(center))"))
  fit_model1[[i]] <- coxph(formu_model1[[i]], weights=w, data = BBDD_CVD)
  
  formu_model2[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", 
                                         approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(center)) +
                                         bmi + as.factor(smoking) + alcoholg + 
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0)"))
  fit_model2[[i]] <- coxph(formu_model2[[i]], weights=w, data = BBDD_CVD)
  
  formu_model3[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(center)) +
                                         bmi + as.factor(smoking) + alcoholg + 
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0) + verdutot + 
                                         frutatot + grupocer + fsecos + huevos +
                                         legumbre + pescados + carnicos +
                                         lacteos"))
  fit_model3[[i]] <- coxph(formu_model3[[i]], weights=w, data = BBDD_CVD)
  
  formu_model4[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(center)) +
                                         bmi + as.factor(smoking) + alcoholg + 
                                         I(alcoholg^2) + educat + getota_1 + 
                                         as.factor(fam_history) + 
                                         as.factor(hipercol0) + as.factor(hta0)
                                         + as.factor(tra_col0) + 
                                         as.factor(trathta0) +
                                         verdutot + frutatot + grupocer + 
                                         fsecos + huevos + legumbre + pescados +
                                         carnicos + lacteos + ", foods[i]))
  fit_model4[[i]] <- coxph(formu_model4[[i]], weights=w, data = BBDD_CVD)
}

results_model1 <- matrix(NA, 3, 4)
results_model2 <- matrix(NA, 3, 4)
results_model3 <- matrix(NA, 3, 4)
results_model4 <- matrix(NA, 3, 4)

rownames(results_model1) <- c("model_m.AOTOT.1",
                              "model_m.AO.1",
                              "model_m.AOVE.1")

rownames(results_model2)<-rownames(results_model1)
rownames(results_model3)<-rownames(results_model1)
rownames(results_model4)<-rownames(results_model1)

colnames(results_model1)<-c("HR", "lower 95% CI", "upper 95% CI", "P_value")
colnames(results_model2)<-colnames(results_model1)
colnames(results_model3)<-colnames(results_model1)
colnames(results_model4)<-colnames(results_model1)

for (i in 1:3){
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
                      c("Model 2"),results_model2,
                      c("Model 3"),results_model3,
                      c("Model 4"),results_model4)

modelos = data.frame(full_results)
modelos$names = rownames(modelos)
export(modelos, "modelos_CVD_basal_ajus_wth drugs_20230606.xlsx")
export(BBDD_CVD, "BBDD_CVD_20230606.csv")

table(BBDD_CVD$cens)

rm(approaches, con, fit_model1, fit_model2, fit_model3, fit_model4, foods, 
   formu_model1, formu_model2, formu_model3, formu_model4, full_results, 
   get_data_sum, hr_data, modelos, results_model1, results_model2, 
   results_model3, results_model4, i, model.met, b)

## 3.3: Modelos de Cox con ECV 1 año. ####

# Merge de las BBDD

ID <- BBDD_CVD[1]
Met_1a_CVD = merge(ID, NEW_Combined_metabolites_phenotypes_230218, by = "id")

# Normalización de los metabolitos:

colnames(Met_1a_CVD)

Met_1a_CVD = cbind(id=Met_1a_CVD[,1],
                   Met_1a_CVD[,234:381],
                   Met_1a_CVD[,588:789],
                   Met_1a_CVD[,842:887]) 
colnames(Met_1a_CVD)

colnames(Met_1a_CVD) = c("id","glycine","alanine","serine","threonine","methionine",                 
                         "glutamate","asparagine","glutamine","histidine","arginine","lysine",                     
                         "valine","leucine","isoleucine","phenylalanine","tyrosine","tryptophan",                 
                         "proline","hydroxyproline","ornithine","citrulline","taurine","gaba",                       
                         "dimethylglycine","adma","sdma","nmma","allantoin","aminoisobutyricacid",        
                         "kynurenicacid","methylhistamine","hydroxyanthranilicacid","ncarbamoylbetaalanine","thiamine","niacinamide",                
                         "betaine","choline","phosphocholine","alphaglycerophosphocholine","acetylcholine","creatine",                   
                         "creatinine","thyroxine","trimethylaminenoxide","adenosine","cytosine","xanthosine",                 
                         "cotinine","pipecolicacid","pyroglutamicacid","methylnicotinamide","methioninesulfoxide","sarcosine",                  
                         "betaalanine","anserine","carnitine","c2carnitine","c3carnitine","c3dcch3carnitine",           
                         "c4carnitine","c4Ohcarnitine","c5carnitine","c51carnitine","c5dccarnitine","c6carnitine",                
                         "c7carnitine","c8carnitine","c9carnitine","c10carnitine","c102carnitine","c12carnitine",               
                         "c121carnitine","c14carnitine","c141carnitine","c142carnitine","c16carnitine","c18carnitine",               
                         "c181carnitine","c181Ohcarnitine","c182carnitine","c20carnitine","c204carnitine","c26carnitine",               
                         "methyladenosine","acetaminophen","bilirubin","cortisol","cortisone","cyclohexylamine",            
                         "ectoine","guanidoaceticacid","hypoxanthine","inosine","metronidazole","myristoleicacid",            
                         "n6acetyllysine","nacetylornithine","pseudouridine","sphinganine","trimethylbenzene","uricacid",                   
                         "urocanicacid","xanthine","butyrobetaine","dz","methylguanosine","methylhistidine",            
                         "nalphaacetyarginine","ed","methylguanine","nacetylcysteinylacetaminoph","acetamidobutanoate","guanidinobutanoicacid",      
                         "hydroxy3methylacetophenone","hydroxyhippurate","acetylamino6amino3methylur","el","atenolol","biliverdin",                 
                         "caffeine","deoxycortisol","dmgv","gabapentin","guanine","homoarginine",               
                         "hydroxycotinine","linoleoylethanolamide","metformin","n1methyl2pyridone5carboxami","n4acetylcytidine","nacetylasparticacid",        
                         "nacetylputrescine","nacetylspermidine","nmethylproline","oleoylglycine","pantothenol","phenylacetylglutamine",      
                         "piperine","prolinebetaine","quinine","ribothymidine","sphingosine","sulfamethoxazole",           
                         "trigonellinenmethylnicotinat","trimethyllysine","valsartan","verapamil","warfarin","c140lpc",                    
                         "c161lpc","c160lpc","c182lpc","c181lpc","c180lpc","c205lpc",                    
                         "c204lpc","c203lpc","c226lpc","c160lpe","c182lpe","c181lpe",                    
                         "c180lpe","c204lpe","c226lpe","c301pc","c300pc","c322pc",                     
                         "c321pc","c320pc","c344pc","c343pc","c342pc","c341pc",                     
                         "c340pc","c364pca","c364pcb","c363pc","c362pc","c361pc",                     
                         "c360pc","c386pc","c384pc","c383pc","c382pc","c4010pc",                    
                         "c409pc","c406pc","c345pcplasmalogen","c343pcplasmalogen","c342pcplasmalogen","c341pcplasmalogena",         
                         "c341pcplasmalogenb","c365pcplasmalogena","c365pcplasmalogenb","c364pcplasmalogen","c363pcplasmalogen","c362pcplasmalogen",          
                         "c361pcplasmalogen","c387pcplasmalogen","c386pcplasmalogen","c384pcplasmalogen","c407pcplasmalogen","c320pe",                     
                         "c342pe","c340pe","c364pe","c363pe","c362pe","c361pe",                     
                         "c386pe","c385pe","c384pe","c382pe","c406pe","c343peplasmalogen",          
                         "c342peplasmalogen","c365peplasmalogen","c364peplasmalogen","c363peplasmalogen","c362peplasmalogen","c361peplasmalogen",          
                         "c387peplasmalogen","c386peplasmalogen","c385peplasmalogen","c383peplasmalogen","c407peplasmalogen","c4211peplasmalogen",         
                         "c340pi","c384pi","c340ps","c406ps","c363psplasmalogen","c362psplasmalogen",          
                         "c361psplasmalogen","c160ceramided181","c220ceramided181","c240ceramided181","c241ceramided181","c140sm",                     
                         "c161sm","c160sm","c182sm","c181sm","c180sm","c200sm",                     
                         "c221sm","c220sm","c241sm","c240sm","c140ce","c161ce",                     
                         "c160ce","c183ce","c182ce","c181ce","c180ce","c205ce",                     
                         "c204ce","c203ce","c226ce","c225ce","c224ce","c141mag",                    
                         "c161mag","c180mag","c221mag","c300dag","c321dag","c320dag",                    
                         "c343dag","c342dag","c341dag","c340dag","c363dag","c362dag",                    
                         "c361dag","c360dag","c364dag","c385dag","c384dag","c420tag",                    
                         "c442tag","c441tag","c440tag","c463tag","c462tag","c461tag",                    
                         "c460tag","c484tag","c483tag","c482tag","c481tag","c480tag",                    
                         "c505tag","c504tag","c503tag","c502tag","c501tag","c500tag",                    
                         "c527tag","c526tag","c525tag","c524tag","c523tag","c522tag",                    
                         "c521tag","c520tag","c5410tag","c549tag","c548tag","c547tag",                    
                         "c546tag","c545tag","c544tag","c543tag","c542tag","c541tag",                    
                         "c569tag","c568tag","c567tag","c566tag","c565tag","c564tag",                    
                         "c563tag","c562tag","c561tag","c5811tag","c5810tag","c589tag",                    
                         "c588tag","c587tag","c586tag","c6012tag","c200lpe","c220lpe",                    
                         "c342hydroxypc","c451tag","c471tag","c491tag","c492tag","c493tag",                    
                         "c510tag","c511tag","c512tag","c513tag","c532tag","c533tag",                    
                         "c552tag","c553tag","cholesterol","aminoadipate","alphaglycerophosphate","pyridoxate",                 
                         "aconitate","adipate","amo","adp","citrate","isocitrate",                 
                         "hexosemonophosphate","fructoseglucosegalactose","fumaratemaleate","gdp","glucuronate","gmp",                        
                         "hippurate","kynurenine","lactate","lactose","malate","oxalate",                    
                         "pantothenate","pyruvate","quinolinate","salicylurate","sorbitol","succinate",                  
                         "sucrose","udp","uracil","urate","uridine","glycocholate",               
                         "glycodeoxychenodeox","suberate","indoxylsulfate","indole3propionate","gentisate","phosphocreatine",            
                         "alphahydroxybutyrate","betahydroxybutyrate","hydroxyglutarate","inositol","methyladipatepimelate","phosphoglycerate",           
                         "taurodeoxychenodeox")

## Eliminar los metabolitos que son estándares:
drop <- c("c240pc", "glycocholated4", "thymined4", "inosine15n4", #QC
          "acetaminophen", "cyclohexylamine", "metronidazole", "valsartan", 
          "warfarin", "verapamil", "metformin", "atenolol")#drugs
Met_1a_CVD = Met_1a_CVD[,!(names(Met_1a_CVD) %in% drop)]

## Determinar missing values en los sujetos y eliminar aquellos > 20%
# Check Subjects with NAs
subjects_NA<-as.data.frame(rowSums(is.na(Met_1a_CVD)))
otra<-cbind(Met_1a_CVD$id, subjects_NA)
df <-  Met_1a_CVD
subjects_NA<-as.data.frame(rowSums(is.na(df)))
otra<-cbind(df$id, subjects_NA)
# threshold based on this databases 0.2*390
rows_remove<-otra$`df$id`[otra$`rowSums(is.na(df))`>78]
newdata <- subset(df, !id %in% rows_remove)
Met_1a_CVD<-newdata

## Determinar missing values en metabolitos
#Generate database with only metabolites
quantitative_variables1<-Met_1a_CVD[,2:(length(Met_1a_CVD))]
#names(quantitative_variables1)

#NAs in each variable
na_count1 <-sapply(quantitative_variables1,
                   function(quantitative_variables1) sum(length(which(
                     is.na(quantitative_variables1)))))
na_count2 <-sapply(quantitative_variables1, 
                   unction(quantitative_variables1) (100*sum(length(which(
                     is.na(quantitative_variables1))))/sum(length((
                       quantitative_variables1)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)
#na_count

#Metabolites with NA higher than 20%
metabo_high_NA<-rownames(na_count[na_count[,2]>20 ,])

#Remove variables with high number of missing values (>20)
drop <- metabo_high_NA
quantitative_variables1_filtered = 
  quantitative_variables1[,!(names(quantitative_variables1) %in% drop)]
na_count1 <-sapply(quantitative_variables1_filtered, 
                   function(quantitative_variables1_filtered) sum(length(which(
                     is.na(quantitative_variables1_filtered)))))
na_count2 <-sapply(quantitative_variables1_filtered, 
                   function(quantitative_variables1_filtered) (100*sum(length(
                     which(is.na(quantitative_variables1_filtered))))/sum(
                       length((quantitative_variables1_filtered)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

na_count = data.frame(na_count)
na_count$names = rownames(na_count)

## Tratamiento de missing values --> missforest.
set.seed(1)

IMP = missForest(as.matrix(quantitative_variables1_filtered), verbose = T)

IMP$OOBerror

IMP = data.frame(IMP$ximp)

colnames(IMP)

## Normalizacion con ranknorm:

IMP = apply(IMP, 2, RankNorm)

Met_1a_CVD = data.frame(cbind(Met_1a_CVD[1], IMP))

export(Met_1a_CVD, "Met_1a_CVD_20230606.xlsx")

rm(df, IMP, merged_dataset_filt, otra, na_count, na_count1, na_count2, 
   metabo_high_NA, quantitative_variables1, quantitative_variables1_filtered, 
   subjects_NA, drop, i, rows_remove, newdata)

# Calculo de los Scores:
# Modelos variables ajustadas:
# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOTOT1a")

model.met = met
str(model.met)

X2 = data.frame(Met_1a_CVD[,2:379])
b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")",
                                          " +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
} 

Scores_1a_CVD = data.frame(cbind(Met_1a_CVD[1], modelo))
colnames(Scores_1a_CVD) = c("id","m.AOTOT_ajust")

save.image(file = "20230606.CVD.olivatot_1a_ajust.RData")

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOVE1a1")

model.met = met
str(model.met)

b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")",
                                          " +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
} 

Scores_1a_CVD = data.frame(cbind(Scores_1a_CVD, modelo))
colnames(Scores_1a_CVD) = c("id", "m.AOTOT_ajust", "m.AOVE_ajust")

save.image(file = "20230606.CVD.ac_olivavir_1a_ajust.RData") # Nota 6.230606

# Calculo Scores:

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AO1a")

model.met = met
str(model.met)

b = model.met

if (length(b) != 0) {
  stdout  = vector('character')
  con = textConnection('stdout', 'wr', local = TRUE)
  sink(con)
  for (i in 1:(dim(b)[1]))
  {
    if (i == 1) {cat("X2$model=", sep="")}
    if ((i < (dim(b)[1])) & (i > 0)) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")",
                                          " +", "\n", sep="")}
    if (i == (dim(b)[1])) {cat("(","X2$",b[[i,1]],"*", b[[i,2]],")", sep="")}
  }
  sink()
  
  close(con)
  
  stdout_full = paste(unlist(stdout), collapse =" ")
  stdout_full[1]
  
  eval(parse(text=stdout_full[1]))
  
  modelo = X2$model
} 

Scores_1a_CVD = data.frame(cbind(Scores_1a_CVD, modelo))
colnames(Scores_1a_CVD) = c("id", "m.AOTOT_ajust", "m.AOVE_ajust","m.AO_ajust")

save.image(file = "20230606.CVD.ac_oliva_1a_ajust.RData")

rm(b, con, i, met, model.met, modelo, stdout, stdout_full, X2, Met_1a_CVD)

# Locate everything at the beginning
data_sup <- CVD

data_sup$survtime <- data_sup$survtime - 1
data_sup$start <- data_sup$start - 1

for (i in 1:dim(data_sup)[1]){
  if (data_sup$cens[i]==0){data_sup$start[i]=0}
}

min(data_sup$survtime)

data_sup <- data_sup[!data_sup$survtime<0,]
min(data_sup$survtime)

table(data_sup$cens)

BBDD_CVD1a <- data_sup

# Fusion con los IDs (757, 159 eventos) con nuevos eventos al año
BBDD_CVD1a = merge(Scores_1a_CVD, BBDD_CVD1a, by = "id")
export(BBDD_CVD1a, "BBDD_CVD1a.csv")
#Se generan duplicados con el merge. Exporto y elimino a mano los duplicados.
BBDD_CVD1a = read_delim("BBDD_CVD1a.csv", ";", escape_double = FALSE, 
                        trim_ws = TRUE)
BBDD_CVD1a = merge(BBDD_CVD1a, propensity, by = "id")
BBDD_CVD1a = merge(BBDD_CVD1a, Alimentos, by = "id")
BBDD_CVD1a = merge(BBDD_CVD1a, sub_base, by = "id")

export(BBDD_CVD1a, "BBDD_CVD1a_20230606.csv")

table(BBDD_CVD1a$cens)

BBDD_CVD1a$m.AOTOT_ajust.1 = BBDD_CVD1a$m.AOTOT_ajust/sd(
  BBDD_CVD1a$m.AOTOT_ajust)
BBDD_CVD1a$m.AO_ajust.1 = BBDD_CVD1a$m.AO_ajust/sd(BBDD_CVD1a$m.AO_ajust)
BBDD_CVD1a$m.AOVE_ajust.1 = BBDD_CVD1a$m.AOVE_ajust/sd(BBDD_CVD1a$m.AOVE_ajust)

# Ajuste de variables a energia:

BBDD_CVD1a<-BBDD_CVD1a[order(-BBDD_CVD1a$olivatot3),]

mean(BBDD_CVD1a$olivatot3, na.rm = T)

df<- data.frame(y = BBDD_CVD1a$olivatot3,
                y1= BBDD_CVD1a$olivatot3,
                x = BBDD_CVD1a$energiat3) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 45.4071)
BBDD_CVD1a$olivatot_ajust3 <- values

BBDD_CVD1a[,c("olivatot3", "olivatot_ajust3")]
rm("df1", "values", "df")

BBDD_CVD1a<-BBDD_CVD1a[order(-BBDD_CVD1a$ac_oliva3),]

mean(BBDD_CVD1a$ac_oliva3, na.rm = T)

df<- data.frame(y = BBDD_CVD1a$ac_oliva3,
                y1= BBDD_CVD1a$ac_oliva3,
                x = BBDD_CVD1a$energiat3) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 10.1952)
BBDD_CVD1a$ac_oliva_ajust3 <- values

BBDD_CVD1a[,c("ac_oliva3", "ac_oliva_ajust3")]
rm("df1", "values", "df")

BBDD_CVD1a<-BBDD_CVD1a[order(-BBDD_CVD1a$ac_olivavir3),]

mean(BBDD_CVD1a$ac_olivavir3, na.rm = T)

df<- data.frame(y = BBDD_CVD1a$ac_olivavir3,
                y1= BBDD_CVD1a$ac_olivavir3,
                x = BBDD_CVD1a$energiat3) 

df1 <- lm(y~x, data=df)

values <- residuals(df1)[1:length(df$y)] + (df1$coefficients[1] + 
                                              df1$coefficients[2] * 35.21022)
BBDD_CVD1a$ac_olivavir_ajust3 <- values

BBDD_CVD1a[,c("ac_olivavir3", "ac_olivavir_ajust3")]
rm("df1", "values", "df")

# Analisis de Cox a 1 a:

colnames(BBDD_CVD1a)

approaches <- c("m.AOTOT_ajust.1",
                "m.AO_ajust.1",
                "m.AOVE_ajust.1")

foods <- c("olivatot_ajust3",
           "ac_oliva_ajust3",
           "ac_olivavir_ajust3")

names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'edad0'] <- 'age'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'sexo'] <- 'sex'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'diabetes0'] <- 'diabetes'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'ps1.x'] <- 'ps1'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'ps2.x'] <- 'ps2'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'centro'] <- 'center'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'fum'] <- 'smoking'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'grup_int'] <- 'interv_g'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'getota_1.x'] <- 'getota_1'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'ant_fam'] <- 'fam_history'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'imc1'] <- 'bmi'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'tra_col0.x'] <- 'tra_col0'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'hta0.x'] <- 'hta0'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'trathta0.x'] <- 'trathta0'
names(BBDD_CVD1a)[names(BBDD_CVD1a) == 'hipercol0.x'] <- 'hipercol0'

formu_model1 <- list()
fit_model1 <- list()
formu_model2 <- list()
fit_model2 <- list()
formu_model3 <- list()
fit_model3 <- list()
formu_model4 <- list()
fit_model4 <- list()

for (i in 1:3){
  
  formu_model1[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + 
                                         strata(as.factor(interv_g)) + 
                                         strata(as.factor(center))"))
  fit_model1[[i]] <- coxph(formu_model1[[i]], weights=w, data = BBDD_CVD1a)
  
  formu_model2[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + strata(as.factor(interv_g)) + strata(as.factor(center)) +
                                         bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + educat + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(hta0) + as.factor(tra_col0) + as.factor(trathta0)"))
  fit_model2[[i]] <- coxph(formu_model2[[i]], weights=w, data = BBDD_CVD1a)
  
  formu_model3[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + strata(as.factor(interv_g)) + strata(as.factor(center)) +
                                         bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + educat + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(hta0) + as.factor(tra_col0) + as.factor(trathta0) +
                                         verdutot3 + frutatot3 + grupocer3 + fsecos3 + huevos3 + legumbre3 + pescados3 + carnicos3 + lacteos3"))
  fit_model3[[i]] <- coxph(formu_model3[[i]], weights=w, data = BBDD_CVD1a)
  
  formu_model4[[i]] <- as.formula(paste0("Surv(survtime, cens) ~ ", approaches[i], 
                                         " + age + sex + ps1 + ps2 + strata(as.factor(interv_g)) + strata(as.factor(center)) +
                                         bmi + as.factor(smoking) + alcoholg + I(alcoholg^2) + educat + getota_1 + as.factor(fam_history) + as.factor(hipercol0) + as.factor(hta0) + as.factor(tra_col0) + as.factor(trathta0) +
                                         verdutot3 + frutatot3 + grupocer3 + fsecos3 + huevos3 + legumbre3 + pescados3 + carnicos3 + lacteos3 + ", foods[i]))
  fit_model4[[i]] <- coxph(formu_model4[[i]], weights=w, data = BBDD_CVD1a)
}

results_model1 <- matrix(NA, 3, 4)
results_model2 <- matrix(NA, 3, 4)
results_model3 <- matrix(NA, 3, 4)
results_model4 <- matrix(NA, 3, 4)

rownames(results_model1) <- c("model_m.AOTOT_ajust.1",
                              "model_m.AO_ajust.1",
                              "model_m.AOVE_ajust.1")

rownames(results_model2)<-rownames(results_model1)
rownames(results_model3)<-rownames(results_model1)
rownames(results_model4)<-rownames(results_model1)

colnames(results_model1)<-c("HR", "lower 95% CI", "upper 95% CI", "P_value")
colnames(results_model2)<-colnames(results_model1)
colnames(results_model3)<-colnames(results_model1)
colnames(results_model4)<-colnames(results_model1)

for (i in 1:3){
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
                      c("Model 2"),results_model2,
                      c("Model 3"),results_model3,
                      c("Model 4"),results_model4)

modelos = data.frame(full_results)
modelos$names = rownames(modelos)
export(modelos, "modelos_CVD_1a_ajus_wth drugs_202306054.xlsx")
export(BBDD_CVD1a, "BBDD_CVD1a_20230606.csv")

rm(approaches, fit_model1, fit_model2, fit_model3, fit_model4, foods, formu_model1, formu_model2, formu_model3,
   formu_model4, full_results, get_data_sum, hr_data, modelos, results_model1, results_model2, results_model3,
   results_model4, i)

# Cambios en de metabolitos entre huellas.

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOTOT")
met1 <- read_excel("borradores/Only coef_Gauss.xlsx", sheet = "AOTOT")

met2 = merge(met1, met, by = "Metabolites", all = T)
met2 = merge(met2, match_names, by = "Metabolites")

rm(met, met1, met2)

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOVE")
met1 <- read_excel("borradores/Only coef_Gauss.xlsx", sheet = "AOVE")

met2 = merge(met1, met, by = "Metabolites", all = T)
met2 = merge(met2, match_names, by = "Metabolites")

rm(met, met1, met2)

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AO")
met1 <- read_excel("borradores/Only coef_Gauss.xlsx", sheet = "AO")

met2 = merge(met1, met, by = "Metabolites", all = T)
met2 = merge(met2, match_names, by = "Metabolites")

rm(met, met1, met2)

met <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOTOT")
met1 <- read_excel("Only coef_Gauss1.xlsx", sheet = "AOVE")
met2 <- read_excel("Only coef_Gauss1.xlsx", sheet = "AO")

met3 = merge(met1, met, by = "Metabolites", all = T)
met3 = merge(met3, met2, by = "Metabolites", all = T)
met3 = merge(met3, match_names, by = "Metabolites")

colnames(met3) = c("Metabolites", "AOVE", "AOTOT", "AO", "names", "HMDB", "PubChem", "KEGG")

export(met3, "coef_met_all_20230606.xlsx")

# Fase 4: ####
#  análisis sencudarios
## 4.1. Diagrama de Venn (Figure 5) ####

AOTOT_1 <- read_excel("Only coef_Gauss1.xlsx", 
                              sheet = "AOTOT")
AOVE_1 <- read_excel("Only coef_Gauss1.xlsx", 
                      sheet = "AOVE")
AO_1 <- read_excel("Only coef_Gauss1.xlsx", 
                      sheet = "AO")

set_AOTOT = (AOTOT_1$Metabolites)
set_AOVE = (AOVE_1$Metabolites)
set_AO = (AO_1$Metabolites)

venn.diagram(x = list(set_AOTOT,set_AOVE,set_AO),
             category.names = c("TOO", "EVOO", "COO"),
             filename = 'venn_tot.png',
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

set_AOTOT = subset(AOTOT_1$Metabolites, AOTOT_1$mean >0)
set_AOVE = subset(AOVE_1$Metabolites, AOVE_1$mean >0)
set_AO = subset(AO_1$Metabolites, AO_1$mean >0)


venn.diagram(x = list(set_AOTOT,set_AOVE,set_AO),
             category.names = c("TOO", "EVOO", "COO"),
             filename = 'venn_post.png',
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

set_AOTOT = subset(AOTOT_1$Metabolites, AOTOT_1$mean <0)
set_AOVE = subset(AOVE_1$Metabolites, AOVE_1$mean <0)
set_AO = subset(AO_1$Metabolites, AO_1$mean <0)


venn.diagram(x = list(set_AOTOT,set_AOVE,set_AO),
             category.names = c("TOO", "EVOO", "COO"),
             filename = 'venn_neg.png',
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

## 4.2.	Correlaciones entre los PM y los diferentes consumos de aceite. ####

# Tabla de espeficidad (añadida como matriz de correlaciones):

mat1 = data.frame(cbind(BBDDmet[389], BBDDmet[388], BBDDmet[387], Scores_base[2:4]))

colnames(mat1) = c("TOO", "EVOO", "COO", "TOO mp", "EVOO mp", "COO mp")

mat = cor(mat1)

png(filename = "Correlations between profiles.png", width = 30, height = 20, res=300, unit="cm")
corrplot(mat, method = 'color', type = "upper", addCoef.col = "black")
dev.off()

rm(mat, mat1)

## 4.3. Por grupo de intervención: ####
#### CVD ####
#Basal
# Creo la BBDD:

CVD <- read_csv("BBDD/CVDprojectweighted.csv")
BBDD_CVD = merge(CVD, BBDDmet, by = "id")
propensity <- read_csv("BBDD/propensity.csv")
BBDD_CVD = merge(BBDD_CVD, propensity, by = "id")
Alimentos <- read_sav("BBDD/FFQ.SAV")
BBDD_CVD = merge(BBDD_CVD, Alimentos, by = "id")
load("BBDD/vars_necesarias_updated.Rdata")
BBDD_CVD = merge(BBDD_CVD, sub_base, by = "id")
Scores_base = data.frame(cbind(Scores[1], Scores[8:13]))
BBDD_CVD = merge(BBDD_CVD, Scores_base, by = "id")
colnames(BBDD_CVD)

mean(BBDD_CVD_EVOO$olivatot.y, na.rm = T)

BBDD_CVD$interv_g
BBDD_CVD_EVOO = subset(BBDD_CVD, interv_g == "MedDiet+EVOO")
BBDD_CVD_NUTS = subset(BBDD_CVD, interv_g == "MedDiet+nuts")
BBDD_CVD_DBG = subset(BBDD_CVD, interv_g == "control")

mean(BBDD_CVD_EVOO$olivatot.y, na.rm = T)
sd(BBDD_CVD_EVOO$olivatot.y, na.rm = T)
mean(BBDD_CVD_EVOO$ac_olivavir.y, na.rm = T)
sd(BBDD_CVD_EVOO$ac_olivavir.y, na.rm = T)
mean(BBDD_CVD_EVOO$ac_oliva.y, na.rm = T)
sd(BBDD_CVD_EVOO$ac_oliva.y, na.rm = T)

mean(BBDD_CVD_NUTS$olivatot.y, na.rm = T)
sd(BBDD_CVD_NUTS$olivatot.y, na.rm = T)
mean(BBDD_CVD_NUTS$ac_olivavir.y, na.rm = T)
sd(BBDD_CVD_NUTS$ac_olivavir.y, na.rm = T)
mean(BBDD_CVD_NUTS$ac_oliva.y, na.rm = T)
sd(BBDD_CVD_NUTS$ac_oliva.y, na.rm = T)

mean(BBDD_CVD_DBG$olivatot.y, na.rm = T)
sd(BBDD_CVD_DBG$olivatot.y, na.rm = T)
mean(BBDD_CVD_DBG$ac_olivavir.y, na.rm = T)
sd(BBDD_CVD_DBG$ac_olivavir.y, na.rm = T)
mean(BBDD_CVD_DBG$ac_oliva.y, na.rm = T)
sd(BBDD_CVD_DBG$ac_oliva.y, na.rm = T)

colnames(BBDD_CVD_EVOO)
table(BBDD_CVD_EVOO$cens)
table(BBDD_CVD_NUTS$cens)
table(BBDD_CVD_DBG$cens)

m1.1.EVOO = coxph(Surv(survtime, cens) ~ m.AOTOT.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                  educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                  as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                  (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                  (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_EVOO)
summary(m1.1.EVOO)

m1.2.EVOO = coxph(Surv(survtime, cens) ~ m.AOVE.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_EVOO)
summary(m1.2.EVOO)

m1.3.EVOO = coxph(Surv(survtime, cens) ~ m.AO.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_EVOO)
summary(m1.3.EVOO)

m1.1.NUTS = coxph(Surv(survtime, cens) ~ m.AOTOT.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_NUTS)
summary(m1.1.NUTS)

m1.2.NUTS = coxph(Surv(survtime, cens) ~ m.AOVE.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_NUTS)
summary(m1.2.NUTS)

m1.3.NUTS = coxph(Surv(survtime, cens) ~ m.AO.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_NUTS)
summary(m1.3.NUTS)

m1.1.DBG = coxph(Surv(survtime, cens) ~ m.AOTOT.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_DBG)
summary(m1.1.DBG)

m1.2.DBG = coxph(Surv(survtime, cens) ~ m.AOVE.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_DBG)
summary(m1.2.DBG)

m1.3.DBG = coxph(Surv(survtime, cens) ~ m.AO.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD_DBG)
summary(m1.3.DBG)

rm(BBDD_CVD_EVOO, BBDD_CVD_NUTS, BBDD_CVD_DBG, m1.1.EVOO, m1.1.NUTS, m1.1.DBG, m1.2.EVOO, m1.2.NUTS, m1.2.DBG,
   m1.3.EVOO, m1.3.NUTS, m1.3.DBG)

# 1 año

BBDD_CVD1a$interv_g
BBDD_CVD1a_EVOO = subset(BBDD_CVD1a, interv_g == "MedDiet+EVOO")
BBDD_CVD1a_NUTS = subset(BBDD_CVD1a, interv_g == "MedDiet+nuts")
BBDD_CVD1a_DBG = subset(BBDD_CVD1a, interv_g == "control")

colnames(BBDD_CVD1a_EVOO)
table(BBDD_CVD1a_EVOO$cens)
table(BBDD_CVD1a_NUTS$cens)
table(BBDD_CVD1a_DBG$cens)

m2.1.EVOO = coxph(Surv(survtime, cens) ~ m.AOTOT_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_EVOO)
summary(m2.1.EVOO)

m2.2.EVOO = coxph(Surv(survtime, cens) ~ m.AOVE_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_EVOO)
summary(m2.2.EVOO)

m2.3.EVOO = coxph(Surv(survtime, cens) ~ m.AO_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_EVOO)
summary(m2.3.EVOO)

m2.1.NUTS = coxph(Surv(survtime, cens) ~ m.AOTOT_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_NUTS)
summary(m2.1.NUTS)

m2.2.NUTS = coxph(Surv(survtime, cens) ~ m.AOVE_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_NUTS)
summary(m2.2.NUTS)

m2.3.NUTS = coxph(Surv(survtime, cens) ~ m.AO_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_NUTS)
summary(m2.3.NUTS)

m2.1.DBG = coxph(Surv(survtime, cens) ~ m.AOTOT_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_DBG)
summary(m2.1.DBG)

m2.2.DBG = coxph(Surv(survtime, cens) ~ m.AOVE_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_DBG)
summary(m2.2.DBG)

m2.3.DBG = coxph(Surv(survtime, cens) ~ m.AO.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_CVD1a_DBG)
summary(m2.3.DBG)

rm(BBDD_CVD1a_EVOO, BBDD_CVD1a_NUTS, BBDD_CVD1a_DBG, m2.1.EVOO, m2.1.NUTS, m2.1.DBG, m2.2.EVOO, m2.2.NUTS, m2.2.DBG,
   m2.3.EVOO, m2.3.NUTS, m2.3.DBG)

#### DT2 ####
#Basal

T2D <- read_csv("BBDD/T2Dprojectweighted.csv")
BBDD_T2D = merge(T2D, BBDDmet, by = "id")
propensity <- read_csv("BBDD/propensity.csv")
BBDD_T2D = merge(BBDD_T2D, propensity, by = "id")
Alimentos <- read_sav("EBBDD/FFQ.SAV")
BBDD_T2D = merge(BBDD_T2D, Alimentos, by = "id")
load("BBDD/vars_necesarias_updated.Rdata")
BBDD_T2D = merge(BBDD_T2D, sub_base, by = "id")
Scores_base = data.frame(cbind(Scores[1], Scores[8:13]))
BBDD_T2D = merge(BBDD_T2D, Scores_base, by = "id")
colnames(BBDD_T2D)

BBDD_T2D$m.AOTOT.1 = BBDD_T2D$m.AOTOT/sd(BBDD_T2D$m.AOTOT)
BBDD_T2D$m.AOTOT_1.1 = BBDD_T2D$m.AOTOT_1/sd(BBDD_T2D$m.AOTOT_1)
BBDD_T2D$m.AO.1 = BBDD_T2D$m.AO/sd(BBDD_T2D$m.AO)
BBDD_T2D$m.AO_1.1 = BBDD_T2D$m.AO_1/sd(BBDD_T2D$m.AO_1)
BBDD_T2D$m.AOVE.1 = BBDD_T2D$m.AOVE/sd(BBDD_T2D$m.AOVE)
BBDD_T2D$m.AOVE_1.1 = BBDD_T2D$m.AOVE_1/sd(BBDD_T2D$m.AOVE_1)

colnames(BBDD_T2D)

BBDD_T2D$interv_g
BBDD_T2D_EVOO = subset(BBDD_T2D, interv_g == "aceite de oliva")
BBDD_T2D_NUTS = subset(BBDD_T2D, interv_g == "frutos secos")
BBDD_T2D_DBG = subset(BBDD_T2D, interv_g == "dieta baja en grasa")

mean(BBDD_CVD_EVOO$olivatot.y, na.rm = T)
sd(BBDD_CVD_EVOO$olivatot.y, na.rm = T)
mean(BBDD_CVD_EVOO$ac_olivavir.y, na.rm = T)
sd(BBDD_CVD_EVOO$ac_olivavir.y, na.rm = T)
mean(BBDD_CVD_EVOO$ac_oliva.y, na.rm = T)
sd(BBDD_CVD_EVOO$ac_oliva.y, na.rm = T)

mean(BBDD_CVD_NUTS$olivatot.y, na.rm = T)
sd(BBDD_CVD_NUTS$olivatot.y, na.rm = T)
mean(BBDD_CVD_NUTS$ac_olivavir.y, na.rm = T)
sd(BBDD_CVD_NUTS$ac_olivavir.y, na.rm = T)
mean(BBDD_CVD_NUTS$ac_oliva.y, na.rm = T)
sd(BBDD_CVD_NUTS$ac_oliva.y, na.rm = T)

mean(BBDD_CVD_DBG$olivatot.y, na.rm = T)
sd(BBDD_CVD_DBG$olivatot.y, na.rm = T)
mean(BBDD_CVD_DBG$ac_olivavir.y, na.rm = T)
sd(BBDD_CVD_DBG$ac_olivavir.y, na.rm = T)
mean(BBDD_CVD_DBG$ac_oliva.y, na.rm = T)
sd(BBDD_CVD_DBG$ac_oliva.y, na.rm = T)

colnames(BBDD_T2D_EVOO)
table(BBDD_T2D_EVOO$cens)
table(BBDD_T2D_NUTS$cens)
table(BBDD_T2D_DBG$cens)

m1.1.EVOO = coxph(Surv(survtime, cens) ~ m.AOTOT.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                    educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                    as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_EVOO)
summary(m1.1.EVOO)

m1.2.EVOO = coxph(Surv(survtime, cens) ~ m.AOVE.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                    educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                    as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_EVOO)
summary(m1.2.EVOO)

m1.3.EVOO = coxph(Surv(survtime, cens) ~ m.AO.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                    educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                    as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_EVOO)
summary(m1.3.EVOO)

m1.1.NUTS = coxph(Surv(survtime, cens) ~ m.AOTOT.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                    educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                    as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_NUTS)
summary(m1.1.NUTS)

m1.2.NUTS = coxph(Surv(survtime, cens) ~ m.AOVE.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                    educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                    as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_NUTS)
summary(m1.2.NUTS)

m1.3.NUTS = coxph(Surv(survtime, cens) ~ m.AO.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                    educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                    as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_NUTS)
summary(m1.3.NUTS)

m1.1.DBG = coxph(Surv(survtime, cens) ~ m.AOTOT.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                   educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                   as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_DBG)
summary(m1.1.DBG)

m1.2.DBG = coxph(Surv(survtime, cens) ~ m.AOVE.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                   educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                   as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_DBG)
summary(m1.2.DBG)

m1.3.DBG = coxph(Surv(survtime, cens) ~ m.AO.1 + ps1.x + ps2.x + edad0+ as.factor(sexo)+ strata(as.factor(nodo)) + 
                   educat + imc1 + alcoholg + I(alcoholg^2) + getota_1.x + as.factor(ant_fam) + 
                   as.factor(hipercol0.x) + as.factor(tra_col0.x) + as.factor(hta0.x) + as.factor(trathta0.x) + as.factor(fum) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D_DBG)
summary(m1.3.DBG)

rm(BBDD_T2D_EVOO, BBDD_T2D_NUTS, BBDD_T2D_DBG, m1.1.EVOO, m1.1.NUTS, m1.1.DBG, m1.2.EVOO, m1.2.NUTS, m1.2.DBG,
   m1.3.EVOO, m1.3.NUTS, m1.3.DBG)

# 1 año

BBDD_T2D1a$interv_g
BBDD_T2D1a_EVOO = subset(BBDD_T2D1a, interv_g == "aceite de oliva")
BBDD_T2D1a_NUTS = subset(BBDD_T2D1a, interv_g == "frutos secos")
BBDD_T2D1a_DBG = subset(BBDD_T2D1a, interv_g == "dieta baja en grasa")

colnames(BBDD_T2D1a_EVOO)
table(BBDD_T2D1a_EVOO$cens)
table(BBDD_T2D1a_NUTS$cens)
table(BBDD_T2D1a_DBG$cens)

m2.1.EVOO = coxph(Surv(survtime, cens) ~ m.AOTOT_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_EVOO)
summary(m2.1.EVOO)

m2.2.EVOO = coxph(Surv(survtime, cens) ~ m.AOVE_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_EVOO)
summary(m2.2.EVOO)

m2.3.EVOO = coxph(Surv(survtime, cens) ~ m.AO_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_EVOO)
summary(m2.3.EVOO)

m2.1.NUTS = coxph(Surv(survtime, cens) ~ m.AOTOT_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_NUTS)
summary(m2.1.NUTS)

m2.2.NUTS = coxph(Surv(survtime, cens) ~ m.AOVE_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_NUTS)
summary(m2.2.NUTS)

m2.3.NUTS = coxph(Surv(survtime, cens) ~ m.AO_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                    educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                    as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                    (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                    (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_NUTS)
summary(m2.3.NUTS)

m2.1.DBG = coxph(Surv(survtime, cens) ~ m.AOTOT_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_DBG)
summary(m2.1.DBG)

m2.2.DBG = coxph(Surv(survtime, cens) ~ m.AOVE_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_DBG)
summary(m2.2.DBG)

m2.3.DBG = coxph(Surv(survtime, cens) ~ m.AO_ajust.1 + ps1 + ps2 + age+ as.factor(sex)+ strata(as.factor(center)) + 
                   educat + bmi + alcoholg + I(alcoholg^2) + getota_1 + as.factor(fam_history) + 
                   as.factor(hipercol0) + as.factor(tra_col0) + as.factor(hta0) + as.factor(trathta0) + as.factor(smoking) + 
                   (frutatot) + (verdutot) + (grupocer) + lacteos + (pescados) +
                   (carnicos) + (huevos) + (legumbre) + (fsecos), weights = w, data = BBDD_T2D1a_DBG)
summary(m2.3.DBG)

rm(BBDD_T2D1a_EVOO, BBDD_T2D1a_NUTS, BBDD_T2D1a_DBG, m2.1.EVOO, m2.1.NUTS, m2.1.DBG, m2.2.EVOO, m2.2.NUTS, m2.2.DBG,
   m2.3.EVOO, m2.3.NUTS, m2.3.DBG)

## 4.4. PCA ####
library(factoextra)
# Preparación de al BBDD:

met = read_excel("Only coef_Gauss1.xlsx", sheet = "AOTOT")
met1 = read_excel("Only coef_Gauss1.xlsx", sheet = "AOVE")
met2 = read_excel("Only coef_Gauss1.xlsx", sheet = "AO")

met = merge(met, met1, by = "Metabolites", all = T)
met = merge(met, met2, by = "Metabolites", all = T)

# Adicción de 0 en los NA:

colnames(met) = c("Metabolites","Total Olive Oil", "EVOO", "COO")
met[is.na(met$AOTOT),]$AOTOT = 0
met[is.na(met$AOVE),]$AOVE = 0
met[is.na(met$AO),]$AO = 0
rownames(met) = met$Metabolites
met$Metabolites = NULL
rownames(met) = c("AAMU","ACh","Adenosine","Adipate",
                  "ADP","Allantoin","α-Hydroxybutyrate","Aminoadipate",
                  "β-Hydroxybutyrate","Bilirubin","Biliverdin","Butyrobetaine",
                  "c10:2 CAR","c12 CAR","c14:0 CE","c14:1 MAG",
                  "c14:2 CAR","c16:0 CER d18:1","c16:1 CE","c16:1 LPC",
                  "c18:0 LPE","c18:0 MAG","c18:1 CAR","c18:1 CE",
                  "c18:1 LPE","c18:2 LPC","c18 CAR","c20:0 SM",
                  "c20:4 LPE","c20:5 CE","c22:4 CE","c22:6 CE",
                  "c24:0 CER d18:1","c24:0 SM","c30:0 DAG","c30:1 PC",
                  "c32:0 PE","c34:0 PE","c34:0 PI","c34:1 PC plas A",
                  "c34:1 PC plas B","c34:2 PC plas","c34:2 PE plas","c343PC",
                  "c34:3 PC plas","c34:4 PC","c34:5 PC plas","c36:0 DAG",
                  "c36:1 PC","c36:1 PE plas","c36:2 PC plas","c36:2 PS plas",
                  "c36:3 PS plas","c38:2 PC","c38:2 PE","c38:4 PC plas",
                  "c38:5 PE plas","c38:6 PC plas","c3 CAR","c40:6 PS",
                  "c42:11 PE plas","c45:1 TAG","c54:10 TAG","c54:2 TAG",
                  "c54:5 TAG","c55:2 TAG","c56:2 TAG","c56:3 TAG",
                  "c58:6 TAG","c5-DC CAR","c9 CAR","Caffeine",
                  "Citrate","Cortisol","Cotinine","Cytosine",
                  "INN","DMGV","Ectoine","Glu",
                  "Gln","Glycocholate","Glyco-deoxy-chenodeox","F6P",
                  "Homoarginine","4-Hydroxy-3-methylacetophenone","Hydroxycotinine","Hydroxyhippurate",
                  "IPA","Indol-3-Ol","Inosine","Inositol",
                  "Lactate","Lactose","LEA","Malate",
                  "Methylguanine","Methylhistidine","N1-methyl-2-pyridone-5-carboxamide","N-acetylputrescine",
                  "Ornithine","Vit B5","Pantothenol","Phe",
                  "Pipecolic Acid","Piperine","Pyroglutamate","Sarcosine",
                  "Ser","Sphinganine","Sphingosine","Taurine",
                  "Thr","T4","TMAO","Psi-cumene",
                  "Trimethyllysine","Urate","Val")

apply(mett, 2, mean)
apply(mett, 2, var)

pca <- prcomp(met, scale = TRUE)
pca$rotation
pca$x
# png(filename = "biplot.png", width = 15, height = 10, res=300, unit="cm")
# biplot(x = pca, scale = 0, cex = 0.6, col = c("blue", "red4"))
# dev.off()
png(filename = "biplot.png", width = 20, height = 15, res=300, unit="cm")
fviz_pca_biplot(pca, col.ind = "cos2", pointsize = 1, geom.ind = c("arrow","text"), habillage = "none", 
                alpha.ind = "contrib", select.ind = list(contrib = 30)) +
  scale_color_gradient2(low="blue", mid="black", high="red", midpoint=0.75) +
  xlim (-7,7) + ylim (-7,7) +
  theme_minimal()
dev.off()
fviz_pca_var(pca)
