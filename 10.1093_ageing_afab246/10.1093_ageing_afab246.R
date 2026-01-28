#Sintaxis paper ingesta vitamina K y cuestionarios de cognición en PREDIMED Plus: resultado de los cuestionarios a los 
#2 años y delta de cambio de ingesta a los dos años.
#Paquetes de inicio.

library(gdata)
library(Hmisc)
library(car)
library(effects)
library(normtest)
library(nortest) 
library(moments)
library(haven)
library(rms)
library(splines)
library(ggplot2)
library(survival)
library(xlsx)
library(readxl)
library(faraway)
library(lmtest)
library(survival)
library(psych)
library(DescTools)
library(foreign)
library(MASS)
library(dplyr)

options(scipen = 999)

#Apertura de las BBDD. Preparacion de la bbdd de trabajo añadiendo las variables idcluster, nodo, noco_cat.

#BBDD 201903131142_PREDIMEDplus_2019-03-12_2years filtrada con aquellos participantes que tienen FFQ y test de cognicion:

PPLUS_cog <- read_sav("F:/ARTICULOS/Lucia/ART vit K - cognicion/BBDD/201903131142_PREDIMEDplus_2019-03-12_2years_cogni_vitK_2a_3737.sav")

#BBDD con idcluster: CLUSTER_201903260828_PREDIMEDplus_2019-03-25_2years

CLUSTER <- read_sav("F:/ARTICULOS/Lucia/ART vit K - cognicion/BBDD/CLUSTER_201903260828_PREDIMEDplus_2019-03-25_2years.sav")

#BBDD con el nodo y el nodo categorizado para todo los participantes:

Nodo_cat <- read_sav("F:/ARTICULOS/Lucia/ART vit K - cognicion/BBDD/Nodo_cat.sav")

#Matching de las BBDD.

str(PPLUS_cog$paciente)
str(CLUSTER$paciente)
PPLUS_cog$paciente<-as.numeric(PPLUS_cog$paciente)
CLUSTER$paciente<-as.numeric(CLUSTER$paciente)
CLUSTER = CLUSTER[,c("paciente","idcluster")]
PPLUS_cog2 <- merge(PPLUS_cog, CLUSTER, by = "paciente")
Nodo_cat = Nodo_cat[,c("paciente","nodo_cat","nodo")]
PPLUS_cog4 = merge(PPLUS_cog2, Nodo_cat, by = "paciente")

#Tras la fusion de todas las BBDD en una sola con todas las variables:Creacion de BBDD de trabajo con la variables que se utilizaran.

PPLUS = PPLUS_cog4[,c("paciente","tcox_cognitive","mmse_categorias24_v02","puntua_reloj_v02","puntua_reloj_v00","Terciles_delta_vitK_2a","sexo_s1","edad_s1","imc_v00","imc_v02","total_letra_p_v00"
                      ,"geaf_tot_v00","fuma_s1","escola_v00","diab_prev_s1","colest_s1","hta_s1","Sintrom_Acenocumarol","nodo","nodo_cat","total_animales_v00"
                      ,"hc_v00","prot_v00","gratot_v00","fibra_v00","alcoholg_v00","alcoholg_quad_v00","energiat_v00","mmse_categorias24","orden_inverso_v00"
                      ,"tto_insu_v00","tto_metfor_v00","tto_dm_v00","delta_vitK_2a","Terciles_delta_vitK_2a_NoDM","delta_vitK_2a_DM","orden_directo_v00","tiempo_totalb_v00"
                      ,"Terciles_delta_vitK_2a_DM","idcluster","bdi_total_v00","hc_v02","prot_v02","gratot_v02","fibra_v02","alcoholg_v02","tiempo_totala_v00"
                      ,"energiat_v02","puntuacion_mmse_v00","puntuacion_mmse_v02","grupo_int_v00","cereal_v00", "cereal_v02","frutatot_v00","frutatot_v02","legumbre_v00","legumbre_v02","carnicos_v00","carnicos_v02","olivatot_v00","olivatot_v02"
                      ,"pescados_v00","pescados_v02","fsecos_v00","fsecos_v02","gallet_v00","gallet_v02","verdutot_v00","verdutot_v02","lacteos_v00","lacteos_v02","vitK_v00_ajust","vitK_v02_ajust","vitK_v00","vitK_v02","vitK_v01")]


#Eliminacion de las otras BBDD inservibles.

rm(CLUSTER, Nodo_cat, PPLUS_cog, PPLUS_cog2, PPLUS_cog4)

#Adecuacion de las variables categoricas.
str(PPLUS)
PPLUS$mmse_categorias24_v02 = as.numeric(PPLUS$mmse_categorias24_v02)
PPLUS$Terciles_delta_vitK_2a = as.factor(PPLUS$Terciles_delta_vitK_2a)
PPLUS$sexo_s1 = as.factor(PPLUS$sexo_s1)
PPLUS$fuma_s1 = as.factor(PPLUS$fuma_s1)
PPLUS$escola_v00 = as.factor(PPLUS$escola_v00)
PPLUS$diab_prev_s1 = as.factor(PPLUS$diab_prev_s1)
PPLUS$colest_s1 = as.factor(PPLUS$colest_s1)
PPLUS$hta_s1 = as.factor(PPLUS$hta_s1)
PPLUS$hta_s1 <- recode(PPLUS$hta_s1, "'0' = 0; '1' = 1; '9' = 1 ")
table(PPLUS$hta_s1)
PPLUS$Sintrom_Acenocumarol = as.factor(PPLUS$Sintrom_Acenocumarol)
PPLUS$mmse_categorias24 = as.factor(PPLUS$mmse_categorias24)
PPLUS$tto_insu_v00 = as.factor(PPLUS$tto_insu_v00)
PPLUS$tto_metfor_v00 = as.factor(PPLUS$tto_metfor_v00)
PPLUS$tto_dm_v00 = as.factor(PPLUS$tto_dm_v00)
PPLUS$Terciles_delta_vitK_2a_NoDM = as.factor(PPLUS$Terciles_delta_vitK_2a_NoDM)
PPLUS$Terciles_delta_vitK_2a_DM = as.factor(PPLUS$Terciles_delta_vitK_2a_DM)
PPLUS$puntuacion_mmse_v00 = as.factor(PPLUS$puntuacion_mmse_v00)
PPLUS$BDI_dicotomica = car::recode(PPLUS$bdi_total_v00, "0:19 = '0'; else = '1'")
table(PPLUS$BDI_dicotomica)
PPLUS$BDI_dicotomica = as.factor(PPLUS$BDI_dicotomica)
PPLUS$d_imc = PPLUS$imc_v02 - PPLUS$imc_v00
tapply(PPLUS$delta_vitK_2a, PPLUS$Terciles_delta_vitK_2a, median)
levels(PPLUS$Terciles_delta_vitK_2a)
PPLUS$tend_vitK <- car::recode(PPLUS$Terciles_delta_vitK_2a, "'1' = -104.78295; '2' = 22.62228; '3' = 195.86808")
PPLUS$tend_vitK<- as.numeric(PPLUS$tend_vitK)
PPLUS$tend_vitK <- car::recode(PPLUS$tend_vitK, "'1' = -104.78295; '3' = 22.62228; '2' = 195.86808")
summary(PPLUS$tend_vitK)
levels(PPLUS$tend_vitK)
table(PPLUS$Terciles_delta_vitK_2a, PPLUS$tend_vitK)
str(PPLUS$tend_vitK)
PPLUS$reloj_categorias_v02 = car::recode(PPLUS$puntua_reloj_v02, "0=NA;c(1,2,3,4)='1'; c(5,6,7)='0'")

#Tabla 2: Odds ratio terciles de delta de vit K y scores de otros tests.

#Apertura de la BBDD.

cogni <- read_sav("F:/ARTICULOS/Lucia/ART vit K - cognicion/BBDD/201903131142_PREDIMEDplus_2019-03-12_2years_cogni_vitK_2a_3737_cogni_2a_questio.sav")

#Matching de las BBDD con los datos de los test.

str(cogni$paciente)

str(PPLUS$paciente)

cogni$paciente<-as.numeric(cogni$paciente)

PPLUS$paciente<-as.numeric(PPLUS$paciente)

PPLUS <- merge(PPLUS, cogni, by = "paciente")

save(PPLUS, file = "PPLUS.RData")

rm(cogni)

#Modelos estadísticos

mt2.1<-glm(orden_directo_eventSD ~ Terciles_delta_vitK_2a, family=binomial, PPLUS)
exp(mt2.1$coefficients)
exp(confint(mt2.1))
summary (mt2.1)

mt2.1.1<-glm(orden_directo_eventSD ~ Terciles_delta_vitK_2a * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
             imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
             cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
             tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) +
             BDI_dicotomica + orden_directo_v00, family=binomial, PPLUS)
exp(mt2.1.1$coefficients)
exp(confint(mt2.1.1))
summary (mt2.1.1)

mt2.1.tend<-glm(orden_directo_eventSD ~ tend_vitK, family=binomial, PPLUS)
exp(mt2.1.tend$coefficients)
exp(confint(mt2.1.tend))
summary (mt2.1.tend)

mt2.1.1.tend<-glm(orden_directo_eventSD ~ tend_vitK * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
             imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
             cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
             tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) +
             BDI_dicotomica + orden_directo_v00, family=binomial, PPLUS)
summary (mt2.1.1.tend)

mt2.2<-glm(orden_inverso_eventSD ~ Terciles_delta_vitK_2a, family=binomial, PPLUS)
exp(mt2.2$coefficients)
exp(confint(mt2.2))
summary (mt2.2)

mt2.2.1<-glm(orden_inverso_eventSD ~ Terciles_delta_vitK_2a * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
             imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
             cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
             tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) +
             BDI_dicotomica + orden_inverso_v00, family=binomial, PPLUS)
exp(mt2.2.1$coefficients)
exp(confint(mt2.2.1))
summary (mt2.2.1)

mt2.2.tend<-glm(orden_inverso_eventSD ~ tend_vitK, family=binomial, PPLUS)
summary(mt2.2.tend)

mt2.2.1.tend<-glm(orden_inverso_eventSD ~ tend_vitK * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                  imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
                  cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                  tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) +
                  BDI_dicotomica + orden_inverso_v00, family=binomial, PPLUS)
summary (mt2.2.1.tend)

mt2.3<-glm(FV_letra_eventSD ~ Terciles_delta_vitK_2a, family=binomial, PPLUS)
exp(mt2.3$coefficients)
exp(confint(mt2.3))
summary (mt2.3)

mt2.3.1<-glm(FV_letra_eventSD ~ Terciles_delta_vitK_2a * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
               imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
               cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
               tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (total_letra_p_v00) +
               BDI_dicotomica, family=binomial, PPLUS)
exp(mt2.3.1$coefficients)
exp(confint(mt2.3.1))
summary (mt2.3.1)

mt2.3.tend<-glm(FV_letra_eventSD ~ tend_vitK, family=binomial, PPLUS)
summary(mt2.3.tend)

mt2.3.1.tend<-glm(FV_letra_eventSD ~ tend_vitK * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                    imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
                    cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                    tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (total_letra_p_v00) +
                    BDI_dicotomica, family=binomial, PPLUS)
summary (mt2.3.1.tend)

mt2.4<-glm(FV_animales_eventSD ~ Terciles_delta_vitK_2a, family=binomial, PPLUS)
exp(mt2.4$coefficients)
exp(confint(mt2.4))
summary (mt2.4)

mt2.4.1<-glm(FV_animales_eventSD ~ Terciles_delta_vitK_2a * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
               imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
               cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
               tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (total_animales_v00) +
               BDI_dicotomica, family=binomial, PPLUS)
exp(mt2.4.1$coefficients)
exp(confint(mt2.4.1))
summary (mt2.4.1)

mt2.4.tend<-glm(FV_animales_eventSD ~ tend_vitK, family=binomial, PPLUS)
summary(mt2.4.tend)

mt2.4.1.tend<-glm(FV_animales_eventSD ~ tend_vitK * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                    imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
                    cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                    tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (total_animales_v00) +
                    BDI_dicotomica, family=binomial, PPLUS)
summary (mt2.4.1.tend)

mt2.5<-glm(TMT_a_eventSD ~ Terciles_delta_vitK_2a, family=binomial, PPLUS)
exp(mt2.5$coefficients)
exp(confint(mt2.5))
summary (mt2.5)

mt2.5.1<-glm(TMT_a_eventSD ~ Terciles_delta_vitK_2a * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
               imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
               cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
               tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (tiempo_totala_v00) +
               BDI_dicotomica, family=binomial, PPLUS)
exp(mt2.5.1$coefficients)
exp(confint(mt2.5.1))
summary (mt2.5.1)

mt2.5.tend<-glm(TMT_a_eventSD ~ tend_vitK, family=binomial, PPLUS)
summary(mt2.5.tend)

mt2.5.1.tend<-glm(TMT_a_eventSD ~ tend_vitK * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                    imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
                    cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                    tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (tiempo_totala_v00) +
                    BDI_dicotomica, family=binomial, PPLUS)
summary (mt2.5.1.tend)

mt2.6<-glm(TMT_b_eventSD ~ Terciles_delta_vitK_2a, family=binomial, PPLUS)
exp(mt2.6$coefficients)
exp(confint(mt2.6))
summary (mt2.6)

mt2.6.1<-glm(TMT_b_eventSD ~ Terciles_delta_vitK_2a * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
               imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
               cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
               tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (tiempo_totalb_v00) +
               BDI_dicotomica, family=binomial, PPLUS)
exp(mt2.6.1$coefficients)
exp(confint(mt2.6.1))
summary (mt2.6.1)

mt2.6.tend<-glm(TMT_b_eventSD ~ tend_vitK, family=binomial, PPLUS)
summary(mt2.6.tend)

mt2.6.1.tend<-glm(TMT_b_eventSD ~ tend_vitK * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                    imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
                    cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                    tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (tiempo_totalb_v00) +
                    BDI_dicotomica, family=binomial, PPLUS)
summary (mt2.6.1.tend)

table(PPLUS$reloj_categorias_v02, PPLUS$Terciles_delta_vitK_2a)

mt2.7<-glm(reloj_categorias_v02 ~ Terciles_delta_vitK_2a, family=binomial, PPLUS)
exp(mt2.7$coefficients)
exp(confint(mt2.7))
summary (mt2.7)

mt2.7.1<-glm(reloj_categorias_v02 ~ Terciles_delta_vitK_2a * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
               imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
               cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
               tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (puntua_reloj_v00) +
               BDI_dicotomica, family=binomial, PPLUS)
exp(mt2.7.1$coefficients)
exp(confint(mt2.7.1))
summary (mt2.7.1)

mt2.7.tend<-glm(reloj_categorias_v02 ~ tend_vitK, family=binomial, PPLUS)
summary(mt2.7.tend)

mt2.7.1.tend<-glm(reloj_categorias_v02 ~ tend_vitK * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                    imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
                    cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                    tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (puntua_reloj_v00) +
                    BDI_dicotomica, family=binomial, PPLUS)
summary (mt2.7.1.tend)

#Tabla 3: asociacion deltas vit K - MMSE segmentado por Diabéticos (si/no)

tapply(PPLUS$delta_vitK_2a, PPLUS$Terciles_delta_vitK_2a_DM, median)
levels(PPLUS$Terciles_delta_vitK_2a_DM)
PPLUS$tend_vitK_dm <- car::recode(PPLUS$Terciles_delta_vitK_2a_DM, "'1' = -108.21241; '2' = 19.09861; '3' = 190.67361")
PPLUS$tend_vitK_dm<- as.numeric(PPLUS$tend_vitK_dm)
PPLUS$tend_vitK_dm <- car::recode(PPLUS$tend_vitK_dm, "'1' = -108.21241; '2' = 19.09861; '3' = 190.67361")
summary(PPLUS$tend_vitK_dm)
levels(PPLUS$tend_vitK_dm)
table(PPLUS$Terciles_delta_vitK_2a_DM, PPLUS$tend_vitK_dm)
str(PPLUS$tend_vitK_dm)

tapply(PPLUS$delta_vitK_2a, PPLUS$Terciles_delta_vitK_2a_NoDM, median)
levels(PPLUS$Terciles_delta_vitK_2a_NoDM)
PPLUS$tend_vitK_nodm <- car::recode(PPLUS$Terciles_delta_vitK_2a_NoDM, "'1' = -102.52284; '3' = 198.86877; '2' = 25.19558")
PPLUS$tend_vitK_nodm<- as.numeric(PPLUS$tend_vitK_nodm)
PPLUS$tend_vitK_nodm <- car::recode(PPLUS$tend_vitK_nodm, "'1' = -102.52284; '3' = 25.19558; '2' = 198.86877")
summary(PPLUS$tend_vitK_nodm)
levels(PPLUS$tend_vitK_nodm)
table(PPLUS$Terciles_delta_vitK_2a_NoDM, PPLUS$tend_vitK_nodm)
str(PPLUS$tend_vitK_nodm)


mt3.1<-glm(mmse_categorias24_v02 ~ Terciles_delta_vitK_2a_DM, family=binomial, PPLUS)
exp(mt3.1$coefficients)
exp(confint(mt3.1))
summary (mt3.1)

mt3.1.1<-glm(mmse_categorias24_v02 ~ Terciles_delta_vitK_2a_DM + Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
               imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + (colest_s1) + hta_s1 + lacteos_v00 + 
               cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
               tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (puntuacion_mmse_v00) +
               BDI_dicotomica, family=binomial, PPLUS)
exp(mt3.1.1$coefficients)
exp(confint(mt3.1.1))
summary (mt3.1.1)

mt3.1.tend<-glm(mmse_categorias24_v02 ~ tend_vitK_dm, family=binomial, PPLUS)
summary(mt3.1.tend)

mt3.1.1.tend<-glm(mmse_categorias24_v02 ~ tend_vitK_dm + Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                    imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + (colest_s1) + hta_s1 + lacteos_v00 + 
                    cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                    tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (puntuacion_mmse_v00) +
                    BDI_dicotomica, family=binomial, PPLUS)
summary (mt3.1.1.tend)

mt3.2<-glm(mmse_categorias24_v02 ~ Terciles_delta_vitK_2a_NoDM, family=binomial, PPLUS)
exp(mt3.2$coefficients)
exp(confint(mt3.2))
summary (mt3.2)

mt3.2.1<-glm(mmse_categorias24_v02 ~ Terciles_delta_vitK_2a_NoDM + Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
               imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + (colest_s1) + hta_s1 + lacteos_v00 + 
               cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
               as.factor(grupo_int_v00) + (puntuacion_mmse_v00) +
               BDI_dicotomica, family=binomial, PPLUS)
exp(mt3.2.1$coefficients)
exp(confint(mt3.2.1))
summary (mt3.2.1)

mt3.2.tend<-glm(mmse_categorias24_v02 ~ tend_vitK_nodm, family=binomial, PPLUS)
summary(mt3.2.tend)

mt3.2.1.tend<-glm(mmse_categorias24_v02 ~ tend_vitK_nodm + Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                    imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + (colest_s1) + hta_s1 + lacteos_v00 + 
                    cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                    as.factor(grupo_int_v00) + (puntuacion_mmse_v00) +
                    BDI_dicotomica, family=binomial, PPLUS)
summary (mt3.2.1.tend)

#Figura 1: Odds ratio terciles de delta de vit K e incidencia <25 de MMSE.

mf1<-glm(mmse_categorias24_v02 ~ Terciles_delta_vitK_2a, family=binomial, PPLUS)
exp(mf1$coefficients)
exp(confint(mf1))
summary (mf1)

mf1.1<-glm(mmse_categorias24_v02 ~ Terciles_delta_vitK_2a * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
             imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
             cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
             tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (puntuacion_mmse_v00) +
             BDI_dicotomica, family=binomial, PPLUS)
exp(mf1.1$coefficients)
exp(confint(mf1.1))
summary (mf1.1)

mf1.tend<-glm(mmse_categorias24_v02 ~ tend_vitK, family=binomial, PPLUS)
summary (mf1.tend)

mf1.1.tend<-glm(mmse_categorias24_v02 ~ tend_vitK * Sintrom_Acenocumarol + cluster(idcluster) + sexo_s1 + edad_s1 + strata(as.factor(nodo_cat)) + 
                  imc_v00 + geaf_tot_v00 + (fuma_s1) + (escola_v00) + diab_prev_s1 + (colest_s1) + hta_s1 + lacteos_v00 + 
                  cereal_v00 + frutatot_v00 + pescados_v00 + fsecos_v00 + carnicos_v00 + legumbre_v00 + alcoholg_v00 + alcoholg_quad_v00 + 
                  tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + as.factor(grupo_int_v00) + (puntuacion_mmse_v00) +
                  BDI_dicotomica, family=binomial, PPLUS)
summary (mf1.1.tend)

#Exportacion de la BBDD de trabajo a excel

write.xlsx(PPLUS,"/F:/ARTICULOS/Lucia/ART vit K - cognicion/R/Ingesta de vit K y MMSE/PPLUS_cog.xlsx", sheetName="cogni1", col.names=T)
