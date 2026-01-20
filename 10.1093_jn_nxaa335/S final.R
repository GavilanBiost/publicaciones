#Apertura de la BBDD Predimed
library(haven)
library("rms")
library("splines")
library("ggplot2")
library("survival")
library("Hmisc")
library(car)
library(xlsx)
library(gdata)
library(effects)
library(readxl)
library(faraway)
library(lmtest)

options(scipen = 999)
#Tabla 1: eGFR por terciles de PRAL

tapply(cluster2$CKD_EPI_v0, cluster2$ter1_PRAL, mean, na.rm = T)
tapply(cluster2$CKD_EPI_v0, cluster2$ter1_PRAL, sd, na.rm = T)
summary(aov(CKD_EPI_v0~ter1_PRAL, cluster2))

cluster2$prev.nefro = cut2(cluster2$CKD_EPI_v0,cuts = 60)
tapply(cluster2$CKD_EPI_v0, cluster2$prev.nefro, min, na.rm = T)
tapply(cluster2$CKD_EPI_v0, cluster2$prev.nefro, max, na.rm = T)
with(cluster2, prop.table(table(IRC_CKD_EPI_v0,ter1_PRAL), margin = 1))
with(cluster2, prop.table(table(IRC_CKD_EPI_v0,ter1_PRAL), margin = 2))

CR = PREDIMED$id
CR1 = PREDIMED$CreatininaSuero_v0
CREAT = cbind(CR, CR1)
colnames(CREAT) = c("id", "CreatininaSuero_v0")

C2 = merge(cluster2, CREAT, by.x = "id")

tapply(C2$CreatininaSuero_v0, C2$ter1_PRAL, mean, na.rm = T)

table(cluster2$IRC_CKD_EPI_v0,cluster2$ter1_PRAL)

#PREDIMED <- read_sav("/Volumes/JES GAVI/ARTICULOS/BBDD/(ref) PREDIMED Base de datos limpia (noviembre 2015) Reus.SAV")

PREDIMED <- read_sav("F:/ARTICULOS/BBDD/PREDIMED/(ref) PREDIMED Base de datos limpia (noviembre 2015) Reus.SAV")
View(PREDIMED)

#Fracturas por terciles:
#PRAL.
with(cluster2, prop.table(table(ter1_PRAL,fract_osteo_new), margin = 1))
table(cluster2$ter1_PRAL, cluster2$fract_osteo_new)

#Terciles de Pral.
PREDIMED$ter_PRAL <- cut2(PREDIMED$media_PRAL,g = 3)
PREDIMED$ter1_PRAL <- relevel(PREDIMED$ter_PRAL,ref = "[ -2.43, 4.63)")
levels(PREDIMED$ter1_PRAL)

PREDIMED$ter_NEAP <- cut2(PREDIMED$media_NEAP,g = 3)
PREDIMED$ter_NEAP <- relevel(PREDIMED$ter_NEAP,ref = "[36.7,40.8)")
levels(PREDIMED$ter_NEAP)
str(PREDIMED$ter_NEAP)

#Fusion y creacion de BBDD con cluster1. 

cluster1 <- read_dta("F:/ARTICULOS/BBDD/PREDIMED/BD variables claves_28062018.dta")

str(cluster1$id)

cluster1$id = as.numeric(cluster1$id)

str(PREDIMED$id)

PREDIMED1 = PREDIMED[,c("id", "peso1", "sexo", "edad0", "imc1", "getota_1", "tabaco0","escolar1", "fractura1", "diabetes0", "hta0", "insulin1", "m_osteoporosis",
                        "m_aas", "ado1", "hormo1", "energiat", "prot", "hc", "gratot", "mo", "sa", "po", "alcoholg", "fibra", "vitD", "calcio",
                        "k", "p", "mg", "PRAL", "T_cox_new", "fract_osteo_new", "ter1_PRAL", "grup_int", "media_ENERGIA", "media_PRAL", "media_NEAP",
                        "ter_NEAP", "IRC_CKD_EPI_v0", "CKD_EPI_v0", "media_CARGLUCE_Mod", "media_IG_Mod", "CARGLUCE_Mod", "IG_Mod", "media_fibr", "media_alcohol",
                        "media_gratot", "media_hc", "media_vitaminaD", "media_PROTEINA", "media_prot_anim","media_prot_veg", "media_k", "media_calci", "media_fosforo",
                        "media_mg")]

PREDIMED1$id = as.numeric(PREDIMED1$id)

cluster2 = merge(PREDIMED1, cluster1, by = "id")

#Inicio del analisis.
#Basal
a <- coxph(Surv(T_cox_new, fract_osteo_new)~ter1_PRAL, data = PREDIMED)
summary(a)

aa <- coxph(Surv(T_cox_new, fract_osteo_new)~ter1_PRAL + cluster(idcluster), robust=T, data = cluster2)
summary(aa)

b <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + cluster(idcluster), robust=T, data = cluster2)
summary(b)

#Interacciones.

ab <- coxph(Surv(T_cox_new, fract_osteo_new)~ter1_PRAL * strata(sexo) + cluster(idcluster), robust=T, data = cluster2)
summary(ab)

bb <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP * sexo + cluster(idcluster), robust=T, data = cluster2)
summary(bb)

ab2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL * sexo + getota_1 + imc1 + tabaco0 + escolar1 + edad0 + insulin1 + diabetes0 
             + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster)  + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(ab2)

bb2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP * sexo + getota_1 + imc1 + tabaco0 + escolar1 + edad0 + insulin1 + diabetes0 
            + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
            + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(bb2)

ac2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL * diabetes0 + getota_1 + imc1 + tabaco0 + escolar1 + edad0 + insulin1 + sexo 
             + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster)  + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(ac2)

ba2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP * CKD_EPI_v0 + diabetes0 + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(ba2)

lrtest(aa, ab)

lrtest(b, bb)

lrtest(aa2, ab2)

lrtest(b2, bb2)

lrtest(aa2, ac2)

lrtest(b2, bc2)

cluster2$ter_grasa <- cut2(cluster2$media_gratot,g = 3)
cluster2$ter_energia <- cut2(cluster2$media_ENERGIA,g = 3)
cluster2$ter_fibra <- cut2(cluster2$media_fibr,g = 3)
cluster2$ter_vitD <- cut2(cluster2$media_vitaminaD,g = 3)
cluster2$ter_hc <- cut2(cluster2$media_hc,g = 3)

aa2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL + strata(hta0) + tabaco0 + imc1 + getota_1 + (sexo) + strata(grup_int) + strata(escolar1) + edad0 + insulin1 + strata(diabetes0) 
             + fractura1 + hormo1 + ter_energia + cluster(idcluster) + CKD_EPI_v0 + m_osteoporosis
             + ter_hc + ter_fibra + ter_vitD + strata(ter_grasa), robust=T, data = cluster2)

aa2.1 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL * strata(ter_grasa) + ter_hc + ter_fibra + ter_vitD + ter_energia + m_osteoporosis + CKD_EPI_v0 + strata(hta0) + hormo1 + fractura1 + strata(diabetes0) + insulin1 + edad0 + strata(escolar1) + strata(grup_int) +(sexo) +getota_1 +imc1 + tabaco0
             + cluster(idcluster), robust=T, data = cluster2)

lrtest(aa2, aa2.1)

aa2.1 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL + tabaco0 +
                 + cluster(idcluster), robust=T, data = cluster2)

summary(aa2.1)


#M1
a1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter1_PRAL + getota_1 + imc1 + tabaco0 + strata(sexo) + edad0 + grup_int, data = PREDIMED)
summary(a1)

aa1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter1_PRAL + getota_1 + imc1 + tabaco0 + strata(sexo) + edad0+ grup_int + cluster(idcluster)
             , robust=T, data = cluster2)
summary(aa1)

b1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
            , robust=T, data = cluster2)
summary(b1)

#M2
a2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL * media_ENERGIA + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 + insulin1 + diabetes0 
            + fractura1 + hormo1 + grup_int + hta0 + CKD_EPI_v0 
            + media_hc + media_fibr + media_vitaminaD + media_gratot, data = PREDIMED)
summary(a2)

aa2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL + (tabaco0) + imc1 + getota_1 + (sexo) + (grup_int) + escolar1 + edad0 + (insulin1) + (diabetes0) 
             + (fractura1) + (hormo1) + media_ENERGIA + cluster(idcluster)  + (hta0) + CKD_EPI_v0 + m_osteoporosis
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(aa2)

b2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP + media_ENERGIA + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 + m_osteoporosis
            + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(b2)

#Analisis continuo cuadratico.
#Basal
#a <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2), data = PREDIMED)
#summary(a)

#ab <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + cluster(idcluster), robust = T
#            , data = cluster2)
#summary(ab)

#bb <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + cluster(idcluster), robust = T
#            , data = cluster2)
#summary(bb)

#M1
#a1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 +
#              grup_int, data = PREDIMED)
#summary(a1)

#ab1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 +
#              grup_int + cluster(idcluster), robust = T, data = cluster2)
#summary(ab1)

#bb1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
#               grup_int + cluster(idcluster), robust = T, data = cluster2)
#summary(bb1)

#M2
#a2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + strata(sexo) + edad0 + insulin1 + 
#              diabetes0 + fractura1 + hormo1 + media_ENERGIA + grup_int + hta0 + CKD_EPI_v0 
#            + media_hc + media_fibr + media_vitaminaD + media_gratot, data = PREDIMED)
#summary(a2)

#ab2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + strata(sexo) + edad0 + insulin1 + 
#              diabetes0 + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + m_osteoporosis
#             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster2)
#summary(ab2)

#bb2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
#               diabetes0 + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + m_osteoporosis
#             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster2)
#summary(bb2)

#P q tendencia.
#Creacion variable p tendencia
#tapply(PREDIMED$media_PRAL, PREDIMED$ter1_PRAL, median)
#levels(PREDIMED$ter1_PRAL)
#PREDIMED$tend_PRAL <- Recode(PREDIMED$ter1_PRAL, "'[ -2.43, 4.63)' = 1.117656; '[-28.98,-2.43)' =-6.911479; '[  4.63,28.98]' = 8.669533")
#PREDIMED$tend_PRAL<- as.numeric(PREDIMED$tend_PRAL)
#PREDIMED$tend_PRAL <- Recode(PREDIMED$tend_PRAL, "'2' = 1.117656; '1' =-6.911479; '3' = 8.669533")
#summary(PREDIMED$tend_PRAL)
#levels(PREDIMED$tend_PRAL)
#table(PREDIMED$ter1_PRAL, PREDIMED$tend_PRAL)
#str(PREDIMED$tend_PRAL)

tapply(cluster2$media_PRAL, cluster2$ter1_PRAL, median)
levels(cluster2$ter1_PRAL)
cluster2$tend_PRAL <- Recode(cluster2$ter1_PRAL, "'[ -2.43, 4.63)' = 1.117656; '[-28.98,-2.43)' =-6.911479; '[  4.63,28.98]' = 8.669533")
cluster2$tend_PRAL<- as.numeric(cluster2$tend_PRAL)
cluster2$tend_PRAL <- Recode(cluster2$tend_PRAL, "'2' = 1.117656; '1' =-6.911479; '3' = 8.669533")
summary(cluster2$tend_PRAL)
levels(cluster2$tend_PRAL)
table(cluster2$ter1_PRAL, cluster2$tend_PRAL)
str(cluster2$tend_PRAL)

tapply(cluster2$media_NEAP, cluster2$ter_NEAP, median)
levels(cluster2$ter1_NEAP)
cluster2$tend_NEAP <- Recode(cluster2$ter_NEAP, "'[36.7,40.8)' = 38.66176; '[18.3,36.7)' = 34.06728; '[40.8,57.0]' = 43.79941")
cluster2$tend_NEAP <- as.numeric(cluster2$tend_NEAP)
cluster2$tend_NEAP <- Recode(cluster2$tend_NEAP, "'2' = 38.66176; '1' = 34.06728; '3' = 43.79941")
summary(cluster2$tend_NEAP)
levels(cluster2$tend_NEAP)
table(cluster2$ter_NEAP, cluster2$tend_NEAP)
str(cluster2$tend_NEAP)

#Basal
a <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2), data = PREDIMED)
summary(a)

ac <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + cluster(idcluster), robust = T, data = cluster2)
summary(ac)

bc <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + cluster(idcluster), robust = T, data = cluster2)
summary(bc)

#M1
a1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 +
              grup_int, data = PREDIMED)
summary(a1)

ac1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 +
              grup_int + cluster(idcluster), robust = T, data = cluster2)
summary(ac1)

bc1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
               grup_int + cluster(idcluster), robust = T, data = cluster2)
summary(bc1)

#M2
a2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 + 
              insulin1 + diabetes0 + fractura1 + hormo1 + grup_int + media_ENERGIA, data = PREDIMED)
summary(a2)

ac2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 + 
              insulin1 + diabetes0 + fractura1 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster2)
summary(ac2)

bc2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
               insulin1 + diabetes0 + fractura1 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster2)
summary(bc2)

#Rango intercuartil

tapply(cluster2$media_PRAL, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_NEAP, cluster2$ter_NEAP, mean)
tapply(cluster2$media_PRAL, cluster2$ter1_PRAL, sd)
tapply(cluster2$media_NEAP, cluster2$ter_NEAP, sd)

#Subanalisis con prot, prot veg y prot animal.
cluster2$ter_PROT <- cut2(cluster2$media_PROTEINA,g = 3)
levels(cluster2$ter_PROT)
str(cluster2$ter_PROT)
cluster2$ter_PROT <- relevel(cluster2$ter_PROT,ref = "[83.9, 96.0)")

cluster2$ter_PROT_AN <- cut2(cluster2$media_prot_anim,g = 3)
levels(cluster2$ter_PROT_AN)
str(cluster2$ter_PROT_AN)
cluster2$ter_PROT_AN <- relevel(cluster2$ter_PROT_AN,ref = "[55.5, 64.4)")

cluster2$ter_PROT_VEG <- cut2(cluster2$media_prot_veg,g = 3)
levels(cluster2$ter_PROT_VEG)
str(cluster2$ter_PROT_VEG)
cluster2$ter_PROT_VEG <- relevel(cluster2$ter_PROT_VEG,ref = "[27.1,31.8)")

tapply(cluster2$media_prot_anim, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_prot_veg, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_prot_anim, cluster2$ter1_PRAL, sd)
tapply(cluster2$media_prot_veg, cluster2$ter1_PRAL, sd)

#Proteina total
pp1 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PROT + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 + insulin1 + diabetes0 
            + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
            + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pp1)

pp2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PROT_AN + getota_1 + imc1 + tabaco0 + escolar1 + strata(sexo) + edad0 + insulin1 + diabetes0 
            + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
            + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pp2)

pp3 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PROT_VEG + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
            + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
            + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pp3)

#Subanalisis con k.

cluster2$ter_k <- cut2(cluster2$media_k,g = 3)
levels(cluster2$ter_k)
str(cluster2$ter_k)
cluster2$ter_k <- relevel(cluster2$ter_k,ref = "[4177,7497]")

pp4 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_k + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pp4)

#Ratios
cluster2$PT.K = cluster2$media_PROTEINA/(cluster2$media_k/1000)
cluster2$PA.K = cluster2$media_prot_anim/(cluster2$media_k/1000)
cluster2$PV.K = cluster2$media_prot_veg/(cluster2$media_k/1000)
cluster2$PT.K.mEq = cluster2$media_PROTEINA/(cluster2$media_k/39)
cluster2$PA.K.mEq = cluster2$media_prot_anim/(cluster2$media_k/39)
cluster2$PV.K.mEq = cluster2$media_prot_veg/(cluster2$media_k/39)

cluster2$ter_PT.K <- cut2(cluster2$PT.K,g = 3)
levels(cluster2$ter_PT.K)
str(cluster2$ter_PT.K)
cluster2$ter_PT.K <- relevel(cluster2$ter_PT.K,ref = "[21.9,23.8)")

cluster2$ter_PA.K <- cut2(cluster2$PA.K,g = 3)
levels(cluster2$ter_PA.K)
str(cluster2$ter_PA.K)
cluster2$ter_PA.K <- relevel(cluster2$ter_PA.K,ref = "[14.4,16.2)")

cluster2$ter_PV.K <- cut2(cluster2$PV.K,g = 3)
levels(cluster2$ter_PV.K)
str(cluster2$ter_PV.K)
cluster2$ter_PV.K <- relevel(cluster2$ter_PV.K,ref = "[6.96, 8.02)")

cluster2$ter_PT.K.mEq <- cut2(cluster2$PT.K.mEq,g = 3)
levels(cluster2$ter_PT.K.mEq)
str(cluster2$ter_PT.K.mEq)
cluster2$ter_PT.K.mEq <- relevel(cluster2$ter_PT.K.mEq,ref = "[0.853,0.927)")

cluster2$ter_PA.K.mEq <- cut2(cluster2$PA.K.mEq,g = 3)
levels(cluster2$ter_PA.K.mEq)
str(cluster2$ter_PA.K.mEq)
cluster2$ter_PA.K.mEq <- relevel(cluster2$ter_PA.K.mEq,ref = "[0.561,0.632)")

cluster2$ter_PV.K.mEq <- cut2(cluster2$PV.K.mEq,g = 3)
levels(cluster2$ter_PV.K.mEq)
str(cluster2$ter_PV.K.mEq)
cluster2$ter_PV.K.mEq <- relevel(cluster2$ter_PV.K.mEq,ref = "[0.271,0.313)")

pptk1 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PT.K + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pptk1)

pptk2 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PA.K + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pptk2)

pptk3 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PV.K + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pptk3)

pptk4 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PT.K.mEq + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
               + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pptk4)

pptk5 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PA.K.mEq + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
               + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pptk5)

pptk6 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PV.K.mEq + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
               + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(pptk6)

tapply(cluster2$media_PROTEINA, cluster2$ter_PROT, mean)
tapply(cluster2$media_PROTEINA, cluster2$ter_PROT, sd)
tapply(cluster2$media_prot_anim, cluster2$ter_PROT_AN, mean)
tapply(cluster2$media_prot_anim, cluster2$ter_PROT_AN, sd)
tapply(cluster2$media_prot_veg, cluster2$ter_PROT_VEG, mean)
tapply(cluster2$media_prot_veg, cluster2$ter_PROT_VEG, sd)

tapply(cluster2$PT.K, cluster2$ter_PT.K, mean)
tapply(cluster2$PT.K, cluster2$ter_PT.K, sd)
tapply(cluster2$PA.K, cluster2$ter_PA.K, mean)
tapply(cluster2$PA.K, cluster2$ter_PA.K, sd)
tapply(cluster2$PV.K, cluster2$ter_PV.K, mean)
tapply(cluster2$PV.K, cluster2$ter_PV.K, sd)

tapply(cluster2$PT.K.mEq, cluster2$ter_PT.K.mEq, mean)
tapply(cluster2$PT.K.mEq, cluster2$ter_PT.K.mEq, sd)
tapply(cluster2$PA.K.mEq, cluster2$ter_PA.K.mEq, mean)
tapply(cluster2$PA.K.mEq, cluster2$ter_PA.K.mEq, sd)
tapply(cluster2$PV.K.mEq, cluster2$ter_PV.K.mEq, mean)
tapply(cluster2$PV.K.mEq, cluster2$ter_PV.K.mEq, sd)

#Modelo de poisson entre PRAL y fracturas.

library(xlsx)

#Extraemos la BBDD para crear una variable de recuento.

write.xlsx(cluster2,"/D:/ARTICULOS/ART Pral - fractures (working)/Estadística/R/PRALcluster2.xlsx", sheetName="Satin_basal", col.names=T)

library(readxl)

cluster2 <- read_excel("D:/ARTICULOS/ART Pral - fractures (working)/Estadística/R/PRAL/cluster2.xlsx")

library(tidyverse)

summary(cluster2$media_PRAL)

cluster2$rr <- Recode(cluster2$media_PRAL, "-30:-25 = '-25';-24.99999:-20 = '-20';-19.99999:-15 = '-15';-14.99999:-10 = '-10';-9.99999:-5 = '-5';-4.99999:0 = '0';
             0.00001:5 = '5';5.00001:10 = '10';10.00001:15 = '15';15.00001:20 = '20';20.00001:25 = '25';25.00001:30 = '30';")

cluster2$rr <- relevel(as.factor(cluster2$rr),ref = "0")
table(as.factor(cluster2$rr))
cluster2$rr = as.factor(cluster2$rr)
levels(cluster2$rr)

pp <- coxph(Surv(T_cox_new,fract_osteo_new)~as.factor(rr) , robust=T, data = cluster2)
summary(pp)

abc2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + strata(sexo) + edad0 + insulin1 + 
               diabetes0 + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 + m_osteoporosis
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster2)
summary(abc2)

asr = coxph.fit(cluster2$media_PRAL^2, Surv(cluster2$T_cox_new, cluster2$fract_osteo_new), strata = NULL)

rcspline.plot(cluster2$media_PRAL, cluster2$fract_osteo_new, nk = 3, showknots = FALSE, show = "prob",
              statloc = "none", main = "", xlab = "Follow-up mean PRAL (mEq/day)", xrange = c(-25,25))


