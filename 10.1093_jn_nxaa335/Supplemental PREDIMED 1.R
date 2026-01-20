#Analisis de sensibilidad. A1: sin fracturas previas
#Formacion de la BBDD.
cluster3 <- subset(cluster2, fractura1 == 0)

#Terciles de Pral y NEAP.
cluster3$ter_PRAL <- cut2(cluster3$media_PRAL,g = 3)
cluster3$ter_PRAL <- relevel(cluster3$ter_PRAL,ref = "[ -2.73, 4.75)")
levels(cluster3$ter_PRAL)

cluster3$ter_NEAP <- cut2(cluster3$media_NEAP,g = 3)
cluster3$ter_NEAP <- relevel(cluster3$ter_NEAP,ref = "[36.6,40.9)")
levels(cluster3$ter_NEAP)
str(cluster3$ter_NEAP)

#Medias y eventos.
table(cluster3$ter_PRAL, cluster3$fract_osteo_new)
with(cluster3, prop.table(table(ter_PRAL,fract_osteo_new), margin = 1))
tapply(cluster3$media_PRAL, cluster3$ter_PRAL, mean)
tapply(cluster3$media_PRAL, cluster3$ter_PRAL, sd)

table(cluster3$ter_NEAP, cluster3$fract_osteo_new)
with(cluster3, prop.table(table(ter_NEAP,fract_osteo_new), margin = 1))
tapply(cluster3$media_NEAP, cluster3$ter_NEAP, mean)
tapply(cluster3$media_NEAP, cluster3$ter_NEAP, sd)

#Inicio del analisis.
#Basal
options(scipen = 999)
asp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_PRAL + cluster(idcluster), robust=T, data = cluster3)
summary(asp1)

asn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + cluster(idcluster), robust=T, data = cluster3)
summary(asn1)

#M1
asp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
             , robust=T, data = cluster3)
summary(asp2)

asn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
            , robust=T, data = cluster3)
summary(asn2)

#M2
asp3 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster3)
summary(asp3)

asn3 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
            + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
            + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster3)
summary(asn3)

#Analisis continuo cuadratico.
#Basal
casp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + cluster(idcluster), robust = T
            , data = cluster3)
summary(casp1)

casn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + cluster(idcluster), robust = T
            , data = cluster3)
summary(casn1)

#M1
casp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
               grup_int + cluster(idcluster), robust = T, data = cluster3)
summary(casp2)

casn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
               grup_int + cluster(idcluster), robust = T, data = cluster3)
summary(casn2)

#M2
casp3 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
               diabetes0 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster3)
summary(casp3)

casn3 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
               diabetes0 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster3)
summary(casn3)

#P q tendencia.
#Creacion variable p tendencia
tapply(cluster3$media_PRAL, cluster3$ter_PRAL, median)
levels(cluster3$ter_PRAL)
cluster3$tend_PRAL <- Recode(cluster3$ter_PRAL, "'[ -2.73, 4.75)' = 1.117656; '[-28.98,-2.73)' =-7.086428; '[  4.75,28.98]' = 8.661089")
cluster3$tend_PRAL<- as.numeric(cluster3$tend_PRAL)
cluster3$tend_PRAL <- Recode(cluster3$tend_PRAL, "'2' = 1.117656; '1' =-7.086428; '3' = 8.661089")
summary(cluster3$tend_PRAL)
levels(cluster3$tend_PRAL)
table(cluster3$ter_PRAL, cluster3$tend_PRAL)
str(cluster3$tend_PRAL)

tapply(cluster3$media_NEAP, cluster3$ter_NEAP, median)
levels(cluster3$ter1_NEAP)
cluster3$tend_NEAP <- Recode(cluster3$ter_NEAP, "'[36.6,40.9)' = 38.66176; '[18.3,36.6)' = 33.91788; '[40.9,54.5]' = 43.65002")
cluster3$tend_NEAP <- as.numeric(cluster3$tend_NEAP)
cluster3$tend_NEAP <- Recode(cluster3$tend_NEAP, "'2' = 38.66176; '1' = 33.91788; '3' = 43.65002")
summary(cluster3$tend_NEAP)
levels(cluster3$tend_NEAP)
table(cluster3$ter_NEAP, cluster3$tend_NEAP)
str(cluster3$tend_NEAP)

#Basal
tasp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + cluster(idcluster), robust = T, data = cluster3)
summary(tasp1)

tasn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + cluster(idcluster), robust = T, data = cluster3)
summary(tasn1)

#M1
tasp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
               grup_int + cluster(idcluster), robust = T, data = cluster3)
summary(tasp2)

tasn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
               grup_int + cluster(idcluster), robust = T, data = cluster3)
summary(tasn2)

#M2
tasp3 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
               insulin1 + diabetes0 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster3)
summary(tasp3)

tasn3 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
               insulin1 + diabetes0 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster3)
summary(tasn3)
############################################
#Analisis de sensibilidad A2: sin fracturas en T cox <1a
cluster4 <- subset(cluster2, fract_osteo_new == 1 & T_cox_new >= 1 | T_cox_new!=0 & fract_osteo_new == 0)

#Terciles de Pral.
cluster4$ter_PRAL <- cut2(cluster4$media_PRAL,g = 3)
cluster4$ter_PRAL <- relevel(cluster4$ter_PRAL,ref = "[ -2.47, 4.53)")
levels(cluster4$ter_PRAL)

cluster4$ter_NEAP <- cut2(cluster4$media_NEAP,g = 3)
cluster4$ter_NEAP <- relevel(cluster4$ter_NEAP,ref = "[36.7,40.8)")
levels(cluster4$ter_NEAP)
str(cluster4$ter_NEAP)

#Medias y eventos.
table(cluster4$ter_PRAL)
table(cluster4$ter_PRAL, cluster4$fract_osteo_new)
with(cluster4, prop.table(table(ter_PRAL,fract_osteo_new), margin = 1))
tapply(cluster4$media_PRAL, cluster4$ter_PRAL, mean)
tapply(cluster4$media_PRAL, cluster4$ter_PRAL, sd)

table(cluster4$ter_NEAP, cluster4$fract_osteo_new)
with(cluster4, prop.table(table(ter_NEAP,fract_osteo_new), margin = 1))
tapply(cluster4$media_NEAP, cluster4$ter_NEAP, mean)
tapply(cluster4$media_NEAP, cluster4$ter_NEAP, sd)

#Inicio del analisis.
#Basal
options(scipen = 999)
asp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_PRAL + cluster(idcluster), robust=T, data = cluster4)
summary(asp1)

asn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + cluster(idcluster), robust=T, data = cluster4)
summary(asn1)

#M1
asp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
              , robust=T, data = cluster4)
summary(asp2)

asn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
              , robust=T, data = cluster4)
summary(asn2)

#M2
asp3 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster4)
summary(asp3)

asn3 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster4)
summary(asn3)

#Analisis continuo cuadratico.
#Basal
casp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + cluster(idcluster), robust = T
               , data = cluster4)
summary(casp1)

casn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + cluster(idcluster), robust = T
               , data = cluster4)
summary(casn1)

#M1
casp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster4)
summary(casp2)

casn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster4)
summary(casn2)

#M2
casp3 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
                 diabetes0 + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster4)
summary(casp3)

casn3 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
                 diabetes0 + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster4)
summary(casn3)

#P q tendencia.
#Creacion variable p tendencia
tapply(cluster4$media_PRAL, cluster4$ter_PRAL, median)
levels(cluster4$ter_PRAL)
cluster4$tend_PRAL <- Recode(cluster4$ter_PRAL, "'[ -2.47, 4.53)' = 1.068500; '[-28.98,-2.47)' =-6.911479; '[  4.53,28.98]' = 8.632769")
cluster4$tend_PRAL<- as.numeric(cluster4$tend_PRAL)
cluster4$tend_PRAL <- Recode(cluster4$tend_PRAL, "'2' = 1.068500; '1' =-6.911479; '3' = 8.632769")
summary(cluster4$tend_PRAL)
levels(cluster4$tend_PRAL)
table(cluster4$ter_PRAL, cluster4$tend_PRAL)
str(cluster4$tend_PRAL)

tapply(cluster4$media_NEAP, cluster4$ter_NEAP, median)
levels(cluster4$ter_NEAP)
cluster4$tend_NEAP <- Recode(cluster4$ter_NEAP, "'[36.7,40.8)' = 38.65412; '[18.3,36.7)' = 34.09120; '[40.8,57.0]' = 43.74351")
cluster4$tend_NEAP <- as.numeric(cluster4$tend_NEAP)
cluster4$tend_NEAP <- Recode(cluster4$tend_NEAP, "'2' = 38.65412; '1' = 34.09120; '3' = 43.74351")
summary(cluster4$tend_NEAP)
levels(cluster4$tend_NEAP)
table(cluster4$ter_NEAP, cluster4$tend_NEAP)
str(cluster4$tend_NEAP)

#Basal
tasp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + cluster(idcluster), robust = T, data = cluster4)
summary(tasp1)

tasn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + cluster(idcluster), robust = T, data = cluster4)
summary(tasn1)

#M1
tasp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster4)
summary(tasp2)

tasn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster4)
summary(tasn2)

#M2
tasp3 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
                 insulin1 + fractura1 + diabetes0 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster4)
summary(tasp3)

tasn3 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
                 insulin1 + fractura1 + diabetes0 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster4)
summary(tasn3)

##################################################
#Analisis de sensibilidad. A3: sin T cox < 1a
cluster5 <- subset(cluster2, T_cox_new >= 1)

#Terciles de Pral.
cluster5$ter_PRAL <- cut2(cluster5$media_PRAL,g = 3)
cluster5$ter_PRAL <- relevel(cluster5$ter_PRAL,ref = "[ -2.54, 4.50)")
levels(cluster5$ter_PRAL)

cluster5$ter_NEAP <- cut2(cluster5$media_NEAP,g = 3)
cluster5$ter_NEAP <- relevel(cluster5$ter_NEAP,ref = "[36.6,40.7)")
levels(cluster5$ter_NEAP)
str(cluster5$ter_NEAP)

#Medias y eventos.
table(cluster5$ter_PRAL)
table(cluster5$ter_PRAL, cluster5$fract_osteo_new)
with(cluster5, prop.table(table(ter_PRAL,fract_osteo_new), margin = 1))
tapply(cluster5$media_PRAL, cluster5$ter_PRAL, mean)
tapply(cluster5$media_PRAL, cluster5$ter_PRAL, sd)

table(cluster5$ter_NEAP, cluster5$fract_osteo_new)
with(cluster5, prop.table(table(ter_NEAP,fract_osteo_new), margin = 1))
tapply(cluster5$media_NEAP, cluster5$ter_NEAP, mean)
tapply(cluster5$media_NEAP, cluster5$ter_NEAP, sd)

#Inicio del analisis.
#Basal
options(scipen = 999)
asp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_PRAL + cluster(idcluster), robust=T, data = cluster5)
summary(asp1)

asn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + cluster(idcluster), robust=T, data = cluster5)
summary(asn1)

#M1
asp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
              , robust=T, data = cluster5)
summary(asp2)

asn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
              , robust=T, data = cluster5)
summary(asn2)

#M2
asp3 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster5)
summary(asp3)

asn3 <- coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster5)
summary(asn3)

#Analisis continuo cuadratico.
#Basal
casp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + cluster(idcluster), robust = T
               , data = cluster5)
summary(casp1)

casn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + cluster(idcluster), robust = T
               , data = cluster5)
summary(casn1)

#M1
casp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster5)
summary(casp2)

casn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster5)
summary(casn2)

#M2
casp3 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
                 diabetes0 + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster5)
summary(casp3)

casn3 <- coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
                 diabetes0 + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster5)
summary(casn3)

#P q tendencia.
#Creacion variable p tendencia
tapply(cluster5$media_PRAL, cluster5$ter_PRAL, median)
levels(cluster5$ter_PRAL)
cluster5$tend_PRAL <- Recode(cluster5$ter_PRAL, "'[ -2.54, 4.50)' = 1.052073; '[-28.98,-2.54)' =-6.958571; '[  4.50,28.98]' = 8.515449")
cluster5$tend_PRAL<- as.numeric(cluster5$tend_PRAL)
cluster5$tend_PRAL <- Recode(cluster5$tend_PRAL, "'2' = 1.052073; '1' =-6.958571; '3' = 8.515449")
summary(cluster5$tend_PRAL)
levels(cluster5$tend_PRAL)
table(cluster5$ter_PRAL, cluster5$tend_PRAL)
str(cluster5$tend_PRAL)

tapply(cluster5$media_NEAP, cluster5$ter_NEAP, median)
levels(cluster5$ter1_NEAP)
cluster5$tend_NEAP <- Recode(cluster5$ter_NEAP, "'[36.6,40.7)' = 38.64732; '[18.3,36.6)' = 34.08279; '[40.7,57.0]' = 43.71394")
cluster5$tend_NEAP <- as.numeric(cluster5$tend_NEAP)
cluster5$tend_NEAP <- Recode(cluster5$tend_NEAP, "'2' = 38.64732; '1' = 34.08279; '3' = 43.71394")
summary(cluster5$tend_NEAP)
levels(cluster5$tend_NEAP)
table(cluster5$ter_NEAP, cluster5$tend_NEAP)
str(cluster5$tend_NEAP)

#Basal
tasp1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + cluster(idcluster), robust = T, data = cluster5)
summary(tasp1)

tasn1 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + cluster(idcluster), robust = T, data = cluster5)
summary(tasn1)

#M1
tasp2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster5)
summary(tasp2)

tasn2 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster5)
summary(tasn2)

#M2
tasp3 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
                 insulin1 + fractura1 + diabetes0 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster5)
summary(tasp3)

tasn3 <- coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
                 insulin1 + fractura1 + diabetes0 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster5)
summary(tasn3)

#Diferencias de PROT entre terciles.
PREDIMED$por.prot = ((PREDIMED$prot*4)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.prot ~ PREDIMED$ter_PRAL)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.prot, PREDIMED$ter_PRAL, mean)
tapply(PREDIMED$por.prot, PREDIMED$ter_PRAL, sd)

PREDIMED$por.hc = ((PREDIMED$hc*4)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.hc ~ PREDIMED$ter_PRAL)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.hc, PREDIMED$ter_PRAL, mean)
tapply(PREDIMED$por.hc, PREDIMED$ter_PRAL, sd)

PREDIMED$por.gratot = ((PREDIMED$gratot *9)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.gratot ~ PREDIMED$ter_PRAL)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.gratot, PREDIMED$ter_PRAL, mean)
tapply(PREDIMED$por.gratot, PREDIMED$ter_PRAL, sd)

PREDIMED$por.mo = ((PREDIMED$mo *9)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.mo ~ PREDIMED$ter_PRAL)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.mo, PREDIMED$ter_PRAL, mean)
tapply(PREDIMED$por.mo, PREDIMED$ter_PRAL, sd)

PREDIMED$por.sa = ((PREDIMED$sa *9)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.sa ~ PREDIMED$ter_PRAL)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.sa, PREDIMED$ter_PRAL, mean)
tapply(PREDIMED$por.sa, PREDIMED$ter_PRAL, sd)

PREDIMED$por.po = ((PREDIMED$po *9)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.po ~ PREDIMED$ter_PRAL)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.po, PREDIMED$ter_PRAL, mean)
tapply(PREDIMED$por.po, PREDIMED$ter_PRAL, sd)

#Diferencias de PROT entre terciles NEAP.
PREDIMED$por.prot = ((PREDIMED$prot*4)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.prot ~ PREDIMED$ter_NEAP)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.prot, PREDIMED$ter_NEAP, mean)
tapply(PREDIMED$por.prot, PREDIMED$ter_NEAP, sd)

PREDIMED$por.hc = ((PREDIMED$hc*4)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.hc ~ PREDIMED$ter_NEAP)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.hc, PREDIMED$ter_NEAP, mean)
tapply(PREDIMED$por.hc, PREDIMED$ter_NEAP, sd)

PREDIMED$por.gratot = ((PREDIMED$gratot *9)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.gratot ~ PREDIMED$ter_NEAP)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.gratot, PREDIMED$ter_NEAP, mean)
tapply(PREDIMED$por.gratot, PREDIMED$ter_NEAP, sd)

PREDIMED$por.mo = ((PREDIMED$mo *9)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.mo ~ PREDIMED$ter_NEAP)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.mo, PREDIMED$ter_NEAP, mean)
tapply(PREDIMED$por.mo, PREDIMED$ter_NEAP, sd)

PREDIMED$por.sa = ((PREDIMED$sa *9)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.sa ~ PREDIMED$ter_NEAP)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.sa, PREDIMED$ter_NEAP, mean)
tapply(PREDIMED$por.sa, PREDIMED$ter_NEAP, sd)

PREDIMED$por.po = ((PREDIMED$po *9)/PREDIMED$energiat)*100
prueba = aov(PREDIMED$por.po ~ PREDIMED$ter_NEAP)
summary(prueba)
TukeyHSD(prueba)
tapply(PREDIMED$por.po, PREDIMED$ter_NEAP, mean)
tapply(PREDIMED$por.po, PREDIMED$ter_NEAP, sd)

#CKD
PREDIMED$IRC_CKD_EPI_v0 <- as.factor(PREDIMED$IRC_CKD_EPI_v0)
table(PREDIMED$ter_PRAL, PREDIMED$IRC_CKD_EPI_v0)
with(PREDIMED, prop.table(table(ter_PRAL,IRC_CKD_EPI_v0), margin = 1))
chisq.test(PREDIMED$IRC_CKD_EPI_v0, PREDIMED$ter_PRAL)
mean(PREDIMED$CKD_EPI_v0, na.rm = T)
tapply(PREDIMED$CKD_EPI_v0, PREDIMED$ter_PRAL, mean, na.rm = T)
prueba = aov(PREDIMED$CKD_EPI_v0 ~ PREDIMED$ter_PRAL)
summary(prueba)
TukeyHSD(prueba)

#IG

tapply(PREDIMED$IG_Mod, PREDIMED$ter_PRAL, mean, na.rm = T)
prueba = aov(PREDIMED$IG_Mod ~ PREDIMED$ter_PRAL)
summary(prueba)
TukeyHSD(prueba)

####----TABLE 1: differences of yearly averaged food between tertiles of PRAL (recommended by reviewer)

tapply(cluster2$media_ENERGIA, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_ENERGIA, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_ENERGIA ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_PROTEINA, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_PROTEINA, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_PROTEINA ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_hc, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_hc, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_hc ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_gratot, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_gratot, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_gratot ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_fibr, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_fibr, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_fibr ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_alcohol, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_alcohol, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_alcohol ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_vitaminaD, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_vitaminaD, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_vitaminaD ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_calci, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_calci, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_calci ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_fosforo, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_fosforo, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_fosforo ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_k, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_k, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_k ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_mg, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_mg, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_mg ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_vitaminaD, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_vitaminaD, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_vitaminaD ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_PRAL, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_PRAL, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_PRAL ~ cluster2$ter1_PRAL)
summary(dif)

tapply(cluster2$media_NEAP, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_NEAP, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_NEAP ~ cluster2$ter1_PRAL)
summary(dif)
