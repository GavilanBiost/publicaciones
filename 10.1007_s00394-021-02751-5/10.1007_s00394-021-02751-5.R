library(haven)
library(readxl)
library(Hmisc)
library(agricolae)
library(ggplot2)
library(forestplot)
library(rio)
library(gvlma)

#Calculo de los z-score:

DIIB$cafeina_z = (DIIB$cafeina_total_FFQ/1000 - 8.05)/6.67
DIIB$alcoholg_z = (DIIB$alcoholg_v00 - 13.98)/3.72
DIIB$vitb12_z = (DIIB$vitb12_v00 - 5.15)/2.7
DIIB$vitb6_z = (DIIB$vitb6_v00 - 1.47)/0.74
DIIB$hc_z = (DIIB$hc_v00 - 272.2)/40
DIIB$col_z = (DIIB$col_v00 - 279.4)/51.2
DIIB$energiat_z = (DIIB$energiat_v00 - 2056)/338
DIIB$gratot_z = (DIIB$gratot_v00 - 71.4)/19.4
DIIB$fibra_z = (DIIB$fibra_v00 - 18.8)/4.9
DIIB$acfol_z = (DIIB$acfol_v00 - 273)/70.7
DIIB$ajos_z = (DIIB$ajos_v00 - 4.35)/2.9
DIIB$fe_z = (DIIB$fe_v00 - 13.35)/3.71
DIIB$mg_z = (DIIB$mg_v00 - 310.1)/139.4
DIIB$mo_z = (DIIB$mo_v00 - 27)/6.1
DIIB$vitb3_z = (DIIB$vitb3_v00 - 25.9)/11.7
DIIB$omega3_z = (DIIB$omega3_v00 - 1.06)/1.06
DIIB$linoleico_z = (DIIB$linoleico_v00 - 10.8)/7.5
DIIB$cebollas_z = (DIIB$cebollas_v00 - 35.9)/18.4
DIIB$prot_z = (DIIB$prot_v00 - 79.4)/13.9
DIIB$po_z = (DIIB$po_v00 - 13.88)/3.76
DIIB$vitb2_z = (DIIB$vitb2_v00 - 1.7)/0.79
DIIB$sa_z = (DIIB$sa_v00 - 28.6)/8
DIIB$selenio_z = (DIIB$selenio_v00 - 67)/25.1
DIIB$vitb1_z = (DIIB$vitb1_v00 - 1.7)/0.66
DIIB$trans_z = (DIIB$trans_v00 - 3.15)/3.75
DIIB$vita_z = (DIIB$vita_v00 - 983.9)/518.6
DIIB$vitc_z = (DIIB$vitc_v00 - 118.2)/43.46
DIIB$vitd_z = (DIIB$vitd_v00 - 6.26)/2.21
DIIB$vite_z = (DIIB$vite_v00 - 8.73)/1.49
DIIB$zinc_z = (DIIB$zinc_v00 - 9.84)/2.19
DIIB$te_z = ((DIIB$te_v00/125*2) - 1.69)/1.53
DIIB$bcaroteno_z = (DIIB$bcaroteno - 3718)/1720

#Creación de la variable DII (adicción de los coeficientes para su cálculo posterior)

model = data.frame(DIIs$Variable, DIIs$Coef)
str(model)
model$DIIs.Variable = as.character(model$DIIs.Variable)
model = subset(model, DIIs.Coef != 0)

X2 = data.frame(DIIB[,34:(dim(DIIB)[2])])
b = model

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
  
} 

DIIs2 = X2$model

DIIB = data.frame(cbind(DIIB, DIIs2))

summary(DIIB$DIIs2)

#######-----Tabla 1: caracteristicas basales:

tapply(ftot$DIIs2, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$DIIs2, ftot$tDIIs2, sd, na.rm = T)

tapply(ftot$edad_s1, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$edad_s1, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$edad_s1 ~ ftot$tDIIs2))

tapply(ftot$imc_v00, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$imc_v00, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$imc_v00 ~ ftot$tDIIs2))

tapply(ftot$geaf_tot_v00/7, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$geaf_tot_v00/7, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$geaf_tot_v00 ~ ftot$tDIIs2))

table(as.factor(ftot$sexo_s1), ftot$tDIIs2)
(prop.table(table(as.factor(ftot$sexo_s1), ftot$tDIIs2),2)*100)
chisq.test(as.factor(ftot$sexo_s1), ftot$tDIIs2)
table(ftot$fuma_s1, ftot$tDIIs2)
(prop.table(table(ftot$fuma_s1, ftot$tDIIs2),2)*100)
chisq.test(ftot$fuma_s1, ftot$tDIIs2)

table(ftot$escola_v00, ftot$tDIIs2)
(prop.table(table(ftot$escola_v00, ftot$tDIIs2),2)*100)
chisq.test(ftot$escola_v00, ftot$tDIIs2)

table(ftot$diab_s1, ftot$tDIIs2)
(prop.table(table(ftot$diab_s1, ftot$tDIIs2),2)*100)
chisq.test(ftot$diab_s1, ftot$tDIIs2)

table(ftot$osteopor_v00, ftot$tDIIs2)
(prop.table(table(ftot$osteopor_v00, ftot$tDIIs2),2)*100)
chisq.test(ftot$osteopor_v00, ftot$tDIIs2)

tapply(ftot$energiat_v00, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$energiat_v00, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$energiat_v00 ~ ftot$tDIIs2))

tapply(ftot$calci_v00, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$calci_v00, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$calci_v00 ~ ftot$tDIIs2))

tapply(ftot$vitd_v00, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$vitd_v00, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$vitd_v00 ~ ftot$tDIIs2))

tapply(ftot$fibra_v00, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$fibra_v00, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$fibra_v00 ~ ftot$tDIIs2))

tapply(ftot$alcoholg_v00, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$alcoholg_v00, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$alcoholg_v00 ~ ftot$tDIIs2))

tapply(ftot$cafeina_total_FFQ, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$cafeina_total_FFQ, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$cafeina_total_FFQ ~ ftot$tDIIs2))

tapply(ftot$vitb12_v00, ftot$tDIIs2, mean, na.rm = T)
tapply(ftot$vitb12_v00, ftot$tDIIs2, sd, na.rm = T)
summary(aov(ftot$vitb12_v00 ~ ftot$tDIIs2))

table(ftot$colest_s1, ftot$tDIIs2)
(prop.table(table(ftot$colest_s1, ftot$tDIIs2),2)*100)
chisq.test(ftot$colest_s1, ftot$tDIIs2)

table(ftot$tto_insu_v00, ftot$tDIIs2)
(prop.table(table(ftot$tto_insu_v00, ftot$tDIIs2),2)*100)
chisq.test(ftot$tto_insu_v00, ftot$tDIIs2)

table(ftot$tto_metfor_v00, ftot$tDIIs2)
(prop.table(table(ftot$tto_metfor_v00, ftot$tDIIs2),2)*100)
chisq.test(ftot$tto_metfor_v00, ftot$tDIIs2)

table(ftot$tto_dm_v00, ftot$tDIIs2)
(prop.table(table(ftot$tto_dm_v00, ftot$tDIIs2),2)*100)
chisq.test(ftot$tto_dm_v00, ftot$tDIIs2)

table(ftot$tto_horm_v00, ftot$tDIIs2)
(prop.table(table(ftot$tto_horm_v00, ftot$tDIIs2),2)*100)
chisq.test(ftot$tto_horm_v00, ftot$tDIIs2)

table(ftot$Suple_Ca_vitD, ftot$tDIIs2)
(prop.table(table(ftot$Suple_Ca_vitD, ftot$tDIIs2),2)*100)
chisq.test(ftot$Suple_Ca_vitD, ftot$tDIIs2)

#######----Tablas 2, 3 y 4:
tapply(ftot$DIIs2, ftot$tDIIs2, mean)

m1 = lm(dmoftot_v00 ~ tDIIs2, ftot)
summary(m1)
confint(m1)

m1.1 = lm(dmoftot_v00 ~ tDIIs2 + (diab_s1) + (sexo_s1) + edad_s1 + imc_v00 + (fuma_s1) + (escola_v00) +
            + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
            + tto_insu_v00 + tto_metfor_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, ftot)
summary(m1.1)
confint(m1.1)
anova(m1.1)
(HSD.test(m1.1, 'tDIIs2'))

m1.t = lm(dmoftot_v00 ~ as.numeric(tDIIs2), ftot)
summary(m1.t)

m1.1.t = lm(dmoftot_v00 ~ as.numeric(tDIIs2) + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
              + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
            + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, ftot)
summary(m1.1.t)

#tf

tapply(tf$DIIs2, tf$tDIIs2, mean)
tapply(tf$DIIs2, tf$tDIIs2, sd)

m1 = lm(dmotf_v00 ~ tDIIs2, tf)
summary(m1)
confint(m1)

m1.1 = lm(dmotf_v00 ~ tDIIs2 + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
            + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
          + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, tf)
summary(m1.1)
confint(m1.1)
anova(m1.1)
(HSD.test(m1.1, 'tDIIs2'))

m1.t = lm(dmotf_v00 ~ as.numeric(tDIIs2), tf)
summary(m1.t)

m1.1.t = lm(dmotf_v00 ~ as.numeric(tDIIs2) + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
              + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
            + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, tf)
summary(m1.1.t)

#L1L4

m3 = lm(dmol1l4_v00 ~ tDIIs2, l1l4)
summary(m3)
confint(m3)

m3.1 = lm(dmol1l4_v00 ~ tDIIs2 + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 + 
            + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
          + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, l1l4)
summary(m3.1)
confint(m3.1)
anova(m3.1)
(HSD.test(m3.1, 'tDIIs2'))

m3.t = lm(dmol1l4_v00 ~ as.numeric(tDIIs2), l1l4)
summary(m3.t)

m3.1.t = lm(dmol1l4_v00 ~ as.numeric(tDIIs2) + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
              + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
            + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, l1l4)
summary(m3.1.t)

#Tscore Ftot

tftot$tDIIs2 = cut2(tftot$DIIs2,g = 3)

tftot = merge(tftot, Suple, by = "paciente")

#Genero una nueva variable: osteo (osteopenia o peor)

tftot$osteo = car::recode(tftot$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

table(tftot$osteo, tftot$tDIIs2)
table(tftot$tDIIs2)

# Modelos logisticos:

m2 <- glm(osteo ~ tDIIs2, data = tftot, family = "binomial")
summary(m2)
exp(m2$coefficients)
exp(confint(m2))

m2.1 <- glm(osteo ~ tDIIs2 + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
              + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
            + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tftot, family = "binomial")
summary(m2.1)
exp(m2.1$coefficients)
exp(confint(m2.1))

m2.t <- glm(osteo ~ as.numeric(tDIIs2), data = tftot, family = "binomial")
summary(m2.t)

m2.1.t <- glm(osteo ~ as.numeric(tDIIs2) + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
                + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
              + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tftot, family = "binomial")
summary(m2.1.t)

tapply(tftot$tscoreftot_v00, tftot$tDIIs2, mean)
tapply(tftot$tscoreftot_v00, tftot$tDIIs2, median)

#Tscore tf

table(ttf$osteopor_v00)

ttf$tDIIs2 = cut2(ttf$DIIs2,g = 3)

#Genero una nueva variable: osteo (osteopenia o peor)

ttf$osteo = car::recode(ttf$tscoretf_v00, "-100:-1 = '1'; else = '0'")

table(ttf$osteo, ttf$tDIIs2)

m2 <- glm(osteo ~ tDIIs2, data = ttf, family = "binomial")
summary(m2)
exp(m2$coefficients)
exp(confint(m2))

m2.1 <- glm(osteo ~ tDIIs2 + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 + 
              + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
            + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ttf, family = "binomial")
summary(m2.1)
exp(m2.1$coefficients)
exp(confint(m2.1))

m2.t <- glm(osteo ~ as.numeric(tDIIs2), data = ttf, family = "binomial")
summary(m2.t)

m2.1.t <- glm(osteo ~ as.numeric(tDIIs2)+ diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 + 
                + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
              + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ttf, family = "binomial")
summary(m2.1.t)

#Tscore l1l4

table(tl1l4$osteopor_v00)

tl1l4$tDIIs2 = cut2(tl1l4$DIIs2,g = 3)

#Genero una nueva variable: osteo (osteopenia o peor)

tl1l4$osteo = car::recode(tl1l4$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

table(tl1l4$osteo, tl1l4$tDIIs2)

m4 <- glm(osteo ~ tDIIs2, data = tl1l4, family = "binomial")
summary(m4)
exp(m4$coefficients)
exp(confint(m4))

m4.1 <- glm(osteo ~ tDIIs2 + sexo_s1 + diab_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
              + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
            + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tl1l4, family = "binomial")
summary(m4.1)
exp(m4.1$coefficients)
exp(confint(m4.1))

m4.t <- glm(osteo ~ as.numeric(tDIIs2), data = tl1l4, family = "binomial")
summary(m4.t)

m4.1.t <- glm(osteo ~ as.numeric(tDIIs2) + diab_s1 + sexo_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
                + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
              + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tl1l4, family = "binomial")
summary(m4.1.t)

######----Medidas para el gráfico:

#Sexo:

ftotm = subset(tftot, sexo_s1 == 1)
ftotm$tDIIs2 = cut2(ftotm$DIIs2,g = 3)
ftotm$osteo = car::recode(ftotm$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

m5 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ftotm, family = "binomial", maxit = 100)
summary(m5)
exp(m5$coefficients)
exp(confint(m5))
table(ftotm$osteo)

ftoth = subset(tftot, sexo_s1 == 0)
ftoth$tDIIs2 = cut2(ftoth$DIIs2,g = 3)
ftoth$osteo = car::recode(ftoth$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

m5 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ftoth, family = "binomial", maxit = 100)
summary(m5)
exp(m5$coefficients)
exp(confint(m5))
table(ftoth$osteo)

l1l4m = subset(tl1l4, sexo_s1 == 1)
l1l4m$tDIIs2 = cut2(l1l4m$DIIs2,g = 3)
l1l4m$osteo = car::recode(l1l4m$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

m5 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = l1l4m, family = "binomial", maxit = 100)
summary(m5)
exp(m5$coefficients)
exp(confint(m5))
table(l1l4m$osteo)

l1l4h = subset(tl1l4, sexo_s1 == 0)
l1l4h$tDIIs2 = cut2(l1l4h$DIIs2,g = 3)
l1l4h$osteo = car::recode(l1l4h$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

m5 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = l1l4h, family = "binomial", maxit = 100)
summary(m5)
exp(m5$coefficients)
exp(confint(m5))
table(l1l4h$osteo)

tfm = subset(ttf, sexo_s1 == 1)
tfm$tDIIs2 = cut2(tfm$DIIs2,g = 3)
tfm$osteo = car::recode(tfm$tscoretf_v00, "-100:-1 = '1'; else = '0'")

m5 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tfm, family = "binomial", maxit = 100)
summary(m5)
exp(m5$coefficients)
exp(confint(m5))
table(tfm$osteo)

tfh = subset(ttf, sexo_s1 == 0)
tfh$tDIIs2 = cut2(tfh$DIIs2,g = 3)
tfh$osteo = car::recode(tfh$tscoretf_v00, "-100:-1 = '1'; else = '0'")

m5 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + imc_v00 + fuma_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tfh, family = "binomial", maxit = 100)
summary(m5)
exp(m5$coefficients)
exp(confint(m5))
table(tfh$osteo)

#edad:

ftot70 = subset(tftot, edad_s1 >= 70)
ftot70$tDIIs2 = cut2(ftot70$DIIs2,g = 3)
ftot70$osteo = car::recode(ftot70$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + imc_v00 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ftot70, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(ftot70$osteo)

ftots70 = subset(tftot, edad_s1 < 70)
ftots70$tDIIs2 = cut2(ftots70$DIIs2,g = 3)
ftots70$osteo = car::recode(ftots70$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + imc_v00 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ftots70, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(ftots70$osteo)

l1l470 = subset(tl1l4, edad_s1 >= 70)
l1l470$tDIIs2 = cut2(l1l470$DIIs2,g = 3)
l1l470$osteo = car::recode(l1l470$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + imc_v00 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = l1l470, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(l1l470$osteo)

l1l4s70 = subset(tl1l4, edad_s1 < 70)
l1l4s70$tDIIs2 = cut2(l1l4s70$DIIs2,g = 3)
l1l4s70$osteo = car::recode(l1l4s70$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + imc_v00 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = l1l4s70, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(l1l4s70$osteo)

tf70 = subset(ttf, edad_s1 >= 70)
tf70$tDIIs2 = cut2(tf70$DIIs2,g = 3)
tf70$osteo = car::recode(tf70$tscoretf_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + imc_v00 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tf70, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(tf70$osteo)

tfs70 = subset(ttf, edad_s1 < 70)
tfs70$tDIIs2 = cut2(tfs70$DIIs2,g = 3)
tfs70$osteo = car::recode(tfs70$tscoretf_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + imc_v00 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tfs70, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(tfs70$osteo)

#imc:

ftot30 = subset(tftot, imc_v00 >= 30)
ftot30$tDIIs2 = cut2(ftot30$DIIs2,g = 3)
ftot30$osteo = car::recode(ftot30$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ftot30, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(ftot30$osteo)

ftots30 = subset(tftot, imc_v00 < 30)
ftots30$tDIIs2 = cut2(ftots30$DIIs2,g = 3)
ftots30$osteo = car::recode(ftots30$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ftots30, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(ftots30$osteo)

l1l430 = subset(tl1l4, imc_v00 >= 30)
l1l430$tDIIs2 = cut2(l1l430$DIIs2,g = 3)
l1l430$osteo = car::recode(l1l430$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = l1l430, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(l1l430$osteo)

l1l4s30 = subset(tl1l4, imc_v00 < 30)
l1l4s30$tDIIs2 = cut2(l1l4s30$DIIs2,g = 3)
l1l4s30$osteo = car::recode(l1l4s30$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = l1l4s30, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(l1l4s30$osteo)

tf30 = subset(ttf, imc_v00 >= 30)
tf30$tDIIs2 = cut2(tf30$DIIs2,g = 3)
tf30$osteo = car::recode(tf30$tscoretf_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tf30, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(tf30$osteo)

tfs30 = subset(ttf, imc_v00 < 30)
tfs30$tDIIs2 = cut2(tfs30$DIIs2,g = 3)
tfs30$osteo = car::recode(tfs30$tscoretf_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + diab_s1 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tfs30, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(tfs30$osteo)

#diab:

ftotd = subset(tftot, diab_s1 == 1)
ftotd$tDIIs2 = cut2(ftotd$DIIs2,g = 3)
ftotd$osteo = car::recode(ftotd$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + imc_v00 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ftotd, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(ftotd$osteo)

ftotsd = subset(tftot, diab_s1 == 0)
ftotsd$tDIIs2 = cut2(ftotsd$DIIs2,g = 3)
ftotsd$osteo = car::recode(ftotsd$tscoreftot_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + imc_v00 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = ftotsd, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(ftotsd$osteo)

l1l4d = subset(tl1l4, diab_s1 == 1)
l1l4d$tDIIs2 = cut2(l1l4d$DIIs2,g = 3)
l1l4d$osteo = car::recode(l1l4d$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + imc_v00 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = l1l4d, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(l1l4d$osteo)

l1l4sd = subset(tl1l4, diab_s1 == 0)
l1l4sd$tDIIs2 = cut2(l1l4sd$DIIs2,g = 3)
l1l4sd$osteo = car::recode(l1l4sd$tscorel1l4_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + imc_v00 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = l1l4sd, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(l1l4sd$osteo)

tfd = subset(ttf, diab_s1 == 1)
tfd$tDIIs2 = cut2(tfd$DIIs2,g = 3)
tfd$osteo = car::recode(tfd$tscoretf_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + imc_v00 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tfd, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(tfd$osteo)

tfsd = subset(ttf, diab_s1 == 0)
tfsd$tDIIs2 = cut2(tfsd$DIIs2,g = 3)
tfsd$osteo = car::recode(tfsd$tscoretf_v00, "-100:-1 = '1'; else = '0'")

m6 = glm(osteo ~ tDIIs2 + imc_v00 + edad_s1 + fuma_s1 + sexo_s1 + escola_v00 +
           + as.factor(act_fis_v00) + energiat_v00.x + (nodo)
         + tto_insu_v00 + tto_dm_v00 + tto_horm_v00 + Suple_Ca_vitD, data = tfsd, family = "binomial", maxit = 100)
summary(m6)
exp(m6$coefficients)
exp(confint(m6))
table(tfsd$osteo)

#Forest plot:

#Grafico 1: ftot

datos = structure(list(
  mean  = c(NA, NA, 1.89,	1.66,	1.46,	1.84,	1.27,	2.82,	1.23,	1.87), 
  lower = c(NA, NA, 0.72,	1.02,	0.83,	0.94,	0.77,	1.17,	0.41,	1.17),
  upper = c(NA, NA, 5.12,	2.71,	2.59,	3.69,	2.12,	7.00,	3.68,	3.03)),
  .Names = c("mean", "lower", "upper"), 
  row.names = c(NA, -11L), 
  class = "data.frame")

tabletext<-cbind(
  c("", "", ">70 y", "<70 y", "Women", 
    "Men", "> 30 kg/m2", "< 30 kg/m2", "Diabetics", 
    "No Diabetics"),
  c("Low BMD Total Femur", "(events/N)", "74/218", "184/867", 
    "151/518", "107/567", "183/825", "75/260", 
    "41/236", "217/849"),
  c("OR (95% IC)", NA, "1.89 (0.72, 5.12)", "1.66 (1.02, 2.71)", 
    "1.46 (0.83, 2.59)", "1.84 (0.94, 3.69)", "1.27 (0.77, 2.12)", "2.82 (1.17, 7.00)", 
    "1.23 (0.41, 3.66)", "1.87 (1.17, 3.03)"))

forestplot(tabletext, 
           datos,new_page = TRUE,
           is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
           clip=c(0.1,7), 
           xlog=TRUE, 
           vertices = T,
           graph.pos = 3,
           boxsize = 0.1,
           col=fpColors(box="black",line="black", summary="black"))

#Grafico 2: TF

datos = structure(list(
  mean  = c(NA, NA, 1.81,	1.90,	1.97,	1.92,	1.57,	3.41,	1.36,	2.18), 
  lower = c(NA, NA, 0.65,	1.13,	1.05,	0.98,	0.91,	1.34,	0.46,	1.32),
  upper = c(NA, NA, 5.21,	3.21,	3.74,	3.84,	2.67,	9.13,	4.14,	3.65)),
  .Names = c("mean", "lower", "upper"), 
  row.names = c(NA, -11L), 
  class = "data.frame")

tabletext<-cbind(
  c("", "", ">70 y", "<70 y", "Women", 
    "Men", "> 30 kg/m2", "< 30 kg/m2", "Diabetics", 
    "No Diabetics"),
  c("Low BMD Trochanter", "(events/N)", "54/225", "160/884", 
    "105/533", "109/576", "149/841", "65/268", 
    "37/244", "177/865"),
  c("OR (95% IC)", NA, "1.81 (0.65, 5.21)", "1.90 (1.13, 3.21)", 
    "1.97 (1.05, 3.74)", "1.92 (0.98, 3.84)", "1.57 (0.91, 2.67)", "3.41 (1.34, 9.13)", 
    "1.36 (0.46, 4.14)", "2.18 (1.32, 3.65)"))

forestplot(tabletext, 
           datos,new_page = TRUE,
           is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
           clip=c(0.1,4), 
           xlog=TRUE, 
           vertices = T,
           graph.pos = 3,
           boxsize = 0.1,
           col=fpColors(box="black",line="black", summary="black"))

#Grafico 3: l1l4

datos = structure(list(
  mean  = c(NA, NA, 1.16,	1.55,	1.37,	1.33,	1.34,	1.38,	1.15,	1.47), 
  lower = c(NA, NA, 0.42,	0.98,	0.79,	0.67,	0.84,	0.55,	0.41,	0.93),
  upper = c(NA, NA, 3.22,	2.47,	2.37,	2.64,	2.15,	3.46,	3.28,	2.33)),
  .Names = c("mean", "lower", "upper"), 
  row.names = c(NA, -11L), 
  class = "data.frame")

tabletext<-cbind(
  c("", "", ">70 y", "<70 y", "Women", 
    "Men", "> 30 kg/m2", "< 30 kg/m2", "Diabetics", 
    "No Diabetics"),
  c("Low BMD Lumbar Spine", "(events/N)", "76/194", "260/777", 
    "221/477", "115/494", "252/746", "84/225", 
    "50/200", "286/771"),
  c("OR (95% IC)", NA, "1.16 (0.42, 3.22)", "1.55 (0.98, 2.47)", 
    "1.37 (0.79, 2.37)", "1.33 (0.67, 2.64)", "1.34 (0.84, 2.15)", "1.38 (0.55, 3.46)", 
    "1.15 (0.41, 3.28)", "1.47 (0.93, 2.33)"))

forestplot(tabletext, 
           datos,new_page = TRUE,
           is.summary=c(TRUE,TRUE,rep(FALSE,8),TRUE),
           clip=c(0.1,4), 
           xlog=TRUE, 
           vertices = T,
           graph.pos = 3,
           boxsize = 0.1,
           col=fpColors(box="black",line="black", summary="black"))

###Suplemental file (cuando toque)

DIIs <- read_excel("DIIs.xlsx")
View(DIIs)

barplot(DIIs$Coef, ylim = c(-0.9, 0.4), horiz = F)

#Tabla de distribución de las variables incluidas (a partir del DF ftot).

medias = c()
sds = c()


for (i in 17:48) {
  medias[[i]] = tapply(ftot[[i]], ftot$tDIIs2, mean, na.rm = T)
  sds[[i]] = tapply(ftot[[i]], ftot$tDIIs2, sd, na.rm = T)
}

medias = data.frame(matrix(unlist(medias), ncol=3, byrow=T),stringsAsFactors=FALSE)
sds = data.frame(matrix(unlist(sds), ncol=3, byrow=T),stringsAsFactors=FALSE)

tot = data.frame(cbind(medias, sds))
tot$X = colnames(ftot)[17:48]

colnames(tot) = c("T1", "T2", "T3", "Sd1", "Sd2", "Sd3", "names")

export(tot, "coef.xlsx")