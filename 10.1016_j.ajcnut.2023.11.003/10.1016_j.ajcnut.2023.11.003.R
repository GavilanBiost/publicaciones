#----------------Apertura de la BBDD

library(haven)
library(labelled)
library(rio)
OGTT <- read_dta("G:/trabajo/ARTICULOS/BBDD/PREDIMED/DB_ogtt_n201_analysesfinal.dta")
View(OGTT)
colnames(OGTT)

#----------------Generaci?n de deltas.

OGTT[137:184] = OGTT[89:136] - OGTT[41:88]

colnames(OGTT)[137:184] = c("TRLP_d","VL_TRLP_d","L_TRLP_d","M_TRLP_d","S_TRLP_d","VS_TRLP_d","cLDLP_d","L_cLDLP_d",     
                            "M_cLDLP_d","S_cLDLP_d","cHDLP_d","L_cHDLP_d","M_cHDLP_d","S_cHDLP_d","H7P_d","H6P_d","H5P_d",          
                            "H4P_d","H3P_d","H2P_d","H1P_d","TRLZ_d","LDLZ_d","HDLZ_d","NTG_d","NTC_d",
                            "NTRLTG_d","NTRLC_d","NLDLC_d","NHDLC_d","ApoB_d","ApoA1_d","BCAA_d","Val_d","Leu_d",          
                            "Ileu_d","Ala_d","Gly_d","Glu_d","Ctr_d","KetBod_d","B_HB_d","AcAc_d","Acetone_d",      
                            "LPIR_d","DRI_d","GlycA_d","TMAO_d")

PREDIMED <- read_sav("E:/ARTICULOS/BBDD/PREDIMED/(ref) PREDIMED Base de datos limpia (noviembre 2015) Reus.SAV")
#PREDIMED <- read_sav("/Volumes/JES GAVI/ARTICULOS/BBDD/PREDIMED/(ref) PREDIMED Base de datos limpia (noviembre 2015) Reus.SAV")
colnames(PREDIMED)
PREDIMED1 = PREDIMED
P14 = PREDIMED$p14

P14.1 = PREDIMED$p14tot_3 + PREDIMED$p14totm_3

PREDIMED = data.frame(cbind(PREDIMED[1], PREDIMED[1900], PREDIMED[1887], PREDIMED[27], PREDIMED[29], PREDIMED[118], P14, P14.1))
colnames(PREDIMED) = c("paciente","getota_1","af_1","diabetes0","hipercol0","m_estatin", "p14", "p14.1")

rm(P14, P14.1)

str(OGTT$paciente)
str(PREDIMED$paciente)

PREDIMED$paciente = as.numeric(PREDIMED$paciente)

OGTT = merge(OGTT, PREDIMED, by = "paciente", all.x = T)

colnames(OGTT)

#----------------Creaci?n de la BBDD con datos normalizados: RankNorm

library(RNOmni)

colnames(OGTT)[41:136]

OGTT1 = ((OGTT)[41:136])

na1 = sapply(OGTT1, function(OGTT1) sum(length(which(is.na(OGTT1)))))
na2 = sapply(OGTT1, function(OGTT1) (100*sum(length(which(is.na(OGTT1))))/sum(length((OGTT1)))))
na =  cbind("NA"=na1, "% NA"=na2)
na

# No se eliminan variables por > 20% de NAs

#----------------Imputaci?n de missings

library(missForest)

set.seed(1)

OGTT2 = missForest(as.matrix(OGTT1), verbose = T)

OGTT2$OOBerror

OGTT2 = data.frame(OGTT2$ximp)

colnames(OGTT2)

rm (na, na1, na2)

#----------------Normalizacion:

library(RNOmni)

OGTT1 = apply(OGTT2, 2, RankNorm)

OGTT_save = data.frame(cbind(OGTT[1:40], OGTT1))

#----------------Creacion de deltas:

OGTT_save[137:184] = OGTT_save[89:136] - OGTT_save[41:88]

colnames(OGTT_save)[137:184] = c("TRLP_d","VL_TRLP_d","L_TRLP_d","M_TRLP_d","S_TRLP_d","VS_TRLP_d","cLDLP_d","L_cLDLP_d",     
                                 "M_cLDLP_d","S_cLDLP_d","cHDLP_d","L_cHDLP_d","M_cHDLP_d","S_cHDLP_d","H7P_d","H6P_d","H5P_d",          
                                 "H4P_d","H3P_d","H2P_d","H1P_d","TRLZ_d","LDLZ_d","HDLZ_d","NTG_d","NTC_d",
                                 "NTRLTG_d","NTRLC_d","NLDLC_d","NHDLC_d","ApoB_d","ApoA1_d","BCAA_d","Val_d","Leu_d",          
                                 "Ileu_d","Ala_d","Gly_d","Glu_d","Ctr_d","KetBod_d","B_HB_d","AcAc_d","Acetone_d",      
                                 "LPIR_d","DRI_d","GlycA_d","TMAO_d")

OGTT_save = data.frame(cbind(OGTT_save, OGTT[185:191]))

rm(OGTT1, OGTT2)

#----------------Creacion de los terciles de P14 y d_P14

OGTT_save$t_p14 = Hmisc::cut2(OGTT_save$p14, g = 3)

table(OGTT_save$t_p14)
tapply(OGTT_save$p14, OGTT_save$t_p14, median)
tapply(OGTT_save$p14, OGTT_save$t_p14, quantile)

OGTT_save$d_p14 = OGTT_save$p14.1 - OGTT_save$p14

OGTT_save$td_p14 = Hmisc::cut2(OGTT_save$d_p14, g = 3)

table(OGTT_save$td_p14)
tapply(OGTT_save$d_p14, OGTT_save$td_p14, median)
tapply(OGTT_save$d_p14, OGTT_save$td_p14, quantile)

export(OGTT_save, "BBDD_RN_P14.xlsx")
  
#----------------Generaci?n de medias [SD] con datos RN (por t_p14):

medias = c()
SD = c()
anovas = c()


for (i in 41:184) {
  medias[[i]] = tapply(OGTT_save[[i]], OGTT_save[[192]], mean, na.rm = T)
  SD[[i]] = tapply(OGTT_save[[i]], OGTT_save[[192]], sd, na.rm = T)
  anovas[[i]] = (summary.aov(aov(OGTT_save[[i]] ~ OGTT_save[[192]]))[[1]][["Pr(>F)"]])
}

medias = round(data.frame(matrix(unlist(medias), ncol=3, byrow=T),stringsAsFactors=FALSE),2)
SD = round(data.frame(matrix(unlist(SD), ncol=3, byrow=T),stringsAsFactors=FALSE),2)
anovas = round(data.frame(matrix(unlist(anovas), ncol=2, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_save)[41:184]

resumen = data.frame(cbind(medias, SD, anovas))

colnames(resumen) = c("m.T1", "m.T2", "m.T3", "sd.T1", "sd.T2", "sd.T3", "P-value", "names")

export(resumen, "media_SD_RN_t_p14.xlsx")

rm(anovas, medias, resumen, SD, i)

table(OGTT_save$t_p14)

#----------------ANCOVAS (t_p14):

anovas = c()

for (i in 41:184) {
  anovas[[i]] = (summary.aov(aov(OGTT_save[[i]] ~ OGTT_save[[192]] + OGTT_save[[4]] + OGTT_save[[5]]
                                 + OGTT_save[[7]] + OGTT_save[[8]] + OGTT_save[[13]] + OGTT_save[[185]]
                                 + OGTT_save[[187]] + OGTT_save[[188]] + OGTT_save[[189]]))[[1]][["Pr(>F)"]])[[1]]
}

anovas = round(data.frame(matrix(unlist(anovas), ncol=1, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_save)[1:184]

resumen = data.frame(cbind(anovas))

colnames(resumen) = c("P-value", "names")

export(resumen, "ancovas_p_RN_tp14.xlsx")

rm(anovas, resumen, i)

#-------------Creacion de la BBDD binomial para t_p14

OGTT_save$t_p14 = as.numeric(OGTT_save$t_p14)

OGTT_save_btp14 = subset(OGTT_save, t_p14 == 1 | t_p14 == 3)

#----------------ANCOVAS (t_p14 modelo binomial):

anovas = c()

for (i in 41:184) {
  anovas[[i]] = (summary.aov(aov(OGTT_save[[i]] ~ OGTT_save[[192]] + OGTT_save[[4]] + OGTT_save[[5]]
                                 + OGTT_save[[7]] + OGTT_save[[8]] + OGTT_save[[13]] + OGTT_save[[185]]
                                 + OGTT_save[[87]] + OGTT_save[[188]] + OGTT_save[[189]]))[[1]][["Pr(>F)"]])[[1]]
}

anovas = round(data.frame(matrix(unlist(anovas), ncol=1, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_save)[1:184]

resumen = data.frame(cbind(anovas))

colnames(resumen) = c("P-value", "names")

export(resumen, "ancovas_p_RN_btp14.xlsx")

rm(anovas, resumen, i)

#----------------Generaci?n de medias [SD] con datos RN (por d_p14):

medias = c()
SD = c()
anovas = c()


for (i in 137:184) {
  medias[[i]] = tapply(OGTT_save[[i]], OGTT_save[[194]], mean, na.rm = T)
  SD[[i]] = tapply(OGTT_save[[i]], OGTT_save[[194]], sd, na.rm = T)
  anovas[[i]] = (summary.aov(aov(OGTT_save[[i]] ~ OGTT_save[[194]]))[[1]][["Pr(>F)"]])
}

medias = round(data.frame(matrix(unlist(medias), ncol=3, byrow=T),stringsAsFactors=FALSE),2)
SD = round(data.frame(matrix(unlist(SD), ncol=3, byrow=T),stringsAsFactors=FALSE),2)
anovas = round(data.frame(matrix(unlist(anovas), ncol=2, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_save)[137:184]

resumen = data.frame(cbind(medias, SD, anovas))

colnames(resumen) = c("m.T1", "m.T2", "m.T3", "sd.T1", "sd.T2", "sd.T3", "P-value", "names")

export(resumen, "media_SD_RN_t_d14.xlsx")

rm(anovas, medias, resumen, SD, i)

table(OGTT_save$td_p14)

#----------------ANCOVAS (t_dp14):

anovas = c()

for (i in 137:184) {
  anovas[[i]] = (summary.aov(aov(OGTT_save[[i]] ~ OGTT_save[[194]] + OGTT_save[[4]] + OGTT_save[[5]] + OGTT_save$p14
                                 + OGTT_save[[7]] + OGTT_save[[8]] + OGTT_save[[13]] + OGTT_save[[185]]
                                 + OGTT_save[[87]] + OGTT_save[[188]] + OGTT_save[[189]] + OGTT_save[[i - 96]]))[[1]][["Pr(>F)"]])[[1]]
}



anovas = round(data.frame(matrix(unlist(anovas), ncol=1, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_save)[1:184]

resumen = data.frame(cbind(anovas))

colnames(resumen) = c("P-value", "names")

export(resumen, "ancovas_p_RN_tdp14.xlsx")

rm(anovas, resumen, i)

#-------------Creacion de la BBDD binomial para d_p14

OGTT_save$td_p14 = as.numeric(OGTT_save$td_p14)

table(OGTT_save$td_p14)

OGTT_save_btdp14 = subset(OGTT_save, td_p14 == 1 | td_p14 == 3)

#----------------ANCOVAS (td_p14 modelo binomial - t1 vs t3):

anovas = c()

for (i in 137:184) {
  anovas[[i]] = (summary.aov(aov(OGTT_save_btdp14[[i]] ~ OGTT_save_btdp14[[194]] + OGTT_save_btdp14[[4]] + OGTT_save_btdp14[[5]] + OGTT_save_btdp14$p14
                                 + OGTT_save_btdp14[[7]] + OGTT_save_btdp14[[8]] + OGTT_save_btdp14[[13]] + OGTT_save_btdp14[[185]]
                                 + OGTT_save_btdp14[[87]] + OGTT_save_btdp14[[188]] + OGTT_save_btdp14[[189]] + OGTT_save_btdp14[[i - 96]]))[[1]][["Pr(>F)"]])[[1]]
}

anovas = round(data.frame(matrix(unlist(anovas), ncol=1, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_save_btdp14)[1:184]

resumen = data.frame(cbind(anovas))

colnames(resumen) = c("P-value", "names")

export(resumen, "ancovas_p_RN_btdp14.xlsx")

rm(anovas, resumen, i)

#----------------ANCOVAS (td_p14 modelo binomial - neg vs post):

#-------------Creacion de la BBDD binomial para d_p14

table(OGTT_save$td_p14)

OGTT_save$bd_p14 = car::recode(OGTT_save$td_p14, "1 = 1;2:3 = 2")

table(OGTT_save$bd_p14)

anovas = c()

for (i in 137:184) {
  anovas[[i]] = (summary(aov(OGTT_save[[137]] ~ OGTT_save[[195]] + OGTT_save[[4]] + OGTT_save[[5]]
                                 + OGTT_save[[7]] + OGTT_save[[8]] + OGTT_save[[13]] + OGTT_save[[185]]
                                 + OGTT_save[[87]] + OGTT_save[[188]] + OGTT_save[[189]] + OGTT_save[[137 - 96]] + OGTT_save$p14))[[1]][["Pr(>F)"]])[[1]]
}

anovas = data.frame(matrix(unlist(anovas), ncol=1, byrow=T),stringsAsFactors=FALSE)

anovas$X2 = colnames(OGTT_save)[1:184]

resumen = data.frame(cbind(anovas))

colnames(resumen) = c("P-value", "names")

export(resumen, "ancovas_p_RN_btdp14.2.xlsx")

rm(anovas, resumen, i)

#Tabla de caracteristicas basales: n, grupo int, sexo, HTA, tabaco, educacion, dislipemia, T2D, mestatin, edad, cintura, IMC, getota
# energia, p14
Tabla1 = data.frame(cbind(PREDIMED1$id, PREDIMED1$energiat))
colnames(Tabla1) = c("paciente","energiat")
Tabla1 = merge(OGTT_save, Tabla1, by = "paciente", all.x = T)
colnames(Tabla1)
Tabla1 = data.frame(cbind(Tabla1[1], Tabla1[3:4], Tabla1[7:8], Tabla1[10], Tabla1[187:189], Tabla1[5], Tabla1[13:14], Tabla1[185],
                          Tabla1[195], Tabla1[190]))
Tabla1$t_p14 = Hmisc::cut2(Tabla1$p14, g = 3)

medias = c()
SD = c()
medias.tot = c()
SD.tot = c()

for (i in 10:15) {
  medias[[i]] = tapply(Tabla1[[i]], Tabla1[[16]], mean, na.rm = T)
  SD[[i]] = tapply(Tabla1[[i]], Tabla1[[16]], sd, na.rm = T)
  medias.tot[[i]] = mean(Tabla1[[i]],na.rm = T)
  SD.tot[[i]] = sd(Tabla1[[i]], na.rm = T)
}

medias = round(data.frame(matrix(unlist(medias), ncol=3, byrow=T),stringsAsFactors=FALSE),1)
SD = round(data.frame(matrix(unlist(SD), ncol=3, byrow=T),stringsAsFactors=FALSE),1)
medias.tot = round(data.frame(matrix(unlist(medias.tot), ncol=1, byrow=T),stringsAsFactors=FALSE),1)
SD.tot = round(data.frame(matrix(unlist(SD.tot), ncol=1, byrow=T),stringsAsFactors=FALSE),1)

SD.tot = data.frame(cbind(medias.tot, SD.tot))

resumen = data.frame(cbind(medias, SD, SD.tot[10:15,]))

resumen$nombres = colnames(Tabla1)[10:15]

colnames(resumen) = c("T1", "T2", "T3", "sd1", "sd2", "sd3", "tot", "sd", "names")

export(resumen, "cbasales_num.xlsx")

rm(medias, medias.tot, resumen, SD, SD.tot, i)

(table(as.factor(Tabla1$sexo))) 
(prop.table(table(as.factor(Tabla1$sexo)))*100) 

(table(as.factor(Tabla1$grup_int))) 
(prop.table(table(as.factor(Tabla1$grup_int)))*100) 

(table(as.factor(Tabla1$hta0))) 
(prop.table(table(as.factor(Tabla1$hta0)))*100)

(table(as.factor(Tabla1$tabaco0))) 
(prop.table(table(as.factor(Tabla1$tabaco0)))*100)

(table(as.factor(Tabla1$escolar1))) 
(prop.table(table(as.factor(Tabla1$escolar1)))*100)

(table(as.factor(Tabla1$diabetes0))) 
(prop.table(table(as.factor(Tabla1$diabetes0)))*100)

(table(as.factor(Tabla1$hipercol0))) 
(prop.table(table(as.factor(Tabla1$hipercol0)))*100)

(table(as.factor(Tabla1$m_estatin))) 
(prop.table(table(as.factor(Tabla1$m_estatin)))*100)

(table(as.factor(Tabla1$sexo), Tabla1$t_p14)) 
(prop.table(table(as.factor(Tabla1$sexo), Tabla1$t_p14))*100) 

(table(as.factor(Tabla1$grup_int), Tabla1$t_p14)) 
(prop.table(table(as.factor(Tabla1$grup_int), Tabla1$t_p14))*100) 

(table(as.factor(Tabla1$hta0), Tabla1$t_p14)) 
(prop.table(table(as.factor(Tabla1$hta0), Tabla1$t_p14))*100)

(table(as.factor(Tabla1$tabaco0), Tabla1$t_p14)) 
(prop.table(table(as.factor(Tabla1$tabaco0), Tabla1$t_p14))*100)

(table(as.factor(Tabla1$escolar1), Tabla1$t_p14)) 
(prop.table(table(as.factor(Tabla1$escolar1), Tabla1$t_p14))*100)

(table(as.factor(Tabla1$diabetes0), Tabla1$t_p14)) 
(prop.table(table(as.factor(Tabla1$diabetes0), Tabla1$t_p14))*100)

(table(as.factor(Tabla1$hipercol0), Tabla1$t_p14)) 
(prop.table(table(as.factor(Tabla1$hipercol0), Tabla1$t_p14))*100)

(table(as.factor(Tabla1$m_estatin), Tabla1$t_p14)) 
(prop.table(table(as.factor(Tabla1$m_estatin), Tabla1$t_p14))*100)

# Correlaciones basal:

corr = cbind(OGTT_save[41:88], OGTT_save$p14)
colnames(corr)[49] = c("p14")

Pearson.IC = c()
Pearson.cor = c()
Person.p = c()
Spearman.cor = c()
Spearman.p = c()

for (i in 1:48) {
  Pearson.IC[[i]] = cor.test(corr[[i]], corr$p14, conf.level = 0.95)$conf.int
  Pearson.cor[[i]] = cor.test(corr[[i]], corr$p14)$estimate
  Person.p [[i]] = cor.test(corr[[i]], corr$p14)$p.value
  Spearman.cor[[i]] = cor.test(corr[[i]], corr$p14, method = "spearman")$estimate
  Spearman.p [[i]] = cor.test(corr[[i]], corr$p14, method = "spearman")$p.value
}

IC = round(data.frame(matrix(unlist(Pearson.IC), ncol=2, byrow=T),stringsAsFactors=FALSE),2)
names = colnames(corr)[1:48]

resumen = data.frame(cbind(names, round(Pearson.cor,2), IC, round(Person.p, 3), round(Spearman.cor,2), round(Spearman.p, 3)))
colnames(resumen) = c("nombres", "Coef", "2.5%", "97.5%", "p-value", "Sper", "p-value")
export(resumen, "correlaciones_p14.xlsx")

# Correlaciones cambios:

corr = cbind(OGTT_save[137:184], OGTT_save$d_p14)
colnames(corr)[49] = c("d_p14")

Pearson.IC = c()
Pearson.cor = c()
Person.p = c()
Spearman.cor = c()
Spearman.p = c()


for (i in 1:48) {
  Pearson.IC[[i]] = cor.test(corr[[i]], corr$d_p14, conf.level = 0.95)$conf.int
  Pearson.cor[[i]] = cor.test(corr[[i]], corr$d_p14)$estimate
  Person.p [[i]] = cor.test(corr[[i]], corr$d_p14)$p.value
  Spearman.cor[[i]] = cor.test(corr[[i]], corr$d_p14, method = "spearman")$estimate
  Spearman.p [[i]] = cor.test(corr[[i]], corr$d_p14, method = "spearman")$p.value
}

IC = round(data.frame(matrix(unlist(Pearson.IC), ncol=2, byrow=T),stringsAsFactors=FALSE),2)
names = colnames(corr)[1:48]

resumen = data.frame(cbind(names, round(Pearson.cor,2), IC, round(Person.p, 3), round(Spearman.cor,2), round(Spearman.p, 3)))
colnames(resumen) = c("nombres", "Coef", "2.5%", "97.5%", "p-value", "Sper", "p-value")
export(resumen, "correlaciones_dp14.xlsx")


# Grafica Indira
# calculo de IC 95%

library(readxl)
ICs <- read_excel("p14/ICs_p14.xlsx")

lowIC = c()
highIC = c()
t = c()

for (i in 1:96) {
  t=qt(p=.95, df=ICs$n[[i]]-1)
  highIC[[i]]=ICs$media[[i]]+t*sqrt(ICs$sd[[i]]^2/ICs$n[[i]])
  lowIC[[i]]=ICs$media[[i]]-t*sqrt(ICs$sd[[i]]^2/ICs$n[[i]])
} 

highIC = round(data.frame(matrix(unlist(highIC), ncol=1, byrow=T),stringsAsFactors=FALSE),2)
lowIC = round(data.frame(matrix(unlist(lowIC), ncol=1, byrow=T),stringsAsFactors=FALSE),2)
ICsTN_basal = data.frame(cbind(ICs$media, lowIC, highIC, ICs$tercil))
ICsTN_basal$nombres = ICs$Variable

export(ICsTN_basal, "ICsp14_changes.xlsx")

library(ggplot2)
library(scales)

# P14 changes VLDL

VLDL <- read_excel("ICsP14_changes.xlsx", sheet = "VLDL")
colnames(VLDL) = c("media", "bajo", "alto", "tercil", "nombres")
VLDL$tercil = as.factor(VLDL$tercil)
str(VLDL$tercil)

g = ggplot (VLDL, aes(y = media, x = nombres, fill = (tercil)))  #base de la figura
g1 = g + theme_bw() #quito fondo gris y cuadricula, remarco ejes (base)
g2 = g1 + geom_bar(position = "dodge", stat = "identity", size = 0.2, color="black", width = 0.7) + #tipo de figura y marco negro entorno a las barras
  coord_flip() + #cambio a horizontal
  ylab ('Mean (95% IC)') +
  geom_errorbar(aes(ymin =bajo , ymax = alto), width = 0.4, position=position_dodge(.7)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, tag = "A") + #quito el titulo de la leyenda
  scale_y_continuous(NULL) + #quito el titulo del eje y
  geom_hline(yintercept = 0) + #aguegar linea horizontal
  scale_x_discrete(NULL, limits = VLDL$nombres[9:1])  #quito el titulo del eje x y ordeno las variables como quiero (ojo, las pone orden inverso)


ggsave(filename = "P14 changes VLDL barras.png", plot = g2, dpi = 320, units = "cm", width = 12, height = 10) 

# P14 changes HDL

VLDL <- read_excel("ICsP14_changes.xlsx", sheet = "HDL")
colnames(VLDL) = c("media", "bajo", "alto", "tercil", "nombres")

VLDL$tercil = as.factor(VLDL$tercil)
str(VLDL$tercil)

g = ggplot (VLDL, aes(y = media, x = nombres, fill = (tercil)))  #base de la figura
g1 = g + theme_bw() #quito fondo gris y cuadricula, remarco ejes (base)
g2 = g1 + geom_bar(position = "dodge", stat = "identity", size = 0.2, color="black", width = 0.7) + #tipo de figura y marco negro entorno a las barras
  coord_flip() + #cambio a horizontal
  geom_errorbar(aes(ymin =bajo , ymax = alto), width = 0.4, position=position_dodge(.7)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, tag = "B") + #quito el titulo de la leyenda
  scale_y_continuous(NULL) + #quito el titulo del eje y
  geom_hline(yintercept = 0) + #aguegar linea horizontal
  scale_x_discrete(NULL, limits = VLDL$nombres[13:1])  #quito el titulo del eje x y ordeno las variables como quiero (ojo, las pone orden inverso)

ggsave(filename = "P14 changes v3 HDL barras.png", plot = g2, dpi = 320, units = "cm", width = 12, height = 10)  # guardar imagen de gran calidad

# P14 changes LDL

VLDL <- read_excel("ICsP14_changes.xlsx", sheet = "LDL")
colnames(VLDL) = c("media", "bajo", "alto", "tercil", "nombres")

VLDL$tercil = as.factor(VLDL$tercil)
str(VLDL$tercil)
levels(VLDL$tercil)

g = ggplot (VLDL, aes(y = media, x = nombres, fill = (tercil)))  #base de la figura
g1 = g + theme_bw() #quito fondo gris y cuadricula, remarco ejes (base)
g2 = g1 + geom_bar(position = "dodge", stat = "identity", size = 0.2, color="black", width = 0.7) + #tipo de figura y marco negro entorno a las barras
  coord_flip() + #cambio a horizontal
  geom_errorbar(aes(ymin =bajo , ymax = alto), width = 0.4, position=position_dodge(.7)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, tag = "C") + #quito el titulo de la leyenda
  scale_y_continuous(NULL) + #quito el titulo del eje y
  geom_hline(yintercept = 0) + #aguegar linea horizontal
  scale_x_discrete(NULL, limits = VLDL$nombres[8:1])#quito el titulo del eje x y ordeno las variables como quiero (ojo, las pone orden inverso)


ggsave(filename = "P14 changes LDL barras.png", plot = g2, dpi = 320, units = "cm", width = 12, height = 10)  # guardar imagen de gran calidad

# P14 changes BCAA

VLDL <- read_excel("ICsP14_changes.xlsx", sheet = "BCAA")
colnames(VLDL) = c("media", "bajo", "alto", "tercil", "nombres")

VLDL$tercil = as.factor(VLDL$tercil)
str(VLDL$tercil)
levels(VLDL$tercil)

g = ggplot (VLDL, aes(y = media, x = nombres, fill = (tercil)))  #base de la figura
g1 = g + theme_bw() #quito fondo gris y cuadricula, remarco ejes (base)
g2 = g1 + geom_bar(position = "dodge", stat = "identity", size = 0.2, color="black", width = 0.7) + #tipo de figura y marco negro entorno a las barras
  coord_flip() + #cambio a horizontal
  geom_errorbar(aes(ymin =bajo , ymax = alto), width = 0.4, position=position_dodge(.7)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, tag = "A") + #quito el titulo de la leyenda
  scale_y_continuous(NULL) + #quito el titulo del eje y
  geom_hline(yintercept = 0) + #aguegar linea horizontal
  scale_x_discrete(NULL, limits = VLDL$nombres[6:1])#quito el titulo del eje x y ordeno las variables como quiero (ojo, las pone orden inverso)


ggsave(filename = "P14 changes BCAA barras.png", plot = g2, dpi = 320, units = "cm", width = 12, height = 10)  # guardar imagen de gran calidad

# P14 changes other

VLDL <- read_excel("ICsP14_changes.xlsx", sheet = "Otros")
colnames(VLDL) = c("media", "bajo", "alto", "tercil", "nombres")

VLDL$tercil = as.factor(VLDL$tercil)
str(VLDL$tercil)
levels(VLDL$tercil)

g = ggplot (VLDL, aes(y = media, x = nombres, fill = (tercil)))  #base de la figura
g1 = g + theme_bw() #quito fondo gris y cuadricula, remarco ejes (base)
g2 = g1 + geom_bar(position = "dodge", stat = "identity", size = 0.2, color="black", width = 0.7) + #tipo de figura y marco negro entorno a las barras
  coord_flip() + #cambio a horizontal
  geom_errorbar(aes(ymin =bajo , ymax = alto), width = 0.4, position=position_dodge(.7)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(fill = NULL, tag = "B") + #quito el titulo de la leyenda
  scale_y_continuous(NULL) + #quito el titulo del eje y
  geom_hline(yintercept = 0) + #aguegar linea horizontal
  scale_x_discrete(NULL, limits = VLDL$nombres[12:1])#quito el titulo del eje x y ordeno las variables como quiero (ojo, las pone orden inverso)


ggsave(filename = "P14 changes Otros barras.png", plot = g2, dpi = 320, units = "cm", width = 12, height = 10)  # guardar imagen de gran calidad

# Revisores: AJCN ####

## R1 Pregunta 2:####
tapply(OGTT_save$col1, OGTT_save$t_p14, mean, na.rm = T)
tapply(OGTT_save$col1, OGTT_save$t_p14, sd, na.rm = T)

tapply(OGTT_save$hdl1, OGTT_save$t_p14, mean, na.rm = T)
tapply(OGTT_save$hdl1, OGTT_save$t_p14, sd, na.rm = T)

tapply(OGTT_save$ldl1, OGTT_save$t_p14, mean, na.rm = T)
tapply(OGTT_save$ldl1, OGTT_save$t_p14, sd, na.rm = T)

tapply(OGTT_save$tg1, OGTT_save$t_p14, mean, na.rm = T)
tapply(OGTT_save$tg1, OGTT_save$t_p14, sd, na.rm = T)

## R1 Pregunta 3: ####

Tabla1$estudio1 = 1
P3 =  read_sav("G:/trabajo/ARTICULOS/BBDD/PREDIMED/BD_ExtendedFU_Alim_Lab_renal-03.12.15-LACTIS-CCR.sav")
colnames(Tabla1)
Tabla11= Tabla1
Tabla11$t_p14 = NULL
Tabla11$m_estatin = NULL

library(dplyr)

P33 = select(P3, paciente, grup_int, sexo, hta0, tabaco0, escolar1, diabetes0, hipercol0,
                  edad0, imc1, cint1, getota_1, energiat, p14tot_1, p14totm_1)
colnames(P33)

P33$p14 = P33$p14tot_1 + P33$p14totm_1
P33$p14tot_1 = NULL
P33$p14totm_1 = NULL

P33$estudio1 = 2
TP = rbind(P33, Tabla11)

medias = c()
SD = c()
anovas = c()

for (i in 9:14) {
  medias[[i]] = tapply(TP[[i]], TP[[15]], mean, na.rm = T)
  SD[[i]] = tapply(TP[[i]], TP[[15]], sd, na.rm = T)
  anovas[[i]] = (summary.aov(aov(TP[[i]] ~ TP[[15]]))[[1]][["Pr(>F)"]])[[1]]
}

medias = round(data.frame(matrix(unlist(medias), ncol=2, byrow=T),stringsAsFactors=FALSE),1)
SD = round(data.frame(matrix(unlist(SD), ncol=2, byrow=T),stringsAsFactors=FALSE),1)
anovas = round(data.frame(matrix(unlist(anovas), ncol=1, byrow=T),stringsAsFactors=FALSE),3)

resumen = data.frame(cbind(medias, SD, anovas))

resumen$nombres = colnames(TP)[9:14]

colnames(resumen) = c("Sub", "Predimed", "sd1", "sd2", "P","names")

export(resumen, "comparativa_estudios.xlsx")

rm(medias, resumen, SD, i)

(table(as.factor(TP$sexo), TP$estudio1)) 
chisq.test(as.factor(TP$sexo), TP$estudio1)

(table(as.factor(TP$grup_int), TP$estudio1)) 
chisq.test(as.factor(TP$grup_int), TP$estudio1)

(table(as.factor(TP$hta0), TP$estudio1)) 
chisq.test(as.factor(TP$hta0), TP$estudio1)

(table(as.factor(TP$tabaco0), TP$estudio1)) 
chisq.test(as.factor(TP$tabaco0), TP$estudio1)

(table(as.factor(TP$diabetes0), TP$estudio1)) 
chisq.test(as.factor(TP$diabetes0), TP$estudio1)

(table(as.factor(TP$hipercol0), TP$estudio1)) 
chisq.test(as.factor(TP$hipercol0), TP$estudio1)

## R1 Pregunta 5: ####

OS = select(OGTT_save, paciente, t_p14, d_p14, td_p14)
OS = merge(OS, OGTT, by = "paciente")

medias = c()
SD = c()
anovas = c()

for (i in 44:187) {
  medias[[i]] = tapply(OS[[i]], OS[[2]], median, na.rm = T)
  SD[[i]] = tapply(OS[[i]], OS[[2]], IQR, na.rm = T)
  anovas[[i]] = (summary.aov(aov(OS[[i]] ~ OS[[2]]))[[1]][["Pr(>F)"]])
}

medias = round(data.frame(matrix(unlist(medias), ncol=3, byrow=T),stringsAsFactors=FALSE),2)
SD = round(data.frame(matrix(unlist(SD), ncol=3, byrow=T),stringsAsFactors=FALSE),2)
anovas = round(data.frame(matrix(unlist(anovas), ncol=2, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OS)[44:187]

resumen = data.frame(cbind(medias, SD, anovas))

colnames(resumen) = c("m.T1", "m.T2", "m.T3", "sd.T1", "sd.T2", "sd.T3", "P-value", "names")

export(resumen, "media_SD_t_p14.xlsx")

rm(anovas, medias, resumen, SD, i)

table(OS$t_p14)

# R2 Pregunta 1: ####

library(qvalue)

a = c(0.524,
      0.024)

p.adjust(a, method='fdr')

# R2 Pregunta 4: ####

colnames(OGTT_save)

OGTT_MedDiet = subset(OGTT_save, grup_int != 3)

OGTT_MedDiet$td_p14 = Hmisc::cut2(OGTT_MedDiet$d_p14, g = 3)
table(OGTT_MedDiet$td_p14)

tapply(OGTT_MedDiet$d_p14, OGTT_MedDiet$td_p14, median)
tapply(OGTT_MedDiet$d_p14, OGTT_MedDiet$td_p14, quantile)

#----------------Generaci?n de medias [SD] con datos RN (por td_p14):

medias = c()
SD = c()
anovas = c()


for (i in 137:184) {
  medias[[i]] = tapply(OGTT_MedDiet[[i]], OGTT_MedDiet[[194]], mean, na.rm = T)
  SD[[i]] = tapply(OGTT_MedDiet[[i]], OGTT_MedDiet[[194]], sd, na.rm = T)
  anovas[[i]] = (summary.aov(aov(OGTT_MedDiet[[i]] ~ OGTT_MedDiet[[194]]))[[1]][["Pr(>F)"]])
}

medias = round(data.frame(matrix(unlist(medias), ncol=3, byrow=T),stringsAsFactors=FALSE),2)
SD = round(data.frame(matrix(unlist(SD), ncol=3, byrow=T),stringsAsFactors=FALSE),2)
anovas = round(data.frame(matrix(unlist(anovas), ncol=2, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_MedDiet)[137:184]

resumen = data.frame(cbind(medias, SD, anovas))

colnames(resumen) = c("m.T1", "m.T2", "m.T3", "sd.T1", "sd.T2", "sd.T3", "P-value", "names")

export(resumen, "media_SD_RN_td_p14_wo_LFD.xlsx")

rm(anovas, medias, resumen, SD, i)

#----------------ANCOVAS (td_p14):

anovas = c()

for (i in 137:184) {
  anovas[[i]] = (summary.aov(aov(OGTT_MedDiet[[i]] ~ OGTT_MedDiet[[194]] + OGTT_MedDiet[[4]] + OGTT_MedDiet[[5]]
                                 + OGTT_MedDiet[[7]] + OGTT_MedDiet[[8]] + OGTT_MedDiet[[13]] + OGTT_MedDiet[[185]]
                                 + OGTT_MedDiet[[187]] + OGTT_MedDiet[[188]] + OGTT_MedDiet[[189]]))[[1]][["Pr(>F)"]])[[1]]
}

anovas = round(data.frame(matrix(unlist(anovas), ncol=1, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_MedDiet)[137:184]

resumen = data.frame(cbind(anovas))

colnames(resumen) = c("P-value", "names")

export(resumen, "ancovas_p_RN_tdp14_woLFD.xlsx")

rm(anovas, resumen, i)

#-------------Creacion de la BBDD binomial para td_p14

OGTT_MedDiet$td_p14 = as.numeric(OGTT_MedDiet$td_p14)

OGTT_MedDiet = subset(OGTT_MedDiet, td_p14 == 1 | td_p14 == 3)

anovas = c()

for (i in 137:184) {
  anovas[[i]] = (summary.aov(aov(OGTT_MedDiet[[i]] ~ OGTT_MedDiet[[194]] + OGTT_MedDiet[[4]] + OGTT_MedDiet[[5]] + OGTT_MedDiet$p14
                                 + OGTT_MedDiet[[7]] + OGTT_MedDiet[[8]] + OGTT_MedDiet[[13]] + OGTT_MedDiet[[185]]
                                 + OGTT_MedDiet[[87]] + OGTT_MedDiet[[188]] + OGTT_MedDiet[[189]] + OGTT_MedDiet[[i - 96]]))[[1]][["Pr(>F)"]])[[1]]
}

anovas = round(data.frame(matrix(unlist(anovas), ncol=1, byrow=T),stringsAsFactors=FALSE),3)

anovas$X2 = colnames(OGTT_MedDiet)[137:184]

resumen = data.frame(cbind(anovas))

colnames(resumen) = c("P-value", "names")

export(resumen, "ancovas_p_RN_btdp14_woLFD.xlsx")

rm(anovas, resumen, i)

# R3 Pregunta 16: ####

# T1

OGTT_save_T1b = subset(OGTT_save, t_p14 == "[ 4, 9)")

medias = c()
SD = c()

for (i in 41:88) {
  medias[[i]] = mean(OGTT_save_T1b[[i]], na.rm = T)
  SD[[i]] = t.test(OGTT_save_T1b[[i]],conf.level = 0.95, na.rm = T)$conf.int
}

medias = round(data.frame(matrix(unlist(medias), ncol=1, byrow=T),stringsAsFactors=FALSE),2)
SD = round(data.frame(matrix(unlist(SD), ncol=2, byrow=T),stringsAsFactors=FALSE),2)
tercil = data.frame(rep(1, 48))
tercil$X2 = colnames(OGTT_MedDiet)[41:88]

resumen = data.frame(cbind(medias, SD, tercil))

colnames(resumen) = c("m", "IC_low", "IC_high", "tercil", "names")

rm(OGTT_save_T1b, medias, SD, tercil)

# T3

OGTT_save_T3b = subset(OGTT_save, t_p14 == "[10,12]")

medias = c()
SD = c()

for (i in 41:88) {
  medias[[i]] = mean(OGTT_save_T3b[[i]], na.rm = T)
  SD[[i]] = t.test(OGTT_save_T3b[[i]],conf.level = 0.95, na.rm = T)$conf.int
}

medias = round(data.frame(matrix(unlist(medias), ncol=1, byrow=T),stringsAsFactors=FALSE),2)
SD = round(data.frame(matrix(unlist(SD), ncol=2, byrow=T),stringsAsFactors=FALSE),2)
tercil = data.frame(rep(3, 48))
tercil$X2 = colnames(OGTT_MedDiet)[41:88]

resumen2 = data.frame(cbind(medias, SD, tercil))

colnames(resumen2) = c("m", "IC_low", "IC_high", "tercil", "names")
resumen = rbind(resumen, resumen2)

rm(OGTT_save_T3b, medias, SD, tercil)

# T1

OGTT_save_T1d = subset(OGTT_save, td_p14 == "[-10,1)")

medias = c()
SD = c()

for (i in 137:184) {
  medias[[i]] = mean(OGTT_save_T1d[[i]], na.rm = T)
  SD[[i]] = t.test(OGTT_save_T1d[[i]],conf.level = 0.95, na.rm = T)$conf.int
}

medias = round(data.frame(matrix(unlist(medias), ncol=1, byrow=T),stringsAsFactors=FALSE),2)
SD = round(data.frame(matrix(unlist(SD), ncol=2, byrow=T),stringsAsFactors=FALSE),2)
tercil = data.frame(rep(1, 48))
tercil$X2 = colnames(OGTT_MedDiet)[41:88]

resumen3 = data.frame(cbind(medias, SD, tercil))

colnames(resumen3) = c("m", "IC_low", "IC_high", "tercil", "names")

rm(OGTT_save_T1d, medias, SD, tercil)

# T3

OGTT_save_T3d = subset(OGTT_save, td_p14 == "[  3,8]")

medias = c()
SD = c()

for (i in 137:184) {
  medias[[i]] = mean(OGTT_save_T3d[[i]], na.rm = T)
  SD[[i]] = t.test(OGTT_save_T3d[[i]],conf.level = 0.95, na.rm = T)$conf.int
}

medias = round(data.frame(matrix(unlist(medias), ncol=1, byrow=T),stringsAsFactors=FALSE),2)
SD = round(data.frame(matrix(unlist(SD), ncol=2, byrow=T),stringsAsFactors=FALSE),2)
tercil = data.frame(rep(3, 48))
tercil$X2 = colnames(OGTT_MedDiet)[41:88]

resumen4 = data.frame(cbind(medias, SD, tercil))

colnames(resumen4) = c("m", "IC_low", "IC_high", "tercil", "names")
resumen = rbind(resumen, resumen3, resumen4)

rm(OGTT_save_T3d, medias, SD, tercil)

export(resumen, "media_IC_RN_R3_16.xlsx")

rm(resumen, resumen2, resumen3, resumen4, i)

# HSD Tukey

m1 = aov(OGTT_save$VL_TRLP1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$cHDLP1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$H1P1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$TRLZ1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$NTG1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$NTRLTG1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$BCAA1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$Leu1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$DRI1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

m1 = aov(OGTT_save$GlycA1 ~ OGTT_save$t_p14 + as.factor(OGTT_save[[4]]) + OGTT_save[[5]]
         + as.factor(OGTT_save[[7]]) + as.factor(OGTT_save[[8]]) + OGTT_save[[13]] + OGTT_save[[185]]
         + as.factor(OGTT_save[[187]]) + as.factor(OGTT_save[[188]]) + as.factor(OGTT_save[[189]]))
summary(m1)
TukeyHSD(m1, "OGTT_save$t_p14")

