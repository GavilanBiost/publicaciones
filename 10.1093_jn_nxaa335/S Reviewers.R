#Recuento de FFQ por visita:

PREDIMED1 = PREDIMED[,c("id","ter1_PRAL", "media_PRAL","T_cox_new","energiat", "energiat3", "energiat4", "energiat5", "energiat6", "energiat7", 
                        "energiat8","energiat9","energiat10")]
PREDIMED2 = PREDIMED[,c("energiat", "energiat3", "energiat4", "energiat5", "energiat6", "energiat7", 
                        "energiat8","energiat9","energiat10")]

FFQ = sapply(PREDIMED1, function(PREDIMED1) sum(length(which(is.na(PREDIMED1)))))
FFQ2 = sapply(PREDIMED1, function(PREDIMED1) (100*sum(length(which(is.na(PREDIMED1))))/sum(length((PREDIMED1)))))
FFQ =  cbind("NA"=FFQ, "% NA"=FFQ2)
FFQ

PREDIMED1$tiempo.evento = car::recode(PREDIMED1$T_cox_new, "0:1 = 0;1.001:2 = 1;2.001:3 = 2;3.001:4 = 3;4.001:5 = 4;5.001:6 = 5;
                                 6.001:7 = 6; 7.001:8 = 7; 8.001:9 = 8; else = 9")

PREDIMED1$FFQ = 9999

for (i in 1:length(PREDIMED1$FFQ)){
  if (PREDIMED1$tiempo.evento[i]== 0) {PREDIMED1$FFQ[i] = 1} 
  else if (PREDIMED1$tiempo.evento[i]== 1) {PREDIMED1$FFQ[i]= 2}
  else if (PREDIMED1$tiempo.evento[i]== 2) {PREDIMED1$FFQ[i]= 3}
  else if (PREDIMED1$tiempo.evento[i]== 3) {PREDIMED1$FFQ[i]= 4}
  else if (PREDIMED1$tiempo.evento[i]== 4) {PREDIMED1$FFQ[i]= 5}
  else if (PREDIMED1$tiempo.evento[i]== 5) {PREDIMED1$FFQ[i]= 6}
  else if (PREDIMED1$tiempo.evento[i]== 6) {PREDIMED1$FFQ[i]= 7}
  else {PREDIMED1$FFQ[i]= 8}
}
table(PREDIMED1$FFQ)

PREDIMED1$S1 = 9999

for (i in 1:length(PREDIMED1)){
  if (PREDIMED1$energiat[i]> 0) {PREDIMED1$S1[i] = 1} 
  else if (PREDIMED1$energiat[i] >0 & PREDIMED1$energiat3[i]> 0) {PREDIMED1$S1[i]= 2}
  else {PREDIMED1$S1[i]= 8}
}
table(PREDIMED1$S1)

library(rio)
export(PREDIMED1, "PREDIMED1.xlsx")

library(readxl)
PREDIMED1 = read_excel("F:/ARTICULOS/ART Pral - fractures (working)/Estadística/R/PRAL/PREDIMED1.xlsx")
View(PREDIMED1)
mean(PREDIMED1$SUMA)
max(PREDIMED1$SUMA)
min(PREDIMED1$SUMA)

tabla1 = table(PREDIMED1$SUMA, PREDIMED1$ter1_PRAL) #Los niveles 2 (T1) y 4 (T3) son significativamente diferentes
chisq.test(tabla1)$stdres

chisq.test(tabla1[4,]) 
chisq.test(cluster2$fract_osteo_new, PREDIMED1$SUMA) #No hay diferencias
T2 = table(cluster2$fract_osteo_new, PREDIMED1$SUMA)

table(T2, PREDIMED1$ter1_PRAL)

summary(aov(PREDIMED1$media_PRAL ~ PREDIMED1$SUMA)) #No hay diferencia entre niveles

tapply(PREDIMED1$SUMA, PREDIMED1$ter1_PRAL, mean) #Media de FFQ por tercil

summary(aov(PREDIMED1$SUMA ~ PREDIMED1$ter1_PRAL)) #Hay tendencia no significativa

PREDIMED1$tend_PRAL = Recode(PREDIMED1$ter1_PRAL, "'[ -2.43, 4.63)' = 1.117656; '[-28.98,-2.43)' =-6.911479; '[  4.63,28.98]' = 8.669533")
PREDIMED1$tend_PRAL= as.numeric(PREDIMED1$tend_PRAL)
PREDIMED1$tend_PRAL = Recode(PREDIMED1$tend_PRAL, "1.117656 = 1; -6.911479 = 2; 8.669533 = 3")


PREDIMED1$m = PREDIMED1$SUMA*(PREDIMED1$tend_PRAL)
tapply(PREDIMED1$media_PRAL, PREDIMED1$m, mean)


FFQ = sapply(cluster2, function(cluster2) sum(length(which(is.na(cluster2)))))
FFQ2 = sapply(cluster2, function(cluster2) (100*sum(length(which(is.na(cluster2))))/sum(length((cluster2)))))
FFQ =  cbind("NA"=FFQ, "% NA"=FFQ2)
FFQ

cluster8 = subset(cluster2, hormo1 < 3)

###Grafica cubic spline (al final se hizo en STATA)

aa2 = coxph(Surv(T_cox_new,fract_osteo_new)~ I(media_PRAL^2) + media_PRAL + (tabaco0) + imc1 + getota_1 + (sexo) + (grup_int) + escolar1 + edad0 + (insulin1) + (diabetes0) 
             + (fractura1) + (hormo1) + media_ENERGIA + cluster(idcluster)  + (hta0) + CKD_EPI_v0 + m_osteoporosis
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
summary(aa2)

library(stats)
termplot(aa2, cluster2, xlab="media_PRAL")

#Tabla grupo de alimentos:

library(tis)

PREDIMED4 = PREDIMED[,c("id", "T_cox_new","verdutot", "frutatot", "legumbre", "grupocer","lacteos","carnicos","pescados", "olivatot","fsecos",
                        "verdutot3", "frutatot3", "legumbre3", "grupocer3","lacteos3","carnicos3","pescados3", "olivatot3","fsecos3",
                        "verdutot4", "frutatot4", "legumbre4", "grupocer4","lacteos4","carnicos4","pescados4", "olivatot4","fsecos4",
                        "verdutot5", "frutatot5", "legumbre5", "grupocer5","lacteos5","carnicos5","pescados5", "olivatot5","fsecos5",
                        "verdutot6", "frutatot6", "legumbre6", "grupocer6","lacteos6","carnicos6","pescados6", "olivatot6","fsecos6",
                        "verdutot7", "frutatot7", "legumbre7", "grupocer7","lacteos7","carnicos7","pescados7", "olivatot7","fsecos7",
                        "verdutot8", "frutatot8", "legumbre8", "grupocer8","lacteos8","carnicos8","pescados8", "olivatot8","fsecos8",
                        "verdutot9", "frutatot9", "legumbre9", "grupocer9","lacteos9","carnicos9","pescados9", "olivatot9","fsecos9",
                        "verdutot10", "frutatot10", "legumbre10", "grupocer10","lacteos10","carnicos10","pescados10", "olivatot10","fsecos10")]

#Estimacion de las medias acumuladas:

PREDIMED4$tiempo.evento = car::recode(PREDIMED4$T_cox_new, "0:1 = 0;1.001:2 = 1;2.001:3 = 2;3.001:4 = 3;4.001:5 = 4;5.001:6 = 5;
                                 6.001:7 = 6; 7.001:8 = 7; 8.001:9 = 8; else = 9")

#Verduras

PREDIMED4$media_verdutot = 9999

for (i in 1:length(PREDIMED4$media_verdutot)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_verdutot[i] = PREDIMED4$verdutot[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_verdutot[i]= RowMeans(PREDIMED4[,c("verdutot","verdutot3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_verdutot[i]= RowMeans(PREDIMED4[,c("verdutot","verdutot3","verdutot4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_verdutot[i]= RowMeans(PREDIMED4[,c("verdutot","verdutot3","verdutot4","verdutot5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_verdutot[i]= RowMeans(PREDIMED4[,c("verdutot","verdutot3","verdutot4","verdutot5","verdutot6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_verdutot[i]= RowMeans(PREDIMED4[,c("verdutot","verdutot3","verdutot4","verdutot5","verdutot6","verdutot7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_verdutot[i]= RowMeans(PREDIMED4[,c("verdutot","verdutot3","verdutot4","verdutot5","verdutot6","verdutot7","verdutot8")],na.rm=T)[i]}
  else {PREDIMED4$media_verdutot[i]= RowMeans(PREDIMED4[,c("verdutot","verdutot3","verdutot4","verdutot5","verdutot6","verdutot7","verdutot8","verdutot9","verdutot10")],na.rm=T)[i]}
}

#Frutas

PREDIMED4$media_frutatot = 9999

for (i in 1:length(PREDIMED4$media_frutatot)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_frutatot[i] = PREDIMED4$frutatot[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_frutatot[i]= RowMeans(PREDIMED4[,c("frutatot","frutatot3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_frutatot[i]= RowMeans(PREDIMED4[,c("frutatot","frutatot3","frutatot4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_frutatot[i]= RowMeans(PREDIMED4[,c("frutatot","frutatot3","frutatot4","frutatot5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_frutatot[i]= RowMeans(PREDIMED4[,c("frutatot","frutatot3","frutatot4","frutatot5","frutatot6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_frutatot[i]= RowMeans(PREDIMED4[,c("frutatot","frutatot3","frutatot4","frutatot5","frutatot6","frutatot7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_frutatot[i]= RowMeans(PREDIMED4[,c("frutatot","frutatot3","frutatot4","frutatot5","frutatot6","frutatot7","frutatot8")],na.rm=T)[i]}
  else {PREDIMED4$media_frutatot[i]= RowMeans(PREDIMED4[,c("frutatot","frutatot3","frutatot4","frutatot5","frutatot6","frutatot7","frutatot8","frutatot9","frutatot10")],na.rm=T)[i]}
}

#Legumbre

PREDIMED4$media_legumbre = 9999

for (i in 1:length(PREDIMED4$media_legumbre)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_legumbre[i] = PREDIMED4$legumbre[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_legumbre[i]= RowMeans(PREDIMED4[,c("legumbre","legumbre3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_legumbre[i]= RowMeans(PREDIMED4[,c("legumbre","legumbre3","legumbre4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_legumbre[i]= RowMeans(PREDIMED4[,c("legumbre","legumbre3","legumbre4","legumbre5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_legumbre[i]= RowMeans(PREDIMED4[,c("legumbre","legumbre3","legumbre4","legumbre5","legumbre6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_legumbre[i]= RowMeans(PREDIMED4[,c("legumbre","legumbre3","legumbre4","legumbre5","legumbre6","legumbre7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_legumbre[i]= RowMeans(PREDIMED4[,c("legumbre","legumbre3","legumbre4","legumbre5","legumbre6","legumbre7","legumbre8")],na.rm=T)[i]}
  else {PREDIMED4$media_legumbre[i]= RowMeans(PREDIMED4[,c("legumbre","legumbre3","legumbre4","legumbre5","legumbre6","legumbre7","legumbre8","legumbre9","legumbre10")],na.rm=T)[i]}
}

#grupocer

PREDIMED4$media_grupocer = 9999

for (i in 1:length(PREDIMED4$media_grupocer)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_grupocer[i] = PREDIMED4$grupocer[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_grupocer[i]= RowMeans(PREDIMED4[,c("grupocer","grupocer3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_grupocer[i]= RowMeans(PREDIMED4[,c("grupocer","grupocer3","grupocer4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_grupocer[i]= RowMeans(PREDIMED4[,c("grupocer","grupocer3","grupocer4","grupocer5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_grupocer[i]= RowMeans(PREDIMED4[,c("grupocer","grupocer3","grupocer4","grupocer5","grupocer6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_grupocer[i]= RowMeans(PREDIMED4[,c("grupocer","grupocer3","grupocer4","grupocer5","grupocer6","grupocer7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_grupocer[i]= RowMeans(PREDIMED4[,c("grupocer","grupocer3","grupocer4","grupocer5","grupocer6","grupocer7","grupocer8")],na.rm=T)[i]}
  else {PREDIMED4$media_grupocer[i]= RowMeans(PREDIMED4[,c("grupocer","grupocer3","grupocer4","grupocer5","grupocer6","grupocer7","grupocer8","grupocer9","grupocer10")],na.rm=T)[i]}
}

#lacteos

PREDIMED4$media_lacteos = 9999

for (i in 1:length(PREDIMED4$media_lacteos)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_lacteos[i] = PREDIMED4$lacteos[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_lacteos[i]= RowMeans(PREDIMED4[,c("lacteos","lacteos3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_lacteos[i]= RowMeans(PREDIMED4[,c("lacteos","lacteos3","lacteos4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_lacteos[i]= RowMeans(PREDIMED4[,c("lacteos","lacteos3","lacteos4","lacteos5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_lacteos[i]= RowMeans(PREDIMED4[,c("lacteos","lacteos3","lacteos4","lacteos5","lacteos6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_lacteos[i]= RowMeans(PREDIMED4[,c("lacteos","lacteos3","lacteos4","lacteos5","lacteos6","lacteos7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_lacteos[i]= RowMeans(PREDIMED4[,c("lacteos","lacteos3","lacteos4","lacteos5","lacteos6","lacteos7","lacteos8")],na.rm=T)[i]}
  else {PREDIMED4$media_lacteos[i]= RowMeans(PREDIMED4[,c("lacteos","lacteos3","lacteos4","lacteos5","lacteos6","lacteos7","lacteos8","lacteos9","lacteos10")],na.rm=T)[i]}
}

#carnicos

PREDIMED4$media_carnicos = 9999

for (i in 1:length(PREDIMED4$media_carnicos)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_carnicos[i] = PREDIMED4$carnicos[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_carnicos[i]= RowMeans(PREDIMED4[,c("carnicos","carnicos3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_carnicos[i]= RowMeans(PREDIMED4[,c("carnicos","carnicos3","carnicos4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_carnicos[i]= RowMeans(PREDIMED4[,c("carnicos","carnicos3","carnicos4","carnicos5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_carnicos[i]= RowMeans(PREDIMED4[,c("carnicos","carnicos3","carnicos4","carnicos5","carnicos6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_carnicos[i]= RowMeans(PREDIMED4[,c("carnicos","carnicos3","carnicos4","carnicos5","carnicos6","carnicos7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_carnicos[i]= RowMeans(PREDIMED4[,c("carnicos","carnicos3","carnicos4","carnicos5","carnicos6","carnicos7","carnicos8")],na.rm=T)[i]}
  else {PREDIMED4$media_carnicos[i]= RowMeans(PREDIMED4[,c("carnicos","carnicos3","carnicos4","carnicos5","carnicos6","carnicos7","carnicos8","carnicos9","carnicos10")],na.rm=T)[i]}
}

#pescados

PREDIMED4$media_pescados = 9999

for (i in 1:length(PREDIMED4$media_pescados)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_pescados[i] = PREDIMED4$pescados[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_pescados[i]= RowMeans(PREDIMED4[,c("pescados","pescados3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_pescados[i]= RowMeans(PREDIMED4[,c("pescados","pescados3","pescados4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_pescados[i]= RowMeans(PREDIMED4[,c("pescados","pescados3","pescados4","pescados5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_pescados[i]= RowMeans(PREDIMED4[,c("pescados","pescados3","pescados4","pescados5","pescados6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_pescados[i]= RowMeans(PREDIMED4[,c("pescados","pescados3","pescados4","pescados5","pescados6","pescados7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_pescados[i]= RowMeans(PREDIMED4[,c("pescados","pescados3","pescados4","pescados5","pescados6","pescados7","pescados8")],na.rm=T)[i]}
  else {PREDIMED4$media_pescados[i]= RowMeans(PREDIMED4[,c("pescados","pescados3","pescados4","pescados5","pescados6","pescados7","pescados8","pescados9","pescados10")],na.rm=T)[i]}
}

#olivatot

PREDIMED4$media_olivatot = 9999

for (i in 1:length(PREDIMED4$media_olivatot)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_olivatot[i] = PREDIMED4$olivatot[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_olivatot[i]= RowMeans(PREDIMED4[,c("olivatot","olivatot3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_olivatot[i]= RowMeans(PREDIMED4[,c("olivatot","olivatot3","olivatot4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_olivatot[i]= RowMeans(PREDIMED4[,c("olivatot","olivatot3","olivatot4","olivatot5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_olivatot[i]= RowMeans(PREDIMED4[,c("olivatot","olivatot3","olivatot4","olivatot5","olivatot6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_olivatot[i]= RowMeans(PREDIMED4[,c("olivatot","olivatot3","olivatot4","olivatot5","olivatot6","olivatot7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_olivatot[i]= RowMeans(PREDIMED4[,c("olivatot","olivatot3","olivatot4","olivatot5","olivatot6","olivatot7","olivatot8")],na.rm=T)[i]}
  else {PREDIMED4$media_olivatot[i]= RowMeans(PREDIMED4[,c("olivatot","olivatot3","olivatot4","olivatot5","olivatot6","olivatot7","olivatot8","olivatot9","olivatot10")],na.rm=T)[i]}
}

#fsecos

PREDIMED4$media_fsecos = 9999

for (i in 1:length(PREDIMED4$media_fsecos)){
  if (PREDIMED4$tiempo.evento[i]== 0) {PREDIMED4$media_fsecos[i] = PREDIMED4$fsecos[i]} 
  else if (PREDIMED4$tiempo.evento[i]== 1) {PREDIMED4$media_fsecos[i]= RowMeans(PREDIMED4[,c("fsecos","fsecos3")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 2) {PREDIMED4$media_fsecos[i]= RowMeans(PREDIMED4[,c("fsecos","fsecos3","fsecos4")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 3) {PREDIMED4$media_fsecos[i]= RowMeans(PREDIMED4[,c("fsecos","fsecos3","fsecos4","fsecos5")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 4) {PREDIMED4$media_fsecos[i]= RowMeans(PREDIMED4[,c("fsecos","fsecos3","fsecos4","fsecos5","fsecos6")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 5) {PREDIMED4$media_fsecos[i]= RowMeans(PREDIMED4[,c("fsecos","fsecos3","fsecos4","fsecos5","fsecos6","fsecos7")],na.rm=T)[i]}
  else if (PREDIMED4$tiempo.evento[i]== 6) {PREDIMED4$media_fsecos[i]= RowMeans(PREDIMED4[,c("fsecos","fsecos3","fsecos4","fsecos5","fsecos6","fsecos7","fsecos8")],na.rm=T)[i]}
  else {PREDIMED4$media_fsecos[i]= RowMeans(PREDIMED4[,c("fsecos","fsecos3","fsecos4","fsecos5","fsecos6","fsecos7","fsecos8","fsecos9","fsecos10")],na.rm=T)[i]}
}

P1 = cbind(PREDIMED[1], PREDIMED[4032])

P1 = merge(P1, PREDIMED4, by = "id")

P2 = P1[86:94]

tapply(P1$media_verdutot, P1$ter1_PRAL, mean)
tapply(P1$media_verdutot, P1$ter1_PRAL, sd)
dif = aov(P1$media_verdutot ~ P1$ter1_PRAL)
summary(dif) 

tapply(P1$media_frutatot, P1$ter1_PRAL, mean)
tapply(P1$media_frutatot, P1$ter1_PRAL, sd)
dif = aov(P1$media_frutatot ~ P1$ter1_PRAL)
summary(dif) 

tapply(P1$media_legumbre, P1$ter1_PRAL, mean)
tapply(P1$media_legumbre, P1$ter1_PRAL, sd)
dif = aov(P1$media_legumbre ~ P1$ter1_PRAL)
summary(dif) 

tapply(P1$media_grupocer, P1$ter1_PRAL, mean)
tapply(P1$media_grupocer, P1$ter1_PRAL, sd)
dif = aov(P1$media_grupocer ~ P1$ter1_PRAL)
summary(dif) 

tapply(P1$media_lacteos, P1$ter1_PRAL, mean)
tapply(P1$media_lacteos, P1$ter1_PRAL, sd)
dif = aov(P1$media_lacteos ~ P1$ter1_PRAL)
summary(dif) 

tapply(P1$media_carnicos, P1$ter1_PRAL, mean)
tapply(P1$media_carnicos, P1$ter1_PRAL, sd)
dif = aov(P1$media_carnicos ~ P1$ter1_PRAL)
summary(dif)

tapply(P1$media_pescados, P1$ter1_PRAL, mean)
tapply(P1$media_pescados, P1$ter1_PRAL, sd)
dif = aov(P1$media_pescados ~ P1$ter1_PRAL)
summary(dif)

tapply(P1$media_olivatot, P1$ter1_PRAL, mean)
tapply(P1$media_olivatot, P1$ter1_PRAL, sd)
dif = aov(P1$media_olivatot ~ P1$ter1_PRAL)
summary(dif)

tapply(P1$media_fsecos, P1$ter1_PRAL, mean)
tapply(P1$media_fsecos, P1$ter1_PRAL, sd)
dif = aov(P1$media_fsecos ~ P1$ter1_PRAL)
summary(dif)

tapply(cluster2$media_PROTEINA/cluster2$peso1, cluster2$ter1_PRAL, mean)
tapply(cluster2$media_PROTEINA/cluster2$peso1, cluster2$ter1_PRAL, sd)
dif = aov(cluster2$media_PROTEINA/cluster2$peso1 ~ cluster2$ter1_PRAL)
summary(dif)

##PREDIMED PLUS

tapply(PPLUS3$verdutot_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$verdutot_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$verdutot_v00 ~ PPLUS3$tPRAL)
summary(dif) 

tapply(PPLUS3$frutatot_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$frutatot_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$frutatot_v00 ~ PPLUS3$tPRAL)
summary(dif) 

tapply(PPLUS3$legumbre_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$legumbre_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$legumbre_v00 ~ PPLUS3$tPRAL)
summary(dif) 

tapply(PPLUS3$cereal_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$cereal_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$cereal_v00 ~ PPLUS3$tPRAL)
summary(dif) 

tapply(PPLUS3$lacteos_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$lacteos_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$lacteos_v00 ~ PPLUS3$tPRAL)
summary(dif) 

tapply(PPLUS3$carnicos_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$carnicos_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$carnicos_v00 ~ PPLUS3$tPRAL)
summary(dif)

tapply(PPLUS3$pescados_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$pescados_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$pescados_v00 ~ PPLUS3$tPRAL)
summary(dif)

tapply(PPLUS3$olivatot_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$olivatot_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$olivatot_v00 ~ PPLUS3$tPRAL)
summary(dif)

tapply(PPLUS3$fsecos_v00, PPLUS3$tPRAL, mean)
tapply(PPLUS3$fsecos_v00, PPLUS3$tPRAL, sd)
dif = aov(PPLUS3$fsecos_v00 ~ PPLUS3$tPRAL)
summary(dif)


#Subanalisis sin fracturas a los 2 años:

#Analisis de sensibilidad A2: sin fracturas en T cox <1a
cluster4 = subset(cluster2, fract_osteo_new == 1 & T_cox_new >= 2 | T_cox_new!= 0 & fract_osteo_new == 0)

#Terciles de Pral.
cluster4$ter_PRAL = cut2(cluster4$media_PRAL,g = 3)
cluster4$ter_PRAL = relevel(cluster4$ter_PRAL,ref = "[ -2.47, 4.48)")
levels(cluster4$ter_PRAL)

table(cluster4$ter_PRAL)
table(cluster4$ter_PRAL, cluster4$fract_osteo_new)
with(cluster4, prop.table(table(ter_PRAL,fract_osteo_new), margin = 1))
tapply(cluster4$media_PRAL, cluster4$ter_PRAL, mean)
tapply(cluster4$media_PRAL, cluster4$ter_PRAL, sd)

asp3 = coxph(Surv(T_cox_new,fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster4)
summary(asp3)

#Subanalisis sin fracturas a los 3 años:

#Analisis de sensibilidad A2: sin fracturas en T cox <1a
cluster4 = subset(cluster2, fract_osteo_new == 1 & T_cox_new >= 3 | T_cox_new!= 0 & fract_osteo_new == 0)

#Terciles de Pral.
cluster4$ter_PRAL = cut2(cluster4$media_PRAL,g = 3)
cluster4$ter_PRAL = relevel(cluster4$ter_PRAL,ref = "[ -2.49, 4.39)")
levels(cluster4$ter_PRAL)

table(cluster4$ter_PRAL)
table(cluster4$ter_PRAL, cluster4$fract_osteo_new)
with(cluster4, prop.table(table(ter_PRAL,fract_osteo_new), margin = 1))
tapply(cluster4$media_PRAL, cluster4$ter_PRAL, mean)
tapply(cluster4$media_PRAL, cluster4$ter_PRAL, sd)

asp3 = coxph(Surv(T_cox_new,fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster4)
summary(asp3)


colnames(cluster2)
cor.test(cluster2$media_PRAL, cluster2$media_ENERGIA)


####Comprobación cambios de peso:
colnames(PREDIMED)

pesos = PREDIMED[,c("id","peso1","peso3","peso4","peso5","peso6","peso7","peso8","peso9","peso10")]

clusterpesos = merge(cluster2, pesos, by = "id")

clusterpesos$dp1 = clusterpesos$peso3 - clusterpesos$peso1.x
clusterpesos$dp2 = clusterpesos$peso4 - clusterpesos$peso3
clusterpesos$dp3 = clusterpesos$peso5 - clusterpesos$peso4
clusterpesos$dp4 = clusterpesos$peso6 - clusterpesos$peso5
clusterpesos$dp5 = clusterpesos$peso7 - clusterpesos$peso6
clusterpesos$dp6 = clusterpesos$peso8 - clusterpesos$peso7
clusterpesos$dp7 = clusterpesos$peso9 - clusterpesos$peso8
clusterpesos$dp8 = clusterpesos$peso10 - clusterpesos$peso9

clusterpesos$tiempo.evento = car::recode(clusterpesos$T_cox_new, "0:1 = 0;1.001:2 = 1;2.001:3 = 2;3.001:4 = 3;4.001:5 = 4;5.001:6 = 5;
                                 6.001:7 = 6; 7.001:8 = 7; 8.001:9 = 8; else = 9")

table(clusterpesos$tiempo.evento)
tapply(clusterpesos$T_cox_new, clusterpesos$tiempo.evento, mean)

clusterpesos$media_dp = 9999

library(tis)

for (i in 1:length(clusterpesos$media_dp)){
  if (clusterpesos$tiempo.evento[i]== 0) {clusterpesos$media_dp[i] = clusterpesos$dp1[i]} 
  else if (clusterpesos$tiempo.evento[i]== 1) {clusterpesos$media_dp[i]= clusterpesos$dp1[i]}
  else if (clusterpesos$tiempo.evento[i]== 2) {clusterpesos$media_dp[i]= RowMeans(clusterpesos[,c("dp1","dp2")],na.rm=T)[i]}
  else if (clusterpesos$tiempo.evento[i]== 3) {clusterpesos$media_dp[i]= RowMeans(clusterpesos[,c("dp1","dp2","dp3")],na.rm=T)[i]}
  else if (clusterpesos$tiempo.evento[i]== 4) {clusterpesos$media_dp[i]= RowMeans(clusterpesos[,c("dp1","dp2","dp3","dp4")],na.rm=T)[i]}
  else if (clusterpesos$tiempo.evento[i]== 5) {clusterpesos$media_dp[i]= RowMeans(clusterpesos[,c("dp1","dp2","dp3","dp4","dp5")],na.rm=T)[i]}
  else if (clusterpesos$tiempo.evento[i]== 6) {clusterpesos$media_dp[i]= RowMeans(clusterpesos[,c("dp1","dp2","dp3","dp4","dp5","dp6")],na.rm=T)[i]}
  else if (clusterpesos$tiempo.evento[i]== 7) {clusterpesos$media_dp[i]= RowMeans(clusterpesos[,c("dp1","dp2","dp3","dp4","dp5","dp6","dp7")],na.rm=T)[i]}
  else {clusterpesos$media_dp[i]= RowMeans(clusterpesos[,c("dp1","dp2","dp3","dp4","dp5","dp6","dp7","dp8")],na.rm=T)[i]}
}

tapply(clusterpesos$media_dp, clusterpesos$ter1_PRAL, mean, na.rm = T)
tapply(clusterpesos$media_dp, clusterpesos$ter1_PRAL, sd, na.rm = T)

a1 = aov(media_dp ~ ter1_PRAL, clusterpesos)
summary(a1)

tapply(clusterpesos$media_dp, clusterpesos$ter_NEAP, mean, na.rm = T)
tapply(clusterpesos$media_dp, clusterpesos$ter_NEAP, sd, na.rm = T)

a1 = aov(media_dp ~ ter_NEAP, clusterpesos)
summary(a1)

library(survival)

dpa = coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL + getota_1 + media_dp + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = clusterpesos)
summary(dpa)

lrtest(dpa, b2)

dpat = coxph(Surv(T_cox_new,fract_osteo_new)~ tend_PRAL + I(tend_PRAL^2) + getota_1 + media_dp + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = clusterpesos)
summary(dpat)

dpa1 = coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP + getota_1 + media_dp + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = clusterpesos)
summary(dpa1)

dpat1 = coxph(Surv(T_cox_new,fract_osteo_new)~ tend_NEAP + I(tend_PRAL^2) + getota_1 + media_dp + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = clusterpesos)
summary(dpat1)

#####Modelo considerando calcio como termino de interacción:

library(lmtest)

ca = coxph(Surv(T_cox_new,fract_osteo_new)~ter1_PRAL*calcio + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
             + hormo1 + fractura1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = clusterpesos)

b2 = coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
            + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 + m_osteoporosis
            + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster2)
lrtest(ca, b2)


#Validacion de las asunciones de proporcionalidad:

#Longitudinal:

#Riesgos proporcionales:

cox.zph(aa2)
cox.zph(b2)

#Transversal:

#Independecia:

plot(a1.1$residuals) 

#Normalidad:

summary(a1.1$residuals)
boxplot(a1.1$residuals)
hist(a1.1$residuals)
qqnorm(a1.1$residuals) 
qqline(a1.1$residuals)
shapiro.test(a1.1$residuals)

#Homocesteicidad:
tPRAL = PPLUS3[,c("paciente", "tPRAL")]
tPRAL$n = rownames(tPRAL)
res = data.frame(a1.1$residuals)
res$n = rownames(res)
res2 = merge(res, tPRAL, by = "n")
colnames(res2)

boxplot(res2$a1.1.residuals ~ res2$tPRAL) 
desviaciones = tapply(res2$a1.1.residuals, res2$tPRAL, sd) 
max(desviaciones) / min(desviaciones)
bartlett.test(res2$a1.1.residuals ~ res2$tPRAL)

###Normalidad de la variable alcohol:

shapiro.test(PPLUS3$alcoholg_v00)
hist(PPLUS3$alcoholg_v00)
boxplot(PPLUS3$alcoholg_v00 ~ PPLUS3$tPRAL)
kruskal.test(PPLUS3$alcoholg_v00 ~ PPLUS3$tPRAL)

####Pregunta 6, R1.

b2.1 = coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
            + fractura1 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 + m_osteoporosis
            + media_hc + media_fibr + media_vitaminaD + media_gratot + media_calci, robust=T, data = cluster2)
summary(b2.1)

lrtest(b2, b2.1)

bc2 = coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
               insulin1 + diabetes0 + fractura1 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 
             + media_hc + media_fibr + media_vitaminaD + media_gratot + media_calci, robust = T, data = cluster2)
summary(bc2)

####Pregunta 6, R2. Analisis de sensibilidad sin supl.

#Analisis de sensibilidad. A1: sin fracturas previas
#Formacion de la BBDD.
cluster9 = subset(cluster2, m_osteoporosis == 0)

#Terciles de Pral y NEAP.
cluster9$ter_PRAL = cut2(cluster9$media_PRAL,g = 3)
cluster9$ter_PRAL = relevel(cluster9$ter_PRAL,ref = "[ -2.19, 4.91)")
levels(cluster9$ter_PRAL)

cluster9$ter_NEAP = cut2(cluster9$media_NEAP,g = 3)
cluster9$ter_NEAP = relevel(cluster9$ter_NEAP,ref = "[36.8,41.1)")
levels(cluster9$ter_NEAP)
str(cluster9$ter_NEAP)

#Medias y eventos.
table(cluster9$ter_PRAL, cluster9$fract_osteo_new)
with(cluster9, prop.table(table(ter_PRAL,fract_osteo_new), margin = 1))
tapply(cluster9$media_PRAL, cluster9$ter_PRAL, mean)
tapply(cluster9$media_PRAL, cluster9$ter_PRAL, sd)

table(cluster9$ter_NEAP, cluster9$fract_osteo_new)
with(cluster9, prop.table(table(ter_NEAP,fract_osteo_new), margin = 1))
tapply(cluster9$media_NEAP, cluster9$ter_NEAP, mean)
tapply(cluster9$media_NEAP, cluster9$ter_NEAP, sd)

#Inicio del analisis.
#Basal
options(scipen = 999)
asp1 = coxph(Surv(T_cox_new, fract_osteo_new)~ter_PRAL + cluster(idcluster), robust=T, data = cluster9)
summary(asp1)

asn1 = coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + cluster(idcluster), robust=T, data = cluster9)
summary(asn1)

#M1
asp2 = coxph(Surv(T_cox_new, fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
              , robust=T, data = cluster9)
summary(asp2)

asn2 = coxph(Surv(T_cox_new, fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + sexo + edad0+ grup_int + cluster(idcluster)
              , robust=T, data = cluster9)
summary(asn2)

#M2
asp3 = coxph(Surv(T_cox_new,fract_osteo_new)~ter_PRAL + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 + fractura1
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster9)
summary(asp3)

asn3 = coxph(Surv(T_cox_new,fract_osteo_new)~ter_NEAP + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + insulin1 + diabetes0 
              + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 + fractura1
              + media_hc + media_fibr + media_vitaminaD + media_gratot, robust=T, data = cluster9)
summary(asn3)

#Analisis continuo cuadratico.
#Basal
casp1 = coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + cluster(idcluster), robust = T
               , data = cluster9)
summary(casp1)

casn1 = coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + cluster(idcluster), robust = T
               , data = cluster9)
summary(casn1)

#M1
casp2 = coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster9)
summary(casp2)

casn2 = coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster9)
summary(casn2)

#M2
casp3 = coxph(Surv(T_cox_new, fract_osteo_new)~media_PRAL + I(media_PRAL^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
                 diabetes0 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster9)
summary(casp3)

casn3 = coxph(Surv(T_cox_new, fract_osteo_new)~media_NEAP + I(media_NEAP^2) + getota_1 + imc1 + tabaco0 + sexo + edad0 + insulin1 + 
                 diabetes0 + hormo1 + media_ENERGIA + grup_int + cluster(idcluster) + hta0 + CKD_EPI_v0 
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster9)
summary(casn3)

#P q tendencia.
#Creacion variable p tendencia
tapply(cluster9$media_PRAL, cluster9$ter_PRAL, median)
levels(cluster9$ter_PRAL)
cluster9$tend_PRAL = Recode(cluster9$ter_PRAL, "'[ -2.19, 4.91)' = 1.277977; '[-28.98,-2.19)' =-6.483642; '[  4.91,28.98]' = 8.749716")
cluster9$tend_PRAL= as.numeric(cluster9$tend_PRAL)
cluster9$tend_PRAL = Recode(cluster9$tend_PRAL, "'2' = 1.277977; '1' =-6.483642; '3' = 8.749716")
summary(cluster9$tend_PRAL)
levels(cluster9$tend_PRAL)
table(cluster9$ter_PRAL, cluster9$tend_PRAL)
str(cluster9$tend_PRAL)

tapply(cluster9$media_NEAP, cluster9$ter_NEAP, median)
levels(cluster9$ter1_NEAP)
cluster9$tend_NEAP = Recode(cluster9$ter_NEAP, "'[36.8,41.1)' = 38.80339; '[18.3,36.8)' = 34.26175; '[41.1,57.0]' = 44.00008")
cluster9$tend_NEAP = as.numeric(cluster9$tend_NEAP)
cluster9$tend_NEAP = Recode(cluster9$tend_NEAP, "'2' = 38.80339; '1' = 34.26175; '3' = 44.00008")
summary(cluster9$tend_NEAP)
levels(cluster9$tend_NEAP)
table(cluster9$ter_NEAP, cluster9$tend_NEAP)
str(cluster9$tend_NEAP)

#Basal
tasp1 = coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + cluster(idcluster), robust = T, data = cluster9)
summary(tasp1)

tasn1 = coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + cluster(idcluster), robust = T, data = cluster9)
summary(tasn1)

#M1
tasp2 = coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster9)
summary(tasp2)

tasn2 = coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 +
                 grup_int + cluster(idcluster), robust = T, data = cluster9)
summary(tasn2)

#M2
tasp3 = coxph(Surv(T_cox_new, fract_osteo_new)~tend_PRAL + I(tend_PRAL^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
                 insulin1 + diabetes0 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 + fractura1
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster9)
summary(tasp3)

tasn3 = coxph(Surv(T_cox_new, fract_osteo_new)~tend_NEAP + I(tend_NEAP^2) + getota_1 + imc1 + tabaco0 + escolar1 + sexo + edad0 + 
                 insulin1 + diabetes0 + hormo1 + grup_int + media_ENERGIA + cluster(idcluster) + hta0 + CKD_EPI_v0 + fractura1
               + media_hc + media_fibr + media_vitaminaD + media_gratot, robust = T, data = cluster9)
summary(tasn3)

###Con DMO:

PPLUS3b = subset(PPLUS3, tto_supleCaD_v00 == "No")
PPLUS4b = subset(PPLUS4, tto_supleCaD_v00 == "No")
PPLUS5b = subset(PPLUS5, tto_supleCaD_v00 == "No")
PPLUS6b = subset(PPLUS6, tto_supleCaD_v00 == "No")
PPLUS7b = subset(PPLUS7, tto_supleCaD_v00 == "No")
PPLUS8b = subset(PPLUS8, tto_supleCaD_v00 == "No")

#Formacion de tPRAL
PPLUS3b$tPRAL=cut2(PPLUS3b$PRAL,g = 3)
PPLUS4b$tPRAL=cut2(PPLUS4b$PRAL,g = 3)
PPLUS5b$tPRAL=cut2(PPLUS5b$PRAL,g = 3)
PPLUS7b$tPRAL=cut2(PPLUS7b$PRAL,g = 3)
PPLUS8b$tPRAL=cut2(PPLUS8b$PRAL,g = 3)

#Formacion de tNEAP
PPLUS3b$tNEAP=cut2(PPLUS3b$NEAP,g = 3)
PPLUS4b$tNEAP=cut2(PPLUS4b$NEAP,g = 3)
PPLUS5b$tNEAP=cut2(PPLUS5b$NEAP,g = 3)
PPLUS7b$tNEAP=cut2(PPLUS7b$NEAP,g = 3)
PPLUS8b$tNEAP=cut2(PPLUS8b$NEAP,g = 3)

options(scipen = 999)
table(PPLUS3b$tPRAL)
acp2.2 <- aov(dmoftot_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tPRAL, PPLUS3b)
summary(acp2.2)
adjmeanacp1.1 = effect("tPRAL",acp2.2, se=T, xlevels=3)
adjmeanacp1.1$se
adjmeanacp1.1$fit
TukeyHSD(acp2.2,"tPRAL")

table(PPLUS4b$tPRAL)
acp2.3 <- aov(dmol1l4_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tPRAL, PPLUS4b)
summary(acp2.3)
adjmeanacp1.1 = effect("tPRAL",acp2.3, se=T, xlevels=3)
adjmeanacp1.1$se
adjmeanacp1.1$fit
TukeyHSD(acp2.3,"tPRAL")

table(PPLUS5b$tPRAL)
acp2.4 <- aov(dmocf_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tPRAL, PPLUS5b)
summary(acp2.4)
adjmeanacp1.1 = effect("tPRAL",acp2.4, se=T, xlevels=3)
adjmeanacp1.1$se
adjmeanacp1.1$fit
TukeyHSD(acp2.4,"tPRAL")

table(PPLUS7b$tPRAL)
acp2.5 <- aov(dmotf_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tPRAL, PPLUS7b)
summary(acp2.5)
adjmeanacp1.1 = effect("tPRAL",acp2.5, se=T, xlevels=3)
adjmeanacp1.1$se
adjmeanacp1.1$fit
TukeyHSD(acp2.5,"tPRAL")

table(PPLUS8b$tPRAL)
acp2.6 <- aov(dmodf_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tPRAL, PPLUS8b)
summary(acp2.6)
adjmeanacp1.1 = effect("tPRAL",acp2.6, se=T, xlevels=3)
adjmeanacp1.1$se
adjmeanacp1.1$fit
TukeyHSD(acp2.6,"tPRAL")

table(PPLUS3b$tNEAP)
acn2.2 <- aov(dmoftot_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tNEAP, PPLUS3b)
summary(acn2.2)
adjmeanacn1.1 = effect("tNEAP",acn2.2, se=T, xlevels=3)
adjmeanacn1.1$se
adjmeanacn1.1$fit
TukeyHSD(acn2.2,"tNEAP")

acn2.3 <- aov(dmol1l4_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tNEAP, PPLUS4b)
summary(acn2.3)
adjmeanacn1.1 = effect("tNEAP",acn2.3, se=T, xlevels=3)
adjmeanacn1.1$se
adjmeanacn1.1$fit
TukeyHSD(acn2.3,"tNEAP")

acn2.4 <- aov(dmocf_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tNEAP, PPLUS5b)
summary(acn2.4)
adjmeanacn1.1 = effect("tNEAP",acn2.4, se=T, xlevels=3)
adjmeanacn1.1$se
adjmeanacn1.1$fit
TukeyHSD(acn2.4,"tNEAP")

acn2.5 <- aov(dmotf_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tNEAP, PPLUS7b)
summary(acn2.5)
adjmeanacn1.1 = effect("tNEAP",acn2.5, se=T, xlevels=3)
adjmeanacn1.1$se
adjmeanacn1.1$fit
TukeyHSD(acn2.5,"tNEAP")

acn2.6 <- aov(dmodf_v00 ~ edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
              + fuma_s1 + act_fis_v00 + obesidad_s1 + eGFR_EPI_v00 
              + tto_horm_v00 + tto_insu_v00 + tNEAP, PPLUS8b)
summary(acn2.6)
adjmeanacn1.1 = effect("tNEAP",acn2.6, se=T, xlevels=3)
adjmeanacn1.1$se
adjmeanacn1.1$fit
TukeyHSD(acn2.6,"tNEAP")


#P q tend.
#Creacion variable p tendencia
tapply(PPLUS3b$PRAL, PPLUS3b$tPRAL, median)
PPLUS3b$tend_PRAL = PPLUS3b$tPRAL
PPLUS3b$tend_PRAL<- as.numeric(PPLUS3b$tend_PRAL)
summary(PPLUS3b$tend_PRAL)
str(PPLUS3b$tend_PRAL)

tapply(PPLUS4b$PRAL, PPLUS4b$tPRAL, median)
PPLUS4b$tend_PRAL = PPLUS4b$tPRAL
PPLUS4b$tend_PRAL<- as.numeric(PPLUS4b$tend_PRAL)
summary(PPLUS4b$tend_PRAL)
str(PPLUS4b$tend_PRAL)

tapply(PPLUS5b$PRAL, PPLUS5b$tPRAL, median)
PPLUS5b$tend_PRAL = PPLUS5b$tPRAL
PPLUS5b$tend_PRAL<- as.numeric(PPLUS5b$tend_PRAL)
summary(PPLUS5b$tend_PRAL)
str(PPLUS5b$tend_PRAL)

tapply(PPLUS7b$PRAL, PPLUS7b$tPRAL, median)
PPLUS7b$tend_PRAL = PPLUS7b$tPRAL
PPLUS7b$tend_PRAL<- as.numeric(PPLUS7b$tend_PRAL)
summary(PPLUS7b$tend_PRAL)
str(PPLUS7b$tend_PRAL)

tapply(PPLUS8b$PRAL, PPLUS8b$tPRAL, median)
PPLUS8b$tend_PRAL = PPLUS8b$tPRAL
PPLUS8b$tend_PRAL<- as.numeric(PPLUS8b$tend_PRAL)
summary(PPLUS8b$tend_PRAL)
str(PPLUS8b$tend_PRAL)

#lineal
a1.2.1 <- lm(dmoftot_v00 ~ tend_PRAL + I(tend_PRAL^2) + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1 + eGFR_EPI_v00
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS3b)
summary(a1.2.1)

b1.2.1 <- lm(dmol1l4_v00 ~ tend_PRAL + I(tend_PRAL^2)  + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1 + eGFR_EPI_v00
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS4b)
summary(b1.2.1)

c1.2.1 <- lm(dmocf_v00 ~ tend_PRAL + I(tend_PRAL^2)  + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS5b)
summary(c1.2.1)

e1.2.1 <- lm(dmotf_v00 ~ tend_PRAL + I(tend_PRAL^2)  + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1 + eGFR_EPI_v00
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS7b)
summary(e1.2.1)

f1.2.1 <- lm(dmodf_v00 ~ tend_PRAL + I(tend_PRAL^2)  + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1 + eGFR_EPI_v00
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS8b)
summary(f1.2.1)


#Creacion variable p tendencia
tapply(PPLUS3b$PRAL, PPLUS3b$tNEAP, median)
PPLUS3b$tend_PRAL = PPLUS3b$tNEAP
PPLUS3b$tend_PRAL<- as.numeric(PPLUS3b$tend_PRAL)
summary(PPLUS3b$tend_PRAL)
str(PPLUS3b$tend_PRAL)

tapply(PPLUS4b$PRAL, PPLUS4b$tNEAP, median)
PPLUS4b$tend_PRAL = PPLUS4b$tNEAP
PPLUS4b$tend_PRAL<- as.numeric(PPLUS4b$tend_PRAL)
summary(PPLUS4b$tend_PRAL)
str(PPLUS4b$tend_PRAL)

tapply(PPLUS5b$PRAL, PPLUS5b$tNEAP, median)
PPLUS5b$tend_PRAL = PPLUS5b$tNEAP
PPLUS5b$tend_PRAL<- as.numeric(PPLUS5b$tend_PRAL)
summary(PPLUS5b$tend_PRAL)
str(PPLUS5b$tend_PRAL)

tapply(PPLUS7b$PRAL, PPLUS7b$tNEAP, median)
PPLUS7b$tend_PRAL = PPLUS7b$tNEAP
PPLUS7b$tend_PRAL<- as.numeric(PPLUS7b$tend_PRAL)
summary(PPLUS7b$tend_PRAL)
str(PPLUS7b$tend_PRAL)

tapply(PPLUS8b$PRAL, PPLUS8b$tNEAP, median)
PPLUS8b$tend_PRAL = PPLUS8b$tNEAP
PPLUS8b$tend_PRAL<- as.numeric(PPLUS8b$tend_PRAL)
summary(PPLUS8b$tend_PRAL)
str(PPLUS8b$tend_PRAL)

#lineal
a1.2.1 <- lm(dmoftot_v00 ~ tend_PRAL + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1 + eGFR_EPI_v00
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS3b)
summary(a1.2.1)

b1.2.1 <- lm(dmol1l4_v00 ~ tend_PRAL  + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1 + eGFR_EPI_v00
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS4b)
summary(b1.2.1)

c1.2.1 <- lm(dmocf_v00 ~ tend_PRAL  + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS5b)
summary(c1.2.1)

e1.2.1 <- lm(dmotf_v00 ~ tend_PRAL  + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1 + eGFR_EPI_v00
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS7b)
summary(e1.2.1)

f1.2.1 <- lm(dmodf_v00 ~ tend_PRAL  + edad_s1 + hba1c_v00 + energiat_v00 + sexo_s1 + eGFR_EPI_v00
             + fuma_s1 + act_fis_v00 + obesidad_s1 + osteopor_v00 + tto_horm_v00 + tto_insu_v00, PPLUS8b)
summary(f1.2.1)
