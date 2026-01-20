# Apertura de la BBDD
# Comienzo revisando la BBDD en excel. Observo que hay valores que indican #¡NULO! (típico cuando se exporta de SPSS a excel)
# Para evitar error en R, los elimino previamente.
# Comprueba que la BBDD está traspuesta pero no adecuadamente. Siendo consciente de mi error de comunicación inicial, 
# procedo inicialmente a preparar la BBDD previo inicio de su análisis.

# Para el correcto funcionamiento del modelo, necesito transformar las variables iguales que están divididas en diferentes
# categorias en una sola variable, es decir, deshacer la transposición y generar una sola variable de
# edad, tmt, SAPS, SANS y CPZ. 

library(readxl)
library(rio)
library(ggplot2)
library(nlme)
library(car)

BBDD <- read_excel("BD_Santander_Transpuesta.xlsx")
View(BBDD)

BBDD$Time = NULL

str(BBDD)

BBDD = t(BBDD)

colnames(BBDD) = BBDD[1,]

BBDD = data.frame(BBDD[-1,])

str(BBDD)

BBDD$ID = rownames(BBDD)

# Observo que la variable Time está mal aplicada (está como si fuera un participante más), por lo que
# en las subdivisiones que voy a realizar generaré una nueva variable time. La elimino antes de transponer
# la BBDD. Además creo una nueva variable (ID) para tener fichados a los pacientes una vez transpuesta.
# Así mismo, observo que algunas variables que deberían ser numéricas, tienen valores 
# decimales diferentes por lo que exporto la BBDD para correguir los valores irregulares en excel

export(BBDD, "BBDD.xlsx")

BBDD <- read_excel("BBDD.xlsx")
View(BBDD)

# Generación de la BBDD basal:

colnames(BBDD)

str(BBDD)

BBDD_V0 = data.frame(cbind(BBDD[1:5], BBDD[9:11], BBDD[15], BBDD[19], BBDD[23], BBDD[27:30]))
BBDD_V0$Time = 0
BBDD_V1 = data.frame(cbind(BBDD[1:4], BBDD[6], BBDD[9:10], BBDD[12], BBDD[16], BBDD[20], BBDD[24], BBDD[27:30]))
BBDD_V1$Time = 1
BBDD_V3 = data.frame(cbind(BBDD[1:4], BBDD[7], BBDD[9:10], BBDD[13], BBDD[17], BBDD[21], BBDD[25], BBDD[27:30]))
BBDD_V3$Time = 3
BBDD_V10 = data.frame(cbind(BBDD[1:4], BBDD[8:10], BBDD[14], BBDD[18], BBDD[22], BBDD[26:30]))
BBDD_V10$Time = 10

colnames(BBDD_V0) = c("ID","pacient_control","SEXE","years_edu","Edad",         
                      "porro","cannabis_rec","tmta","SAPS","SANS",          
                      "CPZ","anys_evol_psico","HAPLO","rs1264323","rs22399518",     
                      "Time")
colnames(BBDD_V1) = c("ID","pacient_control","SEXE","years_edu","Edad",         
                      "porro","cannabis_rec","tmta","SAPS","SANS",          
                      "CPZ","anys_evol_psico","HAPLO","rs1264323","rs22399518",     
                      "Time")
colnames(BBDD_V3) = c("ID","pacient_control","SEXE","years_edu","Edad",         
                      "porro","cannabis_rec","tmta","SAPS","SANS",          
                      "CPZ","anys_evol_psico","HAPLO","rs1264323","rs22399518",     
                      "Time")
colnames(BBDD_V10) = c("ID","pacient_control","SEXE","years_edu","Edad",         
                       "porro","cannabis_rec","tmta","SAPS","SANS",          
                       "CPZ","anys_evol_psico","HAPLO","rs1264323","rs22399518",     
                       "Time")

BBDD_2 = rbind(BBDD_V0, BBDD_V1, BBDD_V3, BBDD_V10)

export(BBDD_2, "BD_Santander_long.xlsx")

str(BBDD_2)

# No puede haber NAs en la BBDD para realizar los análisis mixtos por lo que doy valor 0 a todas las
# variables con NAs menos tmta (variable de estudio)con la que realizare un subset para eliminar los
# NAs (coincidiran con aquellos participantes que no han realizado la prueba en alguno de los años
# siguientes)

BBDD_2 = subset(BBDD_2, tmta > 0)

na1 = sapply(BBDD_2, function(BBDD_2) sum(length(which(is.na(BBDD_2)))))
na2 = sapply(BBDD_2, function(BBDD_2) (100*sum(length(which(is.na(BBDD_2))))/sum(length((BBDD_2)))))
na3 =  cbind("NA"=na1, "% NA"=na2)
na3

BBDD_2$porro = ifelse(is.na(BBDD_2$porro), 0, BBDD_2$porro)
BBDD_2$SAPS = ifelse(is.na(BBDD_2$SAPS), 0, BBDD_2$SAPS)
BBDD_2$SANS = ifelse(is.na(BBDD_2$SANS), 0, BBDD_2$SANS)
BBDD_2$CPZ = ifelse(is.na(BBDD_2$CPZ), 0, BBDD_2$CPZ)
BBDD_2$anys_evol_psico = ifelse(is.na(BBDD_2$anys_evol_psico), 0, BBDD_2$anys_evol_psico)

rm(BBDD, BBDD_V0, BBDD_V1, BBDD_V10, BBDD_V3, fit, na3, na1, na2)

# Valoracion de la distribucion de las variables:

hist(BBDD_2$tmta)
hist(log(BBDD_2$tmta))

BBDD_2$SEXE = as.factor(BBDD_2$SEXE)
BBDD_2$Time = as.factor(BBDD_2$Time)
BBDD_2$cannabis_rec = as.factor(BBDD_2$cannabis_rec)

ggplot(BBDD_2, aes(x = Time, y = tmta, group=ID, color = ID)) +
  geom_line(alpha=.3) + labs(title="Tmta values of participant Across follow-up") +
  theme(legend.position = "none")

# Modelos mixtos con toda la poblacion:

# Modelos HAPLO:

m0 = gls(log(tmta) ~ HAPLO, data = BBDD_2) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ HAPLO, random = ~1 + HAPLO|ID, data = BBDD_2)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR. No obstante, se puede ver que la significancia varia considerablemente
anova.lme(m1, type = "marginal")

# Se procede a seguir con los MLR:

m1 = lme(log(tmta) ~ HAPLO, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
m2 = lme(log(tmta) ~ HAPLO*pacient_control + Time + SEXE + Edad, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
summary(m2)

# Se observa que hay efectos significativos con la variable tiempo (disminucion de la puntuacion con el tiempo) y edad pero no parece que sexo
# mejore el modelo. Se compara con el modelo basal

anova(m1, m2) # m2 mejora m1 significativamente. Se procede a valorar la importancia de la variable sexo:

m3 = lme(log(tmta) ~ HAPLO + pacient_control + Time + SEXE + Edad + years_edu, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
anova(m3, type = "marginal")

# Al añadir años de educacion, el efecto de HAPLO se pierde. Posible interacion?

m3.1 = lme(log(tmta) ~ HAPLO*years_edu + Time + SEXE + Edad + pacient_control, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
anova(m3.1, type = "marginal") 

anova(m2, m3)
anova(m3.1, m3)

# m3 mejora m2. Nos quedamos m3 de referencia.

m4 = lme(log(tmta) ~ HAPLO + years_edu + Time + SEXE + Edad + pacient_control
         + porro + cannabis_rec, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
anova(m4, type = "marginal") 

# La variable dicotomica de cannabis parace estar relacionada pero no asi porro. Se valora si es necesario mantener ambas variables ya que representan
# lo mismo.

m4.1 = lme(log(tmta) ~ HAPLO + years_edu + Time + SEXE + Edad + pacient_control
           + cannabis_rec, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")

anova(m4, m4.1)

anova(m4.1, m3, m4) #Nos quedamos como refencia m4.1 

m5 = lme(log(tmta) ~ HAPLO + years_edu + Time + SEXE + Edad + pacient_control
         + cannabis_rec + SANS+ SAPS, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
anova(m5, type = "marginal")

m5.1 = lme(log(tmta) ~ HAPLO + years_edu + Time + SEXE + Edad + pacient_control
           + cannabis_rec + SANS, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")

anova(m5, m4.1, m5.1) #m5 mejora m4.1. m5.1 no mejora nada. Descartamos m5.1 y m5 queda como referencia.

m6 = lme(log(tmta) ~ HAPLO + years_edu + Time + SEXE + Edad + pacient_control
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
anova(m6, type = "marginal")

anova(m6, m5) #m6 mejora m5

# Finalmente, comprobamos el efecto del gen en el tiempo (TimexHAPLO) y interaccion por grupos

m7 = lme(log(tmta) ~ HAPLO * Time + years_edu + SEXE + Edad + pacient_control
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
anova(m7, type = "marginal")
anova(m6, m7)

m7.1 = lme(log(tmta) ~ HAPLO * pacient_control + Time + years_edu + SEXE + Edad 
           + cannabis_rec + SANS + SAPS
           + CPZ + anys_evol_psico, random = ~1 + HAPLO|ID, data = BBDD_2, method = "ML")
anova(m7.1, type = "marginal")
anova(m6, m7.1)

summary(m7)

# Se descartan interacciones y finalmente nos quedamos con m6. 

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m6)
leveneTest(log(BBDD_2$tmta), BBDD_2$HAPLO)
qqnorm(resid(m6))
qqline(resid(m6))
hist(resid(m6))

# m7 cumple con los supuestos. 

coefm7 = round(data.frame((coef(summary(m6)))),3)
coefm7$names = rownames(coefm7)

export(coefm7, "coef_HAPLO.xlsx")

rm(coefm7, m0, m1, m2, m2.1, m3, m3.1, m4, m4.1, m4.2,  m5, m5.1,m5.2,  m6, m6.1, m7, m7.1)

# Modelos rs1264323:

m0 = gls(log(tmta) ~ rs1264323, data = BBDD_2) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ rs1264323, random = ~1 + rs1264323|ID, data = BBDD_2)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR
anova.lme(m1, type = "marginal")

# Se procede a seguir con los MLR aunque no parece que este gen vaya a tener influencia sobre TMTa

m1 = lme(log(tmta) ~ rs1264323, random = ~1 + rs1264323|ID, data = BBDD_2, method = "ML")
m2 = lme(log(tmta) ~ rs1264323 + pacient_control + Time + SEXE + Edad, random = ~1 + rs1264323|ID, data = BBDD_2, method = "ML")
summary(m2)

anova(m1, m2) # m2 mejora m1 significativamente. Se procede a valorar la importancia de la variable sexo:

m3 = lme(log(tmta) ~ rs1264323 + pacient_control + Time + SEXE + Edad + years_edu, random = ~1 + rs1264323|ID, data = BBDD_2, method = "ML")
anova(m3, type = "marginal")

anova(m2, m3) #m3 mejora m2. Referencia m3.

m4 = lme(log(tmta) ~ rs1264323 + years_edu + Time + SEXE + Edad + pacient_control
         + cannabis_rec, random = ~1 + rs1264323|ID, data = BBDD_2, method = "ML")
anova(m4, type = "marginal") 

anova( m3, m4) #M4 mejora m3. m4 referencia

m5 = lme(log(tmta) ~ rs1264323 + years_edu + Time + SEXE + Edad + pacient_control
         + cannabis_rec + SANS+ SAPS, random = ~1 + rs1264323|ID, data = BBDD_2, method = "ML")
anova(m5, type = "marginal")

anova(m5, m4) #m5 mejora m4. Refencia m5.

m6 = lme(log(tmta) ~ rs1264323 + years_edu + Time + SEXE + Edad + pacient_control
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + rs1264323|ID, data = BBDD_2, method = "ML")
anova(m6, type = "marginal")

anova(m6, m5) #m6 mejora m5

# Finalmente, comprobamos el efecto del gen en el tiempo (Timexrs1264323) y interaccion por grupos

m7 = lme(log(tmta) ~ rs1264323 * Time + years_edu + SEXE + Edad + pacient_control
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + rs1264323|ID, data = BBDD_2, method = "ML")
anova(m7, type = "marginal")
anova(m6, m7)

m7.1 = lme(log(tmta) ~ rs1264323 * pacient_control + Time + years_edu + SEXE + Edad 
           + cannabis_rec + SANS + SAPS
           + CPZ + anys_evol_psico, random = ~1 + rs1264323|ID, data = BBDD_2, method = "ML")
anova(m7.1, type = "marginal")
anova(m6, m7.1)

summary(m7)

# Se descartan interacciones y finalmente nos quedamos con m6. 

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m6)
leveneTest(log(BBDD_2$tmta), BBDD_2$rs1264323)
qqnorm(resid(m6))
qqline(resid(m6))
hist(resid(m6))

# m6 cumple con los supuestos. 

coefm7 = round(data.frame((coef(summary(m6)))),3)
coefm7$names = rownames(coefm7)

export(coefm7, "coef_rs1264323.xlsx")

rm(coefm7, m0, m1, m2, m3, m3.1, m4, m5, m6, m7, m7.1)

# Modelos rs22399518:

m0 = gls(log(tmta) ~ rs22399518, data = BBDD_2) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ rs22399518, random = ~1 + rs22399518|ID, data = BBDD_2)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR. No obstante, se puede ver que la significancia varia considerablemente entre ambos modelos
anova.lme(m1, type = "marginal")

# Se procede a seguir con los MLR:

m1 = lme(log(tmta) ~ rs22399518, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
m2 = lme(log(tmta) ~ rs22399518 + pacient_control + Time + SEXE + Edad, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
summary(m2)

anova(m1, m2) # m2 mejora m1 significativamente. Se procede a valorar la importancia de la variable sexo:

m3 = lme(log(tmta) ~ rs22399518 + pacient_control + Time + SEXE + Edad + years_edu, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
anova(m3, type = "marginal")

anova(m2, m3) #m3 mejora m2. Referencia m3.

m4 = lme(log(tmta) ~ rs22399518 + pacient_control + Time + SEXE + Edad + years_edu
         + cannabis_rec, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
anova(m4, type = "marginal") 

anova( m3, m4) #M4 mejora m3. m4 referencia

m5 = lme(log(tmta) ~ rs22399518 + pacient_control + Time + SEXE + Edad + years_edu
         + cannabis_rec + SANS + SAPS, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
anova(m5, type = "marginal")

anova(m5, m4) #m5 mejora m4. Refencia m5.

m6 = lme(log(tmta) ~ rs22399518 + pacient_control + Time + SEXE + Edad + years_edu
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
anova(m6, type = "marginal")

anova(m6, m5) #m6 mejora m5

# Finalmente, comprobamos el efecto del gen en el tiempo (Timexrs22399518) y interaccion por grupos

m7 = lme(log(tmta) ~ rs22399518 + pacient_control + Time*rs22399518 + SEXE + Edad + years_edu
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
anova(m7, type = "marginal")
anova(m6, m7)

m7.1 = lme(log(tmta) ~ rs22399518 * pacient_control + Time + SEXE + Edad + years_edu 
           + cannabis_rec + SANS + SAPS
           + CPZ + anys_evol_psico, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
anova(m7.1, type = "marginal")
anova(m6, m7.1)

m7.2 = lme(log(tmta) ~ rs22399518 + pacient_control + Time + SEXE + Edad + years_edu*rs22399518 
           + cannabis_rec + SANS + SAPS
           + CPZ + anys_evol_psico, random = ~1 + rs22399518|ID, data = BBDD_2, method = "ML")
anova(m7.2, type = "marginal")
anova(m6, m7.2)

summary(m7)

# Se descartan interacciones y finalmente nos quedamos con m6. 

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m6)
leveneTest(log(BBDD_2$tmta), BBDD_2$rs22399518)
qqnorm(resid(m6))
qqline(resid(m6))
hist(resid(m6))

# m6 cumple con los supuestos. 

coefm7 = round(data.frame((coef(summary(m6)))),3)
coefm7$names = rownames(coefm7)

export(coefm7, "coef_rs22399518.xlsx")

rm(coefm7, m0, m1, m2, m3, m3.1, m4, m5, m6, m7, m7.1, m7.2)

# Modelos mixtos por cohorte:

BBDD_control = subset(BBDD_2, pacient_control == 0)
BBDD_paciente = subset(BBDD_2, pacient_control == 1)

ggplot(BBDD_control, aes(x = Time, y = tmta, group=ID, color = ID)) +
  geom_line(alpha=.3) + labs(title="Tmta values of participant Across follow-up") +
  theme(legend.position = "none")

ggplot(BBDD_paciente, aes(x = Time, y = tmta, group=ID, color = ID)) +
  geom_line(alpha=.3) + labs(title="Tmta values of participant Across follow-up") +
  theme(legend.position = "none")

# Control:

# Modelos HAPLO:

m0 = gls(log(tmta) ~ HAPLO, data = BBDD_control) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ HAPLO, random = ~1 + HAPLO|ID, data = BBDD_control)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR. No obstante, se puede ver que la significancia varia considerablemente
anova.lme(m1, type = "marginal")

# Se procede a seguir con los MLR:

m1 = lme(log(tmta) ~ HAPLO, random = ~1 + HAPLO|ID, data = BBDD_control, method = "ML")
m2 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad, random = ~1 + HAPLO|ID, data = BBDD_control, method = "ML")
summary(m2)

# Se observa que hay efectos significativos con la variable tiempo (disminucion de la puntuacion con el tiempo) y edad pero no parece que sexo
# mejore el modelo. Se compara con el modelo basal

anova(m1, m2) # m2 mejora m1 significativamente. Se procede a valorar la importancia de la variable sexo:

m2.1 = lme(log(tmta) ~ HAPLO + Time + Edad, random = ~1 + HAPLO|ID, data = BBDD_control, method = "ML")

anova(m2, m2.1) # no se observa una mejora significativa, se deja la variable sexo (posible inclusion por explicacion biologica)

m3 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad + years_edu, random = ~1 + HAPLO|ID, data = BBDD_control, method = "ML")
anova(m3, type = "marginal")

# Al añadir años de educacion, el efecto de HAPLO se pierde. Posible interacion?

m3.1 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad + years_edu*HAPLO, random = ~1 + HAPLO|ID, data = BBDD_control, method = "ML")
anova(m3.1, type = "marginal") 

anova(m2, m3, m3.1)

# No hay interaccion y la variable continua siendo significativa. M3 es mejor que m2, se mantiene m3 y se procede a añadir mas variables

m4 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad + years_edu*HAPLO
         + cannabis_rec, random = ~1 + HAPLO|ID, data = BBDD_control, method = "ML")
anova(m4, type = "marginal")

anova(m4, m3.1) # Se mantiene consumo de cannabis como dicotomica, mejora el modelo.

# Finalmente, comprobamos el efecto del gen en el tiempo (TimexHAPLO)

m7 = lme(log(tmta) ~ HAPLO + Time*HAPLO + SEXE + Edad + years_edu*HAPLO
         + cannabis_rec, random = ~1 + HAPLO|ID, data = BBDD_control, method = "ML")
anova(m7, type = "marginal")
anova(m4, m7)

summary(m4)

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m4)
leveneTest(log(BBDD_control$tmta), BBDD_control$HAPLO)
qqnorm(resid(m4))
qqline(resid(m4))
hist(resid(m4))

# m4 cumple con los supuestos. 

coefm7 = round(data.frame((coef(summary(m4)))),3)
coefm7$names = rownames(coefm7)

export(coefm7, "coef_HAPLO_cont.xlsx")

rm(coefm7, m0, m1, m2, m2.1, m3, m3.1, m4, m4.1, m7)

# Curiosamente, a diferencia de los pacientes, existe una interacción en los controles respecto a los años de educacion.
# Parece que ser HAPLO = 1 y cuantos más años, se relaciona con menor puntuacion pero su interaccion se relaciona 
# de forma positiva (aunque poco eficaz). Valoraremos la misma interaccion en los siguientes modelos.

# Modelos rs1264323:

m0 = gls(log(tmta) ~ rs1264323, data = BBDD_control) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ rs1264323, random = ~1 + rs1264323|ID, data = BBDD_control)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR.
anova.lme(m1, type = "marginal") 
# No se observa que haya efectos significativos del gen sobre tmtA. Se procede a realizar el análisis con cada variable
# por si hay interaccion y se acabará con el modelo más ajustado como el m3.1 de HAPLO.

m2 = lme(log(tmta) ~ rs1264323 * Time + SEXE*rs1264323 + Edad*rs1264323, random = ~1 + rs1264323|ID, data = BBDD_control, method = "ML")
summary(m2)
m2.1 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad, random = ~1 + rs1264323|ID, data = BBDD_control, method = "ML")
summary(m2.1)

m3 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu*rs1264323, random = ~1 + rs1264323|ID, data = BBDD_control, method = "ML")
anova(m3, type = "marginal")
m3.1 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu, random = ~1 + rs1264323|ID, data = BBDD_control, method = "ML")
anova(m3.1, type = "marginal")

anova(m2.1, m3.1) # m3.1 mejora m2.1, se utiliza como referencia. No hay efecto interaccion con este gen de años de educacion.

m4 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu
         + cannabis_rec*rs1264323, random = ~1 + rs1264323|ID, data = BBDD_control, method = "ML")
anova(m4, type = "marginal") 

m4.1 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu
           + cannabis_rec, random = ~1 + rs1264323|ID, data = BBDD_control, method = "ML")
anova(m4, m4.1)
anova(m4.1, m3.1) # m4.1 mejora m3.1. Referencia m4.1

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m4.1)
leveneTest(log(BBDD_control$tmta), BBDD_control$rs1264323)
qqnorm(resid(m4.1))
qqline(resid(m4.1))
hist(resid(m4.1))

coefm6 = round(data.frame((coef(summary(m4.1)))),3)
coefm6$names = rownames(coefm6)

export(coefm6, "coef_rs1264323_cont.xlsx")

rm(coefm6, m0, m1, m2, m2.1, m3, m3.1, m4, m4.1)

# Modelos rs22399518:

m0 = gls(log(tmta) ~ rs22399518, data = BBDD_control) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ rs22399518, random = ~1 + rs22399518|ID, data = BBDD_control)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR.
anova.lme(m1, type = "marginal") 
# Parece haber una cierta tendencia de efecto significativo.
m1 = lme(log(tmta) ~ rs22399518, random = ~1 + rs22399518|ID, data = BBDD_control, method = "ML")
m2 = lme(log(tmta) ~ rs22399518 * Time + SEXE*rs22399518 + Edad*rs22399518, random = ~1 + rs22399518|ID, data = BBDD_control, method = "ML")
summary(m2) #No hay efecto interaccion
m2.1 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad, random = ~1 + rs22399518|ID, data = BBDD_control, method = "ML")
summary(m2.1)

anova(m1, m2.1) #m2.1 mejora m1, referencia m2.1.

m3 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu*rs22399518, random = ~1 + rs22399518|ID, data = BBDD_control, method = "ML")
anova(m3, type = "marginal")
m3.1 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu, random = ~1 + rs22399518|ID, data = BBDD_control, method = "ML")
anova(m3.1, type = "marginal")

anova(m2.1, m3) # m3.1 mejora m2, se utiliza como referencia. OJO, al igual que en HAPLO, hay efecto interaccion con años de educacion

m4 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu*rs22399518
         + cannabis_rec*rs22399518, random = ~1 + rs22399518|ID, data = BBDD_control, method = "ML")
anova(m4, type = "marginal") 

m4.1 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu*rs22399518
           + cannabis_rec, random = ~1 + rs22399518|ID, data = BBDD_control, method = "ML")
anova(m4.1, type = "marginal") 
anova(m4, m4.1) 
anova(m4.1, m3.1) 

# Nos quedamos con m4.1 para facilitar la explicacion biologica, pero la inclusion de cannabis no supone una mejora en el modelo.

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m4.1)
leveneTest(log(BBDD_control$tmta), BBDD_control$rs22399518)
qqnorm(resid(m4.1))
qqline(resid(m4.1))
hist(resid(m4.1))

coefm6 = round(data.frame((coef(summary(m4.1)))),3)
coefm6$names = rownames(coefm6)

export(coefm6, "coef_rs22399518_cont.xlsx")

rm(coefm6, m0, m1, m2, m2.1, m3, m3.1, m4, m4.1)

# Las conclusiones tras estos modelos son: HAPLO y rs22399518 parecen tener un efecto opuesto sobre la puntuacion del tmtA
# cuando se tienen en cuenta las interacciones. Secundariamente, no parece haber un perfil de interacciones igual a
# cada variable. 

# Paciente:

# Modelos HAPLO:

m0 = gls(log(tmta) ~ HAPLO, data = BBDD_paciente) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ HAPLO, random = ~1 + HAPLO|ID, data = BBDD_paciente)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR. No obstante, se puede ver que la significancia varia considerablemente
anova.lme(m1, type = "marginal")

# Se procede a seguir con los MLR:

m1 = lme(log(tmta) ~ HAPLO, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")
m2 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")
summary(m2)

anova(m1, m2) # m2 mejora m1 significativamente. Se procede a valorar la importancia de la variable sexo:

m2.1 = lme(log(tmta) ~ HAPLO + Time + Edad, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")

anova(m2, m2.1) # no se observa una mejora significativa, se deja la variable sexo (posible inclusion por explicacion biologica)

m3 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad + years_edu, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")
anova(m3, type = "marginal")

anova(m2, m3) # m3 mejora m2

m4 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad + years_edu
         + porro + cannabis_rec, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")
anova(m4, type = "marginal") 

m4.1 = lme(log(tmta) ~ HAPLO + years_edu + Time + SEXE + Edad
           + cannabis_rec, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")

anova(m4, m4.1) # Se decide no incluir porro en el modelo.

anova(m4.1, m3) # Se mantiene consumo de cannabis como dicotomica, mejora el modelo.

m5 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad + years_edu
         + cannabis_rec + SANS + SAPS, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")
anova(m5, type = "marginal")

anova(m5, m4.1) #m5 mejora m4.1. Referencia m5

m6 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad + years_edu
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")
anova(m6, type = "marginal")
m6.1 = lme(log(tmta) ~ HAPLO + Time + SEXE + Edad + years_edu
           + cannabis_rec + SANS
           + CPZ, random = ~1|ID, data = BBDD_paciente, method = "ML")
anova(m6.1, type = "marginal")

anova(m6, m5, m6.1) # Aunque m6.1 mejora el modelo mas que m6, nos quedamos con m6 para mantener los mismos ajustes que los otros modelos.

# Finalmente, comprobamos el efecto del gen en el tiempo (TimexHAPLO)

m7 = lme(log(tmta) ~ HAPLO * Time + SEXE + Edad + years_edu
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + HAPLO|ID, data = BBDD_paciente, method = "ML")
anova(m7, type = "marginal")
anova(m6, m7)

summary(m6)
summary(m7)

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m6)
leveneTest(log(BBDD_paciente$tmta), BBDD_paciente$HAPLO)
qqnorm(resid(m6))
qqline(resid(m6))
hist(resid(m6))

# m6 cumple con los supuestos. 

coefm7 = round(data.frame((coef(summary(m6)))),3)
coefm7$names = rownames(coefm7)

export(coefm7, "coef_HAPLO_pac.xlsx")

rm(coefm7, m0, m1, m2, m2.1, m3, m3.1, m4, m4.1, m5, m5.1, m6, m6.1, m7)

# Modelos rs1264323:

m0 = gls(log(tmta) ~ rs1264323, data = BBDD_paciente) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ rs1264323, random = ~1 + rs1264323|ID, data = BBDD_paciente)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR.
anova.lme(m1, type = "marginal") 
# No se observa que haya efectos significativos del gen sobre tmtA. Se procede a realizar el análisis con cada variable
# por si hay interaccion y se acabará con el modelo más ajustado como el m7 de HAPLO.

m2 = lme(log(tmta) ~ rs1264323 * Time + SEXE*rs1264323 + Edad*rs1264323, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
summary(m2)
m2.1 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
summary(m2.1)

m3 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu*rs1264323, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m3, type = "marginal")
m3.1 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m3.1, type = "marginal")

anova(m2, m3.1) # m3.1 mejora m2, se utiliza como referencia.

m4 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu
         + cannabis_rec*rs1264323, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m4, type = "marginal") 

m4.1 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu
           + cannabis_rec, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m4, m4.1)
anova(m4.1, m3.1) # m4.1 mejora m3.1. Referencia m4.1

m5 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu
         + cannabis_rec + SANS*rs1264323 + SAPS*rs1264323, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m5, type = "marginal")

m5.1 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu
           + cannabis_rec + SANS + SAPS, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m5.1, type = "marginal")

anova(m5, m4.1, m5.1) #m5.1 mejora m4.1. Referencia m5.1


m6 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu
         + cannabis_rec + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m6, type = "marginal")
m6.1 = lme(log(tmta) ~ rs1264323 + Time + SEXE + Edad + years_edu
           + cannabis_rec + SANS + SAPS
           + CPZ*rs1264323 + anys_evol_psico*rs1264323, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m6.1, type = "marginal")

m6.2 = lme(log(tmta) ~ rs1264323 * Time + SEXE + Edad + years_edu
           + cannabis_rec + SANS + SAPS
           + CPZ + anys_evol_psico, random = ~1 + rs1264323|ID, data = BBDD_paciente, method = "ML")
anova(m6.2, type = "marginal")

anova(m6, m5.1) #m6 mejora ligeramente m5.1. Modelo final, m6.

anova(m6, m6.2) #Nos quedamos con m6 para mantener los mismos ajustes que el modelo de HAPLO.

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m6)
leveneTest(log(BBDD_paciente$tmta), BBDD_paciente$rs1264323)
qqnorm(resid(m6))
qqline(resid(m6))
hist(resid(m6))

# m6 cumple con los supuestos. 

coefm6 = round(data.frame((coef(summary(m6)))),3)
coefm6$names = rownames(coefm6)

export(coefm6, "coef_rs1264323_pac.xlsx")

rm(coefm6, m0, m1, m2, m2.1, m3, m3.1, m4, m4.2, m4.1, m5, m5.1, m6, m6.1, m6.2)

# Modelos rs22399518:

m0 = gls(log(tmta) ~ rs22399518, data = BBDD_paciente) #modelo lineal
summary(m0)

#m1 = modelo mixto basal. Se va a considerar que cada participante es un componente aleatorio. Asi mismo, la determinacion genetica se hizo a
# posteriori, por lo que se considera aleatorio que un participante tenga o no el gen. Como es dependiente de la persona, se anida dentro de ID.
m1 = lme(log(tmta) ~ rs22399518, random = ~1 + rs22399518|ID, data = BBDD_paciente)
summary(m1)
anova(m0, m1) #Se observa un menor error con MLR que con LR.
anova.lme(m1, type = "marginal") 
# Parece haber una cierta tendencia de efecto significativo.
m1 = lme(log(tmta) ~ rs22399518, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
m2 = lme(log(tmta) ~ rs22399518 * Time + SEXE*rs22399518 + Edad*rs22399518, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
summary(m2) #No hay efecto interaccion
m2.1 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
summary(m2.1)

anova(m1, m2.1) #m2.1 mejora m1, referencia m2.1.

m3 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu*rs22399518, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m3, type = "marginal")
m3.1 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m3.1, type = "marginal")

anova(m2, m3.1) # m3.1 mejora m2, se utiliza como referencia. OJO, se pierde la tendencia a significacion

m4 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu
         + cannabis_rec*rs22399518, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m4, type = "marginal") 

m4.1 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu
           + cannabis_rec, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m4.1, type = "marginal")
anova(m4, m4.1)
anova(m4.1, m3.1) 
anova(m4, m3.1) # m4 mejora m3.1. Referencia m4.

m5 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu
         + cannabis_rec*rs22399518 + SANS*rs22399518 + SAPS*rs22399518, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m5, type = "marginal")

m5.1 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu
           + cannabis_rec*rs22399518 + SANS + SAPS, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m5.1, type = "marginal")

anova(m5, m4, m5.1) #m5.1 mejora m4. Referencia m5.1


m6 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu
         + cannabis_rec*rs22399518 + SANS + SAPS
         + CPZ + anys_evol_psico, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m6, type = "marginal")
m6.1 = lme(log(tmta) ~ rs22399518 + Time + SEXE + Edad + years_edu
           + cannabis_rec*rs22399518 + SANS + SAPS
           + CPZ*rs22399518 + anys_evol_psico*rs22399518, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m6.1, type = "marginal")

m6.2 = lme(log(tmta) ~ rs22399518 * Time + SEXE + Edad + years_edu
           + cannabis_rec*rs22399518 + SANS + SAPS
           + CPZ + anys_evol_psico, random = ~1 + rs22399518|ID, data = BBDD_paciente, method = "ML")
anova(m6.2, type = "marginal")

anova(m6, m5.1) #m6 mejora ligeramente m5.1. Modelo final, m6.

anova(m6, m6.2) 

# Comprobacion de supuestos de homocedasticidad y distribucion normal de los residuos

plot(m6)
leveneTest(log(BBDD_paciente$tmta), BBDD_paciente$rs22399518)
qqnorm(resid(m6))
qqline(resid(m6))
hist(resid(m6))

# m6.2 cumple con los supuestos. 

coefm6 = round(data.frame((coef(summary(m6)))),3)
coefm6$names = rownames(coefm6)

export(coefm6, "coef_rs22399518_pac.xlsx")

# Tras analizar los resultados en los pacientes, parece que el único modelo con efectos significativos es en HAPLO y una tendencia con rs22399518
# posiblemente asociada a una interaccion en con el consumo de cannabis