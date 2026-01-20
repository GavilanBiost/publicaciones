#Apertura de BBDD desde SPSS.
library(haven)
library("rms")
library("splines")
library("ggplot2")
library("survival")
library("Hmisc")
library(car)
library(xlsx)
PREDIMED <- read_sav("/Volumes/JES GAVI/ARTICULOS/ART Pral - fractures (working)/Estadística/SPSS/PREDIMED Base de datos limpia (noviembre 2015) ART.SAV")
View(PREDIMED)

#Segmentacion por sexo (mujer = 1, hombre = 0)
PREDIMED$sexo
table(PREDIMED$sexo)
str(PREDIMED$sexo)
PREDDona <- subset(PREDIMED, sexo == "1")
PREDHome <- subset(PREDIMED, sexo == "0")

table(PREDIMED$sexo, PREDIMED$fract_osteo_new)

#Terciles de Pral.
PREDIMED$ter_PRAL <- cut2(PREDIMED$media_PRAL,g = 3)
PREDDona$ter_PRAL <- cut2(PREDDona$media_PRAL,g = 3)
PREDHome$ter_PRAL <- cut2(PREDHome$media_PRAL,g = 3)

PREDIMED$ter1_PRAL <- Recode(PREDIMED$ter_PRAL, "'[-28.98,-2.43)' = '2'; '[ -2.43, 4.63)' =' 1'; '[  4.63,28.98]' = '3'")
PREDDona$ter1_PRAL <- Recode(PREDDona$ter_PRAL, "'[-27.55,-2.58)' = '2'; '[ -2.58, 3.90)' =' 1'; '[  3.90,21.22]' = '3'")
PREDHome$ter1_PRAL <- Recode(PREDHome$ter_PRAL, "'[-28.98,-2.17)' = '2'; '[ -2.17, 5.11)' =' 1'; '[  5.11,28.98]' = '3'")

#Inicio del analisis.
#Inicialmente se va a comprobar que h0 = las funciones no son distintas.
#Kaplan Meier
km1 <- survfit(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter1_PRAL)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter1_PRAL)
#No se puede rechazar h0. 

#Kaplan Meier
km1 <- survfit(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter1_PRAL)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter1_PRAL)
#No se puede rechazar h0. 

#Kaplan Meier
km1 <- survfit(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter1_PRAL)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter1_PRAL)
#No se puede rechazar h0. 

#Realizacion cox basal
a <- cph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$media_PRAL, x=T, y=T)
ab <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$media_PRAL)

rcspline.plot(PREDIMED$media_PRAL, summary(ab)$coef, model = "cox", event=PREDIMED$fract_osteo_new, nk = 3, xlab = "PRAL", plotcl = T, showknots = F,
              statloc = T, ylim = c(-2,2))#Grafico dosis-respuesta pral - log relative hazard
termplot(a, data = PREDIMED, se=T, rug = T, partial.resid = T)


a <- cph(Surv(STATA$PREDIMED.T_cox_new, STATA$PREDIMED.fract_osteo_new)~rcs(STATA$PREDIMED.media_PRAL,3), x=T, y=T)

ddist<-datadist(STATA)
ddist$limits$PREDIMED.media_PRAL[1:7]<- 1.1
options(datadist = "ddist")
termplot2(a, se = T, rug.type = "density", rug = T, density.proportion = .05, se.type = "polygon", yscale = "exponential",
          log = "y")

a11 <- cph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~(PREDIMED$ter1_PRAL))
summary(a11)
b <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter1_PRAL)
summary(b)

c <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter1_PRAL)
summary(c)

#Mas sexo, edad, estado civil, tabaco, imc.
a1 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter1_PRAL + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1)
summary(a1)

b1 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter1_PRAL + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1)
summary(b1)

c1 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter1_PRAL + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1)
summary(c1)

#Mas enfermedades y medicamentos.
a2 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter1_PRAL + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1)
summary(a2)

b2 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter1_PRAL + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1
            + PREDDona$diabetes0 + PREDDona$hormo1 + PREDDona$insulin1 + PREDDona$vitamin1 + PREDDona$m_estatin + PREDDona$fractura1)
summary(b2)

c2 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter1_PRAL + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1
            + PREDHome$diabetes0 + PREDHome$hormo1 + PREDHome$insulin1 + PREDHome$vitamin1 + PREDHome$m_estatin + PREDHome$fractura1)
summary(c2)

#Mas alimentos/p14.
a3 <- cph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter1_PRAL + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$media_ENERGIA + PREDIMED$p14_v1 + PREDIMED$media_alcohol)
summary(a3)

a4 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~ I(PREDIMED$media_PRAL^2) + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$media_ENERGIA + PREDIMED$p14_v1 + PREDIMED$media_alcohol)
coxph.fit(PREDIMED$media_PRAL,Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new), strata = 3, method = "efron")
summary(a4)

b3 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter1_PRAL + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1
            + PREDDona$diabetes0 + PREDDona$hormo1 + PREDDona$insulin1 + PREDDona$vitamin1 + PREDDona$m_estatin + PREDDona$fractura1)
summary(b3)

c3 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter1_PRAL + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1
            + PREDHome$diabetes0 + PREDHome$hormo1 + PREDHome$insulin1 + PREDHome$vitamin1 + PREDHome$m_estatin + PREDHome$fractura1)
summary(c3)

#Creacion de excel con los datos utilizados.

STATA <- data.frame(PREDIMED$T_cox_new,PREDIMED$fract_osteo_new,PREDIMED$ter1_PRAL, PREDIMED$media_PRAL, PREDIMED$sexo, PREDIMED$edad0, PREDIMED$est_civi1, 
                    PREDIMED$fum, PREDIMED$imc1, PREDIMED$diabetes0, PREDIMED$hormo1, PREDIMED$insulin1, PREDIMED$vitamin1, PREDIMED$m_estatin, PREDIMED$fractura1,
                    PREDIMED$media_ENERGIA,PREDIMED$p14_v1,PREDIMED$media_alcohol)

write.xlsx(STATA,"/Volumes/JES GAVI/ARTICULOS/ART Pral - fractures (working)/Estadística/R/STATA.xlsx", sheetName="STATA", col.names=T)
