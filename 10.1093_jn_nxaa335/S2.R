#Subanalisis fosforo - fracturas.
PREDIMED$media_fosforo
PREDIMED$RatioCaP

#Terciles de fosforo.
library("Hmisc")
PREDIMED$ter_fosforo <- cut2(PREDIMED$media_fosforo,g = 3)
PREDDona$ter_fosforo <- cut2(PREDDona$media_fosforo,g = 3)
PREDHome$ter_fosforo <- cut2(PREDHome$media_fosforo,g = 3)

table(PREDIMED$ter_fosforo, PREDIMED$fract_osteo_new)
#Inicio del analisis.
#Inicialmente se va a comprobar que h0 = las funciones no son distintas.
library("survival")
#Kaplan Meier
km1 <- survfit(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_fosforo)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_fosforo)
#No se puede rechazar h0. 

#Kaplan Meier
km1 <- survfit(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_fosforo)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_fosforo)
#No se puede rechazar h0. 

#Kaplan Meier
km1 <- survfit(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_fosforo)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_fosforo)
#No se puede rechazar h0. 

#Realizacion cox basal
a <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_fosforo)
summary(a)
plot(residuals(a))
plot(survfit(a))
cox.zph(a)
plot(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new))

b <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_fosforo)
summary(b)

c <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_fosforo)
summary(c)

#Mas sexo, edad, estado civil, tabaco, imc.
a1 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_fosforo + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1)
summary(a1)

b1 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_fosforo + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1)
summary(b1)

c1 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_fosforo + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1)
summary(c1)

#Mas enfermedades y medicamentos.
a2 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_fosforo + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1)
summary(a2)

b2 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_fosforo + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1
            + PREDDona$diabetes0 + PREDDona$hormo1 + PREDDona$insulin1 + PREDDona$vitamin1 + PREDDona$m_estatin + PREDDona$fractura1)
summary(b2)

c2 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_fosforo + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1
            + PREDHome$diabetes0 + PREDHome$hormo1 + PREDHome$insulin1 + PREDHome$vitamin1 + PREDHome$m_estatin + PREDHome$fractura1)
summary(c2)

#Mas alimentos/p14.
a3 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_fosforo + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$energiat + PREDIMED$media_fibr + PREDIMED$media_alcohol + PREDIMED$media_getot)
summary(a3)
plot(a3)

a4 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$media_fosforo + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$energiat + PREDIMED$media_fibr + PREDIMED$media_alcohol + PREDIMED$media_getot)
summary(a4)

b3 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_fosforo + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1
            + PREDDona$diabetes0 + PREDDona$hormo1 + PREDDona$insulin1 + PREDDona$vitamin1 + PREDDona$m_estatin + PREDDona$fractura1 + PREDDona$energiat)
summary(b3)

c3 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_fosforo + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1
            + PREDHome$diabetes0 + PREDHome$hormo1 + PREDHome$insulin1 + PREDHome$vitamin1 + PREDHome$m_estatin + PREDHome$fractura1 + PREDHome$energiat)
summary(c3)

#Analisis ratio ca:P
#Terciles de Pral.
library("Hmisc")
PREDIMED$ter_CaP <- cut2(PREDIMED$RatioCaP,g = 3)
PREDDona$ter_CaP <- cut2(PREDDona$RatioCaP,g = 3)
PREDHome$ter_CaP <- cut2(PREDHome$RatioCaP,g = 3)

#Inicio del analisis.
#Inicialmente se va a comprobar que h0 = las funciones no son distintas.
library("survival")
#Kaplan Meier
km1 <- survfit(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_CaP)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_CaP)
#No se puede rechazar h0. 

#Kaplan Meier
km1 <- survfit(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_CaP)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_CaP)
#No se puede rechazar h0. 

#Kaplan Meier
km1 <- survfit(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_CaP)
summary(km1)
plot(km1, xlab="Años", ylab = "Supervivencia", main = "Grafico no. 1. Estimador de Kaplan y Meier para terciles de Pral",
     lty = c(1,2,3), mark.time = F) #Grafico del estimador kaplan-meier
survdiff(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_CaP)
#No se puede rechazar h0. 

#Realizacion cox basal
a <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_CaP)
summary(a)
plot(residuals(a))
plot(survfit(a))
cox.zph(a)
plot(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new))

b <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_CaP)
summary(b)

c <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_CaP)
summary(c)

#Mas sexo, edad, estado civil, tabaco, imc.
a1 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_CaP + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1)
summary(a1)

b1 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_CaP + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1)
summary(b1)

c1 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_CaP + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1)
summary(c1)

#Mas enfermedades y medicamentos.
a2 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_CaP + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1)
summary(a2)

b2 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_CaP + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1
            + PREDDona$diabetes0 + PREDDona$hormo1 + PREDDona$insulin1 + PREDDona$vitamin1 + PREDDona$m_estatin + PREDDona$fractura1)
summary(b2)

c2 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_CaP + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1
            + PREDHome$diabetes0 + PREDHome$hormo1 + PREDHome$insulin1 + PREDHome$vitamin1 + PREDHome$m_estatin + PREDHome$fractura1)
summary(c2)

#Mas alimentos/p14.
a3 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_CaP + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$energiat)
summary(a3)

b3 <- coxph(Surv(PREDDona$T_cox_new, PREDDona$fract_osteo_new)~PREDDona$ter_CaP + PREDDona$edad0 + PREDDona$est_civi1 + PREDDona$fum + PREDDona$imc1
            + PREDDona$diabetes0 + PREDDona$hormo1 + PREDDona$insulin1 + PREDDona$vitamin1 + PREDDona$m_estatin + PREDDona$fractura1 + PREDDona$energiat)
summary(b3)

c3 <- coxph(Surv(PREDHome$T_cox_new, PREDHome$fract_osteo_new)~PREDHome$ter_CaP + PREDHome$edad0 + PREDHome$est_civi1 + PREDHome$fum + PREDHome$imc1
            + PREDHome$diabetes0 + PREDHome$hormo1 + PREDHome$insulin1 + PREDHome$vitamin1 + PREDHome$m_estatin + PREDHome$fractura1 + PREDHome$energiat)
summary(c3)

#Opcion 2: ratio fosforo/calcio

PREDIMED$RFosCa <- PREDIMED$media_fosforo/PREDIMED$media_calci
PREDIMED$ter_RFosCa <- cut2(PREDIMED$RFosCa,g = 3)
a <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_RFosCa)
summary(a)
a1 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_RFosCa + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1)
summary(a1)
a2 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_RFosCa + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1)
summary(a2)
a3 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_RFosCa + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$energiat)
summary(a3)

#Opcion 3: proteinas
PREDIMED$ter_PROT <- cut2(PREDIMED$media_PROTEINA,g = 3)
a <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_PROT)
summary(a)
a1 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_PROT + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1)
summary(a1)
a2 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_PROT + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1)
summary(a2)
a3 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_PROT + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$energiat)
summary(a3)

#Opcion 4: ingesta de fosforo/kcal
PREDIMED$P_Kcal <- (PREDIMED$media_fosforo/PREDIMED$media_ENERGIA)*1000
PREDIMED$ter_P_Kcal <- cut2(PREDIMED$P_Kcal,g = 3)
a <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_P_Kcal)
summary(a)
a1 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_P_Kcal + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1)
summary(a1)
a2 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_P_Kcal + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1)
summary(a2)
a3 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_P_Kcal + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$media_HC)
summary(a3)

#Opcion 5: calcio
PREDIMED$ter_calcio <- cut2(PREDIMED$media_calci,g = 3)
a <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_calcio)
summary(a)
a1 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_calcio + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1)
summary(a1)
a2 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_calcio + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1)
summary(a2)
a3 <- coxph(Surv(PREDIMED$T_cox_new, PREDIMED$fract_osteo_new)~PREDIMED$ter_calcio + PREDIMED$sexo + PREDIMED$edad0 + PREDIMED$est_civi1 + PREDIMED$fum + PREDIMED$imc1
            + PREDIMED$diabetes0 + PREDIMED$hormo1 + PREDIMED$insulin1 + PREDIMED$vitamin1 + PREDIMED$m_estatin + PREDIMED$fractura1
            + PREDIMED$energiat)
summary(a3)
