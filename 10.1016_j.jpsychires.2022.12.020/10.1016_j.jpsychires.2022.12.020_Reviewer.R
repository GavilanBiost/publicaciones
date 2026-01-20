library(nlme)

BBDD <- read_excel("BD_Santander_Transpuesta.xlsx")

BBDD_control = subset(BBDD, patient_control == 0)
BBDD_patient = subset(BBDD, patient_control == 1)

# Control analysis:

m1 = lme(log(tmta) ~ Genotype * Education + Time + Sex + Age
           + Cannabis, random = ~1 + Genotype|ID, data = BBDD_control, method = "ML")
summary(m1)

m1.1 = lme(log(tmta) ~ Genotype * Education + Genotype * Time + Sex + Age
         + Cannabis, random = ~1 + Genotype|ID, data = BBDD_control, method = "ML")
summary(m1.1)
anova(m1.1, m1)

# Patient analysis:

m2 = lme(log(tmta) ~ Genotype + Time + Sex + Age + Education
         + Cannabis + SANS + SAPS + CPZ + years_of_disease, random = ~1 + Genotype|ID, data = BBDD_patient, method = "ML")
summary(m2)

m2.1 = lme(log(tmta) ~ Genotype * Time + Sex + Age + Education
         + Cannabis + SANS + SAPS + CPZ + years_of_disease, random = ~1 + Genotype|ID, data = BBDD_patient, method = "ML")
summary(m2.1)
anova(m2.1, m2)
