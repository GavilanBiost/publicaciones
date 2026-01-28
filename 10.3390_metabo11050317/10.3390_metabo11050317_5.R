###Prediccion a partir de los valores obtenidos en V0, de los valores que se pueden obtener en v8 y v14.
###es decir, validacion semi-externa del modelo generado

##Se cargan los modelos en V0:

modelos = read_excel("modelos.xlsx", col_types = c("text","text","numeric", "numeric", "numeric"))
mo.MM.v8 = subset(modelos, MM.v0 > -10, select = c("metabolitos...1","MM.v0"))
mo.MO.v8 = subset(modelos, MO.v0 > -10, select = c("metabolitos...1","MO.v0"))
mo.MG.v8 = subset(modelos, MG.v0 > -10, select = c("metabolitos...1","MG.v0"))

mo.MM.v14 = subset(modelos, MM.v0 > -10, select = c("metabolitos...2","MM.v0"))
mo.MO.v14 = subset(modelos, MO.v0 > -10, select = c("metabolitos...2","MO.v0"))
mo.MG.v14 = subset(modelos, MG.v0 > -10, select = c("metabolitos...2","MG.v0"))

###BBDD de validación:
##MM
load("/Volumes/JES GAVI/ARTICULOS/Autor/Satin/ART huella composicion corporal/Statistic/MMV8.rankNorm.06052020.rda")
load("/Volumes/JES GAVI/ARTICULOS/Autor/Satin/ART huella composicion corporal/Statistic/MMV14.rankNorm.06052020.rda")

##MO
load("/Volumes/JES GAVI/ARTICULOS/Autor/Satin/ART huella composicion corporal/Statistic/MOV8.rankNorm.06052020.rda")
load("/Volumes/JES GAVI/ARTICULOS/Autor/Satin/ART huella composicion corporal/Statistic/MOV14.rankNorm.06052020.rda")

##MG
load("/Volumes/JES GAVI/ARTICULOS/Autor/Satin/ART huella composicion corporal/Statistic/MGV8.rankNorm.06052020.rda")
load("/Volumes/JES GAVI/ARTICULOS/Autor/Satin/ART huella composicion corporal/Statistic/MGV14.rankNorm.06052020.rda")

###Validación V8:

X2 = data.frame(MG.rankNorm[,2:(dim(MG.rankNorm)[2])])
Y2 = MG.rankNorm$v14_fat_mass
b = mo.MG.v14

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
  
  correr = cor.test(Y2,modelo, conf.level = .95)
  
} 

##Correlaciones entre body mass y body fat en v2:

cor.test(V0$v2_lean_mass, V0$v2_fat_mass)

###Distibución de la edad en las mujeres:

Table1.1 = subset(Table1, v1_gq_1==1)
hist(Table1.1$v1_gq_11)

T1.1 = subset(Table1.1, v1_gq_11 >= 30)
T1.1 = subset(Table1.1, v1_gq_11 <= 50)

###Cambios en bone mass entre v2 y v14:

wilcox.test(Table1$v2_bone_mass, Table1$v8_bone_mass, paired = F)
