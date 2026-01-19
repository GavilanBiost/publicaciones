* Asistente de fecha y hora: T_cox_jesus (creación de la variable numérica) Muestra los años que pasan hasta el diagnóstico de la fractura y durante todo el seguimiento.
*Creacion de la variable T_cox_nueva -> años que pasan hasta que aparece fractura osteo según nueva definicion.
COMPUTE  T_cox_fract=(fract1_data - datinclu) / (365.25 * time.days(1)).
VARIABLE LABELS  T_cox_fract   "años que pasan hasta el diagnóstico de fractura"+
    "durante todo el seguimento".
VARIABLE LEVEL  T_cox_fract (SCALE).
FORMATS  T_cox_fract (F8.2).
VARIABLE WIDTH  T_cox_fract(8).
EXECUTE.
*Creacion variable de seguimiento hasta final de estudio.
COMPUTE  T_cox_seg=(data_fin-datinclu) / (365.25 * time.days(1)).
VARIABLE LABELS  T_cox_seg  "años de seguimento hasta fin de estudio".
VARIABLE LEVEL T_cox_seg (SCALE).
FORMATS  T_cox_seg(F8.2).
VARIABLE WIDTH  T_cox_seg(8).
EXECUTE.
*Creacion de variable seguimiento hasta fractura 2.
COMPUTE  T_cox_fract2=(fract2_data - datinclu) / (365.25 * time.days(1)).
VARIABLE LABELS  T_cox_fract2   "años que pasan hasta el diagnóstico de 2º fractura"+
    "durante todo el seguimento".
VARIABLE LEVEL  T_cox_fract2 (SCALE).
FORMATS  T_cox_fract2 (F8.2).
VARIABLE WIDTH  T_cox_fract2(8).
EXECUTE.
*Creacion de la variable T_cox_clas -> años que pasan hasta que aparece fractura osteo según definicion clasica teniendo en cuenta fractura 1 y fractura 2.
If fract1 =1 AND fract1_osteo =1  T_cox_clas=T_cox_fract.
If fract1 =1 AND fract1_osteo =0 OR fract1= 0 OR fract2=1 AND fract2_osteo=0 T_cox_clas=T_cox_seg.
if fract1=1 AND fract1_osteo=0 AND fract2=1 AND fract2_osteo=1 T_cox_clas=T_cox_fract2.
VARIABLE LABELS  T_cox_clas   "años que pasan hasta el diagnóstico de fractura osteo definicion clasica"+ "durante todo el seguimento".
EXECUTE.
*regresion supervivencia con t_cox_clas. 
COXREG T_cox_clas
  /STATUS= fract_osteo(1)
/METHOD=ENTER NTI002  imc1  getota_1 edad0 fractura1
  /CONTRAST (NTI002)=Indicator
  /METHOD=ENTER NTI002 
 /PATTERN BY NTI002
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
*percentil 50 ingesta de aceite de oliva virgen.
RANK VARIABLES= media_fsecos_mod (A)
  /RANK
  /NTILES(2)
  /PRINT=YES
  /TIES=MEAN.
**Visita final de seguimiento en el caso de las personas que desarrollan evento1
Ejemplo: si el evento se desarrolla entre el inicio y el primer año, se le asocia visita 3 para después tener en cuenta que solo se tiene que considerar la primera visita V1 y nunca la V3
Si el evento se desarrolla entre el primer año y el segundo año, el valor asociado es 4, de modo que sólo contarán la V1 y la V3.
*recodificacion de la variable T_cox_clas en número enteros.
RECODE T_cox_clas (0 thru 1=3) (1 thru 2=4) (2 thru 3=5) (3 thru 4=6) (4
    thru 5=7) (5 thru 6=8) (6 thru 7=9) (7 thru Highest=10) INTO v_final_evento1.
VARIABLE LABELS  v_final_evento1 'visita final de seguimiento o hasta aparición de evento fractura osteo'.
EXECUTE.
*Calculo de media de ingesta en funcion de aparicion del evento o final de estudio.
COMPUTE media_fsecos_mod = fsecos_mod. 
IF ((v_final_evento1=10)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7, fsecos_mod8, fsecos_mod9, fsecos_mod10).
IF ((v_final_evento1=9)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7, fsecos_mod8).
IF ((v_final_evento1=8)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7).
IF ((v_final_evento1=7)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6).
IF ((v_final_evento1=6)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5).
IF ((v_final_evento1=5)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4).
IF ((v_final_evento1=4)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3).
IF ((v_final_evento1=3)) media_fsecos_mod = mean(fsecos_mod).
VARIABLE LABELS media_fsecos_mod 'media de ingesta de aceite de oliva durante el estudio'.
EXECUTE.
*filtrado de sexo.
USE ALL. 
COMPUTE filter_$=(sexo = 0). 
VARIABLE LABELS filter_$ 'sexo = 0 (FILTER)'. 
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'. 
FORMATS filter_$ (f1.0). 
FILTER BY filter_$. 
EXECUTE.
*quita filtros.
filter off.
***__________________________________________INICIO AJUSTE (ajustar variables)_______________________
*En primer lugar se calcula la media de la variable de estudio - 
**Energía.
DESCRIPTIVES VARIABLES=energiat energiat3 energiat4 energiat5 energiat6 energiat7 energiat8 energiat9 energiat10
  /STATISTICS=MEAN STDDEV MIN MAX.
**Generamos un Paciente que tenga como valor de: 
**energiat = media de la población: 2394,1925	
**SE EJECUTA LA REGRESIÓN CON VARIABLE DEPENDIENTE (DELTA DE energía, en este caso ac_olivarvir) Y CON LA VARIABLE DE AJUSTE (BASAL)
**RECUERDA QUE SE EJECUTA UNA REGRESIÓN POR CADA PERIODO.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod
  /METHOD=ENTER energiat
  /SAVE PRED RESID. 
*creacion de nueva variable fsecos_mod_mod que suma el PRE_1 de sujeto creado + el RES_1 de cada paciente, es la variable ajustada a la dependiente.
Compute fsecos_mod_mod= 33.84795 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.
*resto de las variables de AOVE.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod3
  /METHOD=ENTER energiat3
  /SAVE PRED RESID.
Compute fsecos_mod_mod3= 40.82497783515779 + RES_1.
VARIABLE LABELS fsecos_mod_mod3 'aceite de oliva ajustada a energia visita 3'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod4
  /METHOD=ENTER energiat4
  /SAVE PRED RESID.
Compute fsecos_mod_mod4= 42.74806267153797 + RES_1.
VARIABLE LABELS fsecos_mod_mod4 'aceite de oliva ajustada a energia visita 4'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod5
  /METHOD=ENTER energiat5
  /SAVE PRED RESID.
Compute fsecos_mod_mod5= 47.7315611697117 + RES_1.
VARIABLE LABELS fsecos_mod_mod5 'aceite de oliva ajustada a energia visita 5'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod6
  /METHOD=ENTER energiat6
  /SAVE PRED RESID.
Compute fsecos_mod_mod6= 49.36643057722158 + RES_1.
VARIABLE LABELS fsecos_mod_mod6 'aceite de oliva ajustada a energia visita 6'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod7
  /METHOD=ENTER energiat7
  /SAVE PRED RESID.
Compute fsecos_mod_mod7= 49.87720504221997 + RES_1.
VARIABLE LABELS fsecos_mod_mod7 'aceite de oliva ajustada a energia visita 7'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod8
  /METHOD=ENTER energiat8
  /SAVE PRED RESID.
Compute fsecos_mod_mod8= 49.39090094877229 + RES_1.
VARIABLE LABELS fsecos_mod_mod8 'aceite de oliva ajustada a energia visita 8'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod9
  /METHOD=ENTER energiat9
  /SAVE PRED RESID.
Compute fsecos_mod_mod9= 49.24317079912151 + RES_1.
VARIABLE LABELS fsecos_mod_mod9 'aceite de oliva ajustada a energia visita 9'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod10
  /METHOD=ENTER energiat10
  /SAVE PRED RESID.
Compute fsecos_mod_mod10= 56.66665769423409 + RES_1.
VARIABLE LABELS fsecos_mod_mod10 'aceite de oliva ajustada a energia visita 10'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.
*Creacion de nueva variable dicotómica que tiene en cuenta los terciles 2-3 (1) frente al tercil 1 (0).
compute Nrac_oliB= Nrac_oli.
RECODE Nrac_oliB (0 thru 1=0) (2 thru 3=1).
VARIABLE LABELS  Nrac_oliB   "tercil1 contra union tercil2,3 solo teniendo en cuenta mujeres".
VARIABLE LEVEL  Nrac_oliB (SCALE).
FORMATS  Nrac_oliB (F8.0).
VARIABLE WIDTH  Nrac_oliB(8).
execute.
*Creacion de filtro para excluir mínimos y máximos de energía.
compute filt_energiat=1.
VARIABLE LABELS filt_energiat 'filtro de máximos y mínimos de energía'.
if ((energiat>3500 or energiat<500) and sexo=1) filt_energiat=0.
if ((energiat>4000 or energiat<800) and sexo=0) filt_energiat=0.
if (missing (energiat) and sexo=0) filt_energiat=0.
if (missing (energiat) and sexo=1) filt_energiat=0.
execute.
*Creacion de la variable T_cox_new -> años que pasan hasta que aparece fractura osteo según definicion nueva teniendo en cuenta fractura 1 y fractura 2.
If fract1 =1 AND fract1_new =1  T_cox_new=T_cox_fract.
If fract1 =1 AND fract1_new =0 OR fract1= 0 OR fract2=1 AND fract2_new=0 T_cox_new=T_cox_seg.
if fract1=1 AND fract1_new=0 AND fract2=1 AND fract2_new=1 T_cox_new=T_cox_fract2.
VARIABLE LABELS  T_cox_clas   "años que pasan hasta el diagnóstico de fractura osteo definicion nueva" + "durante todo el seguimento".
EXECUTE.
*Creacion de nueva variable con fsecos_mod ajustada a energía.
Compute energia = energiat.
VARIABLE LABELS  energia 'ingesta energética visita 0'.
Compute energia3 = energiat3.
VARIABLE LABELS  energia3 'ingesta energética visita 1a'.
Compute energia4 = energiat4.
VARIABLE LABELS  energia4 'ingesta energética visita 2a'.
Compute energia5 = energiat5.
VARIABLE LABELS  energia5 'ingesta energética visita 3a'.
Compute energia6 = energiat6.
VARIABLE LABELS  energia6 'ingesta energética visita 4a'.
Compute energia7 = energiat7.
VARIABLE LABELS  energia7 'ingesta energética visita 5a'.
Compute energia8 = energiat8.
VARIABLE LABELS  energia8 'ingesta energética visita 6a'.
Compute energia9 = energiat9.
VARIABLE LABELS  energia9 'ingesta energética visita 7a'.
Compute energia10 = energiat10.
VARIABLE LABELS energia10 'ingesta energétivca visita 8a'.
execute.

DESCRIPTIVES VARIABLES=energia energia3 energia4 energia5 energia6 energia7 energia8 energia9 energia10
  /STATISTICS=MEAN STDDEV MIN MAX.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod
  /METHOD=ENTER energia
  /SAVE PRED RESID. 

Compute fsecos_mod_mod= 41.22386426137056 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod3
  /METHOD=ENTER energia3
  /SAVE PRED RESID.
Compute fsecos_mod_mod3= 44.77743825143361 + RES_1.
VARIABLE LABELS fsecos_mod_mod3 'aceite de oliva ajustada a energia visita 3'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod4
  /METHOD=ENTER energiat4
  /SAVE PRED RESID.
Compute fsecos_mod_mod4= 47.01709569898596 + RES_1.
VARIABLE LABELS fsecos_mod_mod4 'aceite de oliva ajustada a energia visita 4'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod5
  /METHOD=ENTER energiat5
  /SAVE PRED RESID.
Compute fsecos_mod_mod5= 50.93239025724706 + RES_1.
VARIABLE LABELS fsecos_mod_mod5 'aceite de oliva ajustada a energia visita 5'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod6
  /METHOD=ENTER energiat6
  /SAVE PRED RESID.
Compute fsecos_mod_mod6= 51.78086618960294 + RES_1.
VARIABLE LABELS fsecos_mod_mod6 'aceite de oliva ajustada a energia visita 6'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod7
  /METHOD=ENTER energiat7
  /SAVE PRED RESID.
Compute fsecos_mod_mod7= 52.3567921438483 + RES_1.
VARIABLE LABELS fsecos_mod_mod7 'aceite de oliva ajustada a energia visita 7'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod8
  /METHOD=ENTER energiat8
  /SAVE PRED RESID.
Compute fsecos_mod_mod8= 51.12302711177065 + RES_1.
VARIABLE LABELS fsecos_mod_mod8 'aceite de oliva ajustada a energia visita 8'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod9
  /METHOD=ENTER energiat9
  /SAVE PRED RESID.
Compute fsecos_mod_mod9= 51.18918918144495 + RES_1.
VARIABLE LABELS fsecos_mod_mod9 'aceite de oliva ajustada a energia visita 9'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos_mod10
  /METHOD=ENTER energiat10
  /SAVE PRED RESID.
Compute fsecos_mod_mod10= 56.66666665249966 + RES_1.
VARIABLE LABELS fsecos_mod_mod10 'aceite de oliva ajustada a energia visita 10'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.
*creacion de la variable media ingesta de aceite de oliva ajustada a energía.
*Nuevo recode en funcion de T_cox_new para el cálculo de medias.
RECODE T_cox_new (0 thru 1=3) (1 thru 2=4) (2 thru 3=5) (3 thru 4=6) (4
    thru 5=7) (5 thru 6=8) (6 thru 7=9) (7 thru Highest=10) INTO v_final_evento1.
VARIABLE LABELS  v_final_evento1 'visita final de seguimiento o hasta aparición de evento fractura osteo'.
EXECUTE.
*Calculo de media de ingesta en funcion de aparicion del evento o final de estudio.
COMPUTE media_fsecos_mod = fsecos_mod. 
IF ((v_final_evento1=10)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7, fsecos_mod8, fsecos_mod9, fsecos_mod10).
IF ((v_final_evento1=9)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7, fsecos_mod8).
IF ((v_final_evento1=8)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7).
IF ((v_final_evento1=7)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6).
IF ((v_final_evento1=6)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5).
IF ((v_final_evento1=5)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4).
IF ((v_final_evento1=4)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3).
IF ((v_final_evento1=3)) media_fsecos_mod = mean(fsecos_mod).
VARIABLE LABELS media_fsecos_mod 'media de ingesta de aceite de oliva durante el estudio ajustado a energía'.
DELETE VARIABLES fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7, fsecos_mod8, fsecos_mod9, fsecos_mod10 energia energia3 energia4 energia5 energia6 energia7 energia8 energia9 energia10.
EXECUTE.
*terciles media_fsecos_mod.
RANK VARIABLES= media_fsecos_mod (A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*creacion de nueva variable que indica si hay evento osteo o no segun new.
compute fract_osteo_new = fract1_new.
if fract1_new= 1 fract_osteo_new = 1.
if fract1_new= 0 AND fract2_new=1 fract_osteo_new = 1.
if fract1_new= 0 AND fract2_new=0 fract_osteo_new = 0.
*regresion de con con t_cox_new y fract_osteo_new. 
COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER rac10 imc1  getota_1 edad0   
  /CONTRAST (rac10)=Indicator (1)
  /METHOD=ENTER rac10 
 /PATTERN BY rac10
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
*creacion de variables por raciones desde la variable media_fsecos_mod.
*variable por raciones en tres estratos: mayor de 6, entre 3 y 6, menor de 3.
compute rac_olivatot = media_fsecos_mod/10.
RECODE rac_olivatot (Lowest thru 3=1) (3 thru 6=2) (6 thru Highest=3).
VARIABLE LABELS rac_olivatot 'rac de aceite de oliva: 0-3 rac/3-6 rac/6o+ rac'.
EXECUTE.
*recodificacion rac_olivatot: ingesta 3 o menos, 3 o más.
compute rac_olivatot = media_fsecos_mod/10.
RECODE rac_olivatot (Lowest thru 3=1) (3 thru Highest=2).
VARIABLE LABELS rac_olivatot 'rac de aceite de oliva: 0-3 rac/3o + rac'.
EXECUTE.
*creacion de variables por raciones desde la variable media_ac_olivatot.
*variable por raciones en tres estratos: mayor de 6, entre 3 y 6, menor de 3.
compute rac_olivatot1 = media_ac_olivatot/10.
RECODE rac_olivatot1 (Lowest thru 3=1) (3 thru 6=2) (6 thru Highest=3).
VARIABLE LABELS rac_olivatot1 'rac de aceite de oliva: 0-3 rac/3-6 rac/6o+ rac'.
EXECUTE.

RANK VARIABLES= rac_olivatot (A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*pruebas con frutos secos.
RANK VARIABLES= media_f_secos(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*Creacion de la variable media_fsecos.
COMPUTE media_fsecos = fsecos. 
IF ((v_final_evento1=10)) media_fsecos = mean(fsecos, fsecos3, fsecos4, fsecos5, fsecos6, fsecos7, fsecos8, fsecos9, fsecos10).
IF ((v_final_evento1=9)) media_fsecos = mean(fsecos, fsecos3, fsecos4, fsecos5, fsecos6, fsecos7, fsecos8).
IF ((v_final_evento1=8)) media_fsecos = mean(fsecos, fsecos3, fsecos4, fsecos5, fsecos6, fsecos7).
IF ((v_final_evento1=7)) media_fsecos = mean(fsecos, fsecos3, fsecos4, fsecos5, fsecos6).
IF ((v_final_evento1=6)) media_fsecos = mean(fsecos, fsecos3, fsecos4, fsecos5).
IF ((v_final_evento1=5)) media_fsecos = mean(fsecos, fsecos3, fsecos4).
IF ((v_final_evento1=4)) media_fsecos = mean(fsecos, fsecos3).
IF ((v_final_evento1=3)) media_fsecos = mean(fsecos).
VARIABLE LABELS media_fsecos 'media de ingesta de frutos secos durante el estudio g/día'.
*Creacion de nueva variable con fsecos ajustada a energía.
Compute energia = energiat.
VARIABLE LABELS  energia 'ingesta energética visita 0'.
Compute energia3 = energiat3.
VARIABLE LABELS  energia3 'ingesta energética visita 1a'.
Compute energia4 = energiat4.
VARIABLE LABELS  energia4 'ingesta energética visita 2a'.
Compute energia5 = energiat5.
VARIABLE LABELS  energia5 'ingesta energética visita 3a'.
Compute energia6 = energiat6.
VARIABLE LABELS  energia6 'ingesta energética visita 4a'.
Compute energia7 = energiat7.
VARIABLE LABELS  energia7 'ingesta energética visita 5a'.
Compute energia8 = energiat8.
VARIABLE LABELS  energia8 'ingesta energética visita 6a'.
Compute energia9 = energiat9.
VARIABLE LABELS  energia9 'ingesta energética visita 7a'.
Compute energia10 = energiat10.
VARIABLE LABELS energia10 'ingesta energétivca visita 8a'.
execute.

DESCRIPTIVES VARIABLES=energia energia3 energia4 energia5 energia6 energia7 energia8 energia9 energia10
  /STATISTICS=MEAN STDDEV MIN MAX.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos
  /METHOD=ENTER energia
  /SAVE PRED RESID. 

Compute fsecos_mod= 12.87471260561983 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos3
  /METHOD=ENTER energia3
  /SAVE PRED RESID.
Compute fsecos_mod3= 22.58517702404718 + RES_1.
VARIABLE LABELS fsecos_mod3 'aceite de oliva ajustada a energia visita 3'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos4
  /METHOD=ENTER energia4
  /SAVE PRED RESID.
Compute fsecos_mod4= 20.99901798966257 + RES_1.
VARIABLE LABELS fsecos_mod4 'aceite de oliva ajustada a energia visita 4'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos5
  /METHOD=ENTER energia5
  /SAVE PRED RESID.
Compute fsecos_mod5= 16.74477011266994 + RES_1.
VARIABLE LABELS fsecos_mod5 'aceite de oliva ajustada a energia visita 5'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos6
  /METHOD=ENTER energia6
  /SAVE PRED RESID.
Compute fsecos_mod6= 12.56647299762103 + RES_1.
VARIABLE LABELS fsecos_mod6 'aceite de oliva ajustada a energia visita 6'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos7
  /METHOD=ENTER energia7
  /SAVE PRED RESID.
Compute fsecos_mod7= 12.02291304000221 + RES_1.
VARIABLE LABELS fsecos_mod7 'aceite de oliva ajustada a energia visita 7'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos8
  /METHOD=ENTER energia8
  /SAVE PRED RESID.
Compute fsecos_mod8= 12.35127479839741 + RES_1.
VARIABLE LABELS fsecos_mod8 'aceite de oliva ajustada a energia visita 8'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos9
  /METHOD=ENTER energia9
  /SAVE PRED RESID.
Compute fsecos_mod9= 11.83011577995825 + RES_1.
VARIABLE LABELS fsecos_mod9 'aceite de oliva ajustada a energia visita 9'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT fsecos10
  /METHOD=ENTER energia10
  /SAVE PRED RESID.
Compute fsecos_mod10= 3.52380928545704 + RES_1.
VARIABLE LABELS fsecos_mod10 'aceite de oliva ajustada a energia visita 10'.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.
*Calculo de media de ingesta en funcion de aparicion del evento o final de estudio: fsecos ajustado.
COMPUTE media_fsecos_mod = fsecos_mod. 
IF ((v_final_evento1=10)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7, fsecos_mod8, fsecos_mod9, fsecos_mod10).
IF ((v_final_evento1=9)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7, fsecos_mod8).
IF ((v_final_evento1=8)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7).
IF ((v_final_evento1=7)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6).
IF ((v_final_evento1=6)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5).
IF ((v_final_evento1=5)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3, fsecos_mod4).
IF ((v_final_evento1=4)) media_fsecos_mod = mean(fsecos_mod, fsecos_mod3).
IF ((v_final_evento1=3)) media_fsecos_mod = mean(fsecos_mod).
VARIABLE LABELS media_fsecos_mod 'media de ingesta de aceite de oliva durante el estudio ajustado a energía'.
execute.
DELETE VARIABLES fsecos_mod, fsecos_mod3, fsecos_mod4, fsecos_mod5, fsecos_mod6, fsecos_mod7, fsecos_mod8, fsecos_mod9, fsecos_mod10 energia energia3 energia4 energia5 energia6 energia7 energia8 energia9 energia10.
EXECUTE.

RANK VARIABLES= media_fsecos_mod(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*regresion de con con t_cox_new y fract_osteo_new con frutos secos.
*grup_int edad0 escolar1 sexo imc1 fum  GETOTA_1 diabetes0 m_estatin1  hipercol0 hta0 fractura1 vitamin1  ado1 m_diuret1 m_aas1. 
COXREG T_cox_new
  /STATUS= fract_osteo_new2013(1)
/METHOD=ENTER grup_int imc1 edad0 sexo energiat hipercol0 edad0 grup_int  escolar1 media_fsecos_mod media_fsecos media_ac_olivatot media_olivatot_mod
 /CONTRAST (grup_int)=Indicator (1)
  /METHOD=ENTER grup_int 
 /PATTERN BY grup_int
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
*creacion de variables por raciones desde la variable media_fsecos_mod.
*variable por raciones en tres estratos: mayor de 6, entre 3 y 6, menor de 3.
compute rac_fsecos1 = media_fsecos/10.
VARIABLE LABELS rac_fsecos1 'rac de fsecos'.
RECODE rac_fsecos1 (Lowest thru 1=1) (1 thru 2=2) (2 thru Highest=3).
VARIABLE LABELS rac_fsecos1 'rac de fsecos: 0-1 rac/1-2 rac/2o+ rac'.
EXECUTE.
*recodificacion rac_olivatot: ingesta 3 o menos, 3 o más.
compute rac_fsecos2 = media_fsecos/10.
RECODE rac_fsecos2 (Lowest thru 1.5=1) (1.5 thru Highest=2).
VARIABLE LABELS rac_fsecos2 'rac de fsecos: 0-1,5 rac/1,5o + rac'.
EXECUTE.
*creacion de variables por raciones desde la variable media_ac_olivatot.
*variable por raciones en tres estratos: mayor de 6, entre 3 y 6, menor de 3.
compute rac_fsecos3 = media_fsecos_mod/10.
VARIABLE LABELS rac_fsecos3 'rac de fsecos ajustados'.
RECODE rac_fsecos3 (Lowest thru 1.5=1) (1.5 thru Highest=2).
VARIABLE LABELS rac_fsecos3 'rac de fsecos: 0-1,5 rac/1,5o + rac'.
EXECUTE.
RECODE rac_fsecos3 (Lowest thru 3=1) (3 thru 6=2) (6 thru Highest=3).
VARIABLE LABELS rac_fsecos3 'rac de fsecos ajustados: 0-3 rac/3-6 rac/6o+ rac'.
EXECUTE.
*terciles rac fsecos.
RANK VARIABLES= media_fsecos_mod(A)
  /RANK
  /NTILES(2)
  /PRINT=YES
  /TIES=MEAN.
*Prueba con nueces.
Compute nuezt = nuez.
compute nuezt3 = nuez3. 
compute nuezt4 = nuez4.
compute nuezt5 = nuez5.
compute nuezt6 = nuez6. 
compute nuezt7 = nuez7.
compute nuezt8 = nuez8.
compute nuezt9 = nuez9.
compute nuezt10 = nuez10.
VARIABLE LABELS  nuezt 'ingesta nueces visita 0'  nuezt3 'ingesta nueces visita 1a' nuezt4 'ingesta nueces visita 2a' nuezt5 'ingesta nueces visita 3a' nuezt6 'ingesta nueces visita 4a' 
nuezt7 'ingesta nueces visita 5a' nuezt8 'ingesta nueces visita 6a' nuezt9 'ingesta nueces visita 7a'  nuezt10 'ingesta energétivca visita 8a'.
execute.
*creación de variable de media de ingesta de nueces.
COMPUTE media_nuezt = nuezt. 
IF ((v_final_evento1=10)) media_nuezt = mean(nuezt, nuezt3, nuezt4, nuezt5, nuezt6, nuezt7, nuezt8, nuezt9, nuezt10).
IF ((v_final_evento1=9)) media_nuezt = mean(nuezt, nuezt3, nuezt4, nuezt5, nuezt6, nuezt7, nuezt8).
IF ((v_final_evento1=8)) media_nuezt = mean(nuezt, nuezt3, nuezt4, nuezt5, nuezt6, nuezt7).
IF ((v_final_evento1=7)) media_nuezt = mean(nuezt, nuezt3, nuezt4, nuezt5, nuezt6).
IF ((v_final_evento1=6)) media_nuezt = mean(nuezt, nuezt3, nuezt4, nuezt5).
IF ((v_final_evento1=5)) media_nuezt = mean(nuezt, nuezt3, nuezt4).
IF ((v_final_evento1=4)) media_nuezt = mean(nuezt, nuezt3).
IF ((v_final_evento1=3)) media_nuezt = mean(nuezt).
VARIABLE LABELS media_nuezt 'media de ingesta de nueces'.
execute.
DELETE VARIABLES nuezt, nuezt3, nuezt4, nuezt5, nuezt6, nuezt7, nuezt8, nuezt9, nuezt10. 
execute.
*terciles media de nueces.
RANK VARIABLES= media_fsecos_mod(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*regresion de con con t_cox_new y fract_osteo_new con frutos secos.
*grup_int edad0 escolar1 sexo imc1 fum  GETOTA_1 diabetes0 m_estatin1  hipercol0 hta0 fractura1 vitamin1  ado1 m_diuret1 m_aas1. 
COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NTI001 imc1 edad0 fractura1 energiat hipercol0 edad0 grup_int  escolar1
 /CONTRAST (NTI001)=Indicator (1)
  /METHOD=ENTER NTI001 
 /PATTERN BY NTI001
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
*Prueba con el cuestionario de adherencia a la dieta mediterranea.
COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER puntos_cuestionario imc1 edad0 fractura1 energiat hipercol0 edad0 
 /CONTRAST (puntos_cuestionario)=Indicator (1)
  /METHOD=ENTER puntos_cuestionario 
 /PATTERN BY puntos_cuestionario
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

compute puntos_cuestionario = p14totm_1.
VARIABLE LABELS  puntos_cuestionario '<6 puntos o >6 puntos'.
RECODE puntos_cuestionario (Lowest thru 6=1) (6 thru Highest=2).
EXECUTE.

*calculo media de energía.
Compute energia = energiat.
compute energia3 = energiat3. 
compute energia4 = energiat4.
compute energia5 = energiat5.
compute energia6 = energiat6. 
compute energia7 = energiat7.
compute energia8 = energiat8.
compute energia9 = energiat9.
compute energia10 = energiat10.
VARIABLE LABELS  energia 'ingesta nueces visita 0'  energia3 'ingesta nueces visita 1a' energia4 'ingesta nueces visita 2a' energia5 'ingesta nueces visita 3a' energia6 'ingesta nueces visita 4a' 
energia7 'ingesta nueces visita 5a' energia8 'ingesta nueces visita 6a' energia9 'ingesta nueces visita 7a'  energia10 'ingesta energétivca visita 8a'.
execute.
*creación de variable de media de ingesta de energia.
COMPUTE media_energia = energia. 
IF ((v_final_evento1=10)) media_energia = mean(energia, energia3, energia4, energia5, energia6, energia7, energia8, energia9, energia10).
IF ((v_final_evento1=9)) media_energia = mean(energia, energia3, energia4, energia5, energia6, energia7, energia8).
IF ((v_final_evento1=8)) media_energia = mean(energia, energia3, energia4, energia5, energia6, energia7).
IF ((v_final_evento1=7)) media_energia = mean(energia, energia3, energia4, energia5, energia6).
IF ((v_final_evento1=6)) media_energia = mean(energia, energia3, energia4, energia5).
IF ((v_final_evento1=5)) media_energia = mean(energia, energia3, energia4).
IF ((v_final_evento1=4)) media_energia = mean(energia, energia3).
IF ((v_final_evento1=3)) media_energia = mean(energia).
VARIABLE LABELS media_energia 'media de ingesta de nueces'.
execute.
DELETE VARIABLES energia, energia3, energia4, energia5, energia6, energia7, energia8, energia9, energia10. 
execute.
*Creacion variable de seguimiento hasta final de estudio.
COMPUTE  T_cox_seg2=(data_fin2-datinclu) / (365.25 * time.days(1)).
VARIABLE LABELS  T_cox_seg2  "años de seguimento hasta fin de seguimiento 31 agosto 2014".
VARIABLE LEVEL T_cox_seg2 (SCALE).
FORMATS  T_cox_seg2(F8.2).
VARIABLE WIDTH  T_cox_seg2(8).
EXECUTE.
*Creacion de la variable T_cox_clas2 -> años que pasan hasta que aparece fractura osteo según definicion clasica teniendo en cuenta fractura 1 y fractura 2 hasta última revisión (31-ago-2014).
compute T_cox_clas2 = 1.
if death = 1 T_cox_clas2 = T_cox_seg.
if death = 0 AND fract1 =1 AND fract1_osteo =1  T_cox_clas2=T_cox_fract.
if death = 0 AND fract1 =1 AND fract1_osteo =0 OR death = 0 AND fract1= 0 OR death = 0 AND fract2=1 AND fract2_osteo=0 T_cox_clas2=T_cox_seg2.
if death = 0 AND fract1=1 AND fract1_osteo=0 AND fract2=1 AND fract2_osteo=1 T_cox_clas2=T_cox_fract2.
VARIABLE LABELS  T_cox_clas2  "años que pasan hasta el diagnóstico de fractura osteo definicion clasica"+ "durante todo el seguimento".
EXECUTE.
*Creacion de la variable T_cox_new2 -> años que pasan hasta que aparece fractura osteo según definicion nueva teniendo en cuenta fractura 1 y fractura 2 hasta última revisión (31-ago-2014).
compute T_cox_new2 = 1.
if death = 1 T_cox_new2 = T_cox_seg.
if death = 0 AND fract1 =1 AND fract1_new =1  T_cox_new2=T_cox_fract.
if death = 0 AND fract1 =1 AND fract1_new =0 OR death = 0 AND fract1= 0 OR death = 0 AND fract2=1 AND fract2_new=0 T_cox_new2=T_cox_seg2.
if death = 0 AND fract1=1 AND fract1_new=0 AND fract2=1 AND fract2_new=1 T_cox_new2=T_cox_fract2.
VARIABLE LABELS  T_cox_new2  "años que pasan hasta el diagnóstico de fractura osteo definicion clasica"+ "durante todo el seguimento".
EXECUTE.
*terciles media de nueces.
RANK VARIABLES= media_ac_olivatot(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*Prueba con la variable t_cox actualizada y la ingesta en kcal.
COXREG T_cox_new2
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER grup_int imc1 edad0 sexo  
 /CONTRAST (grup_int)=Indicator (1)
  /METHOD=ENTER grup_int 
 /PATTERN BY grup_int
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
*Creacion de variable media_getota.
Compute getot = getota_1.
compute getot3 = getota_3. 
compute getot4 = getota_4.
compute getot5 = getota_5.
compute getot6 = getota_6. 
compute getot7 = getota_7.
compute getot8 = getota_8.
compute getot9 = getota_9.
compute getot10 = getota_10.
VARIABLE LABELS  getot 'ingesta nueces visita 0'  getot3 'ingesta nueces visita 1a' getot4 'ingesta nueces visita 2a' getot5 'ingesta nueces visita 3a' getot6 'ingesta nueces visita 4a' 
getot7 'ingesta nueces visita 5a' getot8 'ingesta nueces visita 6a' getot9 'ingesta nueces visita 7a'  getot10 'ingesta energétivca visita 8a'.
execute.
*creación de variable de media de ingesta de energia.
COMPUTE media_getot = getot. 
IF ((v_final_evento1=10)) media_getot = mean(getot, getot3, getot4, getot5, getot6, getot7, getot8, getot9, getot10).
IF ((v_final_evento1=9)) media_getot = mean(getot, getot3, getot4, getot5, getot6, getot7, getot8).
IF ((v_final_evento1=8)) media_getot = mean(getot, getot3, getot4, getot5, getot6, getot7).
IF ((v_final_evento1=7)) media_getot = mean(getot, getot3, getot4, getot5, getot6).
IF ((v_final_evento1=6)) media_getot = mean(getot, getot3, getot4, getot5).
IF ((v_final_evento1=5)) media_getot = mean(getot, getot3, getot4).
IF ((v_final_evento1=4)) media_getot = mean(getot, getot3).
IF ((v_final_evento1=3)) media_getot = mean(getot).
VARIABLE LABELS media_getot 'media gasto calorico'.
execute.
DELETE VARIABLES getot, getot3, getot4, getot5, getot6, getot7, getot8, getot9, getot10. 
execute.

*Creacion de variable %relativo IMC.
Compute IMCa = imc1.
compute IMCa3 = imc3. 
compute IMCa4 = imc4.
compute IMCa5 = imc5.
compute IMCa6 = imc6. 
compute IMCa7 = imc7.
compute IMCa8 = imc8.
compute IMCa9 = imc9.
compute IMCa10 = imc10.
VARIABLE LABELS  IMCa 'ingesta nueces visita 0'  IMCa3 'ingesta nueces visita 1a' IMCa4 'ingesta nueces visita 2a' IMCa5 'ingesta nueces visita 3a' IMCa6 'ingesta nueces visita 4a' 
IMCa7 'ingesta nueces visita 5a' IMCa8 'ingesta nueces visita 6a' IMCa9 'ingesta nueces visita 7a'  IMCa10 'ingesta energétivca visita 8a'.
execute.

compute IMCrel3=(IMCa3 - IMCa)/IMCa.
compute IMCrel4=(IMCa4 - IMCa)/IMCa.
compute IMCrel5=(IMCa5 - IMCa)/IMCa.
compute IMCrel6=(IMCa6 - IMCa)/IMCa.
compute IMCrel7=(IMCa7 - IMCa)/IMCa.
compute IMCrel8=(IMCa8 - IMCa)/IMCa.
compute IMCrel9=(IMCa9 - IMCa)/IMCa.
compute IMCrel10=(IMCa10 - IMCa)/IMCa.

COMPUTE IMCrelfin = IMCrel3. 
IF ((v_final_evento1=10)) IMCrelfin = IMCrel10*100.
IF ((v_final_evento1=9)) IMCrelfin = IMCrel9*100.
IF ((v_final_evento1=8)) IMCrelfin = IMCrel8*100.
IF ((v_final_evento1=7)) IMCrelfin = IMCrel7*100.
IF ((v_final_evento1=6)) IMCrelfin = IMCrel6*100.
IF ((v_final_evento1=5)) IMCrelfin = IMCrel5*100.
IF ((v_final_evento1=4)) IMCrelfin = IMCrel4*100.
IF ((v_final_evento1=3)) IMCrelfin = IMCrel3*100.
VARIABLE LABELS IMCrelfin 'diferencia relativa de perdida o ganancia de IMC'.
execute.
DELETE VARIABLES getot, getot3, getot4, getot5, getot6, getot7, getot8, getot9, getot10. 
execute.

*Creación de variables con la densiometría.
compute sosizq_rel3 = ((imp3_sosizq -  imp1_sosizq)/imp1_sosizq)*100.
variable labels sosizq_rel3 'diferencia relativa hasta visita 3 de sos izq'.
VARIABLE LEVEL sosizq_rel3 (SCALE).

compute buaizq_rel3 = ((imp3_buaizq -  imp1_buaizq)/imp1_buaizq)*100.
variable labels buaizq_rel3 'diferencia relativa hasta visita 3 de bua izq'.
VARIABLE LEVEL buaizq_rel3 (SCALE).

compute quiizq_rel3 = ((imp3_quiizq -  imp1_quiizq)/imp1_quiizq)*100.
variable labels quiizq_rel3 'diferencia relativa hasta visita 3 de qui izq'.
VARIABLE LEVEL quiizq_rel3 (SCALE).

compute dmoizq_rel3 = ((imp3_dmoizq -  imp1_dmoizq)/imp1_dmoizq)*100.
variable labels dmoizq_rel3 'diferencia relativa hasta visita 3 de dmo izq'.
VARIABLE LEVEL dmoizq_rel3 (SCALE).

compute tscoreizq_rel3 = ((imp3_tscoreizq -  imp1_tscoreizq)/imp1_tscoreizq)*100.
variable labels tscoreizq_rel3 'diferencia relativa hasta visita 3 de tscore izq'.
VARIABLE LEVEL tscoreizq_rel3 (SCALE).

compute zscoreizq_rel3 = ((imp3_zscoreizq -  imp1_zscoreizq)/imp1_zscoreizq)*100.
variable labels zscoreizq_rel3 'diferencia relativa hasta visita 3 de zscore izq'.
VARIABLE LEVEL zscoreizq_rel3 (SCALE).

compute sosder_rel3 = ((imp3_sosder -  imp1_sosder)/imp1_sosder)*100.
variable labels sosder_rel3 'diferencia relativa hasta visita 3 de sos der'.
VARIABLE LEVEL sosder_rel3 (SCALE).

compute buader_rel3 = ((imp3_buader -  imp1_buader)/imp1_buader)*100.
variable labels buader_rel3 'diferencia relativa hasta visita 3 de bua der'.
VARIABLE LEVEL buader_rel3 (SCALE).

compute quider_rel3 = ((imp3_quider -  imp1_quider)/imp1_quider)*100.
variable labels quider_rel3 'diferencia relativa hasta visita 3 de qui der'.
VARIABLE LEVEL quider_rel3 (SCALE).

compute dmoder_rel3 = ((imp3_dmoder -  imp1_dmoder)/imp1_dmoder)*100.
variable labels dmoder_rel3 'diferencia relativa hasta visita 3 de dmo der'.
VARIABLE LEVEL dmoder_rel3 (SCALE).

compute tscoreder_rel3 = ((imp3_tscoreder -  imp1_tscoreder)/imp1_tscoreder)*100.
variable labels tscoreder_rel3 'diferencia relativa hasta visita 3 de tscore der'.
VARIABLE LEVEL tscoreder_rel3 (SCALE).

compute zscoreder_rel3 = ((imp3_zscoreder -  imp1_zscoreder)/imp1_zscoreder)*100.
variable labels zscoreder_rel3 'diferencia relativa hasta visita 3 de zscore der'.
VARIABLE LEVEL zscoreder_rel3 (SCALE).

compute sosizq_rel4 = ((imp4_sosizq -  imp1_sosizq)/imp1_sosizq)*100.
variable labels sosizq_rel4 'diferencia relativa hasta visita 4 de sos izq'.
VARIABLE LEVEL sosizq_rel4 (SCALE).

compute buaizq_rel4 = ((imp4_buaizq -  imp1_buaizq)/imp1_buaizq)*100.
variable labels buaizq_rel4 'diferencia relativa hasta visita 4 de bua izq'.
VARIABLE LEVEL buaizq_rel4 (SCALE).

compute quiizq_rel4 = ((imp4_quiizq -  imp1_quiizq)/imp1_quiizq)*100.
variable labels quiizq_rel4 'diferencia relativa hasta visita 4 de qui izq'.
VARIABLE LEVEL quiizq_rel4 (SCALE).

compute dmoizq_rel4 = ((imp4_dmoizq -  imp1_dmoizq)/imp1_dmoizq)*100.
variable labels dmoizq_rel4 'diferencia relativa hasta visita 4 de dmo izq'.
VARIABLE LEVEL dmoizq_rel4 (SCALE).

compute tscoreizq_rel4 = ((imp4_tscoreizq -  imp1_tscoreizq)/imp1_tscoreizq)*100.
variable labels tscoreizq_rel4 'diferencia relativa hasta visita 4 de tscore izq'.
VARIABLE LEVEL tscoreizq_rel4 (SCALE).

compute zscoreizq_rel4 = ((imp4_zscoreizq -  imp1_zscoreizq)/imp1_zscoreizq)*100.
variable labels zscoreizq_rel4 'diferencia relativa hasta visita 4 de zscore izq'.
VARIABLE LEVEL zscoreizq_rel4 (SCALE).

compute sosder_rel4 = ((imp4_sosder -  imp1_sosder)/imp1_sosder)*100.
variable labels sosder_rel4 'diferencia relativa hasta visita 4 de sos der'.
VARIABLE LEVEL sosder_rel4 (SCALE).

compute buader_rel4 = ((imp4_buader -  imp1_buader)/imp1_buader)*100.
variable labels buader_rel4 'diferencia relativa hasta visita 4 de bua der'.
VARIABLE LEVEL buader_rel4 (SCALE).

compute quider_rel4 = ((imp4_quider -  imp1_quider)/imp1_quider)*100.
variable labels quider_rel4 'diferencia relativa hasta visita 4 de qui der'.
VARIABLE LEVEL quider_rel4 (SCALE).

compute dmoder_rel4 = ((imp4_dmoder -  imp1_dmoder)/imp1_dmoder)*100.
variable labels dmoder_rel4 'diferencia relativa hasta visita 4 de dmo der'.
VARIABLE LEVEL dmoder_rel4 (SCALE).

compute tscoreder_rel4 = ((imp4_tscoreder -  imp1_tscoreder)/imp1_tscoreder)*100.
variable labels tscoreder_rel4 'diferencia relativa hasta visita 4 de tscore der'.
VARIABLE LEVEL tscoreder_rel4 (SCALE).

compute zscoreder_rel4 = ((imp4_zscoreder -  imp1_zscoreder)/imp1_zscoreder)*100.
variable labels zscoreder_rel4 'diferencia relativa hasta visita 4 de zscore der'.
VARIABLE LEVEL zscoreder_rel4 (SCALE).

COXREG T_cox_new2
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER grup_int imc1 edad0 fractura1 media_energia sexo  media_getot dmoizq_rel3 dmoder_rel3
 /CONTRAST (grup_int)=Indicator (1)
  /METHOD=ENTER grup_int 
 /PATTERN BY grup_int
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

RANK VARIABLES= media_ac_olivatot(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

*ANOVA v3 SOS, QUI, DMO, tscore, zcore entre grupos de intervencion.
*v3.
DATASET ACTIVATE Conjunto_de_datos1.
ONEWAY sosizq_rel3 buaizq_rel3 quiizq_rel3 dmoizq_rel3 tscoreizq_rel3 zscoreizq_rel3 sosder_rel3 
    buader_rel3 quider_rel3 dmoder_rel3 tscoreder_rel3 zscoreder_rel3 BY grup_int
  /MISSING ANALYSIS.
*v4.
ONEWAY sosizq_rel4 buaizq_rel4 quiizq_rel4 dmoizq_rel4 tscoreizq_rel4 zscoreizq_rel4 sosder_rel4 
    buader_rel4 quider_rel4 dmoder_rel4 tscoreder_rel4 zscoreder_rel4 BY grup_int
  /MISSING ANALYSIS.
*creacion de variable porcentaje de ingesta de AOVE v3 y v4.
compute acolivavir = ac_olivavir.
compute acolivavir3 = ac_olivavir3.
compute acolivavir4 = ac_olivavir4.
compute acolivatot = olivatot.
compute acolivatot3 = olivatot3.
compute acolivatot4 = olivatot4.

compute ac_olivavir_rel3 = (((acolivavir3) -  (acolivavir))/(acolivavir))*100.
variable labels ac_olivavir_rel3 'diferencia relativa hasta visita 3 de aceite de oliva virgen'.
VARIABLE LEVEL ac_olivavir_rel3 (SCALE).

compute ac_olivavir_rel4 = (((acolivavir4) -  (acolivavir))/(acolivavir))*100.
variable labels ac_olivavir_rel4 'diferencia relativa hasta visita 4 de aceite de oliva virgen'.
VARIABLE LEVEL ac_olivavir_rel4 (SCALE).

compute olivatot_rel3 = (((acolivatot3) -  (acolivatot))/(acolivatot))*100.
variable labels olivatot_rel3 'diferencia relativa hasta visita 3 de aceite de oliva virgen'.
VARIABLE LEVEL olivatot_rel3 (SCALE).

compute olivatot_rel4 = (((acolivatot4) -  (acolivatot))/(acolivatot))*100.
variable labels olivatot_rel4 'diferencia relativa hasta visita 4 de aceite de oliva virgen'.
VARIABLE LEVEL olivatot_rel4 (SCALE).

RANK VARIABLES= edad0(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*ANOVA v3 SOS, QUI, DMO, tscore, zcore entre terciles de ingesta AOVE.
*v3.
ONEWAY sosizq_rel3 buaizq_rel3 quiizq_rel3 dmoizq_rel3 tscoreizq_rel3 zscoreizq_rel3 sosder_rel3 
    buader_rel3 quider_rel3 dmoder_rel3 tscoreder_rel3 zscoreder_rel3 BY NTI002
  /MISSING ANALYSIS.
*v4.
ONEWAY sosizq_rel4 buaizq_rel4 quiizq_rel4 dmoizq_rel4 tscoreizq_rel4 zscoreizq_rel4 sosder_rel4 
    buader_rel4 quider_rel4 dmoder_rel4 tscoreder_rel4 zscoreder_rel4 BY Nac_oliv
  /MISSING ANALYSIS.
*ANOVA v3 SOS, QUI, DMO, tscore, zcore entre terciles de ingesta olivatot.
*v3.
ONEWAY sosizq_rel3 buaizq_rel3 quiizq_rel3 dmoizq_rel3 tscoreizq_rel3 zscoreizq_rel3 sosder_rel3 
    buader_rel3 quider_rel3 dmoder_rel3 tscoreder_rel3 zscoreder_rel3 BY Nolivato
  /MISSING ANALYSIS.
*v4.
ONEWAY sosizq_rel4 buaizq_rel4 quiizq_rel4 dmoizq_rel4 tscoreizq_rel4 zscoreizq_rel4 sosder_rel4 
    buader_rel4 quider_rel4 dmoder_rel4 tscoreder_rel4 zscoreder_rel4 BY NTI001
  /MISSING ANALYSIS.

compute ac_olivavir_rel3a = ((acolivavir3) -  (acolivavir)).
variable labels ac_olivavir_rel3a 'diferencia relativa hasta visita 3 de aceite de oliva virgen'.
VARIABLE LEVEL ac_olivavir_rel3a (SCALE).

compute  = ((acolivavir4) -  (acolivavir)).
variable labels ac_olivavir_rel4a 'diferencia relativa hasta visita 4 de aceite de oliva virgen'.
VARIABLE LEVEL ac_olivavir_rel4a (SCALE).

compute olivatot_rel3a = ((acolivatot3) -  (acolivatot)).
variable labels olivatot_rel3a 'diferencia relativa hasta visita 3 de aceite de oliva virgen'.
VARIABLE LEVEL olivatot_rel3a (SCALE).

compute olivatot_rel4a = ((acolivatot4) -  (acolivatot)).
variable labels olivatot_rel4a 'diferencia relativa hasta visita 4 de aceite de oliva virgen'.
VARIABLE LEVEL olivatot_rel4a (SCALE).

RANK VARIABLES= media_actot_ajus2(A)
  /RANK
  /NTILES(5)
  /PRINT=YES
  /TIES=MEAN.

ONEWAY sosizq_rel3 buaizq_rel3 quiizq_rel3 dmoizq_rel3 tscoreizq_rel3 zscoreizq_rel3 sosder_rel3 
    buader_rel3 quider_rel3 dmoder_rel3 tscoreder_rel3 zscoreder_rel3 BY Nolivato
  /MISSING ANALYSIS.
ONEWAY sosizq_rel4 buaizq_rel4 quiizq_rel4 dmoizq_rel4 tscoreizq_rel4 zscoreizq_rel4 sosder_rel4 
    buader_rel4 quider_rel4 dmoder_rel4 tscoreder_rel4 zscoreder_rel4 BY NTI002
  /MISSING ANALYSIS.


COMPUTE deltaBUA_2a=(imp4_buaizq)-(imp1_buaizq).
EXECUTE.

COMPUTE deltaDMO_2a=(imp4_dmoizq)-(imp1_dmoizq).
EXECUTE.

COMPUTE deltaQUI_2a=(imp4_quiizq)-(imp1_quiizq).
EXECUTE.
 

ONEWAY deltaBUA_1a deltaDMO_1a deltaQUI_1a deltasos_1a deltatscore_1a BY NTI001
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=LSD ALPHA(0.05).


COMPUTE deltaBUA_1a=((imp3_buader+IMP3_buaizq)/2)-((imp1_buader+imp1_buaizq)/2).
EXECUTE.

COMPUTE deltaDMO_1a=((imp3_dmoder+IMP3_dmoizq)/2)-((imp1_dmoder+imp1_dmoizq)/2).
EXECUTE.

COMPUTE deltaQUI_1a=((imp3_quider+IMP3_quiizq)/2)-((imp1_quider+imp1_quiizq)/2).
EXECUTE.

COMPUTE deltaSOS_1a=((imp3_sosder+IMP3_sosizq)/2)-((imp1_sosder+imp1_sosizq)/2).
EXECUTE.

COMPUTE deltaTscore_1a=((imp3_tscoreder+IMP3_tscoreizq)/2)-((imp1_tscoreder+imp1_tscoreizq)/2).
EXECUTE.

COMPUTE deltaZscore_1a=((imp3_zscoreder+IMP3_zscoreizq)/2)-((imp1_zscoreder+imp1_zscoreizq)/2).
EXECUTE.

COMPUTE deltaBUA_2a= mitja_BUA_2a -BUA_MITJA_BASAL.
EXECUTE.

COMPUTE deltaQUI_2a= mitja_QUI_2a -QUi_MITJA_BASAL.
EXECUTE.
COMPUTE deltaSOS_2a= mitja_SOS_2a -SOS_MITJA_BASAL.
EXECUTE.
COMPUTE deltaDMO_2a= mitja_DMO_2a -DMO_MITJA_BASAL.
EXECUTE.

COMPUTE deltaBUA_1_2a= mitja_BUA_2a -MITJA_BUA_any.
EXECUTE.
COMPUTE deltaQUI_1_2a= mitja_QUI_2a -MITJA_QUi_any.
EXECUTE.
COMPUTE deltaSOS_1_2a= mitja_SOS_2a -MITJA_SOS_any.
EXECUTE.
COMPUTE deltaDMO_1_2a= mitja_DMO_2a -MITJA_DMO_any.
EXECUTE.
 
ONEWAY deltaBUA_2a deltaDMO_2a deltaQUI_2a BY Nac_oliv
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=LSD ALPHA(0.05).

RECODE edad0 (55 thru 60=1) (60 thru 65=2) (65 thru 70=3) (70 thru 75=4) (75 thru 80=5) INTO grup_edad. 
VARIABLE LABELS  grup_edad 'grupos de edad cada 5 años'. 
EXECUTE.

COMPUTE deltaimc_1a=imc3 - imc1.
EXECUTE.

list deltaBUA_1a imp3_buader IMP3_buaizq imp1_buader imp1_buaizq deltaDMO_1a imp3_dmoder IMP3_dmoizq imp1_dmoder imp1_dmoizq .

DATASET ACTIVATE Conjunto_de_datos1.
* Generador de gráficos.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=grup_int deltaSOS_1a MISSING=LISTWISE REPORTMISSING=NO    
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: grup_int=col(source(s), name("grup_int"), unit.category())
  DATA: deltaSOS_1a=col(source(s), name("deltaSOS_1a"))
  DATA: id=col(source(s), name("$CASENUM"), unit.category())
  GUIDE: axis(dim(1), label("GENERAL: Grupo de intervencion"))
  GUIDE: axis(dim(2), label("deltaSOS_1a"))
  SCALE: cat(dim(1), include("1", "2", "3"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: schema(position(bin.quantile.letter(grup_int*deltaSOS_1a)), label(id))
END GPL.

* Generador de gráficos.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=grup_int deltaBUA_1a MISSING=LISTWISE REPORTMISSING=NO    
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: grup_int=col(source(s), name("grup_int"), unit.category())
  DATA: deltaBUA_1a=col(source(s), name("deltaBUA_1a"))
  DATA: id=col(source(s), name("$CASENUM"), unit.category())
  GUIDE: axis(dim(1), label("GENERAL: Grupo de intervencion"))
  GUIDE: axis(dim(2), label("deltaBUA_1a"))
  SCALE: cat(dim(1), include("1", "2", "3"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: schema(position(bin.quantile.letter(grup_int*deltaBUA_1a)), label(id))
END GPL.

* Generador de gráficos.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=grup_int deltaQUI_1a MISSING=LISTWISE REPORTMISSING=NO    
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: grup_int=col(source(s), name("grup_int"), unit.category())
  DATA: deltaQUI_1a=col(source(s), name("deltaQUI_1a"))
  DATA: id=col(source(s), name("$CASENUM"), unit.category())
  GUIDE: axis(dim(1), label("GENERAL: Grupo de intervencion"))
  GUIDE: axis(dim(2), label("deltaQUI_1a"))
  SCALE: cat(dim(1), include("1", "2", "3"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: schema(position(bin.quantile.letter(grup_int*deltaQUI_1a)), label(id))
END GPL.

* Generador de gráficos.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=grup_int deltaDMO_1a MISSING=LISTWISE REPORTMISSING=NO    
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: grup_int=col(source(s), name("grup_int"), unit.category())
  DATA: deltaDMO_1a=col(source(s), name("deltaDMO_1a"))
  DATA: id=col(source(s), name("$CASENUM"), unit.category())
  GUIDE: axis(dim(1), label("GENERAL: Grupo de intervencion"))
  GUIDE: axis(dim(2), label("deltaDMO_1a"))
  SCALE: cat(dim(1), include("1", "2", "3"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: schema(position(bin.quantile.letter(grup_int*deltaDMO_1a)), label(id))
END GPL.

* Generador de gráficos.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=grup_int deltaTscore_1a MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: grup_int=col(source(s), name("grup_int"), unit.category())
  DATA: deltaTscore_1a=col(source(s), name("deltaTscore_1a"))
  DATA: id=col(source(s), name("$CASENUM"), unit.category())
  GUIDE: axis(dim(1), label("GENERAL: Grupo de intervencion"))
  GUIDE: axis(dim(2), label("deltaTscore_1a"))
  SCALE: cat(dim(1), include("1", "2", "3"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: schema(position(bin.quantile.letter(grup_int*deltaTscore_1a)), label(id))
END GPL.

* Generador de gráficos.
GGRAPH
  /GRAPHDATASET NAME="graphdataset" VARIABLES=grup_int deltaZscore_1a MISSING=LISTWISE 
    REPORTMISSING=NO
  /GRAPHSPEC SOURCE=INLINE.
BEGIN GPL
  SOURCE: s=userSource(id("graphdataset"))
  DATA: grup_int=col(source(s), name("grup_int"), unit.category())
  DATA: deltaZscore_1a=col(source(s), name("deltaZscore_1a"))
  DATA: id=col(source(s), name("$CASENUM"), unit.category())
  GUIDE: axis(dim(1), label("GENERAL: Grupo de intervencion"))
  GUIDE: axis(dim(2), label("deltaZscore_1a"))
  SCALE: cat(dim(1), include("1", "2", "3"))
  SCALE: linear(dim(2), include(0))
  ELEMENT: schema(position(bin.quantile.letter(grup_int*deltaZscore_1a)), label(id))
END GPL.

*tercer intento. relacion OC y dmo.
COMPUTE deltaosteoprotegerina1a=osteoprotegerinav3 - osteoprotegerinav1.
EXECUTE.

COMPUTE delta@25OHvitD1a=@25OHvitDv3-@25OHvitDv1.
EXECUTE.

COMPUTE deltacalciosuero1a=calciosuerov3-calciosuerov1.
EXECUTE.

COMPUTE deltaPTH1a=PTHv3 - PTHv1.
EXECUTE.

COMPUTE deltafosalc_hepatica1a=fosalc_hepaticav3-fosalc_hepaticav1.
EXECUTE.

COMPUTE deltafosalc_osea1a=fosalc_oseav3 - fosalc_oseav1.
EXECUTE.

COMPUTE deltafosalc_intestinal1a=fosalc_intestinalv3-fosalc_intestinalv1.
EXECUTE.

COMPUTE deltafosalc_biliar1a=fosalc_biliarv3-fosalc_biliarv1.
EXECUTE.

COMPUTE deltacreatinina1a=creatininav3-creatininav1.
EXECUTE.

COMPUTE deltacal24hrs1a=cal24hrsv3 - cal24hrsv1.
EXECUTE.

COMPUTE deltadesoxcreati1a=desoxcreativ3-desoxcreativ1.
EXECUTE.

COMPUTE deltaOC_1a=OC_anual - OC_inicial.
EXECUTE.

COMPUTE deltaGLU_OC_1a=GLU_OC_anual-GLU_OC_inicial.
EXECUTE.

COMPUTE deltaGLU_OC_100_1a=GLU_OC_100_anual-GLU_OC_100_inicial.
EXECUTE.

COMPUTE deltacalciosuero1a=calciosuerov3-calciosuerov1.
EXECUTE.

COMPUTE deltaPTH1a=PTHv3 - PTHv1.
EXECUTE.

COMPUTE deltafosalc_hepatica1a=fosalc_hepaticav3-fosalc_hepaticav1.
EXECUTE.

COMPUTE deltafosalc_osea1a=fosalc_oseav3 - fosalc_oseav1.
EXECUTE.

COMPUTE deltafosalc_intestinal1a=fosalc_intestinalv3-fosalc_intestinalv1.
EXECUTE.

COMPUTE deltafosalc_biliar1a=fosalc_biliarv3-fosalc_biliarv1.
EXECUTE.

COMPUTE deltacreatinina1a=creatininav3-creatininav1.
EXECUTE.

COMPUTE deltacal24hrs1a=cal24hrsv3 - cal24hrsv1.
EXECUTE.

COMPUTE deltadesoxcreati1a=desoxcreativ3-desoxcreativ1.
EXECUTE.


UNIANOVA deltaDMO_1a WITH sexo imc1 tabaco0 AOTOT_AJUS diabetes0
  /METHOD=SSTYPE(3)
  /INTERCEPT=INCLUDE
  /CRITERIA=ALPHA(0.05)
  /DESIGN=sexo imc1 tabaco0  diabetes0 AOTOT_AJUS.

compute deltaolivatot = olivatot3 - olivatot.
execute.


RECODE acolivavir acolivavir3 acolivavir4 acolivatot acolivatot3 acolivatot4 (Lowest thru 30=1) (30 
    thru 60=2) (60 thru Highest=3) INTO rac_AOVEbas rac_AOVE1a rac_AOVE2a rac_AOTOTbas rac_AOTOT1a 
    rac_AOTOT2a.
VARIABLE LABELS  rac_AOVEbas 'raciones AOVE basal' /rac_AOVE1a 'raciones AOVE 1a' /rac_AOVE2a 
    'raciones AOVE 2a' /rac_AOTOTbas 'raciones AOTOT basal' /rac_AOTOT1a 'raciones AOTOT 1a' 
    /rac_AOTOT2a 'raciones AOTOT 2a'.
EXECUTE.

compute deltaAOVE1a = (acolivavir3 - acolivavir).
compute deltaAOVE2a = (acolivavir4 - acolivavir).
compute deltaAOTOT1a = (acolivatot3 - acolivatot).
compute deltaAOTOT2a = (acolivatot4 - acolivatot).
execute.

RECODE deltaAOVE1a deltaAOVE2a deltaAOTOT1a deltaAOTOT2a (0=1) (Lowest thru 0=1) (0 thru Highest=2) 
    INTO rac_delta1a rac_deltAOVE2a rac_deltAOtot1a rac_deltAOtot2a.
VARIABLE LABELS  rac_delta1a 'diferencia de raciones' /rac_deltAOVE2a 'diferencia de raciones' 
    /rac_deltAOtot1a 'diferencia de raciones' /rac_deltAOtot2a 'diferencia de raciones'.
EXECUTE.

compute deltaAOVE1_2a = (acolivavir4 - acolivavir3).
compute deltaAOTOT1_2a = (acolivatot4 - acolivatot3).
execute.

RECODE deltaAOVE1_2a deltaAOTOT1_2a (0=1) (Lowest thru 0=1) (0 thru Highest=2) INTO rac_delta1_2 
    rac_deltaotot1_2.
VARIABLE LABELS  rac_delta1_2 'diferencia AOVE 1-2 años' /rac_deltaotot1_2 'diferencia AOTOT 1-2 '+
    'años'.
EXECUTE.

compute deltaOC =OC_anual - OC_inicial.
execute.

ONEWAY deltaBUA_1a deltaDMO_1a deltaQUI_1a deltaSOS_1a BY rac_deltAOtot1a
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS.

compute deltaOC = OC_anual - OC_inicial.
execute.

compute media_actot2 = (acolivatot4+acolivatot3 + acolivatot)/3.
execute.

**ajuste de la variable aceite de oliva total y AOVE vista basal.
compute AOTOT = olivatot.
compute AOVE = ac_olivavir.
compute ENERGIA = energiat.
execute.
*calculo de media de la variable independiente.
DESCRIPTIVES VARIABLES=ENERGIA  
  /STATISTICS=MEAN STDDEV MIN MAX.
*generacion variable AOTOT ajustada.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT
  /METHOD=ENTER ENERGIA
  /SAVE PRED RESID. 

Compute AOTOT_AJUS= 41.22386091177626 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.
*generacion de variable AOVE ajustada.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE
  /METHOD=ENTER ENERGIA
  /SAVE PRED RESID. 

Compute AOVE_AJUS= 33.84794524505519 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1 AOTOT AOVE ENERGIA.
execute.

RANK VARIABLES= AOVE_AJUS(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

RANK VARIABLES= AOTOT_AJUS(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

*regresion supervivencia con imc1 getota_1 fractura1 diabetes0 t_cox_clas2. 
COXREG T_cox_new2
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NAOVE_AJ    imc1  fractura1    fum   m_estatin  sexo
  /CONTRAST (NAOVE_AJ)=Indicator 
  /METHOD=ENTER NAOVE_AJ 
 /PATTERN BY NAOVE_AJ
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

compute inter = 1.
if NAOTOT_A =1 OR NAOTOT_A=2 inter=1.
if NAOTOT_A=3 inter = 0.
execute.

*preparacion acumulative ingesta AOVE AOTO.
Compute ENERGIA = energiat.
Compute ENERGIA3 = energiat3.
Compute ENERGIA4 = energiat4.
Compute ENERGIA5 = energiat5.
Compute ENERGIA6 = energiat6.
Compute ENERGIA7 = energiat7.
Compute ENERGIA8 = energiat8.
Compute ENERGIA9 = energiat9.
Compute ENERGIA10 = energiat10.
compute AOVE = ac_olivavir.
compute AOVE3 = ac_olivavir3.
compute AOVE4 = ac_olivavir4.
compute AOVE5 = ac_olivavir5.
compute AOVE6 = ac_olivavir6.
compute AOVE7 = ac_olivavir7.
compute AOVE8 = ac_olivavir8.
compute AOVE9 = ac_olivavir9.
compute AOVE10 = ac_olivavir10.
compute AOTOT = olivatot.
compute AOTOT3 = olivatot3.
compute AOTOT4 = olivatot4.
compute AOTOT5 = olivatot5.
compute AOTOT6 = olivatot6.
compute AOTOT7 = olivatot7.
compute AOTOT8 = olivatot8.
compute AOTOT9 = olivatot9.
compute AOTOT10 = olivatot10.

DESCRIPTIVES VARIABLES=ENERGIA ENERGIA3 ENERGIA4 ENERGIA5 ENERGIA6 ENERGIA7 ENERGIA8 ENERGIA9 ENERGIA10
  /STATISTICS=MEAN STDDEV MIN MAX.
*generacion de variables AOVE ajustadas.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE
  /METHOD=ENTER ENERGIA
  /SAVE PRED RESID. 

Compute AOVE_AJUS= 33.84794524505519 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE3
  /METHOD=ENTER ENERGIA3
  /SAVE PRED RESID. 

Compute AOVE_AJUS3= 40.82497783515778 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE4
  /METHOD=ENTER ENERGIA4
  /SAVE PRED RESID. 

Compute AOVE_AJUS4= 42.74806267153795 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE5
  /METHOD=ENTER ENERGIA5
  /SAVE PRED RESID. 

Compute AOVE_AJUS5= 47.73156116971169 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE6
  /METHOD=ENTER ENERGIA6
  /SAVE PRED RESID. 

Compute AOVE_AJUS6= 49.36643057722155 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE7
  /METHOD=ENTER ENERGIA7
  /SAVE PRED RESID. 

Compute AOVE_AJUS7= 49.87720504221999 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE8
  /METHOD=ENTER ENERGIA8
  /SAVE PRED RESID. 

Compute AOVE_AJUS8= 49.39090094877229 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE9
  /METHOD=ENTER ENERGIA9
  /SAVE PRED RESID. 

Compute AOVE_AJUS9= 49.24317079912152 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOVE10
  /METHOD=ENTER ENERGIA10
  /SAVE PRED RESID. 

Compute AOVE_AJUS10= 56.66665769423409 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

*generacion de variables AOTOT ajustadas.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT
  /METHOD=ENTER ENERGIA
  /SAVE PRED RESID. 

Compute AOTOT_AJUS= 33.84794524505518 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT3
  /METHOD=ENTER ENERGIA3
  /SAVE PRED RESID. 

Compute AOTOT_AJUS3= 40.82497783515778 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT4
  /METHOD=ENTER ENERGIA4
  /SAVE PRED RESID. 

Compute AOTOT_AJUS4= 42.74806267153795 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT5
  /METHOD=ENTER ENERGIA5
  /SAVE PRED RESID. 

Compute AOTOT_AJUS5= 47.73156116971169 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT6
  /METHOD=ENTER ENERGIA6
  /SAVE PRED RESID. 

Compute AOTOT_AJUS6= 49.36643057722155 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT7
  /METHOD=ENTER ENERGIA7
  /SAVE PRED RESID. 

Compute AOTOT_AJUS7= 49.87720504221999 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT8
  /METHOD=ENTER ENERGIA8
  /SAVE PRED RESID. 

Compute AOTOT_AJUS8= 49.39090094877229 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT9
  /METHOD=ENTER ENERGIA9
  /SAVE PRED RESID. 

Compute AOTOT_AJUS9= 49.24317079912152 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT AOTOT10
  /METHOD=ENTER ENERGIA10
  /SAVE PRED RESID. 

Compute AOTOT_AJUS10= 56.66665769423409 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

delete variables ENERGIA .
delete variables ENERGIA3 .
delete variables ENERGIA4 .
delete variables ENERGIA5 .
delete variables ENERGIA6 .
delete variables ENERGIA7 .
delete variables ENERGIA8. 
delete variables ENERGIA9 .
delete variables ENERGIA10.
delete variables AOVE .
delete variables AOVE3 .
delete variables AOVE4 .
delete variables AOVE5 .
delete variables AOVE6 .
delete variables AOVE7 .
delete variables AOVE8 .
delete variables AOVE9 .
delete variables AOVE10 .
delete variables AOTOT .
delete variables AOTOT3 .
delete variables AOTOT4 .
delete variables AOTOT5 .
delete variables AOTOT6 .
delete variables AOTOT7 .
delete variables AOTOT8 .
delete variables AOTOT9 .
delete variables AOTOT10 .

*creación de variable de media de AOVE AOTOT.
RECODE T_cox_new2 (0 thru 1=3) (1 thru 2=4) (2 thru 3=5) (3 thru 4=6) (4
    thru 5=7) (5 thru 6=8) (6 thru 7=9) (7 thru Highest=10) INTO v_final_evento2.
VARIABLE LABELS  v_final_evento2 'visita final de seguimiento o hasta aparición de evento fractura osteo'.
EXECUTE.

COMPUTE media_AOVE = AOVE. 
IF ((v_final_evento2=10)) media_AOVE = mean(AOVE, AOVE3, AOVE4, AOVE5, AOVE6, AOVE7, AOVE8, AOVE9, AOVE10).
IF ((v_final_evento2=9)) media_AOVE = mean(AOVE, AOVE3, AOVE4, AOVE5, AOVE6, AOVE7, AOVE8).
IF ((v_final_evento2=8)) media_AOVE = mean(AOVE, AOVE3, AOVE4, AOVE5, AOVE6, AOVE7).
IF ((v_final_evento2=7)) media_AOVE = mean(AOVE, AOVE3, AOVE4, AOVE5, AOVE6).
IF ((v_final_evento2=6)) media_AOVE = mean(AOVE, AOVE3, AOVE4, AOVE5).
IF ((v_final_evento2=5)) media_AOVE = mean(AOVE, AOVE3, AOVE4).
IF ((v_final_evento2=4)) media_AOVE = mean(AOVE, AOVE3).
IF ((v_final_evento2=3)) media_AOVE = mean(AOVE).
VARIABLE LABELS media_AOVE 'media ingesta AOVE ajus'.
execute.

COMPUTE media_AOTOT = AOTOT. 
IF ((v_final_evento2=10)) media_AOTOT = mean(AOTOT, AOTOT3, AOTOT4, AOTOT5, AOTOT6, AOTOT7, AOTOT8, AOTOT9, AOTOT10).
IF ((v_final_evento2=9)) media_AOTOT = mean(AOTOT, AOTOT3, AOTOT4, AOTOT5, AOTOT6, AOTOT7, AOTOT8).
IF ((v_final_evento2=8)) media_AOTOT = mean(AOTOT, AOTOT3, AOTOT4, AOTOT5, AOTOT6, AOTOT7).
IF ((v_final_evento2=7)) media_AOTOT = mean(AOTOT, AOTOT3, AOTOT4, AOTOT5, AOTOT6).
IF ((v_final_evento2=6)) media_AOTOT = mean(AOTOT, AOTOT3, AOTOT4, AOTOT5).
IF ((v_final_evento2=5)) media_AOTOT = mean(AOTOT, AOTOT3, AOTOT4).
IF ((v_final_evento2=4)) media_AOTOT = mean(AOTOT, AOTOT3).
IF ((v_final_evento2=3)) media_AOTOT = mean(AOTOT).
VARIABLE LABELS media_AOTOT 'media ingesta AOTOT ajus'.
execute.

RANK VARIABLES= media_AOVE_AJ(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

RANK VARIABLES= media_AOTOT_AJ(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

*regresion supervivencia con imc1 getota_1 fractura1 diabetes0 t_cox_new2. 
COXREG T_cox_new2
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NTI006    imc1  sexo  edad0  m_estatin grup_int
  /CONTRAST (NTI006)=Indicator (1)
  /METHOD=ENTER NTI006 
 /PATTERN BY NTI006
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

compute inter1 = 1.
if Nmedia_A =2 OR Nmedia_A=3 inter1=1.
if Nmedia_A=1 inter1 = 0.
execute.

compute MEDIA_AOT = (aotot_ajus + aotot_ajus3)/2.
compute MEDIA_AOV = (aove_ajus + aove_ajus3)/2.
RANK VARIABLES= media_AOVE_AJ(A)
  /RANK
  /NTILES(2)
  /PRINT=YES
  /TIES=MEAN.
RANK VARIABLES= MEDIA_AOV(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

*nuevo análisis datos nutricionales hasta 4a.
COMPUTE media_AOVE_4a = AOVE_AJUS. 
IF ((v_final_evento2=10)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5, AOVE_AJUS6).
IF ((v_final_evento2=9)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5, AOVE_AJUS6).
IF ((v_final_evento2=8)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5, AOVE_AJUS6).
IF ((v_final_evento2=7)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5, AOVE_AJUS6).
IF ((v_final_evento2=6)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5).
IF ((v_final_evento2=5)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4).
IF ((v_final_evento2=4)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3).
IF ((v_final_evento2=3)) media_AOVE_4a = mean(AOVE_AJUS).
VARIABLE LABELS media_AOVE_4a 'media ingesta AOVE ajus hasta visita 6'.
execute.

COMPUTE media_AOTOT_4a = AOTOT_AJUS. 
IF ((v_final_evento2=10)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5, AOTOT_AJUS6).
IF ((v_final_evento2=9)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5, AOTOT_AJUS6).
IF ((v_final_evento2=8)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5, AOTOT_AJUS6).
IF ((v_final_evento2=7)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5, AOTOT_AJUS6).
IF ((v_final_evento2=6)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5).
IF ((v_final_evento2=5)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4).
IF ((v_final_evento2=4)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3).
IF ((v_final_evento2=3)) media_AOTOT_4a = mean(AOTOT_AJUS).
VARIABLE LABELS media_AOTOT_4a 'media ingesta AOTOT ajus hasta visita 6'.
execute.

RANK VARIABLES= media_AOTOT_4a(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
RANK VARIABLES= media_AOVE_4a(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

*regresion supervivencia con imc1 getota_1 fractura1 diabetes0 t_cox_new2. 
COXREG T_cox_new2
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NTI002    imc1  sexo  edad0  m_estatin grup_int
  /CONTRAST (NTI002)=Indicator 
  /METHOD=ENTER NTI002 
 /PATTERN BY NTI002
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*nuevo analisis con frutos secos datos actualizados 31-08-2014.
Compute ENERGIA = energiat.
Compute ENERGIA3 = energiat3.
Compute ENERGIA4 = energiat4.
Compute ENERGIA5 = energiat5.
Compute ENERGIA6 = energiat6.
Compute ENERGIA7 = energiat7.
Compute ENERGIA8 = energiat8.
Compute ENERGIA9 = energiat9.
Compute ENERGIA10 = energiat10.
compute secos = fsecos.
compute secos3 = fsecos3.
compute secos4 = fsecos4.
compute secos5 = fsecos5.
compute secos6 = fsecos6.
compute secos7 = fsecos7.
compute secos8 = fsecos8.
compute secos9 = fsecos9. 
compute secos10 = fsecos10.
*media sin ajustar todo seguimiento.
COMPUTE media_secos = secos. 
IF ((v_final_evento2=10)) media_secos = mean(secos, secos3, secos4, secos5, secos6, secos7, secos8, secos9, secos10).
IF ((v_final_evento2=9)) media_secos = mean(secos, secos3, secos4, secos5, secos6, secos7, secos8).
IF ((v_final_evento2=8)) media_secos = mean(secos, secos3, secos4, secos5, secos6, secos7).
IF ((v_final_evento2=7)) media_secos = mean(secos, secos3, secos4, secos5, secos6).
IF ((v_final_evento2=6)) media_secos = mean(secos, secos3, secos4, secos5).
IF ((v_final_evento2=5)) media_secos = mean(secos, secos3, secos4).
IF ((v_final_evento2=4)) media_secos = mean(secos, secos3).
IF ((v_final_evento2=3)) media_secos = mean(secos).
VARIABLE LABELS media_secos 'media ingesta frustos secos sin ajuseguimiento'.
execute.
*terciles de ingesta.
RANK VARIABLES= media_secos(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*tcox con terciles de ingesta media de frutos secos todo el seguimiento --> no hay diferencias.
COXREG T_cox_new2
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NTI004    imc1  sexo  edad0  m_estatin 
  /CONTRAST (NTI004)=Indicator 
  /METHOD=ENTER NTI004 
 /PATTERN BY NTI004
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
*media sin ajustar todo el seguimiento hasta 4a.
compute media_secos2 = secos.
IF ((v_final_evento2=10)) media_secos2 = mean(secos, secos3, secos4, secos5, secos6).
IF ((v_final_evento2=9)) media_secos2 = mean(secos, secos3, secos4, secos5, secos6).
IF ((v_final_evento2=8)) media_secos2 = mean(secos, secos3, secos4, secos5, secos6).
IF ((v_final_evento2=7)) media_secos2 = mean(secos, secos3, secos4, secos5, secos6).
IF ((v_final_evento2=6)) media_secos2 = mean(secos, secos3, secos4, secos5).
IF ((v_final_evento2=5)) media_secos2 = mean(secos, secos3, secos4).
IF ((v_final_evento2=4)) media_secos2 = mean(secos, secos3).
IF ((v_final_evento2=3)) media_secos2 = mean(secos).
VARIABLE LABELS media_secos2 'media ingesta frustos secos sin ajuste seguimiento 4a'.
execute.
*terciles de ingesta.
RANK VARIABLES= media_secos2(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*tcox con terciles de ingesta media de frutos secos todo el seguimiento --> no hay diferencias.
COXREG T_cox_new2
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NTI002    imc1  sexo  edad0  m_estatin 
  /CONTRAST (NTI002)=Indicator 
  /METHOD=ENTER NTI002 
 /PATTERN BY NTI002
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*nuevo analisis con frutos secos datos actualizados 31-08-2014. Ajuste de variables.PENDIENTE ANALISIS CON AJUSTES.

*nuevo análisis con datos actualizados 31-08-2014 incluyendo TGN.
* Asistente de fecha y hora: T_cox_fract3 (creación de la variable numérica) Muestra los años que pasan hasta el diagnóstico de la fractura y durante todo el seguimiento.
*Creacion de la variable T_cox_nueva -> años que pasan hasta que aparece fractura osteo según nueva definicion.
COMPUTE  T_cox_fract3=(fract1_data - datinclu) / (365.25 * time.days(1)).
VARIABLE LABELS  T_cox_fract3   "años que pasan hasta el diagnóstico de fractura"+
    "durante todo el seguimento".
VARIABLE LEVEL  T_cox_fract3 (SCALE).
FORMATS  T_cox_fract3 (F8.2).
VARIABLE WIDTH  T_cox_fract3(8).
EXECUTE.
*Creacion variable de seguimiento hasta final de estudio.
COMPUTE  T_cox_seg3=(data_fin2-datinclu) / (365.25 * time.days(1)).
VARIABLE LABELS  T_cox_seg3  "años de seguimento hasta fin de estudio 31-08-2014".
VARIABLE LEVEL T_cox_seg3 (SCALE).
FORMATS  T_cox_seg3(F8.2).
VARIABLE WIDTH  T_cox_seg3(8).
EXECUTE.
*Creacion de variable seguimiento hasta fractura 2.
COMPUTE  T_cox_fract23=(fract2_data - datinclu) / (365.25 * time.days(1)).
VARIABLE LABELS  T_cox_fract23   "años que pasan hasta el diagnóstico de 2º fractura"+
    "durante todo el seguimento 31-08-2014".
VARIABLE LEVEL  T_cox_fract23 (SCALE).
FORMATS  T_cox_fract23 (F8.2).
VARIABLE WIDTH  T_cox_fract23(8).
EXECUTE.
*Creacion de la variable T_cox_new3 -> años que pasan hasta que aparece fractura osteo según definicion clasica teniendo en cuenta fractura 1 y fractura 2 31-08-2014.
compute T_cox_new3 = 100.
If fract1 =1 AND fract1_new =1  T_cox_new3=T_cox_fract3.
If fract1 =1 AND fract1_new =0 OR fract1= 0 OR fract2=1 AND fract2_new=0 T_cox_new3=T_cox_seg3.
if fract1=1 AND fract1_new=0 AND fract2=1 AND fract2_new=1 T_cox_new3=T_cox_fract23.
VARIABLE LABELS  T_cox_new3   "años que pasan hasta el diagnóstico de fractura osteo definicion clasica"+ "durante todo el seguimento 31-08-2014".
EXECUTE.
*nuevo análisis datos nutricionales hasta 4a con nueva T_cox_new3.
*creación de variable de media de AOVE AOTOT.
RECODE T_cox_new3 (0 thru 1=3) (1 thru 2=4) (2 thru 3=5) (3 thru 4=6) (4
    thru 5=7) (5 thru 6=8) (6 thru 7=9) (7 thru Highest=10) INTO v_final_evento2.
VARIABLE LABELS  v_final_evento2 'visita final de seguimiento o hasta aparición de evento fractura osteo'.
EXECUTE.
COMPUTE media_AOVE_4a = AOVE_AJUS. 
IF ((v_final_evento2=10)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5, AOVE_AJUS6).
IF ((v_final_evento2=9)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5, AOVE_AJUS6).
IF ((v_final_evento2=8)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5, AOVE_AJUS6).
IF ((v_final_evento2=7)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5, AOVE_AJUS6).
IF ((v_final_evento2=6)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4, AOVE_AJUS5).
IF ((v_final_evento2=5)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3, AOVE_AJUS4).
IF ((v_final_evento2=4)) media_AOVE_4a = mean(AOVE_AJUS, AOVE_AJUS3).
IF ((v_final_evento2=3)) media_AOVE_4a = mean(AOVE_AJUS).
VARIABLE LABELS media_AOVE_4a 'media ingesta AOVE ajus hasta visita 6'.
execute.

COMPUTE media_AOTOT_4a = AOTOT_AJUS. 
IF ((v_final_evento2=10)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5, AOTOT_AJUS6).
IF ((v_final_evento2=9)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5, AOTOT_AJUS6).
IF ((v_final_evento2=8)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5, AOTOT_AJUS6).
IF ((v_final_evento2=7)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5, AOTOT_AJUS6).
IF ((v_final_evento2=6)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4, AOTOT_AJUS5).
IF ((v_final_evento2=5)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3, AOTOT_AJUS4).
IF ((v_final_evento2=4)) media_AOTOT_4a = mean(AOTOT_AJUS, AOTOT_AJUS3).
IF ((v_final_evento2=3)) media_AOTOT_4a = mean(AOTOT_AJUS).
VARIABLE LABELS media_AOTOT_4a 'media ingesta AOTOT ajus hasta visita 6'.
execute.
*tcox con terciles de ingesta media de AOTOT y AOVE 4a 31-08-2014.
RANK VARIABLES= media_AOVE_AJ(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
*no sale.
COXREG T_cox_new3
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NTI005   imc1  sexo  edad0  m_estatin  grup_int 
  /CONTRAST (NTI005)=Indicator (1)
  /METHOD=ENTER NTI005
 /PATTERN BY NTI005
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new3
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER inter7   imc1  sexo  edad0  m_estatin grup_int 
  /CONTRAST (inter7)=Indicator (1)
  /METHOD=ENTER inter7
 /PATTERN BY inter7
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new3
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER inter9   imc1  sexo  edad0  m_estatin grup_int fractura1 diabetes0 sintrom tabaco0
  /CONTRAST (inter9)=Indicator (1)
  /METHOD=ENTER inter9
 /PATTERN BY inter9
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*creacion de la variable AOVE_AJus media con media de energia.
Compute ENERGIA = energiat.
Compute ENERGIA3 = energiat3.
Compute ENERGIA4 = energiat4.
Compute ENERGIA5 = energiat5.
Compute ENERGIA6 = energiat6.
Compute ENERGIA7 = energiat7.
Compute ENERGIA8 = energiat8.
Compute ENERGIA9 = energiat9.
Compute ENERGIA10 = energiat10.
COMPUTE media_energia = 100. 
IF ((v_final_evento2=10)) media_energia = mean(energia, energia3, energia4, energia5, energia6, energia7, energia8, energia9, energia10).
IF ((v_final_evento2=9)) media_energia = mean(energia, energia3, energia4, energia5, energia6, energia7, energia8).
IF ((v_final_evento2=8)) media_energia = mean(energia, energia3, energia4, energia5, energia6, energia7).
IF ((v_final_evento2=7)) media_energia = mean(energia, energia3, energia4, energia5, energia6).
IF ((v_final_evento2=6)) media_energia = mean(energia, energia3, energia4, energia5).
IF ((v_final_evento2=5)) media_energia = mean(energia, energia3, energia4).
IF ((v_final_evento2=4)) media_energia = mean(energia, energia3).
IF ((v_final_evento2=3)) media_energia = mean(energia).
VARIABLE LABELS media_energia 'media ingesta  energia'.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT media_AOVE
  /METHOD=ENTER media_energia
  /SAVE PRED RESID. 

Compute media_aove_ajust2= 43.068941 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

RANK VARIABLES= media_aove_ajust2(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

COXREG T_cox_new3
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NTI001   imc1  sexo  edad0  m_estatin  fractura1 diabetes0 sintrom tabaco0 grup_int 
  /CONTRAST (NTI001)=Indicator (1)
  /METHOD=ENTER NTI001
 /PATTERN BY NTI001
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

RECODE Nmedia_a (Lowest thru 1=0) (ELSE=1) INTO inter10.
VARIABLE LABELS  inter10 't1 frente t2-3 AOVE ajustada metodo 2 31-08-2014'.
EXECUTE.
*modelo 1 --> OK.
COXREG T_cox_new3
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER inter10 sexo  edad0   grup_int
  /CONTRAST (inter10)=Indicator (1)
  /METHOD=ENTER inter10
 /PATTERN BY inter10
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
*modelo 2--> ok.
COXREG T_cox_new3
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER inter10  m_insulin ado1 m_diuret   vitamin1 hta0  m_aas hipercol0 m_estatin sintrom imc1  sexo  edad0  fractura1 diabetes0  tabaco0 escolar1 grup_int media_getot imc1
  /CONTRAST (inter10)=Indicator (1)
  /METHOD=ENTER inter10
 /PATTERN BY inter10
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT media_AOTOT
  /METHOD=ENTER media_energia
  /SAVE PRED RESID. 

Compute media_aotot_ajust2= 47.435052 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

RANK VARIABLES= media_aotot_ajust2(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.
RECODE Nmedia_a (Lowest thru 1=0) (ELSE=1) INTO inter11.
*modelo 1 --> no OK pero en linea.
COXREG T_cox_new3
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER inter10     sexo  edad0   grup_int
  /CONTRAST (inter10)=Indicator (1)
  /METHOD=ENTER inter10
 /PATTERN BY inter10
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
*modelo 2--> no ok pero en linea.
COXREG T_cox_new3
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER inter10  m_insulin ado1 m_diuret   vitamin1 hta0  m_aas hipercol0 m_estatin sintrom imc1  sexo  edad0  fractura1 diabetes0  tabaco0 escolar1 grup_int media_getot imc1
  /CONTRAST (inter10)=Indicator (1)
  /METHOD=ENTER inter10
 /PATTERN BY inter10
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
