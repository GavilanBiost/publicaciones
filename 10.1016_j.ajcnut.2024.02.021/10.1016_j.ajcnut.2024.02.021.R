library(readxl)
library(missForest)
library(RNOmni)
library(haven)
library(rio)
library(qvalue)
library(dynamicTreeCut)
library(cluster)
library(flashClust)
library(Hmisc)
library(reshape)
library(foreach)
library(doParallel)
library(WGCNA)
library(rio)
library(corrplot)
library(ppcor)
library(doParallel)
registerDoParallel(cores = 12)

# Fase 1: Tratamiento de la base de datos. ####
######### Apertura de las base de datos (los datos ya están ajustados por hidratación de la muestra)
######### i.e. centralización a la mediana

C8pos = read_excel("E:/ARTICULOS/BBDD/PPLUS/Met Heces NIH/22_0809_PREDIMED_T2D_stool_C8-pos_modif_all.xlsx", 
                    sheet = "long_ID_16S_adj")
HILICneg = read_excel("E:/ARTICULOS/BBDD/PPLUS/Met Heces NIH/22_0809_PREDIMED_T2D_stool_HILIC-neg_modif_all.xlsx", 
           sheet = "long_ID_16S_adj")
HILICpos = read_excel("E:/ARTICULOS/BBDD/PPLUS/Met Heces NIH/22_0809_PREDIMED_T2D_stool_HILIC-pos_modif_all.xlsx", 
                       sheet = "long_ID_16S_adj")
C18neg = read_excel("E:/ARTICULOS/BBDD/PPLUS/Met Heces NIH/23_0214_PREDIMED_T2D_stool_C18-neg_modif_all.xlsx", 
                    sheet = "long_ID_16S_adj")

## Fusión de las BBDD:

BBDD = merge(C8pos, HILICneg, by = c("patient_ID","patient_ID_16S","sample_ID_16S",                     
                                     "sample_ID","sample","Time_point"))
BBDD = merge(BBDD, HILICpos, by = c("patient_ID","patient_ID_16S","sample_ID_16S",                     
                                    "sample_ID","sample","Time_point"))
BBDD = merge(BBDD, C18neg, by = c("patient_ID","patient_ID_16S","sample_ID_16S",                     
                                  "sample_ID","sample","Time_point"))
colnames(BBDD)

## Eliminación de los metabolitos QC:

drop <- c("PC.12.0.12.0..iSTD.", #C8
          "Thymine.d4..iSTD.", "Inosine.15N4..iSTD.", "Glycocholate.d4..iSTD.", #HILICneg 
          "Phenylalanine.d8..iSTD.", "Valine.d8..iSTD.", #HILICpos
          "15R-15-methyl PGA2 [iSTD]", "15S-15-methyl PGE2 [iSTD]","15S-15-methyl PGE1 [iSTD]") #C18neg
BBDD1 = BBDD[,!(names(BBDD) %in% drop)]

# Eliminación de meteabolitos duplicados: criterio 1: metabolito con más NAs. Criterio 2: metabolito con peor distribucion
# Criterio 3: estadistico de shapiro test peor (a número más pequeño, peor)

na_count1 <-sapply(BBDD1, function(BBDD1) sum(length(which(is.na(BBDD1)))))
na_count2 <-sapply(BBDD1, function(BBDD1) (100*sum(length(which(is.na(BBDD1))))/sum(length((BBDD1)))))
na_count <- cbind(names = rownames(na_count),"NA"=na_count1, "% NA"=na_count2)
export(na_count, "na_count.xlsx")

drop <- c("alpha.N.Phenylacetylglutamine.y","Aspartic.acid.y","Cer.18.1.O2.16.0.y",
          "Cer.18.1.O2.22.0.y","Cer.18.1.O2.24.1.y","Deoxycholic.acid.x","DG.34.1.y",
          "DG.34.2.y","DG.34.3.y","DG.36.2.y","DG.36.3.y","DG.36.4.y","Dihydrocholesterol.x",
          "Glutamic.acid.y","Glycocholic.acid.x","Glycodeoxycholic.acid.or.Glycochenodeoxycholic.acid.x",
          "Guanine.x","Guanosine.x","Hexose.y","Hypoxanthine.y","Inosine.x","L.Urobilin.y",
          "Linoleoyl.EA.y","LPC.16.0.y","LPC.18.0.y","LPC.18.1.y","LPC.18.2.y","LPC.O.16.0.x",
          "LPC.P.18.0.or.LPC.O.18.1.y","LPE.16.0.y","LPE.18.1.y","Myristoleic.acid.y",
          "N.Acetylglutamic.acid.y","Nicotinic.acid.y","Oleoyl.EA.y","Olmesartan.x",
          "Oxypurinol.x","Palmitoyl.EA.y","PC.30.0.y","PC.32.0.y","PC.34.1.y","PC.34.2.y",
          "PC.36.2.y","PC.36.3.y","PE.34.0.y","PE.34.2.y","PE.36.2.y","PE.P.34.2.or.PE.O.34.3.y",
          "PE.P.36.2.or.PE.O.36.3.y","Piperine.x","Pseudouridine.x","Ribothymidine.y",
          "Sebacic.acid.y","SM.18.1.O2.16.0.y","SM.18.1.O2.18.0.y","Sphingosine.y",
          "Taurocholic.acid.x","Uric.acid.y","X3.Methylxanthine.x","Xanthine.y","Lithocholic.acid.y",
          "Pantothenic.acid.y")

# para ver el motivo de la retirada de estos metabolitos ver na_count.xlsx

BBDD2 = BBDD1[,!(names(BBDD1) %in% drop)]

# eliminacion de los metabolitos con más de 20% NAs

na_count1 <-sapply(BBDD2, function(BBDD2) sum(length(which(is.na(BBDD2)))))
na_count2 <-sapply(BBDD2, function(BBDD2) (100*sum(length(which(is.na(BBDD2))))/sum(length((BBDD2)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

drop<-rownames(na_count[na_count[,2]>20 ,])
export(as.data.frame(drop), "drop_metabolitos_NAs.xlsx")

BBDD3 = BBDD2[,!(names(BBDD2) %in% drop)]

# eliminacion de participantes con más de 20% de NAs
BBDD4 = data.frame(t(BBDD3))

na_count1 <-sapply(BBDD4, function(BBDD4) sum(length(which(is.na(BBDD4)))))
na_count2 <-sapply(BBDD4, function(BBDD4) (100*sum(length(which(is.na(BBDD4))))/sum(length((BBDD4)))))
na_count <- cbind("NA"=na_count1, "% NA"=na_count2)

drop<-rownames(na_count[na_count[,2]>20 ,])

# No hay participantes con más de un 20% de NAs. Nos quedamos con BBDD3

rm(BBDD4, na_count, na_count1, na_count2, drop, BBDD, BBDD1, BBDD2)

## Fase 1.1: imputación de NAs y transformación logarítmica ####
registerDoParallel(cores=6)
set.seed(1)

IMP = missForest(as.matrix(BBDD3[7:538]), verbose = T, parallelize = "forest")

IMP$OOBerror

IMP = data.frame(IMP$ximp)

colnames(IMP)

BBDD4_log = data.frame(cbind(BBDD3[1:6], log(IMP)))
BBDD4 = data.frame(cbind(BBDD3[1:6], IMP))

export(BBDD4, "20230224_StoolMetClear.xlsx")
export(BBDD4_log, "20230224_StoolMetClear_log.xlsx")

rm(BBDD3, BBDD4, IMP, C18neg, C8pos, HILICneg, HILICpos)

###### Cambio de formato de la BBDD (long --> wide)

BBDD4_log1 = subset(BBDD4_log, Time_point == 0)
BBDD4_log2 = subset(BBDD4_log, Time_point == 1)

# Renombre:

a = colnames(BBDD4_log2)
export(as.matrix(a), "a.txt")

colnames(BBDD4_log2) = c("patient_ID","patient_ID_16S","sample_ID_16S","sample_ID","sample",
                         "Time_point","LPC.16.0.x.1","LPC.18.2.x.1","LPC.18.1.x.1","LPC.18.0.x.1","LPC.20.1.1","LPC.20.0.1","LPC.24.0.1","LPC.P.18.0.or.LPC.O.18.1.x.1",
                         "LPE.16.0.x.1","LPE.18.1.x.1","LPE.18.0.1","LPE.20.0.1","LPE.22.0.1","PC.30.0.x.1","PC.32.1.1","PC.32.0.x.1",
                         "PC.34.2.x.1","PC.34.1.x.1","PC.34.0.1","PC.36.4_A.1","PC.36.4_B.1","PC.36.3.x.1","PC.36.2.x.1","PC.36.1.1",
                         "PE.32.1.1","PE.34.2.x.1","PE.34.0.x.1","PE.36.4.1","PE.36.2.x.1","PE.P.34.2.or.PE.O.34.3.x.1","PE.P.36.2.or.PE.O.36.3.x.1",
                         "PE.P.36.1.or.PE.O.36.2.1","Sphingosine.x.1","Palmitoyl.EA.x.1","Linoleoyl.EA.x.1","Oleoyl.EA.x.1","X7.Dehydro.desmosterol.1",
                         "Cholesterol.1","Sitosterol.1","delta.Tocopherol.1","Cer.18.1.O2.16.0.x.1","Cer.18.1.O2.22.0.x.1","Cer.18.1.O2.24.1.x.1",
                         "Cer.18.1.O2.24.0.1","SM.18.1.O2.16.0.x.1","SM.18.1.O2.18.0.x.1","SM.18.1.O2.24.1.1","SM.18.1.O2.24.0.1","CE.16.0.1",
                         "CE.16.1.1","CE.18.0.1","CE.18.1.1","CE.18.2.1","CE.18.3.1","CE.20.3.1","CE.20.4.1","CE.20.5.1","CE.22.4.1","CE.22.6.1","DG.30.0.1","DG.32.0.1",
                         "DG.32.1.1","DG.32.2.1","DG.34.0.1","DG.34.1.x.1","DG.34.2.x.1","DG.34.3.x.1","DG.36.0.1","DG.36.1.1","DG.36.2.x.1","DG.36.3.x.1","DG.36.4.x.1",
                         "DG.38.4.1","MG.16.1.1","MG.18.0.1","TG.45.0.1","TG.48.0.1","TG.50.0.1","TG.50.1.1","TG.50.2.1","TG.50.3.1","TG.50.4.1",
                         "TG.51.2.1","TG.52.0.1","TG.52.2.1","TG.52.3.1","TG.52.4.1","TG.52.5.1","TG.52.6.1","TG.53.3.1","TG.53.2.1","TG.54.1.1","TG.54.2.1","TG.54.3.1","TG.54.4.1","TG.54.5.1",                         "TG.54.6.1","TG.54.7.1","TG.54.8.1","TG.54.9.1","TG.55.3.1","TG.56.1.1","TG.56.2.1","TG.56.3.1","TG.56.4.1","TG.56.5.1","TG.56.6.1","X1.Methylxanthine.1",
                         "Aminoadipic.acid.1","X2.Aminobutyric.acid.1","X2.Hydroxy.3.methylbutyric.acid.or.3.Hydroxyisovaleric.acid.1",
                         "X2.Hydroxy.3.methyl.pentanoic.acid.or.2.Hydroxyisocaproic.acid.1","X2.Hydroxyglutaric.acid.1",
                         "X3..3.Hydroxyphenyl.propanoic.acid.1","X3.Methyladipic.acid.or.Pimelic.acid.1","X4.Hydroxybenzaldehyde.1",
                         "X4.hydroxybenzeneacetonitrile.1","X4.Hydroxymandelic.acid.or.Homogentisic.acid.1","Adenine.1",
                         "Adipic.acid.or.Methylglutaric.acid.1","Adonitol.or.Arabitol.1","sn.Glycero.3.phosphate.1",
                         "X3.Methyl.2.oxovaleric.acid.or.Ketoleucine.1","Oxoglutaric.acid.1","Ketoisovaleric.acid.1",
                         "Glucosan.or.3.Hydroxymethylglutaric.acid.1","Aspartic.acid.x.1","Benzoic.acid.1","Butyric.acid.1",
                         "Citric.acid.or.Isocitric.acid.1","Erythronic.acid.or.Threonic.acid.1","Hexose.x.1","Fumaric.acid.or.Maleic.acid.1",
                         "Gentisic.acid.1","Glucuronic.acid.1","Glutamic.acid.x.1","Glyceric.acid.1","Hypoxanthine.x.1",
                         "X3.Indolepropionic.acid.1","Indolelactic.acid.1","Isovaleric.acid.or.Valeric.acid.1","Lactic.acid.1",
                         "Lithocholic.acid.x.1","Malic.acid.1","Malonic.acid.1","N.Acetylglutamic.acid.x.1","Nicotinic.acid.x.1",
                         "Orotic.acid.1","Oxalic.acid.1","Pantothenic.acid.x.1","Pentose.5.phosphate.1","Phenylacetic.acid.1",
                         "Propionic.acid.1","Quinic.acid.1","Sebacic.acid.x.1","Sorbitol.1","Suberic.acid.1","Succinic.acid.1",
                         "Sucrose.or.Lactose.or.Trehalose.1","Tartaric.acid.1","Tetrahydroharman.3.carboxylic.acid.1","Thymine.1",
                         "Uracil.1","Uric.acid.x.1","Uridine.1","Xanthine.x.1","Ecgonine.1","Cotinine.N.oxide.1","trans.3.Hydroxycotinine.1",
                         "Piperine.y.1","Harman.1","Nor.psi.tropine.1","Ethyl.N.ethylanthranilic.acid.1","Tyramine.1",
                         "Methyl.N.methylanthranilate.1","X3.Hydroxyanthranilic.acid.1","O.Desmethyltramadol.1","Acetaminophen.1",
                         "X3.Methoxytyramine.1","L.Metanephrine.1","Atenolol.1","Venlafaxine.1","N.Acetyl.D.galactosamine.1",
                         "Aminoisobutyric.acid.1","gamma.Aminobutyric.acid.1","Myristoleic.acid.x.1","Pantothenol.1","N.Oleoyl.glycine.1",
                         "Palmitoleoyl.EA.1","Stearamide.1","Putrescine.1","Cadaverine.1","Tetradecylamine.1","Carnitine.1",
                         "X3.Dehydroxycarnitine.1","CAR.2.0.1","CAR.3.0.1","CAR.4.0.1","CAR.DC3.0.2Me.1","CAR.7.0.1","CAR.14.0.1",
                         "CAR.16.0.1","CAR.18.1.1","CAR.18.0.1","CAR.18.0.OH.1","CAR.20.4.1","CAR.20.0.1","CAR.26.0.1","X7.Methyladenine.1",
                         "Guanine.y.1","X6.8.Dihydroxypurine.1","X1.Methylguanine.1","X7.Methylguanine.1","Deoxyinosine.1","Didanosine.1",
                         "Deoxyadenosine.1","Deoxyguanosine.1","Adenosine.1","Inosine.y.1","X1.Methyladenosine.1","Guanosine.y.1",
                         "Xanthosine.1","X1.Methylguanosine.1","Allopurinol.riboside.1","X3.Methylxanthine.y.1","X7.Methylxanthine.1",
                         "Caffeine.1","X1.7.Dimethyluric.acid.1","Ectoine.1","Deoxycytidine.1","Cytidine.1","Pseudouridine.y.1",
                         "X5.Methylcytidine.1","Ribothymidine.x.1","Cytosine.1","X5.Hydroxymethyl.4.methyluracil.1","Thiamine.1",
                         "Monoethylglycinexylidide.1","Glycine.1","Alanine.1","Serine.1","Proline.1","Betaine.1","Valine.1","Threonine.1",
                         "N.Methyl.proline.1","Pipecolic.acid.1","Hydroxyproline.1","Creatine.1","Alloisoleucine.1","Isoleucine.1","Leucine.1",
                         "Asparagine.1","Proline.betaine.1","X4.Acetamidobutanoic.acid.1","X4.Guanidinobutanoic.acid.1","Glutamine.1",
                         "Lysine.1","Methionine.1","S.Methylcysteine.S.oxide.1","Histidine.1","N.Acetylvaline.1","N.6..Methyllysine.1",
                         "Methionine.sulfoxide.1","Phenylalanine.1","X3.Methylhistidine.1","Gabapentin.1","N.Acetylleucine.1",
                         "N.Acetylornithine.1","Arginine.1","N.6..N.6..Dimethyl.lysine.1","N.Acetylaspartic.acid.1","Citrulline.1",
                         "Tyrosine.1","N.Acetylglutamine.1","N6.Acetyllysine.1","Targinine.1","N.6.Trimethyllysine.1","N.Acetylhistidine.1",
                         "Dimethylarginine.1","Symmetric.dimethyl.arginine.1","Tryptophan.1","Cinnamoylglycine.1","N.Acetylarginine.1",
                         "alpha.N.Phenylacetylglutamine.x.1","X3..N.Acetyl.L.cystein.S.yl..acetaminophen.1","Ala.Ala.1","Gly.Pro.1","Gly.Val.1",
                         "Ser.Ala.1","Pro.Ala.1","Leu.Gly.1","Ser.Pro.1","Ala.Ile.1","Lys.Gly.1","Pro.Pro.1","Val.Pro.1","Thr.Pro.1",
                         "Ala.Lys.1","Glu.Ala.1","Pro.hydroxyPro.1","Ile.Pro.1","Ile.Val.1","Asn.Val.1","Gly.Arg.1","Asp.Val.1","Val.Asp.1",
                         "Ile.Thr.1","Thr.Ile.1","Ala.Phe.1","Pro.Lys.1","Glu.Pro.1","Ile.Ile.1","Val.Lys.1","Val.Glu.1","Thr.Glu.1",
                         "His.Pro.1","His.Val.1","Gln.Ile.1","Ile.Lys.1","Lys.Ile.1","Ile.Glu.1","Pro.Phe.1","His.Ile.1","Arg.Pro.1",
                         "Val.Arg.1","Glu.Gln.1","Tyr.Pro.1","Leu.Phe.1","Phe.Ile.1","Val.Tyr.1","Phe.Glu.1","Trp.Val.1","Tyr.Lys.1",
                         "Phe.Phe.1","Ile.Trp.1","Phe.Tyr.1","Lisinopril.1","Anserine.1","Dopamine.1","DOPA.1","Ureidopropionic.acid.1",
                         "N.Acetylputrescine.1","N.Acetylputrescine1.1","N.Acetylcadaverine.1","N1.Acetylspermidine.1",
                         "N1.N12.Diacetylspermine.1","sn.Glycero.3.phosphocholine.1","Taurine.1","Diethanolamine.1","Histamine.1",
                         "X1.Methylhistamine.1","L.Histidinol.1","N.Acetylhistamine.1","Triethanolamine.1","Choline.1","Acetylcholine.1",
                         "Cyclohexylamine.1","Metformin.1","Methylguanidine.1","Agmatine.1","X3.Guanidinopropanoic.acid.1",
                         "X5.Acetylamino.6.amino.3.methyluracil.1","Creatinine.1","Desloratadine.1","X.E.E..Trichostachine.1","Biliverdin.1",
                         "D.Urobilinogen.1","L.Urobilin.x.1","Biotin.1","Imidazoleacetic.acid.1","Urocanic.acid.1","Imidazolepropionic.acid.1",
                         "Methylimidazoleacetic.acid.1","Indole.1","Tryptamine.1","Lenticin.1","Indolin.2.one.1","X2.Oxindole.3.acetate.1",
                         "Heme.1","Acisoga.1","Protoporphyrin.1","Pterin.1","Pyridoxal.1","Niacinamide.1","X1.Methyl.nicotinamide.1",
                         "N1.Methyl.2.pyridone.5.carboxamide.1","X4.Pyridoxic.acid.1","Pyridoxamine.1","Pyridoxine.1","Pyroglutamic.acid.1",
                         "Kynurenic.acid.1","Bilirubin.1","X5..2.Hydroxyethyl..4.methylthiazole.1","N.Methylserotonin.1",
                         "N.Acetylserotonin.1","Daidzein.1","Genistein.1","Glycitein.1","Cryptoxanthin.1","Retinol.1","X13.cis.Retinoic.acid.1",
                         "alpha.Tocopherol.1","X3.hydroxynorvaline.1","indole.4.carboxaldehyde.1","X2.amino.6.hydroxyhexanoic.acid.1",
                         "isoquinoline.1.5.diol.1","Triisopropanolamine.1","Tridecylamine.1","N.Acetylisoputreanine.1",
                         "N.N.dimethylhexadecylamine.1","glycidyl.linoleate.1","D.alpha.tocopherylquinone.1","PC.36.4.1","LPE.15.0.1",
                         "LPE.17.0.1","LPE.18.3.1","LPE.18.2.1","LPE.20.1.1","LPE.P.18.0.or.LPE.O.18.1.1","PE.P.34.1.or.PE.O.34.2.1",
                         "PE.30.0.1","PE.34.1.1","Cer.18.1.O2.4.0.1","Cer.18.1.O2.14.1.1","Cer.18.1.O2.14.0.1","Cer.18.1.O2.15.0.1",
                         "Cer.18.1.O2.16.2.1","Cer.18.1.O2.16.1.1","Cer.18.1.O2.17.1.1","Cer.18.1.O2.17.0.1","Cer.18.1.O2.18.1.1","Cer.18.1.O2.18.0.1","Cer.18.1.O2.19.0.1","Cer.18.1.O2.20.2.1","Cer.18.1.O2.20.1.1","Cer.18.1.O2.20.0.1","Cer.18.1.O2.22.1.1","Cer.18.1.O2.24.2.1","Phytosphingosine.1","Sphinganine.1","C20.Sphinganine.1","C16.Sphingosine.1","C16.Sphinganine.1","C17.Sphinganine.1","C20.Sphingosine.1","C22.Sphingosine.1","SM.18.1.O2.18.3.1","Glycodeoxycholic.acid.or.Glycochenodeoxycholic.acid.y.1","Glycocholic.acid.y.1","X7alpha.Hydroxy.4.cholesten.3.one.1","Dihydrocholesterol.y.1","Cholestenone.1","Solanidine.1","Paraxanthine.or.Theobromine.1","X2..O.Methyladenosine.or.3..O.Methyladenosine.1","Caprylic.acid.1","Capric.acid.1","Lauric.acid.1","Myristic.acid.1","Palmitic.acid.1","Margaric.acid.1","Stearic.acid.1","Nonadecylic.acid.1","Arachidic.acid.1","Palmitoleic.acid.1","X10Z.Heptadecenoic.acid.1","alpha.Linolenic.acid.1","Linoleic.acid.1","Oleic.acid.1","Nonadeca.10Z.enoic.acid.1","Eicosapentaenoic.acid.1","Arachidonic.acid.1","Eicosatrienoic.acid.1","Eicosadienoic.acid.1","Eicosenoic.acid.1","Docosahexaenoic.acid.1","X4.7.10.13.16.Docosapentaenoic.acid.1","Adrenic.acid.1","cis.Erucic.acid.1","Nervonic.acid.1","X11.Methyllauric.acid.1","Xi.17.Methyloctadecanoic.acid.1","X3.Methyladipic.acid.1","Azelaic.acid.1","Undecanedioic.acid.1","Tetradecanedioic.acid.1","Hexadecanedioic.acid.1","Eicosanedioic.acid.1","X2.Hydroxyoctanoic.acid.1","X2.Hydroxymyristic.acid.1","X2.Hydroxy.palmitic.acid.1","X2.Hydroxystearic.acid.1","Levulinic.acid.1","X13.HODE.1","X12.HETE.1","X20.Carboxy.LTB4.1","Palmitoyl.EA.1","Phytanic.acid.1","Oleanolic.acid.1","Maslinic.acid.1","X9.cis.Retinoic.acid.1","Carnosol.1","Dehydrolithocholic.acid.1","Isolithocholic.acid.1","Deoxycholic.acid.y.1","X7.Ketodeoxycholic.acid.1","Cholic.acid.1","Glycochenodeoxycholic.acid.1","Glycodeoxycholic.acid.1","Glycocholic.acid.1","Taurochenodeoxycholic.acid.1","Taurocholic.acid.y.1","Leucocholic.acid.1","Phenylalanocholic.acid.1","Tyrosocholic.acid.1","Androsterone.3.glucuronide.1","p.Hydroxyphenylacetic.acid.1","Theophylline.1","X1.3.Dimethyluric.acid.1","Hexose.1","N.Acetyltyrosine.1","Porphobilinogen.1","Saccharin.1","Hydrocinnamic.acid.1","Phenyllactic.acid.1","Olmesartan.y.1","Carboxyibuprofen.1","Oxypurinol.y.1","Acesulfame.1","Simvastatin.1","LPC.O.16.0.y.1")

BBDD4_log1$Time_point = NULL
BBDD4_log2$Time_point = NULL
BBDD4_log1$sample_ID_16S = NULL
BBDD4_log2$sample_ID_16S = NULL
BBDD4_log1$sample_ID = NULL
BBDD4_log2$sample_ID = NULL

BBDD4_log_wide = merge(BBDD4_log1, BBDD4_log2, by = c("patient_ID","patient_ID_16S","sample"))

BBDD4_log_w_deltas = data.frame(cbind(BBDD4_log_wide[1:3],BBDD4_log_wide[536:1067] - BBDD4_log_wide[4:535]))
colnames(BBDD4_log_w_deltas)
# Consideraciones: no se cambia el nombre de las variables. Las variables delta acaban en .1


### Creacion de redes

# Creación de modulos basales:

BBDD4_basal = BBDD4_log1

# Outliers

arbol = hclust(dist(BBDD4_basal[4:535]), method = "average") #outliers
plot(arbol,main = "Sample clustering to detect outliers",
     sub="", xlab="", cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
abline(h = 52, col = "red");
clusters = WGCNA::cutreeStatic(arbol, cutHeight = 52)
table(clusters)
keepSamples = (clusters==1)
BBDD4_basal2 = BBDD4_basal[keepSamples, ]

rm(clusters, keepSamples)

# Creacion de los modulos:

## Soft-thresholding power:

powers = c(c(1:10), seq(from = 12, to=20, by=2))
sft = WGCNA::pickSoftThreshold(BBDD4_basal2[4:535], powerVector = powers)
png(filename = "Soft-thresholding power.png", width = 30, height = 20, res=300, unit="cm")
par(mfrow = c(1,2))
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold β",ylab="Signed R^2",type="n",
     main = paste("Scale independence"))
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=0.6,col="red")
abline(h=0.88,col="red")
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold β",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=0.6,col="red")
dev.off() #Power 4

## Matriz de adyacencia

adj = WGCNA::adjacency(BBDD4_basal2[4:535], power = 4, type = "signed")
TOM = WGCNA::TOMsimilarity(adj)
disTOM = 1-TOM

## Clustering

arbol2 = hclust(as.dist(disTOM), method = "average")

png(filename = "Clusterización basal.png", width = 30, height = 20, res=300, unit="cm")
plot(arbol2, xlab="", sub="", main = "Clustering on TOM-based dissimilarity baseline",
     labels = FALSE, hang = 0.04)
dev.off()

minModuleSize = 5

dynamicMods = cutreeDynamic(dendro = arbol2, distM = disTOM, 
                            deepSplit = 4, pamRespectsDendro = T, pamStage = T,
                            minClusterSize = minModuleSize)
table(dynamicMods) # 17 modulos 

Colores = labels2colors(dynamicMods)
table(Colores)

#Dendograma con los plots
png(filename = "Clusterización basal colores.png", width = 30, height = 20, res=300, unit="cm")
WGCNA::plotDendroAndColors(arbol2, Colores, "Module Colors",
                           dendroLabels = FALSE, hang = 0.03,
                           addGuide = TRUE, guideHang = 0.05,
                           main = "Metabolite cluster dendogram baseline")
dev.off()

png(filename = "escalado de los modulos.png", width = 30, height = 20, res=300, unit="cm")
cmd1=cmdscale(as.dist(disTOM),2) 
plot(cmd1, col=as.character(dynamicMods),  main="MDS plot",xlab="Scaling Dimension 1",
     ylab="Scaling Dimension 2", cex.axis=1.5,cex.lab=1.5, cex.main=1.5) 
dev.off()
rm(cmd1)

# Fusión de los módulos similares:
# Calculate eigengenes
MEList = WGCNA::moduleEigengenes(BBDD4_basal2[4:535], colors = Colores)
MEs = MEList$eigengenes

# Calculo la dis-similaridad entre los módulos.
MEDis = 1 - round(cor(MEs),6)

arbol3 = hclust(as.dist(MEDis), method = "average")

png(filename = "Clusterización modulos.png", width = 30, height = 20, res=300, unit="cm")
plot(arbol3, main = "Clustering of eigenmetabolite modules",
     xlab = "", sub = "")
MEDissThres = 0.25 # We choose a height cut of 0.25 (ref: 10.1016/j.chest.2018.05.038//10.1186/s12916-015-0282-y)
abline(h=MEDissThres, col = "red") # Plot the cut line into the dendrogram
dev.off()

# Fusion modulos cercanos
merge = WGCNA::mergeCloseModules(BBDD4_basal2[4:535], MEs = MEs, Colores, cutHeight = 0.25,iterate = TRUE, verbose = 3)
nColores = merge$colors
table(nColores)
nMEs = merge$newMEs

png(filename = "Arbol dinamico basal merged 0.25.png", width = 30, height = 20, res=300, unit="cm")
WGCNA::plotDendroAndColors(arbol2, nColores,
                           c("module colors"),
                           dendroLabels = FALSE, hang = 0.03,
                           addGuide = TRUE, guideHang = 0.05,
                           main = "Metabolite Cluster Dendogram at the baseline")
dev.off()

# Construct numerical labels corresponding to the colors
#ordenColores = c("grey", standardColors(50))
#etiquetasmol = match(nColores, ordenColores)-1
#save(nMEs, etiquetasmol, nColores, arbol2, file = "redes_pasoapaso.RData")

names(BBDD4_basal2)
table(nColores)

modulos = data.frame(cbind(names(BBDD4_basal2[4:535]),nColores))
names <- read_excel("E:/ARTICULOS/BBDD/PPLUS/Met Heces NIH/names.xlsx")
colnames(modulos) = c("names.R", "nColores")
modulos = merge(modulos, names, by = "names.R")
table(modulos$nColores)

export(modulos, "modulos.xlsx")

# Estimacion de la conectividad modular:
CM_basal = intramodularConnectivity(adj, colors = nColores)
CM_basal_df = data.frame(names.R = rownames(CM_basal), nColores, CM_basal)
export(CM_basal_df, "CM_basal_df.xlsx")

# Analisis 1 año:
BBDD4_1a = BBDD4_log2

# Outliers:
arbol1a = hclust(dist(BBDD4_1a[4:535]), method = "average") #outliers
par(cex = 0.6)
par(mar = c(0,4,2,0)) 
plot(arbol1a,main = "Sample clustering to detect outliers",
     sub="", xlab="", cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
abline(h = 50, col = "red");
clusters = WGCNA::cutreeStatic(arbol1a, cutHeight = 50)
table(clusters)
keepSamples = (clusters==1)
BBDD4_1a2 = BBDD4_1a[keepSamples, ]

rm(keepSamples, clusters)

## Matriz de adyacencia 1 año

adj1 = WGCNA::adjacency(BBDD4_1a2[4:535], power = 4, type = "signed")
TOM1 = WGCNA::TOMsimilarity(adj1)
disTOM1 = 1-TOM1

## Clustering

arbol1a2 = hclust(as.dist(disTOM1), method = "average")

png(filename = "Clusterización 1a.png", width = 30, height = 20, res=300, unit="cm")
plot(arbol1a2, xlab="", sub="", main = "Clustering on TOM-based dissimilarity 1-year",
     labels = FALSE, hang = 0.04)
dev.off()

# Para ver que modulos se mantienen al año, asigno los colores del basal al arbolito del año:

png(filename = "Clusterización 1a colores.png", width = 30, height = 20, res=300, unit="cm")
plotDendroAndColors(arbol1a2, nColores, "Module Colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05,
                    main = "Metabolite cluster dendogram 1-year")
dev.off()

# Eigenvalues 1 año:
MEList1a = WGCNA::moduleEigengenes(BBDD4_1a2[4:535], colors = nColores)
MEs1a = MEList1a$eigengenes
colnames(MEs1a)
# Conectividad modular 1a:
CM_1a = intramodularConnectivity(adj1, colors = nColores)
CM_1a_df = data.frame(names.R = rownames(CM_1a), nColores, CM_1a)
export(CM_1a_df, "CM_1a_df.xlsx")

# Regresiones lineales cambios entre modulos:
PPLUS <- read_sav("E:/ARTICULOS/BBDD/PPLUS/PREDIMEDplus_2023-01-18.sav")
PPLUS1 = data.frame(cbind(PPLUS$paciente, PPLUS$sexo_s1, PPLUS$diab_prev_s1, PPLUS$edad_s1,
                          PPLUS$grupo_int_v00, PPLUS$nodo))
colnames(PPLUS1) = c("ID", "sexo_s1", "diab_prev_s1", "edad_s1", "grupo_int_v00", "nodo")
PPLUS1 = subset(PPLUS1, grupo_int_v00 >= 0)

nMES = nMEs [ , order(c(names(nMEs)))]

nMES = data.frame(cbind(BBDD4_basal2[1], nMES))
nMES1a = data.frame(cbind(BBDD4_1a2[1], MEs1a))

MEs_gen = merge(nMES, nMES1a, by = "patient_ID")
colnames(MEs_gen)
colnames(MEs_gen)[1] = c("paciente")
ME_deltas = MEs_gen[18:33] - MEs_gen[2:17]
ID = data.frame(ID = MEs_gen$paciente)
PPLUS2 = merge(ID, PPLUS1, by = "ID")
PPLUS2_ME = data.frame(cbind(PPLUS2, ME_deltas, MEs_gen[2:17]))

colnames(PPLUS2_ME)

resumen = c()
IC = c()
inter = c()

for (i in 7:22) {
  modelos = lm(as.numeric(PPLUS2_ME[[i]]) ~ PPLUS2_ME[[5]] + PPLUS2_ME[[2]] + PPLUS2_ME[[3]] + PPLUS2_ME[[4]] + 
                 PPLUS2_ME[[6]] + PPLUS2_ME[[i + 16]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=28, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "grupo", "sexo", "diab","edad", "nodo", "basal",
                         "interSD", "grupoSD", "sexoSD", "diabSD1","edadSD", "nodoSD", "basalSD",
                         "intert", "grupot", "sexot", "diabt1","edadt", "nodot", "basalt",
                         "interp", "grupop", "sexop", "diabp1","edadp", "nodop", "basalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=14, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "grupo2.5","sexo2.5", "diab2.5","edad2.5", "nodo2.5", "basal2.5",
                    "inter97.5", "grupo97.5", "sexo97.51","diab97.5", "edad97.51","nodo97.5", "basal97.5")

IC.df$names = colnames(PPLUS2_ME)[7:22]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS2_ME_rl.xlsx")

rm(IC, IC.df, modelos, resumen, resumen.df, i)

# Calculo de medias de los deltas de los grupos por grupo:

medias = c()
SD = c()

for (i in 7:22) {
  medias[[i]] = tapply(PPLUS2_ME[[i]], PPLUS2_ME[[5]], mean, na.rm = T)
  SD[[i]] = tapply(PPLUS2_ME[[i]], PPLUS2_ME[[5]], sd, na.rm = T)
}

medias = data.frame(matrix(unlist(medias), ncol=2, byrow=T),stringsAsFactors=FALSE)
SD = data.frame(matrix(unlist(SD), ncol=2, byrow=T),stringsAsFactors=FALSE)
SD$names = colnames(PPLUS2_ME)[7:22]

resumen = data.frame(cbind(medias, SD))
colnames(resumen) = c("media CG", "media IG", "SD CG", "SD IG", "nombres")

export(resumen, "medias_delta_redes.xlsx")

rm(anovas, medias, resumen, SD, i)

# ajuste FDR
# 
# data = read_excel("PPLUS2_ME_rl.xlsx")
# 
# p_value <- data$`p-value`
# fdr <- p.adjust(p_value, method='BH')
# idx <- which(fdr < 0.05)
# data$`Module name`[idx]
# 
# export(data.frame(round(fdr,3)), "fdr.xlsx")

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# 
# BiocManager::install("qvalue")
# 
# hist(p_value)
# 
# library(qvalue)
# 
# q_value <- qvalue(p_value)
# summary(q_value)
# 
# idx <- which(q_value$qvalues < 0.05)
# data$`names`[idx]
# 
# export(data.frame(q_value$qvalues), "q_value.xlsx")

export(PPLUS2_ME, "PPLUS_ME.xlsx")

# BBDD Alessandro con los ME:

PPLUS_ME_A = data.frame(cbind(PPLUS2, MEs_gen))
colnames(PPLUS_ME_A)
export(PPLUS_ME_A, "PPLUS_ME_A.xlsx")

# Determinaciones en los modulos significativos:

# Determinación de los hubs por módulo:
table(nColores)
tophubs = 5
df=data.frame(Met=rownames(CM_basal),CM_basal)[nColores=="black",] #23
export(df, "black_basal.xlsx")
df=data.frame(Met=rownames(CM_basal),CM_basal)[nColores=="midnightblue",] #6
export(df, "midnightblue_basal.xlsx")
df=data.frame(Met=rownames(CM_basal),CM_basal)[nColores=="pink",] #20
export(df, "pink_basal.xlsx")
df=data.frame(Met=rownames(CM_basal),CM_basal)[nColores=="salmon",] #7
export(df, "salmon_basal.xlsx")
df=data.frame(Met=rownames(CM_basal),CM_basal)[nColores=="yellow",] #39
export(df, "yellow_basal.xlsx")

# Calculo de las correlaciones intramodulares:
ME_black = data.frame(ME = nMES$MEblack, BBDD4_basal2[4:535][,nColores=="black"])
cor_black = data.frame(names.R = rownames(cor(ME_black)[-1,]),r = round(cor(ME_black)[-1,1],2))
cor_black = merge(names[1:2], cor_black[1:2], by = "names.R")
export(cor_black, "cor_black_basal.xlsx")
ME_midnightblue = data.frame(ME = nMES$MEmidnightblue, BBDD4_basal2[4:535][,nColores=="midnightblue"])
cor_midnightblue = data.frame(names.R = rownames(cor(ME_midnightblue)[-1,]),r = round(cor(ME_midnightblue)[-1,1],2))
cor_midnightblue = merge(names[1:2], cor_midnightblue[1:2], by = "names.R")
export(cor_midnightblue, "cor_midnightblue_basal.xlsx")
ME_pink = data.frame(ME = nMES$MEpink, BBDD4_basal2[4:535][,nColores=="pink"])
cor_pink = data.frame(names.R = rownames(cor(ME_pink)[-1,]),r = round(cor(ME_pink)[-1,1],2))
cor_pink = merge(names[1:2], cor_pink[1:2], by = "names.R")
export(cor_pink, "cor_pink_basal.xlsx")
ME_salmon = data.frame(ME = nMES$MEsalmon, BBDD4_basal2[4:535][,nColores=="salmon"])
cor_salmon = data.frame(names.R = rownames(cor(ME_salmon)[-1,]),r = round(cor(ME_salmon)[-1,1],2))
cor_salmon = merge(names[1:2], cor_salmon[1:2], by = "names.R")
export(cor_salmon, "cor_salmon_basal.xlsx")
ME_yellow = data.frame(ME = nMES$MEyellow, BBDD4_basal2[4:535][,nColores=="yellow"])
cor_yellow = data.frame(names.R = rownames(cor(ME_yellow)[-1,]),r = round(cor(ME_yellow)[-1,1],2))
cor_yellow = merge(names[1:2], cor_yellow[1:2], by = "names.R")
export(cor_yellow, "cor_yellow_basal.xlsx")

# Pair-wise partial correlations:

pcor_black = as.matrix(round(pcor(ME_black[-1])$estimate,2))
png(filename = "Black_baseline.png", width = 30, height = 30, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_black, xLabels = rownames(pcor_black), 
               yLabels = rownames(pcor_black), ySymbols = rownames(pcor_black), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_black, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("Black metabolite subnetwork relationships at the baseline"))
dev.off()
pcor_midnightblue = as.matrix(round(pcor(ME_midnightblue[-1], method = "pearson")$estimate,2))
png(filename = "midnightblue_baseline.png", width = 15, height = 15, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_midnightblue, xLabels = rownames(pcor_midnightblue), 
               yLabels = rownames(pcor_midnightblue), ySymbols = rownames(pcor_midnightblue), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_midnightblue, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("midnightblue metabolite subnetwork relationships at the baseline"))
dev.off()
pcor_salmon = as.matrix(round(pcor(ME_salmon[-1])$estimate,2))
png(filename = "salmon_baseline.png", width = 20, height = 20, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_salmon, xLabels = rownames(pcor_salmon), 
               yLabels = rownames(pcor_salmon), ySymbols = rownames(pcor_salmon), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_salmon, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("salmon metabolite subnetwork relationships at the baseline"))
dev.off()
pcor_pink = as.matrix(round(pcor(ME_pink[-1])$estimate,2))
png(filename = "pink_baseline.png", width = 25, height = 25, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_pink, xLabels = rownames(pcor_pink), 
               yLabels = rownames(pcor_pink), ySymbols = rownames(pcor_pink), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_pink, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("pink metabolite subnetwork relationships at the baseline"))
dev.off()
pcor_yellow = as.matrix(round(pcor(ME_yellow[-1], method = "spearman")$estimate,2))
png(filename = "yellow_baseline.png", width = 40, height = 40, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_yellow, xLabels = rownames(pcor_yellow), 
               yLabels = rownames(pcor_yellow), ySymbols = rownames(pcor_yellow), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_yellow, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("yellow metabolite subnetwork relationships at the baseline"))
dev.off()

# Sintaxis para valorar las conectividades entre modulos:
# whichmodule="red"
#   restrict1=nColores==whichmodule
#   verboseScatterplot(CM_basal$kTotal[restrict1],
#                      CM_1a$kTotal[restrict1],
#                      col=nColores[restrict1],xlab="Connectivity k, Basal",ylab="Connectivity k, 1a")

# Determinación de los hubs por módulo:
tophubs = 5
df=data.frame(Met=rownames(CM_1a),CM_1a)[nColores=="black",] #23
export(df, "black_1a.xlsx")
df=data.frame(Met=rownames(CM_1a),CM_1a)[nColores=="midnightblue",] #6
export(df, "midnightblue_1a.xlsx")
df=data.frame(Met=rownames(CM_1a),CM_1a)[nColores=="pink",] #20
export(df, "pink_1a.xlsx")
df=data.frame(Met=rownames(CM_1a),CM_1a)[nColores=="salmon",] #7
export(df, "salmon_1a.xlsx")
df=data.frame(Met=rownames(CM_1a),CM_1a)[nColores=="yellow",] #39
export(df, "yellow_1a.xlsx")

rm(df)

# Calculo de las correlaciones intramodulares al año:
ME_black = data.frame(ME = nMES1a$MEblack, BBDD4_1a2[4:535][,nColores=="black"])
cor_black = data.frame(names.R1 = rownames(cor(ME_black)[-1,]),r = round(cor(ME_black)[-1,1],2))
cor_black = merge(names[2:3], cor_black[1:2], by = "names.R1")
export(cor_black, "cor_black_1a.xlsx")
ME_midnightblue = data.frame(ME = nMES1a$MEmidnightblue, BBDD4_1a2[4:535][,nColores=="midnightblue"])
cor_midnightblue = data.frame(names.R1 = rownames(cor(ME_midnightblue)[-1,]),r = round(cor(ME_midnightblue)[-1,1],2))
cor_midnightblue = merge(names[2:3], cor_midnightblue[1:2], by = "names.R1")
export(cor_midnightblue, "cor_midnightblue_1a.xlsx")
ME_pink = data.frame(ME = nMES1a$MEpink, BBDD4_1a2[4:535][,nColores=="pink"])
cor_pink = data.frame(names.R1 = rownames(cor(ME_pink)[-1,]),r = round(cor(ME_pink)[-1,1],2))
cor_pink = merge(names[2:3], cor_pink[1:2], by = "names.R1")
export(cor_pink, "cor_pink_1a.xlsx")
ME_salmon = data.frame(ME = nMES1a$MEsalmon, BBDD4_1a2[4:535][,nColores=="salmon"])
cor_salmon = data.frame(names.R1 = rownames(cor(ME_salmon)[-1,]),r = round(cor(ME_salmon)[-1,1],2))
cor_salmon = merge(names[2:3], cor_salmon[1:2], by = "names.R1")
export(cor_salmon, "cor_salmon_1a.xlsx")
ME_yellow = data.frame(ME = nMES1a$MEyellow, BBDD4_1a2[4:535][,nColores=="yellow"])
cor_yellow = data.frame(names.R1 = rownames(cor(ME_yellow)[-1,]),r = round(cor(ME_yellow)[-1,1],2))
cor_yellow = merge(names[2:3], cor_yellow[1:2], by = "names.R1")
export(cor_yellow, "cor_yellow_1a.xlsx")

rm(cor_black, cor_midnightblue, cor_pink, cor_salmon, cor_yellow)

# Pair-wise partial correlations:

pcor_black = as.matrix(round(pcor(ME_black[-1])$estimate,2))
png(filename = "Black_1a.png", width = 30, height = 30, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_black, xLabels = rownames(pcor_black), 
               yLabels = rownames(pcor_black), ySymbols = rownames(pcor_black), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_black, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("Black metabolite subnetwork relationships at 1-year"))
dev.off()
pcor_midnightblue = as.matrix(round(pcor(ME_midnightblue[-1], method = "spearman")$estimate,2))
png(filename = "midnightblue_1a.png", width = 15, height = 15, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_midnightblue, xLabels = rownames(pcor_midnightblue), 
               yLabels = rownames(pcor_midnightblue), ySymbols = rownames(pcor_midnightblue), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_midnightblue, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("midnightblue metabolite subnetwork relationships at 1-year"))
dev.off()
pcor_pink = as.matrix(round(pcor(ME_pink[-1])$estimate,2))
png(filename = "pink_1a.png", width = 25, height = 25, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_pink, xLabels = rownames(pcor_pink), 
               yLabels = rownames(pcor_pink), ySymbols = rownames(pcor_pink), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_pink, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("pink metabolite subnetwork relationships at 1-year"))
dev.off()
pcor_salmon = as.matrix(round(pcor(ME_salmon[-1])$estimate,2))
png(filename = "salmon_1a.png", width = 20, height = 20, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_salmon, xLabels = rownames(pcor_salmon), 
               yLabels = rownames(pcor_salmon), ySymbols = rownames(pcor_salmon), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_salmon, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("salmon metabolite subnetwork relationships at 1-year"))
dev.off()
pcor_yellow = as.matrix(round(pcor(ME_yellow[-1], method = "spearman")$estimate,2))
png(filename = "yellow_1a.png", width = 40, height = 40, res=300, unit="cm")
labeledHeatmap(Matrix = pcor_yellow, xLabels = rownames(pcor_yellow), 
               yLabels = rownames(pcor_yellow), ySymbols = rownames(pcor_yellow), colorLabels = FALSE, 
               colors = blueWhiteRed(50), textMatrix = pcor_yellow, cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-1,1),
               main = paste("yellow metabolite subnetwork relationships at 1-year"))
dev.off()

rm(pcor_black, pcor_midnightblue, pcor_pink, pcor_salmon, pcor_yellow)

# Bases de datos alessandro:

PPLUS <- read_sav("E:/ARTICULOS/BBDD/PPLUS/PREDIMEDplus_2023-01-18.sav")
PPLUS1 = data.frame(cbind(PPLUS$paciente, PPLUS$sexo_s1, PPLUS$diab_prev_s1, PPLUS$edad_s1,
                          PPLUS$grupo_int_v00, PPLUS$nodo))
colnames(PPLUS1) = c("ID", "sexo_s1", "diab_prev_s1", "edad_s1", "grupo_int_v00", "nodo")
PPLUS1 = subset(PPLUS1, grupo_int_v00 >= 0)

nMES = nMEs [ , order(c(names(nMEs)))]

nMES = data.frame(cbind(BBDD4_basal2[1], nMES))
nMES1a = data.frame(cbind(BBDD4_1a2[1], MEs1a))

MEs_gen = merge(nMES, nMES1a, by = "patient_ID")
colnames(MEs_gen)
colnames(MEs_gen)[1] = c("paciente")
ME_deltas = MEs_gen[18:33] - MEs_gen[2:17]
ID = data.frame(ID = MEs_gen$paciente)
PPLUS2 = merge(ID, PPLUS1, by = "ID")
PPLUS2_ME = data.frame(cbind(PPLUS2, ME_deltas, MEs_gen[2:17]))

colnames(PPLUS2_ME)
PPLUS2_ME_deltas = data.frame(cbind(PPLUS2_ME[1],PPLUS2_ME[7], PPLUS2_ME[16:17],PPLUS2_ME[20], PPLUS2_ME[22],
                                    PPLUS2_ME[23], PPLUS2_ME[32:33],PPLUS2_ME[36], PPLUS2_ME[38]))
colnames(PPLUS2_ME_deltas) = c("ID","MEblack_d","MEmidnightblue_d","MEpink_d","MEsalmon_d","MEyellow_d",
                               "MEblack_b","MEmidnightblue_b","MEpink_b","MEsalmon_b","MEyellow_b")
export(PPLUS2_ME_deltas, "PPLUS2_ME_deltas_wide.xlsx")

PPLUS2_ME_long = data.frame(rbind(nMES, nMES1a))
export(PPLUS2_ME_long, "PPLUS2_ME_long.xlsx")

# Objetivos secundarios: D10.iv. ####
# A. cambios entre los metabolitos en las heces y factores de riesgo CV
# BBDD con los cambios:
PRIME <- read_excel("E:/ARTICULOS/BBDD/PPLUS/PRIME/PPLUS_PRIME_HOMA_1a_01092021.xlsx")
colnames(PRIME)[1000:1999]
PRIME = cbind(PRIME[1],PRIME[924],PRIME[923],PRIME[925],PRIME[2184])
colnames(PPLUS)[1000:1999]
PPLUS3 = cbind(PPLUS[1],PPLUS[250],PPLUS[1444],PPLUS[257],PPLUS[1447],PPLUS[252],PPLUS[1449],
               PPLUS[401],PPLUS[1571],PPLUS[418],PPLUS[1588],PPLUS[414],
               PPLUS[1584],PPLUS[415],PPLUS[1585],PPLUS[416],PPLUS[1586],PPLUS[417],PPLUS[1587])

PPLUS3 = merge(PPLUS3, PRIME, by = "paciente", all.x = T)
PPLUS4 = PPLUS3[1]
PPLUS4$peso_d = PPLUS3$peso1_v01 - PPLUS3$peso1_v00
PPLUS4$imc_d = PPLUS3$imc_v01 - PPLUS3$imc_v00
PPLUS4$cintura_d = PPLUS3$cintura1_v01 - PPLUS3$cintura1_v00
PPLUS4$hba1c_d = PPLUS3$hba1c_v01 - PPLUS3$hba1c_v00
PPLUS4$HOMA.IR_d = PPLUS3$HOMA.IR_v01 - PPLUS3$HOMA.IR
PPLUS4$insulin_d = PPLUS3$insulin_v01 - PPLUS3$insulin_v00
PPLUS4$glucosa_d = PPLUS3$glucosa_v01 - PPLUS3$glucosa_v00
PPLUS4$coltot_d = PPLUS3$coltot_v01 - PPLUS3$coltot_v00
PPLUS4$hdl_d = PPLUS3$hdl_v01 - PPLUS3$hdl_v00
PPLUS4$ldl_d = PPLUS3$ldl_calc_v01 - PPLUS3$ldl_calc_v00
PPLUS4$trigli_d = PPLUS3$trigli_v01 - PPLUS3$trigli_v00

colnames(PPLUS2_ME_deltas) = c("paciente","MEblack_d","MEmidnightblue_d","MEpink_d","MEsalmon_d","MEyellow_d",
                               "MEblack_b","MEmidnightblue_b","MEpink_b","MEsalmon_b","MEyellow_b")
PPLUS4_cor_d = merge(PPLUS2_ME_deltas[1:6], PPLUS4, by = "paciente")
hist(PPLUS4_cor_d)

# Outliers:
hist(PPLUS4_cor_d)
boxplot.stats(PPLUS4_cor_d$peso_d)
boxplot(PPLUS4_cor_d$peso_d)
PPLUS4_cor_d[18,7] = NA
PPLUS4_cor_d[309,7] = NA

boxplot.stats(PPLUS4_cor_d$imc_d)
boxplot(PPLUS4_cor_d$imc_d)
PPLUS4_cor_d[18,8] = NA

boxplot.stats(PPLUS4_cor_d$cintura_d)
boxplot(PPLUS4_cor_d$cintura_d)
PPLUS4_cor_d[18,9] = NA
PPLUS4_cor_d[60,9] = NA
PPLUS4_cor_d[178,9] = NA

boxplot.stats(PPLUS4_cor_d$hba1c_d)
boxplot(PPLUS4_cor_d$hba1c_d)
PPLUS4_cor_d[145,10] = NA

boxplot.stats(PPLUS4_cor_d$HOMA.IR_d)
boxplot(PPLUS4_cor_d$HOMA.IR_d)
PPLUS4_cor_d[349,11] = NA
PPLUS4_cor_d[207,11] = NA

boxplot.stats(PPLUS4_cor_d$insulin_d)
boxplot(PPLUS4_cor_d$insulin_d)
PPLUS4_cor_d[349,12] = NA
PPLUS4_cor_d[207,12] = NA
PPLUS4_cor_d[47,12] = NA

boxplot.stats(PPLUS4_cor_d$glucosa_d)
boxplot(PPLUS4_cor_d$glucosa_d)
PPLUS4_cor_d[145,13] = NA
PPLUS4_cor_d[291,13] = NA
PPLUS4_cor_d[383,13] = NA
PPLUS4_cor_d[121,13] = NA
PPLUS4_cor_d[153,13] = NA
PPLUS4_cor_d[356,13] = NA

boxplot.stats(PPLUS4_cor_d$coltot_d)
boxplot(PPLUS4_cor_d$coltot_d)
PPLUS4_cor_d[316,14] = NA
PPLUS4_cor_d[286,14] = NA
PPLUS4_cor_d[282,14] = NA
PPLUS4_cor_d[259,14] = NA
PPLUS4_cor_d[193,14] = NA
PPLUS4_cor_d[13,14] = NA

boxplot.stats(PPLUS4_cor_d$hdl_d)
boxplot(PPLUS4_cor_d$hdl_d)
PPLUS4_cor_d[204,15] = NA
PPLUS4_cor_d[139,15] = NA

boxplot.stats(PPLUS4_cor_d$ldl_d)
boxplot(PPLUS4_cor_d$ldl_d)
PPLUS4_cor_d[259,16] = NA
PPLUS4_cor_d[193,16] = NA
PPLUS4_cor_d[282,16] = NA
PPLUS4_cor_d[286,16] = NA

boxplot.stats(PPLUS4_cor_d$trigli_d)
boxplot(PPLUS4_cor_d$trigli_d)
PPLUS4_cor_d[113,17] = NA
PPLUS4_cor_d[128,17] = NA
PPLUS4_cor_d[179,17] = NA
PPLUS4_cor_d[148,17] = NA
PPLUS4_cor_d[97,17] = NA
PPLUS4_cor_d[268,17] = NA
PPLUS4_cor_d[47,17] = NA

cor_ME_d = as.matrix(round(cor(PPLUS4_cor_d[2:17], use = "complete.obs", method = "spearman"),2))

png(filename = "ME_d_cor1.png", width = 15, height = 10, res=300, unit="cm")
labeledHeatmap(Matrix = cor_ME_d[1:5,6:16], 
               xLabels = c("Weight","BMI","Waist Circunference","HbA1c","HOMA-IR","Insulin","Glucose","Tot-Chol","HDL-chol", "LDL-chol", "TG"), 
               yLabels = c("ME Black","ME Midnightblue","ME Pink","ME Salmon","ME Yellow"), 
               textMatrix = cor_ME_d[1:5,6:16],
               colorLabels = FALSE, plotLegend = TRUE, cex.legendLabel = 0.2,
               colors = blueWhiteRed(100), cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-0.2,0.2),
               main = paste("Changes between Networks and CV risk factors"))
dev.off()

# Regresiones lineales:

PPLUS4a = PPLUS3
PPLUS4a$peso1_v01 = NULL
PPLUS4a$imc_v01 = NULL
PPLUS4a$cintura1_v01 = NULL
PPLUS4a$glucosa_v01 = NULL
PPLUS4a$hba1c_v01 = NULL
PPLUS4a$coltot_v01 = NULL
PPLUS4a$hdl_v01 = NULL
PPLUS4a$ldl_calc_v01 = NULL
PPLUS4a$trigli_v01 = NULL
PPLUS4a$insulin_v01 = NULL
PPLUS4a$HOMA.IR_v01 = NULL

PPLUS4b = cbind(PPLUS2_ME_deltas[1], PPLUS2_ME_deltas[7:11])

# Regresiones lineales cambios entre modulos:
PPLUS4_reg_d = merge(PPLUS4_cor_d, PPLUS4a, by = "paciente") #añado factores de riesgo basal
PPLUS4_reg_d = merge(PPLUS4_reg_d, PPLUS4b, by = "paciente") #añado modulos basal
colnames(PPLUS1) = c("paciente", "sexo_s1", "diab_prev_s1", "edad_s1", "grupo_int_v00", "nodo")
PPLUS4_reg_d = merge(PPLUS4_reg_d, PPLUS1, by = "paciente") #añado covariables

colnames(PPLUS4_reg_d)

PPLUS4_reg_d$MEblack_d = PPLUS4_reg_d$MEblack_d/sd(PPLUS4_reg_d$MEblack_d)
PPLUS4_reg_d$MEmidnightblue_d = PPLUS4_reg_d$MEmidnightblue_d/sd(PPLUS4_reg_d$MEmidnightblue_d)
PPLUS4_reg_d$MEpink_d = PPLUS4_reg_d$MEpink_d/sd(PPLUS4_reg_d$MEpink_d)
PPLUS4_reg_d$MEsalmon_d = PPLUS4_reg_d$MEsalmon_d/sd(PPLUS4_reg_d$MEsalmon_d)
PPLUS4_reg_d$MEyellow_d = PPLUS4_reg_d$MEyellow_d/sd(PPLUS4_reg_d$MEyellow_d)

PPLUS4_reg_d$peso_d = PPLUS4_reg_d$peso_d/sd(PPLUS4_reg_d$peso_d, na.rm = T)
PPLUS4_reg_d$imc_d = PPLUS4_reg_d$imc_d/sd(PPLUS4_reg_d$imc_d, na.rm = T)
PPLUS4_reg_d$cintura_d = PPLUS4_reg_d$cintura_d/sd(PPLUS4_reg_d$cintura_d, na.rm = T)
PPLUS4_reg_d$hba1c_d = PPLUS4_reg_d$hba1c_d/sd(PPLUS4_reg_d$hba1c_d, na.rm = T)
PPLUS4_reg_d$HOMA.IR_d = PPLUS4_reg_d$HOMA.IR_d/sd(PPLUS4_reg_d$HOMA.IR_d, na.rm = T)
PPLUS4_reg_d$insulin_d = PPLUS4_reg_d$insulin_d/sd(PPLUS4_reg_d$insulin_d, na.rm = T)
PPLUS4_reg_d$glucosa_d = PPLUS4_reg_d$glucosa_d/sd(PPLUS4_reg_d$glucosa_d, na.rm = T)
PPLUS4_reg_d$coltot_d = PPLUS4_reg_d$coltot_d/sd(PPLUS4_reg_d$coltot_d, na.rm = T)
PPLUS4_reg_d$hdl_d = PPLUS4_reg_d$hdl_d/sd(PPLUS4_reg_d$hdl_d, na.rm = T)
PPLUS4_reg_d$ldl_d = PPLUS4_reg_d$ldl_d/sd(PPLUS4_reg_d$ldl_d, na.rm = T)
PPLUS4_reg_d$trigli_d = PPLUS4_reg_d$trigli_d/sd(PPLUS4_reg_d$trigli_d, na.rm = T)

# Weight
colnames(PPLUS4_reg_d)

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(PPLUS4_reg_d[[7]] ~ as.numeric(PPLUS4_reg_d[[i]])  + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[18]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "peso", "MEbasal", "sexo","diab", "edad", "grupo","nodo","pesobasal",
                         "interSD", "pesoSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","pesobasalSD", 
                         "intert", "pesot", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","pesobasalt",
                         "interp", "pesop", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","pesobasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "peso2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","pesobasal2.5",
                    "inter97.5", "peso97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","pesobasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_peso.xlsx")

# BMI
resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[8]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[19]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "BMI", "MEbasal", "sexo","diab", "edad", "grupo","nodo","BMIbasal",
                         "interSD", "BMISD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","BMIbasalSD", 
                         "intert", "BMIt", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","BMIbasalt",
                         "interp", "BMIp", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","BMIbasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "BMI2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","BMIbasal2.5",
                    "inter97.5", "BMI97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","BMIbasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_BMI.xlsx")

# cintura
resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[9]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[20]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "cintura", "MEbasal", "sexo","diab", "edad", "grupo","nodo","cinturabasal",
                         "interSD", "cinturaSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","cinturabasalSD", 
                         "intert", "cinturat", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","cinturabasalt",
                         "interp", "cinturap", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","cinturabasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "cintura2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","cinturabasal2.5",
                    "inter97.5", "cintura97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","cinturabasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_cintura.xlsx")

# HbA1c

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[10]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[22]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "hba1c", "MEbasal", "sexo","diab", "edad", "grupo","nodo","hba1cbasal",
                         "interSD", "hba1cSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","hba1cbasalSD", 
                         "intert", "hba1ct", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","hba1cbasalt",
                         "interp", "hba1cp", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","hba1cbasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "hba1c2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","hba1cbasal2.5",
                    "inter97.5", "hba1c97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","hba1cbasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_hba1c.xlsx")

# HOMA.IR

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[11]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[28]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "HOMA.IR", "MEbasal", "sexo","diab", "edad", "grupo","nodo","HOMA.IRbasal",
                         "interSD", "HOMA.IRSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","HOMA.IRbasalSD", 
                         "intert", "HOMA.IRt", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","HOMA.IRbasalt",
                         "interp", "HOMA.IRp", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","HOMA.IRbasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "HOMA.IR2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","HOMA.IRbasal2.5",
                    "inter97.5", "HOMA.IR97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","HOMA.IRbasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_HOMA.IR.xlsx")

# insulin

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[12]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[27]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "insulin", "MEbasal", "sexo","diab", "edad", "grupo","nodo","insulinbasal",
                         "interSD", "insulinSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","insulinbasalSD", 
                         "intert", "insulint", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","insulinbasalt",
                         "interp", "insulinp", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","insulinbasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "insulin2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","insulinbasal2.5",
                    "inter97.5", "insulin97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","insulinbasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_insulin.xlsx")

# glucosa

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[13]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[21]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "glucosa", "MEbasal", "sexo","diab", "edad", "grupo","nodo","glucosabasal",
                         "interSD", "glucosaSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","glucosabasalSD", 
                         "intert", "glucosat", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","glucosabasalt",
                         "interp", "glucosap", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","glucosabasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "glucosa2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","glucosabasal2.5",
                    "inter97.5", "glucosa97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","glucosabasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_glucosa.xlsx")

# coltot

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[14]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[23]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "coltot", "MEbasal", "sexo","diab", "edad", "grupo","nodo","coltotbasal",
                         "interSD", "coltotSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","coltotbasalSD", 
                         "intert", "coltott", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","coltotbasalt",
                         "interp", "coltotp", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","coltotbasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "coltot2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","coltotbasal2.5",
                    "inter97.5", "coltot97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","coltotbasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_coltot.xlsx")

# hdl

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[15]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[24]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "hdl", "MEbasal", "sexo","diab", "edad", "grupo","nodo","hdlbasal",
                         "interSD", "hdlSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","hdlbasalSD", 
                         "intert", "hdlt", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","hdlbasalt",
                         "interp", "hdlp", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","hdlbasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "hdl2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","hdlbasal2.5",
                    "inter97.5", "hdl97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","hdlbasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_hdl.xlsx")

# ldl

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[16]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[25]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "ldl", "MEbasal", "sexo","diab", "edad", "grupo","nodo","ldlbasal",
                         "interSD", "ldlSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","ldlbasalSD", 
                         "intert", "ldlt", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","ldlbasalt",
                         "interp", "ldlp", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","ldlbasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "ldl2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","ldlbasal2.5",
                    "inter97.5", "ldl97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","ldlbasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))

export(resumen, "PPLUS4_ME_rl_ldl.xlsx")

# trigli

resumen = c()
IC = c()

for (i in 2:6) {
  modelos = lm(as.numeric(PPLUS4_reg_d[[i]]) ~ PPLUS4_reg_d[[17]] + PPLUS4_reg_d[[i+27]] + PPLUS4_reg_d[[34]] + PPLUS4_reg_d[[35]] + 
                 PPLUS4_reg_d[[36]] + PPLUS4_reg_d[[37]] + PPLUS4_reg_d[[38]] + PPLUS4_reg_d[[26]])
  resumen[[i]] = summary(modelos)$coef
  IC[[i]] = confint(modelos)
}

resumen.df = round(data.frame(matrix(unlist(resumen), ncol=36, byrow=T),stringsAsFactors=FALSE),3)

colnames(resumen.df) = c("inter", "trigli", "MEbasal", "sexo","diab", "edad", "grupo","nodo","triglibasal",
                         "interSD", "trigliSD", "MEbasalSD", "sexoSD","diabSD", "edadSD", "grupoSD","nodoSD","triglibasalSD", 
                         "intert", "triglit", "MEbasalt", "sexot","diabt", "edadt", "grupot","nodot","triglibasalt",
                         "interp", "triglip", "MEbasalp", "sexop","diabp", "edadp", "grupop","nodop","triglibasalp")

IC.df = round(data.frame(matrix(unlist(IC), ncol=18, byrow=T),stringsAsFactors=FALSE),2)

colnames(IC.df) = c("inter2.5", "trigli2.5", "MEbasal2.5", "sexo2.5","diab2.5", "edad2.5", "grupo2.5","nodo2.5","triglibasal2.5",
                    "inter97.5", "trigli97.5", "MEbasal97.5", "sexo97.5","diab97.5", "edad97.5", "grupo97.5","nodo97.5","triglibasal97.5")

IC.df$names = colnames(PPLUS4_reg_d)[2:6]

resumen = data.frame(cbind(resumen.df, IC.df))


export(resumen, "PPLUS4_ME_rl_trigli.xlsx")

rm(IC, IC.df, modelos, resumen, resumen.df, i)

Reg_ME_coef <- read_excel("Reg-ME-coef.xlsx",sheet = "graficados")
sign = read_excel("Reg-ME-coef.xlsx",sheet = "sign")
rownames(Reg_ME_coef) = Reg_ME_coef$MEs

tiff(filename = "ME_d_reg.tif", width = 15, height = 10, res=300, unit="cm")
labeledHeatmap(Matrix = Reg_ME_coef[1:4,2:12], 
               xLabels = c("Body weight", "Waist Circunference", "Body mass index", "Total Cholesterol", "HDL Cholesterol",
                           "LDL Cholesterol", "Triglycerides", "Glucose", "Insulin", "HbA1c", "HOMA-IR"), 
               yLabels = c("Black", "Midnight Blue", "Pink", "Salmon"),
               textMatrix = sign[1:4,2:12],
               colorLabels = FALSE, plotLegend = TRUE, cex.legendLabel = 0.7,
               colors = blueWhiteRed(100), cex.lab.x = 0.5,cex.lab.y = 0.5,
               setStdMargins = FALSE, cex.text = 0.5, zlim = c(-0.15,0.15))
dev.off()

# Volcano plot:
library(qvalue)

# Calculos de las diferencias por grupos (deltas) y p values:

colnames(BBDD4_log_w_deltas)

BBDD_VP = merge(BBDD4_log_w_deltas, BBDD4_basal2, by = c("patient_ID","patient_ID_16S","sample"))
colnames(PPLUS4_reg_d)[1] = c("patient_ID")
BBDD_VP = merge(BBDD_VP, PPLUS4_reg_d, by = "patient_ID")
colnames(BBDD_VP)[1000:1104]

mean(BBDD_VP$X4.7.10.13.16.Docosapentaenoic.acid.1)

pvalores = c()
pvalores2 = c()
coef = c()
inter = c()

for (i in 4:535) {
  modelos = lm(as.numeric(BBDD_VP[[i]]) ~ BBDD_VP[[i+532]] + BBDD_VP[[1104]] + BBDD_VP[[1102]] + 
                 BBDD_VP[[1101]] + BBDD_VP[[1100]] + as.factor(BBDD_VP[[1103]]))
  pvalores[[i]] = summary(modelos)$coef[7,4]
  coef[[i]] = summary(modelos)$coef[7,1]
  modelos1 = lm(as.numeric(BBDD_VP[[i]]) ~ BBDD_VP[[1104]] + BBDD_VP[[1102]] + 
                 BBDD_VP[[1101]] + BBDD_VP[[1100]] + as.factor(BBDD_VP[[1103]]))
  inter[[i]] = summary(modelos1)$coef[1,1]
}

pvalores = round(data.frame(matrix(unlist(pvalores), ncol=1, byrow=T),stringsAsFactors=FALSE),3)
colnames(pvalores) = c("p.values")
idx <- which(pvalores < 0.05)

coef = round(data.frame(matrix(unlist(coef), ncol=1, byrow=T),stringsAsFactors=FALSE),2)
colnames(coef) = c("coef")

inter = round(data.frame(matrix(unlist(inter), ncol=1, byrow=T),stringsAsFactors=FALSE),2)
colnames(inter) = c("intercepto")

pvalores$names.R1 = colnames(BBDD_VP)[4:535]

resumen = data.frame(cbind(inter, coef, pvalores))
resumen = merge(resumen, names, by = "names.R1")

export(resumen[1:5], "p_values_met_rl.xlsx")

resumen = resumen[order(resumen$method), ]
table(resumen$method)

q_value_c18 = data.frame(q = qvalue(resumen$p.values[1:75])$qvalue)
q_value_c8 = data.frame(q = qvalue(resumen$p.values[76:182])$qvalue)
q_value_hneg = data.frame(q = qvalue(resumen$p.values[183:241])$qvalue)
q_value_hpos = data.frame(q = qvalue(resumen$p.values[242:532])$qvalue)

q__value = rbind(q_value_c18,q_value_c8,q_value_hneg,q_value_hpos)

resumen$qvalue = q__value
colnames(resumen)

volcano = cbind(names=resumen$names,coef=resumen$coef,q=resumen$qvalue)
export(volcano, "p-adj-values.xlsx")

library(EnhancedVolcano)
library(airway)
library(magrittr)

png(filename = "volcano_plot.png", width = 15, height = 10, res=300, unit="cm")
EnhancedVolcano(volcano,
                lab = volcano$names,
                x = 'coef', xlim = c(-0.60,0.60), pCutoff = 0.05, xlab = "B coefficients",axisLabSize = 10,
                y = 'q', ylim = c(0,2), FCcutoff = 0.01, ylab = "log(p-value)",
                title = " ", subtitle = " ", caption = " ",
                pointSize=1.5,labSize=3,colGradient = c('red','blue'),
                legendLabSize = 10, legendPosition = "right", legendLabels = "P-value",
                drawConnectors = T, widthConnectors = 0.5,
                boxedLabels = T)
dev.off()

# Calculo de medias de los deltas de metabolitos por grupo:

medias = c()
SD = c()
pvalores = c()

for (i in 4:535) {
  medias[[i]] = tapply(BBDD_VP[[i]], BBDD_VP[[1103]], mean, na.rm = T)
  SD[[i]] = tapply(BBDD_VP[[i]], BBDD_VP[[1103]], sd, na.rm = T)
  modelos = lm(as.numeric(BBDD_VP[[i]]) ~ BBDD_VP[[i+532]] + BBDD_VP[[1104]] + BBDD_VP[[1102]] + 
                 BBDD_VP[[1101]] + BBDD_VP[[1100]] + as.factor(BBDD_VP[[1103]]))
  pvalores[[i]] = summary(modelos)$coef[7,4]
}

medias = data.frame(matrix(unlist(medias), ncol=2, byrow=T),stringsAsFactors=FALSE)
SD = data.frame(matrix(unlist(SD), ncol=2, byrow=T),stringsAsFactors=FALSE)
pvalores = round(data.frame(matrix(unlist(pvalores), ncol=1, byrow=T),stringsAsFactors=FALSE),3)
colnames(pvalores) = c("p.values")
idx <- which(pvalores < 0.05)

pvalores$names.R1 = colnames(BBDD_VP)[4:535]

resumen = data.frame(cbind(medias, SD, pvalores))
colnames(resumen) = c("media CG", "media IG", "SD CG", "SD IG", "p-values", "nombres")

export(resumen, "medias_delta_metabolitos.xlsx")

rm(anovas, medias, resumen, SD, i)
        
# Creacion de redes: exportación a citoscape
# Ver plan de análisis: 22.05.2023
library(RCy3)
# Modulos versus metabolitos
names2 <- read_excel("names2.xlsx") 
nColores2 = sort(nColores) 
probes = c(names2$names[nColores2=="black"])
modules = c("Black")
modTom = data.frame(cor(ME_black[-1]))
n1 = data.frame(names.R = colnames(modTom))
n1 = merge(n1, names2, by = "names.R", sort = F)
colnames(modTom) = n1$names
rownames(modTom) = colnames(modTom)

cyt = exportNetworkToCytoscape(modTom, 
                               edgeFile = paste("CytoscapeInput-edges-", paste(modules, collapse = "-"), ".txt", sep = ""),
                               nodeFile = paste("CytoscapeInput-nodes-", paste(modules, collapse = "-"), ".txt", sep = ""),
                               weighted = F, threshold = 0.7, nodeNames = probes) 

probes = c(names2$names[nColores2=="midnightblue"])
modules = c("midnightblue")
modTom = data.frame(cor(ME_midnightblue[-1]))
n1 = data.frame(names.R = colnames(modTom))
n1 = merge(n1, names2, by = "names.R", sort = F)
colnames(modTom) = n1$names
rownames(modTom) = colnames(modTom)

cyt = exportNetworkToCytoscape(modTom, 
                               edgeFile = paste("CytoscapeInput-edges-", paste(modules, collapse = "-"), ".txt", sep = ""),
                               nodeFile = paste("CytoscapeInput-nodes-", paste(modules, collapse = "-"), ".txt", sep = ""),
                               weighted = F, threshold = 0.3, nodeNames = probes) 

probes = c(names2$names[nColores2=="pink"])
modules = c("pink")
modTom = data.frame(cor(ME_pink[-1]))
n1 = data.frame(names.R = colnames(modTom))
n1 = merge(n1, names2, by = "names.R", sort = F)
colnames(modTom) = n1$names
rownames(modTom) = colnames(modTom)

cyt = exportNetworkToCytoscape(modTom, 
                               edgeFile = paste("CytoscapeInput-edges-", paste(modules, collapse = "-"), ".txt", sep = ""),
                               nodeFile = paste("CytoscapeInput-nodes-", paste(modules, collapse = "-"), ".txt", sep = ""),
                               weighted = F, threshold = 0.5, nodeNames = probes) 

probes = c(names2$names[nColores2=="salmon"])
modules = c("salmon")
modTom = data.frame(cor(ME_salmon[-1]))
n1 = data.frame(names.R = colnames(modTom))
n1 = merge(n1, names2, by = "names.R", sort = F)
colnames(modTom) = n1$names
rownames(modTom) = colnames(modTom)

cyt = exportNetworkToCytoscape(modTom, 
                               edgeFile = paste("CytoscapeInput-edges-", paste(modules, collapse = "-"), ".txt", sep = ""),
                               nodeFile = paste("CytoscapeInput-nodes-", paste(modules, collapse = "-"), ".txt", sep = ""),
                               weighted = F, threshold = 0.5, nodeNames = probes) 

# Revisiones AJCN: analisis de mediacion: ####

# Paso 1: la intervención modifica los factores de riesgo cardiovascular?

VarAjust = cbind(patient_ID = PPLUS$paciente, #id
           PPLUS[3], PPLUS[5],PPLUS[257],PPLUS[46],PPLUS[49],PPLUS[51],PPLUS[458],nodo = PPLUS$nodo,#variables de ajuste 
           PPLUS[137])#grupo
VarAjust$imc_cat = car::recode(VarAjust$imc_v00, "0:29.999 = 'Overweight'; else = 'obesity'")
VarAjust$edad_cat = car::recode(VarAjust$edad_s1, "0:65 = 'young'; else = 'old'")
VarAjust$imc_v00 = NULL
VarAjust$edad_s1 = NULL
VarAjust$grupo_int = as.numeric(as.factor(VarAjust$grupo_int_v00))
VarAjust$grupo_int = car::recode(as.numeric(as.factor(VarAjust$grupo_int_v00)), "1 = '0'; else = '1'")

BD = PPLUS4_reg_d[1:33]
BD = merge(BD, VarAjust, by = "patient_ID", all.x = T)
colnames(BD)

summary(lm(lm(peso_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00,
                   data = BD))) #Si
summary(lm(lm(cintura_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00,
              data = BD))) #Si
summary(lm(lm(imc_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00,
              data = BD))) #Si
summary(lm(lm(hba1c_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + hba1c_v00,
              data = BD))) #Si
summary(lm(lm(HOMA.IR_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR,
              data = BD))) #Si
summary(lm(lm(insulin_d ~ grupo_int_v00 + as.factor(nodo) + as.factor(fuma_s1) + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00,
              data = BD))) #No
summary(lm(lm(glucosa_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + glucosa_v00,
              data = BD))) #No
summary(lm(lm(coltot_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + coltot_v00,
              data = BD))) #No
summary(lm(lm(hdl_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + hdl_v00,
              data = BD))) #No
summary(lm(lm(ldl_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + ldl_calc_v00,
              data = BD))) #No
summary(lm(lm(trigli_d ~ grupo_int_v00 + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + trigli_v00,
              data = BD))) #No

library(rsample)
set.seed(1)
computo = bootstraps(BD, times = 1000)

## Modules: ####

## Body weight
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:6){
  a = coefficients(lm(lm(aa[[i]] ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+27]],
                         data = aa)))[2] # X coefficient 
  b = coefficients(lm(peso_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+27]],
                          data = aa))[3]
  c = coefficients(lm(lm(peso_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+27]],
                          data = aa)))[2]
  c. = coefficients(lm(lm(peso_d ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00,
                           data = aa)))[2]
  # Indirect effects IE1= a*b, IE2= c-c'
  IE = a*b
  DE = c
  TE = c + a*b
  results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 15, byrow = T),3))
tt = c()
for (i in 2:6){
  tt[[i]] = c(paste("IE", colnames(BD[i])),paste("DE", colnames(BD[i])),paste("TE", colnames(BD[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:15){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_ME_BW.xlsx")

## Insulin
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:6){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+27]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(insulin_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+27]],
                        data = aa))[3]
    c = coefficients(lm(lm(insulin_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+27]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(insulin_d ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 15, byrow = T),3))
tt = c()
for (i in 2:6){
  tt[[i]] = c(paste("IE", colnames(BD[i])),paste("DE", colnames(BD[i])),paste("TE", colnames(BD[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:15){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_ME_insulina.xlsx")

## HOMA.IR
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:6){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+27]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(HOMA.IR_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+27]],
                        data = aa))[3]
    c = coefficients(lm(lm(HOMA.IR_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+27]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(HOMA.IR_d ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 15, byrow = T),3))
tt = c()
for (i in 2:6){
  tt[[i]] = c(paste("IE", colnames(BD[i])),paste("DE", colnames(BD[i])),paste("TE", colnames(BD[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:15){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_ME_HOMAIR.xlsx")

## Metabolites: ####
BD_met_Med = cbind(patient_ID = BBDD_VP$patient_ID, 
                   Adrenic.acid = BBDD_VP$Adrenic.acid.1, DPA = BBDD_VP$X4.7.10.13.16.Docosapentaenoic.acid.1, Oleic.acid = BBDD_VP$Oleic.acid.1, X3MAA = BBDD_VP$X3.Methyladipic.acid.or.Pimelic.acid.1,
                   Adrenic.acid_b = BBDD_VP$Adrenic.acid, DPA_b = BBDD_VP$X4.7.10.13.16.Docosapentaenoic.acid, Oleic.acid_b = BBDD_VP$Oleic.acid, X3MAA_b = BBDD_VP$X3.Methyladipic.acid.or.Pimelic.acid)
BD_met = cbind(BD[1],BD[7:43])
BD_met = merge(BD_met_Med, BD_met, by = 'patient_ID', all.y = T)
colnames(BD_met)

set.seed(1)
computo = bootstraps(BD_met, times = 1000)

pvalores = c()
coef = c()

for (i in 2:5) {
  modelos = lm(peso_d ~ BD_met[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + BD_met[[i+4]],
               data = BD_met)
  pvalores[[i]] = summary(modelos)$coef[2,4]
  coef[[i]] = summary(modelos)$coef[2,1]
}

pvalores = round(data.frame(matrix(unlist(pvalores), ncol=1, byrow=T),stringsAsFactors=FALSE),3)
coef = round(data.frame(matrix(unlist(coef), ncol=1, byrow=T),stringsAsFactors=FALSE),3)
BW_sig = cbind(met = colnames(BD_met)[2:5], coef = coef, p = pvalores)

## Body weight
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(peso_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(peso_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(peso_d ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_met[i])),paste("DE", colnames(BD_met[i])),paste("TE", colnames(BD_met[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Met_BW.xlsx")

## Waist
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(cintura_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(cintura_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(cintura_d ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_met[i])),paste("DE", colnames(BD_met[i])),paste("TE", colnames(BD_met[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Met_WC.xlsx")

## IMC
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(imc_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(imc_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(imc_d ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_met[i])),paste("DE", colnames(BD_met[i])),paste("TE", colnames(BD_met[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Met_BMI.xlsx")

## insulin
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(insulin_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(insulin_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(insulin_d ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_met[i])),paste("DE", colnames(BD_met[i])),paste("TE", colnames(BD_met[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Met_insulin.xlsx")

## HOMA.IR
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(HOMA.IR_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(HOMA.IR_d ~ as.factor(grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(HOMA.IR_d ~ as.factor(grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_met[i])),paste("DE", colnames(BD_met[i])),paste("TE", colnames(BD_met[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Met_HOMA.IR.xlsx")

## Metagenómica: ####
# Preparacion de la BBDD
export(metadata2, "metadata2.xlsx")
library(readxl)
metadata2 <- read_excel("metadata2.xlsx")

colnames(metadata2)
metadata2$Chao1_d = metadata2$Chao1_v01 - metadata2$Chao1_v00
metadata2$Shannon_d = metadata2$Shannon_v01 - metadata2$Shannon_v00
metadata2$Eubacterium_hallii_group_d = metadata2$Eubacterium_hallii_group_v01 - metadata2$Eubacterium_hallii_group_v00
metadata2$Dorea_d = metadata2$Dorea_v01 - metadata2$Dorea_v00

md2 = cbind(metadata2[1],metadata2[10:13],metadata2[2:5])
BD_bat = cbind(BD[1],BD[7:43])
BD_bat = merge(md2, BD_bat, by = 'patient_ID', all.y = T)
colnames(BD_bat)

set.seed(1)
computo = bootstraps(BD_bat, times = 1000)

## Body weight
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(peso_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(peso_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(peso_d ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_bat[i])),paste("DE", colnames(BD_bat[i])),paste("TE", colnames(BD_bat[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Bat_BW.xlsx")

## Waist
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(cintura_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(cintura_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(cintura_d ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_bat[i])),paste("DE", colnames(BD_bat[i])),paste("TE", colnames(BD_bat[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Bat_WC.xlsx")

## IMC
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(imc_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(imc_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(imc_d ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_bat[i])),paste("DE", colnames(BD_bat[i])),paste("TE", colnames(BD_bat[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Bat_BMI.xlsx")

## insulin
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(insulin_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(insulin_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(insulin_d ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_bat[i])),paste("DE", colnames(BD_bat[i])),paste("TE", colnames(BD_bat[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Bat_insulin.xlsx")

## HOMA.IR
results = c()
rr = c()
IE = c()
DE = c()
TE = c()
for (j in 1:1000){
  aa = as.data.frame(computo$splits[[j]])
  for (i in 2:5){
    a = coefficients(lm(lm(aa[[i]] ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+4]],
                           data = aa)))[2] # X coefficient 
    b = coefficients(lm(HOMA.IR_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+4]],
                        data = aa))[3]
    c = coefficients(lm(lm(HOMA.IR_d ~ as.factor(-grupo_int) + aa[[i]] + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + aa[[i+4]],
                           data = aa)))[2]
    c. = coefficients(lm(lm(HOMA.IR_d ~ as.factor(-grupo_int) + as.factor(nodo) + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR,
                            data = aa)))[2]
    # Indirect effects IE1= a*b, IE2= c-c'
    IE = a*b
    DE = c
    TE = c + a*b
    results[[i]] = c(IE = IE, DE = DE, TE = TE)
  }
  rr[[j]] = results
}  
results1 = data.frame(round(matrix(unlist(rr), ncol = 12, byrow = T),3))
tt = c()
for (i in 2:5){
  tt[[i]] = c(paste("IE", colnames(BD_bat[i])),paste("DE", colnames(BD_bat[i])),paste("TE", colnames(BD_bat[i])))
}
tt = unlist(tt)
colnames(results1) = tt

means_med = c()
ICs_med = c()
for(i in 1:12){
  means_med[[i]] = t.test(results1[[i]])$estimate
  ICs_med[[i]] = t.test(results1[[i]])$conf.int
}

means_med = data.frame(round(matrix(unlist(means_med), ncol = 1, byrow = T),3))
ICs_med = data.frame(round(matrix(unlist(ICs_med), ncol = 2, byrow = T),3))
tabla_med = cbind(Nombres = tt, Means = means_med, ICs_med)
colnames(tabla_med) = c('Variable','Media','IC2.5%','IC97.5%')
export(tabla_med, "med_Bat_HOMA.IR.xlsx")

# Mediation analysis with mediation package: ####
## Modules: ####

library(mediation)
BD$grupo_int = as.factor(BD$grupo_int)
BD$nodo = as.factor(BD$nodo)
a = lm(MEpink_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + MEpink_b,
       data = BD)
b = lm(peso_d ~ grupo_int + MEpink_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + MEpink_b,
        data = BD)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "MEpink_d", dropobs = T, control.value = 0, treat.value = 1)
summary(m)$nobs

a = lm(MEpink_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + MEpink_b,
       data = BD)
b = lm(insulin_d ~ grupo_int + MEpink_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + MEpink_b,
       data = BD)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "MEpink_d", dropobs = T)
summary(m)

a = lm(MEpink_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + MEpink_b,
       data = BD)
b = lm(HOMA.IR_d ~ grupo_int + MEpink_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + MEpink_b,
       data = BD)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "MEpink_d", dropobs = T)
summary(m)

## Metabolites: ####
# No los incluyo porque no me acaba de convencer los resultados
colnames(BD_met)
a = lm(Adrenic.acid ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Adrenic.acid_b,
       data = BD_met)
b = lm(peso_d ~ grupo_int + Adrenic.acid + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Adrenic.acid_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Adrenic.acid", dropobs = T)
summary(m)

a = lm(DPA ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + DPA_b,
       data = BD_met)
b = lm(peso_d ~ grupo_int + DPA + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + DPA_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "DPA", dropobs = T)
summary(m)

a = lm(Oleic.acid ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Oleic.acid_b,
       data = BD_met)
b = lm(peso_d ~ grupo_int + Oleic.acid + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Oleic.acid_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Oleic.acid", dropobs = T)
summary(m)

a = lm(X3MAA ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + X3MAA_b,
       data = BD_met)
b = lm(peso_d ~ grupo_int + X3MAA + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + X3MAA_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "X3MAA", dropobs = T)
summary(m)

a = lm(Adrenic.acid ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Adrenic.acid_b,
       data = BD_met)
b = lm(imc_d ~ grupo_int + Adrenic.acid + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Adrenic.acid_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Adrenic.acid", dropobs = T)
summary(m)

a = lm(DPA ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + DPA_b,
       data = BD_met)
b = lm(imc_d ~ grupo_int + DPA + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + DPA_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "DPA", dropobs = T)
summary(m)

a = lm(Oleic.acid ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Oleic.acid_b,
       data = BD_met)
b = lm(imc_d ~ grupo_int + Oleic.acid + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Oleic.acid_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Oleic.acid", dropobs = T)
summary(m)

a = lm(X3MAA ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + X3MAA_b,
       data = BD_met)
b = lm(imc_d ~ grupo_int + X3MAA + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + X3MAA_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "X3MAA", dropobs = T)
summary(m)

a = lm(Adrenic.acid ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Adrenic.acid_b,
       data = BD_met)
b = lm(insulin_d ~ grupo_int + Adrenic.acid + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Adrenic.acid_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Adrenic.acid", dropobs = T)
summary(m)

a = lm(DPA ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + DPA_b,
       data = BD_met)
b = lm(insulin_d ~ grupo_int + DPA + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + DPA_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "DPA", dropobs = T)
summary(m)

a = lm(Oleic.acid ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Oleic.acid_b,
       data = BD_met)
b = lm(insulin_d ~ grupo_int + Oleic.acid + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Oleic.acid_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Oleic.acid", dropobs = T)
summary(m)

a = lm(X3MAA ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + X3MAA_b,
       data = BD_met)
b = lm(insulin_d ~ grupo_int + X3MAA + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + X3MAA_b,
       data = BD_met)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "X3MAA", dropobs = T)
summary(m)

## Metagenomics: ####
colnames(BD_bat)
a = lm(Eubacterium_hallii_group_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Eubacterium_hallii_group_v00,
       data = BD_bat)
b = lm(peso_d ~ grupo_int + Eubacterium_hallii_group_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Eubacterium_hallii_group_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Eubacterium_hallii_group_d", dropobs = T)
summary(m)

a = lm(Dorea_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Dorea_v00,
       data = BD_bat)
b = lm(peso_d ~ grupo_int + Dorea_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + peso1_v00 + Dorea_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Dorea_d", dropobs = T)
summary(m)

a = lm(Eubacterium_hallii_group_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + Eubacterium_hallii_group_v00,
       data = BD_bat)
b = lm(cintura_d ~ grupo_int + Eubacterium_hallii_group_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + Eubacterium_hallii_group_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Eubacterium_hallii_group_d", dropobs = T)
summary(m)

a = lm(Dorea_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + Dorea_v00,
       data = BD_bat)
b = lm(cintura_d ~ grupo_int + Dorea_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + cintura1_v00 + Dorea_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Dorea_d", dropobs = T)
summary(m)

a = lm(Eubacterium_hallii_group_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + Eubacterium_hallii_group_v00,
       data = BD_bat)
b = lm(imc_d ~ grupo_int + Eubacterium_hallii_group_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + Eubacterium_hallii_group_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Eubacterium_hallii_group_d", dropobs = T)
summary(m)

a = lm(Dorea_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + Dorea_v00,
       data = BD_bat)
b = lm(imc_d ~ grupo_int + Dorea_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + imc_v00 + Dorea_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Dorea_d", dropobs = T)
summary(m)

a = lm(Eubacterium_hallii_group_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + Eubacterium_hallii_group_v00,
       data = BD_bat)
b = lm(insulin_d ~ grupo_int + Eubacterium_hallii_group_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + Eubacterium_hallii_group_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Eubacterium_hallii_group_d", dropobs = T)
summary(m)

a = lm(Dorea_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + Dorea_v00,
       data = BD_bat)
b = lm(insulin_d ~ grupo_int + Dorea_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + insulin_v00 + Dorea_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Dorea_d", dropobs = T)
summary(m)

a = lm(Eubacterium_hallii_group_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + Eubacterium_hallii_group_v00,
       data = BD_bat)
b = lm(HOMA.IR_d ~ grupo_int + Eubacterium_hallii_group_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + Eubacterium_hallii_group_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Eubacterium_hallii_group_d", dropobs = T)
summary(m)

a = lm(Dorea_d ~ grupo_int + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + Dorea_v00,
       data = BD_bat)
b = lm(HOMA.IR_d ~ grupo_int + Dorea_d + nodo + fuma_s1 + diab_prev_s1 + sexo_s1 +edad_cat + alcoholg_v00 + hta_s1 + HOMA.IR + Dorea_v00,
       data = BD_bat)
m = mediate(a,b,sims = 1000, treat = "grupo_int", mediator = "Dorea_d", dropobs = T)
summary(m)

## Excel con los p ajustados:

padj <- read_excel("p-adj-values.xlsx")
padj2 = read_excel("p-adj-values.xlsx", 
                   sheet = "Hoja1")
padj3 = merge(padj2,padj, by = 'names')
export(padj3, 'p-values-adj-tabla.xlsx')

## Flow chart:

FC = read_excel("G:/trabajo/ARTICULOS/Co-autor/PPLUS/NIH/Obj 3/AJCN/metagenomica_HOLANDA.xlsx")
table(FC$Timepoint, FC$Nodo)

FC1 = subset(FC, Timepoint == 'A')
table(FC1$Timepoint, FC1$Nodo)
FC2 = subset(FC, Timepoint == 'B')
table(FC2$Timepoint, FC2$Nodo)
FC3 = merge(FC1, FC2, by = "OriginalID", all.y = T)
table(FC3$Timepoint.x, FC3$Nodo.x)
table(FC3$Timepoint.x=='A' & FC3$Timepoint.y=='B', FC3$Nodo.x)
