*Fusion de la BBDD ART IG - FRACT PREDIMED Reus (completa) en BD-Reus-COMPLETA-incluye-PREDIBONE-30032011.
*Primero ordenar las variables de forma ascendente segun el ID en ambas BBDD y observar que todas tienen en mismo número de casos.
MATCH FILES /FILE=*
  /FILE='Conjunto_de_datos2'
  /RENAME (frutatot3 frutatot4 frutatot5 frutatot6 frutatot7 frutatot8 frutatot verdutot3 verdutot4 
    verdutot5 verdutot6 verdutot7 verdutot8 verdutot GLU_OC_100_anual GLU_OC_100_inicial ac_girasol4 
    ac_girasol5 ac_girasol6 ac_girasol7 ac_girasol8 ac_maiz4 ac_maiz5 ac_maiz6 ac_maiz7 ac_maiz8 
    ac_mezcla4 ac_mezcla5 ac_mezcla6 ac_mezcla7 ac_mezcla8 ac_oliva4 ac_oliva5 ac_oliva6 ac_oliva7 
    ac_oliva8 ac_olivavir4 ac_olivavir5 ac_olivavir6 ac_olivavir7 ac_olivavir8 ac_orujo4 ac_orujo5 
    ac_orujo6 ac_orujo7 ac_orujo8 ac_soja4 ac_soja5 ac_soja6 ac_soja7 ac_soja8 ac_girasol ac_girasol3 
    ac_maiz ac_maiz3 ac_orujo ac_orujo3 ac_oliva ac_oliva3 ac_olivavir ac_olivavir3 ac_soja ac_soja3 
    aceitunas aceitunas3 aceitunas4 aceitunas5 aceitunas6 aceitunas7 aceitunas8 acelgas acelgas3 
    acelgas4 acelgas5 acelgas6 acelgas7 acelgas8 linolenico3 linolenico4 linolenico5 linolenico6 
    linolenico7 linolenico8 linolenico ACFOL3 ACFOL4 ACFOL5 ACFOL6 ACFOL7 ACFOL8 acfol n6 n6_3 n6_4 
    n6_5 n6_6 n6_7 n6_8 gehos_1 gehoa_1 ge6_12s_1 ge6_12a_1 ge2_3_5s_1 ge2_3_5a_1 ge4_5_5s_1 ge4_5_5a_1 
    getots_1 getota_1 ano_af_1 dia_af_1 mes_af_1 gehos_2 gehoa_2 ge6_12s_2 ge6_12a_2 ge2_3_5s_2 
    ge2_3_5a_2 ge4_5_5s_2 ge4_5_5a_2 getots_2 getota_2 ano_af_2 dia_af_2 mes_af_2 gehos_3 gehoa_3 
    ge6_12s_3 ge6_12a_3 ge2_3_5s_3 ge2_3_5a_3 ge4_5_5s_3 ge4_5_5a_3 getots_3 getota_3 ano_af_3 dia_af_3 
    mes_af_3 gehos_4 gehoa_4 ge6_12s_4 ge6_12a_4 ge2_3_5s_4 ge2_3_5a_4 ge4_5_5s_4 ge4_5_5a_4 getots_4 
    getota_4 ano_af_4 dia_af_4 mes_af_4 gehos_5 gehoa_5 ge6_12s_5 ge6_12a_5 ge2_3_5s_5 ge2_3_5a_5 
    ge4_5_5s_5 ge4_5_5a_5 getots_5 getota_5 ano_af_5 dia_af_5 mes_af_5 gehos_6 gehoa_6 ge6_12s_6 
    ge6_12a_6 ge2_3_5s_6 ge2_3_5a_6 ge4_5_5s_6 ge4_5_5a_6 getots_6 getota_6 ano_af_6 dia_af_6 mes_af_6 
    gehos_7 gehoa_7 ge6_12s_7 ge6_12a_7 ge2_3_5s_7 ge2_3_5a_7 ge4_5_5s_7 ge4_5_5a_7 getots_7 getota_7 
    ano_af_7 dia_af_7 mes_af_7 gehos_8 gehoa_8 ge6_12s_8 ge6_12a_8 ge2_3_5s_8 ge2_3_5a_8 ge4_5_5s_8 
    ge4_5_5a_8 getots_8 getota_8 ano_af_8 dia_af_8 mes_af_8 MO3 MO4 MO5 MO6 MO7 MO8 mo PO3 PO4 PO5 PO6 
    PO7 PO8 po SA3 SA4 SA5 SA6 SA7 SA8 sa ajos ajos3 ajos4 ajos5 ajos6 ajos7 ajos8 almejas4 almejas5 
    almejas6 almejas7 almejas8 almibar4 almibar5 almibar6 almibar7 almibar8 alubiaseca alubiaseca3 
    alubiaseca4 alubiaseca5 alubiaseca6 alubiaseca7 alubiaseca8 gluv1 @25OHvitDv1 cal24hrsv1 
    calciosuerov1 creatininav1 desoxcreativ1 fosalc_biliarv1 fosalc_hepaticav1 fosalc_intestinalv1 
    fosalc_oseav1 osteoprotegerinav1 PTHv1 gluv3 @25OHvitDv3 cal24hrsv3 calciosuerov3 creatininav3 
    desoxcreativ3 fosalc_biliarv3 fosalc_hepaticav3 fosalc_intestinalv3 fosalc_oseav3 
    osteoprotegerinav3 PTHv3 gluv4 gluv5 gluv6 gluv7 gluv8 arrozblan arrozblan3 arrozblan4 arrozblan5 
    arrozblan6 arrozblan7 arrozblan8 azuc4 azuc5 azuc6 azuc7 azuc8 azuc azuc3 anonac bacalao4 bacalao5 
    bacalao6 bacalao7 bacalao8 bacon4 bacon5 bacon6 bacon7 bacon8 batidos batidos3 batidos4 batidos5 
    batidos6 batidos7 batidos8 betacarot3 betacarot4 betacarot5 betacarot6 betacarot7 betacarot8 
    betacarot bizcocho4 bizcocho5 bizcocho6 bizcocho7 bizcocho8 BUAref c_cerdo4 c_cerdo5 c_cerdo6 
    c_cerdo7 c_cerdo8 c_conejo4 c_conejo5 c_conejo6 c_conejo7 c_conejo8 c_cordero4 c_cordero5 
    c_cordero6 c_cordero7 c_cordero8 c_ternera4 c_ternera5 c_ternera6 c_ternera7 c_ternera8 cacao 
    cacao3 cacao4 cacao5 cacao6 cacao7 cacao8 cafedesc cafedesc3 cafedesc4 cafedesc5 cafedesc6 
    cafedesc7 cafedesc8 cafes cafes3 cafes4 cafes5 cafes6 cafes7 cafes8 calabacin calabacin3 calabacin4 
    calabacin5 calabacin6 calabacin7 calabacin8 calamar4 calamar5 calamar6 calamar7 calamar8 calamar 
    calamar3 CALCIO3 CALCIO4 CALCIO5 CALCIO6 CALCIO7 CALCIO8 calcio CARGLUCE3 CARGLUCE4 CARGLUCE5 
    CARGLUCE6 CARGLUCE7 CARGLUCE8 cargluce c_cerdo c_cerdo3 c_conejo c_conejo3 c_cordero c_cordero3 
    c_ternera c_ternera3 cavas cavas3 cavas4 cavas5 cavas6 cavas7 cavas8 cebollas cebollas3 cebollas4 
    cebollas5 cebollas6 cebollas7 cebollas8 centro cerealdes4 cerealdes5 cerealdes6 cerealdes7 
    cerealdes8 cerealdes cerealdes3 muesli muesli3 cerezas cerezas3 cerezas4 cerezas5 cerezas6 cerezas7 
    cerezas8 cervezas cervezas3 cervezas4 cervezas5 cervezas6 cervezas7 cervezas8 chocolates 
    chocolates3 chocolates4 chocolates5 chocolates6 chocolates7 chocolates8 churro4 churro5 churro6 
    churro7 churro8 churro churro3 coles coles3 coles4 coles5 coles6 coles7 coles8 colest3 colest4 
    colest5 colest6 colest7 colest8 colest croissant4 croissant5 croissant6 croissant7 croissant8 
    croquetas croquetas3 croquetas4 croquetas5 croquetas6 croquetas7 croquetas8 crustaceos4 crustaceos5 
    crustaceos6 crustaceos7 crustaceos8 imp1_buader imp1_buaizq imp1_dmoder imp1_dmoizq imp1_quider 
    imp1_quiizq imp1_sosder imp1_sosizq imp1_tscoreder imp1_tscoreizq imp3_buader imp3_buaizq 
    imp3_dmoder imp3_dmoizq imp3_quider imp3_quiizq imp3_sosder imp3_sosizq imp3_tscoreder 
    imp3_tscoreizq imp4_buader imp4_buaizq imp4_dmoder imp4_dmoizq imp4_quider imp4_quiizq imp4_sosder 
    imp4_sosizq imp4_tscoreder imp4_tscoreizq whisky whisky3 SD_dmo_ref dianac daysseg3 daysseg4 
    daysseg5 daysseg6 daysseg7 daysseg8 DMOref donut donut3 donut4 donut5 donut6 donut7 donut8 
    embutidos embutidos3 embutidos4 embutidos5 embutidos6 embutidos7 embutidos8 energiat3 energiat4 
    energiat5 energiat6 energiat7 energiat8 energiat espagueti4 espagueti5 espagueti6 espagueti7 
    espagueti8 esparragos esparragos3 esparragos4 esparragos5 esparragos6 esparragos7 esparragos8 
    f_secos4 f_secos5 f_secos6 f_secos7 f_secos8 datgral datinclu VAR00001 datseg2 datseg3 datseg4 
    datseg5 datseg6 datseg7 datseg8 fibra3 fibra4 fibra5 fibra6 fibra7 fibra8 fibra FIT3 FIT4 FIT5 FIT6 
    FIT7 FIT8 fit fresa fresa3 fresa4 fresa5 fresa6 fresa7 fresa8 pasas pasas3 almibar almibar3 f_secos 
    f_secos3 p P3 P4 P5 P6 P7 P8 g_choco4 g_choco5 g_choco6 g_choco7 g_choco8 g_integral4 g_integral5 
    g_integral6 g_integral7 g_integral8 g_maria4 g_maria5 g_maria6 g_maria7 g_maria8 g_choco g_choco3 
    g_integral g_integral3 g_maria g_maria3 crustaceos crustaceos3 garbanzos garbanzos3 garbanzos4 
    garbanzos5 garbanzos6 garbanzos7 garbanzos8 gazpach4 gazpach5 gazpach6 gazpach7 gazpach8 gazpach 
    gazpach3 altura1 acei_añ1 vino_añ1 cadera1 cint1 acei_di1 vino_di1 aspirin1 insulin1 hipocol1 
    cardiov1 ado1 hipoten1 analge1 otromed1 tranqui1 hormo1 vitamin1 edad_ane1 edad_apn1 eda_epoc1 
    edad_car1 edad_cat1 edad_dem1 edad_dep1 edad_em1 edad_par1 edad_fra1 edad_ic1 edad_nef1 edad_ret1 
    edad_tro1 tipo_can1 est_civi1 pseidd1 pseiid1 psesdd1 fc_d_1a fc_d_2a fc_i_1a fc_i_2a grup_int 
    acei_ho1 vino_ho1 imc1 lugarnac1 med11_d med11a med11b med11c med11 med11clas med12_d med12a med12b 
    med12c med12 med12clas med13_d med13a med13b med13c med13 med13clas med14_d med14a med14b med14c 
    med14 med14clas med15_d med15a med15b med15c med15 med15clas med16_d med16a med16b med16c med16 
    med16clas med17_d med17a med17b med17c med17 med17clas med18_d med18a med18b med18c med18 med18clas 
    acei_me1 vino_me1 numper1 pad_esd_1a pad_esd_2a pad_esi_1a pad_esi_2a paisnac1 pas_esd_1a 
    pas_esd_2a pas_esi_1a pas_esi_2a peso1 edad_men1 fam_exit1 fam_avc1 fam_col1 fam_hta1 fam_can1 
    beb_mañ1 sitlabor1 escolar1 aneuris1 apneas1 bronqui1 cancer1 cardio1 catarata1 demenc1 depre1 
    embolia1 parckin1 icizq1 nefro1 retino1 troboli1 fractura1 disnea1 beb_meno1 mol_bebe1 edad_can1 
    trab_cab1 trab_pac1 pertensa1 beb_mal1 enferm1 alcoholg3 alcoholg4 alcoholg5 alcoholg6 alcoholg7 
    alcoholg8 alcoholg olivatot3 olivatot4 olivatot5 olivatot6 olivatot7 olivatot8 olivatot carnicos3 
    carnicos4 carnicos5 carnicos6 carnicos7 carnicos8 carnicos grupocer3 grupocer4 grupocer5 grupocer6 
    grupocer7 grupocer8 grupocer fsecos3 fsecos4 fsecos5 fsecos6 fsecos7 fsecos8 fsecos gallet3 gallet4 
    gallet5 gallet6 gallet7 gallet8 gallet legumbre3 legumbre4 legumbre5 legumbre6 legumbre7 legumbre8 
    legumbre lacteos3 lacteos4 lacteos5 lacteos6 lacteos7 lacteos8 lacteos pescados3 pescados4 
    pescados5 pescados6 pescados7 pescados8 pescados vino3 vino4 vino5 vino6 vino7 vino8 vino guisantes 
    guisantes3 guisantes4 guisantes5 guisantes6 guisantes7 guisantes8 hamburguesa hamburguesa3 
    hamburguesa4 hamburguesa5 hamburguesa6 hamburguesa7 hamburguesa8 Hb1Ac Hb1Ac3 Hb1Ac4 Hb1Ac5 Hb1Ac7 
    Hb1Ac8 helados helados3 helados4 helados5 helados6 helados7 helados8 HC3 HC4 HC5 HC6 HC7 HC8 hc FE3 
    FE4 FE5 FE6 FE7 FE8 fe FE_HEMO3 FE_HEMO4 FE_HEMO5 FE_HEMO6 FE_HEMO7 FE_HEMO8 FE_HEMO N_HEMO3 
    N_HEMO4 N_HEMO5 N_HEMO6 N_HEMO7 N_HEMO8 N_HEMO higad4 higad5 higad6 higad7 higad8 higad higad3 
    huevos huevos3 huevos4 huevos5 huevos6 huevos7 huevos8 id pipas0 cigarril0 puros0 año_diag0 
    tipo_arr0 edad0 hdl_inc0 idl_inc0 col_inc0 pdias0 psist0 trigl_inc0 años_tab0 inclus motiv_ex 
    excl_mot proceden0 c_fruta0 ant_iam0 cap_cam0 c_grasan0 tabaco0 angor0 arritmia0 avc0 claudica0 
    iam0 diabetes0 prob_die0 hipercol0 hta0 cambdon0 trathta0 tra_col0 nuez imc3 nuez3 imc4 nuez4 imc5 
    nuez5 imc6 nuez6 imc7 nuez7 imc8 nuez8 insulina_v1 insulina_v3 j_cocido4 j_cocido5 j_cocido6 
    j_cocido7 j_cocido8 j_serrano4 j_serrano5 j_serrano6 j_serrano7 j_serrano8 j_cocido j_cocido3 
    j_serrano j_serrano3 jverde jverde3 jverde4 jverde5 jverde6 jverde7 jverde8 kiwis kiwis3 kiwis4 
    kiwis5 kiwis6 kiwis7 kiwis8 leche_cond leche_cond3 leche_desn leche_desn3 leche_ent leche_ent3 
    leche_semi leche_semi3 leche_cond4 leche_cond5 leche_cond6 leche_cond7 leche_cond8 leche_desn4 
    leche_desn5 leche_desn6 leche_desn7 leche_desn8 leche_ent4 leche_ent5 leche_ent6 leche_ent7 
    leche_ent8 leche_semi4 leche_semi5 leche_semi6 leche_semi7 leche_semi8 lechug4 lechug5 lechug6 
    lechug7 lechug8 lechug lechug3 lenteja lenteja3 lenteja4 lenteja5 lenteja6 lenteja7 lenteja8 
    licores3 licores licores4 licores5 licores6 licores7 licores8 gratot3 gratot4 gratot5 gratot6 
    gratot7 gratot8 gratot magdalena magdalena3 magdalena4 magdalena5 magdalena6 magdalena7 magdalena8 
    mg3 mg4 mg5 mg6 mg7 mg8 mg mantecacer mantecacer3 mantecacer4 mantecacer5 mantecacer6 mantecacer7 
    mantecacer8 mazapan mazapan3 mantequillas mantequillas3 mantequillas4 mantequillas5 mantequillas6 
    mantequillas7 mantequillas8 manzanas manzanas3 manzanas4 manzanas5 manzanas6 manzanas7 manzanas8 
    margarinas margarinas3 margarinas4 margarinas5 margarinas6 margarinas7 margarinas8 mayones4 
    mayones5 mayones6 mayones7 mayones8 mayones mayones3 mazapan4 mazapan5 mazapan6 mazapan7 mazapan8 
    medico melo4 melo5 melo6 melo7 melo8 melocoton4 melocoton5 melocoton6 melocoton7 melocoton8 
    melocoton melocoton3 melo melo3 mermeladas mermeladas3 mermeladas4 mermeladas5 mermeladas6 
    mermeladas7 mermeladas8 mesnac ac_mezcla ac_mezcla3 mieles mieles3 mieles4 mieles5 mieles6 mieles7 
    mieles8 mitja_BUA_2a mitja_DMO_2a mitja_QUI_2a mitja_SOS_2a mostaz4 mostaz5 mostaz6 mostaz7 mostaz8 
    mostaz mostaz3 mostos mostos3 mostos4 mostos5 mostos6 mostos7 mostos8 muesli4 muesli5 muesli6 
    muesli7 muesli8 naranjas naranjas3 naranjas4 naranjas5 naranjas6 naranjas7 naranjas8 nata_crema 
    nata_crema3 nata_crema4 nata_crema5 nata_crema6 nata_crema7 nata_crema8 natilla4 natilla5 natilla6 
    natilla7 natilla8 natilla natilla3 nodo paciente n3marinos3 n3marinos4 n3marinos5 n3marinos6 
    n3marinos7 n3marinos8 n3marinos n3nomarino3 n3nomarino4 n3nomarino5 n3nomarino6 n3nomarino7 
    n3nomarino8 n3nomarino OC_anual OC_inicial almejas almejas3 otrasver otrasver3 otrasver4 otrasver5 
    otrasver6 otrasver7 otrasver8 quesos quesos3 anop14m_1 diap14m_1 mesp14m_1 p14totm_1 p14_8m_1 
    p14_13m_1 p14_7m_1 p14_4m_1 p14_5m_1 p14_9m_1 p14_6m_1 p14_10m_1 p14_3m_1 p14_14m_1 p14_12m_1 
    p14_11m_1 p14_2m_1 p14_1m_1 anop14m_2 diap14m_2 mesp14m_2 p14totm_2 p14_8m_2 p14_13m_2 p14_7m_2 
    p14_4m_2 p14_5m_2 p14_9m_2 p14_6m_2 p14_10m_2 p14_3m_2 p14_14m_2 p14_12m_2 p14_11m_2 p14_2m_2 
    p14_1m_2 anop14m_3 diap14m_3 mesp14m_3 p14totm_3 p14_8m_3 p14_13m_3 p14_7m_3 p14_4m_3 p14_5m_3 
    p14_9m_3 p14_6m_3 p14_10m_3 p14_3m_3 p14_14m_3 p14_12m_3 p14_11m_3 p14_2m_3 p14_1m_3 anop14m_4 
    diap14m_4 mesp14m_4 p14totm_4 p14_8m_4 p14_13m_4 p14_7m_4 p14_4m_4 p14_5m_4 p14_9m_4 p14_6m_4 
    p14_10m_4 p14_3m_4 p14_14m_4 p14_12m_4 p14_11m_4 p14_2m_4 p14_1m_4 anop14m_5 diap14m_5 mesp14m_5 
    p14totm_5 p14_8m_5 p14_13m_5 p14_7m_5 p14_4m_5 p14_5m_5 p14_9m_5 p14_6m_5 p14_10m_5 p14_3m_5 
    p14_14m_5 p14_12m_5 p14_11m_5 p14_2m_5 p14_1m_5 anop14m_6 diap14m_6 mesp14m_6 p14totm_6 p14_8m_6 
    p14_13m_6 p14_7m_6 p14_4m_6 p14_5m_6 p14_9m_6 p14_6m_6 p14_10m_6 p14_3m_6 p14_14m_6 p14_12m_6 
    p14_11m_6 p14_2m_6 p14_1m_6 anop14m_7 diap14m_7 mesp14m_7 p14totm_7 p14_8m_7 p14_13m_7 p14_7m_7 
    p14_4m_7 p14_5m_7 p14_9m_7 p14_6m_7 p14_10m_7 p14_3m_7 p14_14m_7 p14_12m_7 p14_11m_7 p14_2m_7 
    p14_1m_7 anop14m_8 diap14m_8 mesp14m_8 p14totm_8 p14_8m_8 p14_13m_8 p14_7m_8 p14_4m_8 p14_5m_8 
    p14_9m_8 p14_6m_8 p14_10m_8 p14_3m_8 p14_14m_8 p14_12m_8 p14_11m_8 p14_2m_8 p14_1m_8 anop14_1 
    diap14_1 mesp14_1 p14tot_1 p14_8_1 p14_13_1 p14_7_1 p14_4_1 p14_5_1 p14_9_1 p14_6_1 p14_10_1 
    p14_3_1 p14_14_1 p14_12_1 p14_11_1 p14_2_1 p14_1_1 anop14_2 diap14_2 mesp14_2 p14tot_2 p14_8_2 
    p14_13_2 p14_7_2 p14_4_2 p14_5_2 p14_9_2 p14_6_2 p14_10_2 p14_3_2 p14_14_2 p14_12_2 p14_11_2 
    p14_2_2 p14_1_2 anop14_3 diap14_3 mesp14_3 p14tot_3 p14_8_3 p14_13_3 p14_7_3 p14_4_3 p14_5_3 
    p14_9_3 p14_6_3 p14_10_3 p14_3_3 p14_14_3 p14_12_3 p14_11_3 p14_2_3 p14_1_3 anop14_4 diap14_4 
    mesp14_4 p14tot_4 p14_8_4 p14_13_4 p14_7_4 p14_4_4 p14_5_4 p14_9_4 p14_6_4 p14_10_4 p14_3_4 
    p14_14_4 p14_12_4 p14_11_4 p14_2_4 p14_1_4 anop14_5 diap14_5 mesp14_5 p14tot_5 p14_8_5 p14_13_5 
    p14_7_5 p14_4_5 p14_5_5 p14_9_5 p14_6_5 p14_10_5 p14_3_5 p14_14_5 p14_12_5 p14_11_5 p14_2_5 p14_1_5 
    anop14_6 diap14_6 mesp14_6 p14tot_6 p14_8_6 p14_13_6 p14_7_6 p14_4_6 p14_5_6 p14_9_6 p14_6_6 
    p14_10_6 p14_3_6 p14_14_6 p14_12_6 p14_11_6 p14_2_6 p14_1_6 p_aceite4 p_aceite5 p_aceite6 p_aceite7 
    p_aceite8 p_azul4 p_azul5 p_azul6 p_azul7 p_azul8 p_blanco4 p_blanco5 p_blanco6 p_blanco7 p_blanco8 
    p_cocidas4 p_cocidas5 p_cocidas6 p_cocidas7 p_cocidas8 p_fritascas4 p_fritascas5 p_fritascas6 
    p_fritascas8 p_fritascom4 p_fritascom5 p_fritascom6 p_fritascom7 p_fritascom8 p_natural4 p_natural5 
    p_natural6 p_natural7 p_natural8 panblanco panblanco3 paninteg paninteg3 panblanco4 panblanco5 
    panblanco6 panblanco7 panblanco8 paninteg4 paninteg5 paninteg6 paninteg7 paninteg8 pasas4 pasas5 
    pasas6 pasas7 pasas8 espagueti3 espagueti pastel pastel3 pastel4 pastel5 pastel6 pastel7 pastel8 
    p_cocidas p_cocidas3 p_fritascas p_fritascas3 p_fritascom p_fritascom3 pates pates3 pates4 pates5 
    pates6 pates7 pates8 p_azul p_azul3 p_blanco p_blanco3 p_natural p_natural3 p_aceite p_aceite3 
    bacalao bacalao3 petitsuisse petitsuisse3 petitsuisse4 petitsuisse5 petitsuisse6 petitsuisse7 
    petitsuisse8 pimientos pimientos3 pimientos4 pimientos5 pimientos6 pimientos7 pimientos8 pizzas 
    pizzas3 pizzas4 pizzas5 pizzas6 pizzas7 pizzas8 platanos platanos3 platanos4 platanos5 platanos6 
    platanos7 platanos8 pollopiel3 pollopiel pollonopiel pollonopiel3 pollonopiel4 pollonopiel5 
    pollonopiel6 pollonopiel7 pollonopiel8 pollopiel4 pollopiel5 pollopiel6 pollopiel7 pollopiel8 K3 K4 
    K5 K6 K7 K8 k PROT3 PROT4 PROT5 PROT6 PROT7 PROT8 prot q_fresco4 q_fresco5 q_fresco6 q_fresco7 
    q_fresco8 quesito quesito3 quesito4 quesito5 quesito6 quesito7 quesito8 q_fresco q_fresco3 quesos4 
    quesos5 quesos6 quesos7 quesos8 QUIref refrescos refrescos3 refrescsin3 refrescsin refrescos4 
    refrescos5 refrescos6 refrescos7 refrescos8 refrescsin4 refrescsin5 refrescsin6 refrescsin7 
    refrescsin8 bizcocho bizcocho3 croissant croissant3 requeson requeson3 requeson4 requeson5 
    requeson6 requeson7 requeson8 tomatefri tomatefri3 sandi4 sandi5 sandi6 sandi7 sandi8 sandi sandi3 
    SD_BUA_ref SD_QUI_ref SD_SOS_ref adhesion2 retinop2 inciden2 pipas2 cigarr2 puros2 acei_añ2 
    vino_añ2 cadera2 cintura2 acei_di2 vino_di2 aspirin2 insulin2 hipocol2 cardiov2 ado2 hipoten2 
    analge2 otromed2 tranqui2 hormo2 vitamin2 dialis2 nefrodb2 est_civi2 pseidd2 pseiid2 psesdd2 
    fc_d_1a2 fc_d_2a2 fc_i_1a2 fc_i_2a2 acei_ho2 vino_ho2 imc2 med210_d med210a med210b med210c med210 
    med210clas med211_d med211a med211b med211c med211 med211clas med212_d med212a med212b med212c 
    med212 med212clas acei_me2 vino_me2 muestra2 n_perho2 pad_esd_1a2 pad_esd_2a2 pad_esi_1a2 
    pad_esi_2a2 pas_esd_1a2 pas_esd_2a2 pas_esi_1a2 pas_esi_2a2 peso2 tipoenf2 info_recogida2 tipoint2 
    sit_labo2 claudi2 tabaco2 cam_pho2 cam_med2 cam_est2 cam_tab2 cam_slab2 enferm2 iqvasc2 disnea2 
    intquir2 angor2 arritm2 avc2 iam2 paro2 angiop2 hipecol2 hta2 aneuris2 catarat2 n_trab_c2 
    n_treball2 diabete2 adhesion3 retinop3 inciden3 pipas3 cigarr3 puros3 acei_añ3 vino_añ3 cadera3 
    cintura3 acei_di3 vino_di3 aspirin3 insulin3 hipocol3 cardiov3 ado3 hipoten3 analge3 otromed3 
    tranqui3 hormo3 vitamin3 dialis3 nefrodb3 est_civi3 pseidd3 pseiid3 psesdd3 fc_d_1a3 fc_d_2a3 
    fc_i_1a3 fc_i_2a3 acei_ho3 vino_ho3 med310_d med310a med310b med310c med310 med310clas med311_d 
    med311a med311b med311c med311 med311clas med312_d med312a med312b med312c med312 med312clas 
    acei_me3 vino_me3 muestra3 n_perho3 pad_esd_1a3 pad_esd_2a3 pad_esi_1a3 pad_esi_2a3 pas_esd_1a3 
    pas_esd_2a3 pas_esi_1a3 pas_esi_2a3 peso3 tipoenf3 info_recogida3 tipoint3 sit_labo3 claudi3 
    tabaco3 cam_pho3 cam_med3 cam_est3 cam_tab3 cam_slab3 enferm3 iqvasc3 disnea3 intquir3 angor3 
    arritm3 avc3 iam3 paro3 angiop3 hipecol3 hta3 aneuris3 catarat3 n_trab_c3 n_treball3 diabete3 
    adhesion4 retinop4 inciden4 pipas4 cigarr4 puros4 acei_añ4 vino_añ4 cadera4 cintura4 acei_di4 
    vino_di4 aspirin4 insulin4 hipocol4 cardiov4 ado4 hipoten4 analge4 otromed4 tranqui4 hormo4 
    vitamin4 dialis4 nefrodb4 est_civi4 pseidd4 pseiid4 psesdd4 fc_d_1a4 fc_d_2a4 fc_i_1a4 fc_i_2a4 
    acei_ho4 vino_ho4 med410_d med410a med410b med410c med410 med410clas med411_d med411a med411b 
    med411c med411 med411clas med412_d med412a med412b med412c med412 med412clas acei_me4 vino_me4 
    muestra4 n_perho4 pad_esd_1a4 pad_esd_2a4 pad_esi_1a4 pad_esi_2a4 pas_esd_1a4 pas_esd_2a4 
    pas_esi_1a4 pas_esi_2a4 peso4 tipoenf4 info_recogida4 tipoint4 sit_labo4 claudi4 tabaco4 cam_pho4 
    cam_med4 cam_est4 cam_tab4 cam_slab4 enferm4 iqvasc4 disnea4 intquir4 angor4 arritm4 avc4 iam4 
    paro4 angiop4 hipecol4 hta4 aneuris4 catarat4 n_treball4 n_trab_c4 diabete4 adhesion5 retinop5 
    inciden5 pipas5 cigarr5 puros5 acei_añ5 vino_añ5 cadera5 cintura5 acei_di5 vino_di5 aspirin5 
    insulin5 hipocol5 cardiov5 ado5 hipoten5 analge5 otromed5 tranqui5 hormo5 vitamin5 dialis5 nefrodb5 
    est_civi5 pseidd5 pseiid5 psesdd5 fc_d_1a5 fc_d_2a5 fc_i_1a5 fc_i_2a5 acei_ho5 vino_ho5 med510_d 
    med510a med510b med510c med510 med510clas med511_d med511a med511b med511c med511 med511clas 
    med512_d med512a med512b med512c med512 med512clas acei_me5 vino_me5 muestra5 n_perho5 pad_esd_1a5 
    pad_esd_2a5 pad_esi_1a5 pad_esi_2a5 pas_esd_1a5 pas_esd_2a5 pas_esi_1a5 pas_esi_2a5 peso5 tipoenf5 
    info_recogida5 tipoint5 sit_labo5 claudi5 tabaco5 cam_pho5 cam_med5 cam_est5 cam_tab5 cam_slab5 
    enferm5 iqvasc5 disnea5 intquir5 angor5 arritm5 avc5 iam5 paro5 angiop5 hipecol5 hta5 aneuris5 
    catarat5 n_trab_c5 n_treball5 diabete5 adhesion6 retinop6 inciden6 pipas6 cigarr6 puros6 cadera6 
    cintura6 acei_di6 vino_di6 aspirin6 insulin6 hipocol6 cardiov6 ado6 hipoten6 analge6 otromed6 
    tranqui6 hormo6 vitamin6 dialis6 nefrodb6 est_civi6 pseidd6 pseiid6 psesdd6 fc_d_1a6 fc_d_2a6 
    fc_i_1a6 fc_i_2a6 acei_ho6 vino_ho6 med610_d med610a med610b med610c med610 med610clas med611_d 
    med611a med611b med611c med611 med611clas med612_d med612a med612b med612c med612 med612clas 
    acei_me6 vino_me6 muestra6 n_perho6 pad_esd_1a6 pad_esd_2a6 pad_esi_1a6 pad_esi_2a6 pas_esd_1a6 
    pas_esd_2a6 pas_esi_1a6 pas_esi_2a6 peso6 tipoenf6 info_recogida6 tipoint6 sit_labo6 claudi6 
    tabaco6 cam_pho6 cam_med6 cam_est6 cam_tab6 cam_slab6 enferm6 iqvasc6 disnea6 intquir6 angor6 
    arritm6 avc6 iam6 paro6 angiop6 hipecol6 hta6 aneuris6 catarat6 n_trab_c6 n_treball6 diabete6 
    adhesion7 retinop7 inciden7 pipas7 cigarr7 puros7 acei_añ7 vino_añ7 cadera7 cintura7 acei_di7 
    vino_di7 aspirin7 insulin7 hipocol7 cardiov7 ado7 hipoten7 analge7 otromed7 tranqui7 hormo7 
    vitamin7 dialis7 nefrodb7 est_civi7 pseidd7 pseiid7 psesdd7 fc_d_1a7 fc_d_2a7 fc_i_1a7 fc_i_2a7 
    acei_ho7 vino_ho7 med710_d med710a med710b med710c med710 med710clas med711_d med711a med711b 
    med711c med711 med711clas med712_d med712a med712b med712c med712 med712clas acei_me7 vino_me7 
    muestra7 n_perho7 pad_esd_1a7 pad_esd_2a7 pad_esi_1a7 pad_esi_2a7 pas_esd_1a7 pas_esd_2a7 
    pas_esi_1a7 pas_esi_2a7 peso7 tipoenf7 info_recogida7 tipoint7 sit_labo7 claudi7 tabaco7 cam_pho7 
    cam_med7 cam_est7 cam_tab7 cam_slab7 enferm7 iqvasc7 disnea7 intquir7 angor7 arritm7 avc7 iam7 
    paro7 angiop7 hipecol7 hta7 aneuris7 catarat7 n_trab_c7 n_treball7 diabete7 adhesion8 retinop8 
    inciden8 pipas8 cigarr8 puros8 acei_añ8 vino_añ8 cadera8 cintura8 acei_di8 vino_di8 aspirin8 
    insulin8 hipocol8 cardiov8 ado8 hipoten8 analge8 otromed8 tranqui8 hormo8 vitamin8 dialis8 nefrodb8 
    est_civi8 pseidd8 pseiid8 psesdd8 fc_d_1a8 fc_d_2a8 fc_i_1a8 fc_i_2a8 acei_ho8 vino_ho8 med810_d 
    med810a med810b med810c med810 med810clas med811_d med811a med811b med811c med811 med811clas 
    med812_d med812a med812b med812c med812 med812clas acei_me8 vino_me8 muestra8 n_perho8 pad_esd_1a8 
    pad_esd_2a8 pad_esi_1a8 pad_esi_2a8 pas_esd_1a8 pas_esd_2a8 pas_esi_1a8 pas_esi_2a8 peso8 tipoenf8 
    info_recogida8 tipoint8 sit_labo8 claudi8 tabaco8 cam_pho8 cam_med8 cam_est8 cam_tab8 cam_slab8 
    enferm8 iqvasc8 disnea8 intquir8 angor8 arritm8 avc8 iam8 paro8 angiop8 hipecol8 hta8 aneuris8 
    catarat8 n_trab_c8 n_treball8 diabete8 seta4 seta5 seta6 seta7 seta8 seta seta3 sexo snack4 snack5 
    snack6 snack7 snack8 snack snack3 Na3 Na4 Na5 Na6 Na7 Na8 Na sopasobre sopasobre3 sopasobre4 
    sopasobre5 sopasobre6 sopasobre7 sopasobre8 SOSref fract3 B1 B1_3 B1_4 B1_5 B1_6 B1_7 B1_8 bacon 
    bacon3 anotol_3 sol_fsecos_3 salud_general_3 digestion_3 comidas_3 ritmo_3 combinados_3 una_toma_3 
    solos_3 varias_tomas_3 blando_3 duro_3 veces_dia_3 volumen_3 diatol_3 aceite_3 frutos_secos_3 
    mestol_3 otr_obs_fsecos_3 otr_obs_aoliva_3 otr_fsecos_3 tolerar_frutos_3 otr_aoliva_3 
    tolerar_aceite_3 anotol_4 sol_fsecos_4 salud_general_4 digestion_4 comidas_4 ritmo_4 combinados_4 
    una_toma_4 solos_4 varias_tomas_4 blando_4 duro_4 veces_dia_4 volumen_4 diatol_4 aceite_4 
    frutos_secos_4 mestol_4 otr_obs_fsecos_4 otr_obs_aoliva_4 otr_fsecos_4 tolerar_frutos_4 
    otr_aoliva_4 tolerar_aceite_4 anotol_5 sol_fsecos_5 salud_general_5 digestion_5 comidas_5 ritmo_5 
    combinados_5 una_toma_5 solos_5 varias_tomas_5 blando_5 duro_5 veces_dia_5 volumen_5 diatol_5 
    aceite_5 frutos_secos_5 mestol_5 otr_obs_fsecos_5 otr_obs_aoliva_5 otr_fsecos_5 tolerar_frutos_5 
    otr_aoliva_5 tolerar_aceite_5 anotol_6 sol_fsecos_6 salud_general_6 digestion_6 comidas_6 ritmo_6 
    combinados_6 una_toma_6 solos_6 varias_tomas_6 blando_6 duro_6 veces_dia_6 volumen_6 diatol_6 
    aceite_6 frutos_secos_6 mestol_6 otr_obs_fsecos_6 otr_obs_aoliva_6 otr_fsecos_6 tolerar_frutos_6 
    otr_aoliva_6 tolerar_aceite_6 anotol_7 sol_fsecos_7 salud_general_7 digestion_7 comidas_7 ritmo_7 
    combinados_7 una_toma_7 solos_7 varias_tomas_7 blando_7 duro_7 veces_dia_7 volumen_7 diatol_7 
    aceite_7 frutos_secos_7 mestol_7 otr_obs_fsecos_7 otr_obs_aoliva_7 otr_fsecos_7 tolerar_frutos_7 
    otr_aoliva_7 tolerar_aceite_7 anotol_8 sol_fsecos_8 salud_general_8 digestion_8 comidas_8 ritmo_8 
    combinados_8 una_toma_8 solos_8 varias_tomas_8 blando_8 duro_8 veces_dia_8 volumen_8 diatol_8 
    aceite_8 frutos_secos_8 mestol_8 otr_obs_fsecos_8 otr_obs_aoliva_8 otr_fsecos_8 tolerar_frutos_8 
    otr_aoliva_8 tolerar_aceite_8 tomat4 tomat5 tomat6 tomat7 tomat8 tomat tomat3 tomatefri4 tomatefri5 
    tomatefri6 tomatefri7 tomatefri8 turrones turrones3 turrones4 turrones5 turrones6 turrones7 
    turrones8 GLU_OC_inicial GLU_OC_anual uva uva3 uva4 uva5 uva6 uva7 uva8 v_blanco4 v_blanco5 
    v_blanco6 v_blanco7 v_blanco8 v_moscatel4 v_moscatel5 v_moscatel6 v_moscatel7 v_moscatel8 v_rosado4 
    v_rosado5 v_rosado6 v_rosado7 v_rosado8 v_tintojov4 v_tintojov5 v_tintojov6 v_tintojov7 v_tintojov8 
    v_blanco v_blanco3 v_moscatel v_moscatel3 v_rosado v_rosado3 v_tintojov v_tintojov3 visceras 
    visceras3 visceras4 visceras5 visceras6 visceras7 visceras8 dmo3 B12 B12_3 B12_4 B12_5 B12_6 B12_7 
    B12_8 B2 B2_3 B2_4 B2_5 B2_6 B2_7 B2_8 B6 B6_3 B6_4 B6_5 B6_6 B6_7 B6_8 vitC3 vitC4 vitC5 vitC6 
    vitC7 vitC8 vitC vitD3 vitD4 vitD5 vitD6 vitD7 vitD8 vitD vitE3 vitE4 vitE5 vitE6 vitE7 vitE8 vitE 
    whisky4 whisky5 whisky6 whisky7 whisky8 yogur_des yogur_des3 yogur_ent yogur_ent3 yogur_des4 
    yogur_des5 yogur_des6 yogur_des7 yogur_des8 yogur_ent4 yogur_ent5 yogur_ent6 yogur_ent7 yogur_ent8 
    z_botella4 z_botella5 z_botella6 z_botella7 z_botella8 z_frutasnat4 z_frutasnat5 z_frutasnat6 
    z_frutasnat7 z_frutasnat8 z_natural4 z_natural5 z_natural6 z_natural7 z_natural8 zanahor4 zanahor5 
    zanahor6 zanahor7 zanahor8 zanahor zanahor3 Zscore_BUA_2a Zscore_BUA_inicio Zscore_DMO_2a 
    Zscore_DMO_inicio Zscore_QUI_2a Zscore_QUI_inicio Zscore_SOS_inicio z_natural z_natural3 z_botella 
    z_botella3 z_frutasnat z_frutasnat3 q14p_1 q14p_2 q14p_3 q14p_4 q14p_5 q14p_6 q14pm_1 q14pm_2 
    q14pm_3 q14pm_4 q14pm_5 general inclu seg3 seg5 seg2 seg6 seg7 seg8 seg4 q14pm_6 q14pm_7 q14pm_8 
    af_1 af_2 af_3 af_4 af_5 af_6 af_7 af_8 tol_3 tol_4 tol_5 tol_6 tol_7 tol_8 IG3 IG4 IG5 IG6 IG7 IG8 
    ig = d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 d25 
    d26 d27 d28 d29 d30 d31 d32 d33 d34 d35 d36 d37 d38 d39 d40 d41 d42 d43 d44 d45 d46 d47 d48 d49 d50 
    d51 d52 d53 d54 d55 d56 d57 d58 d59 d60 d61 d62 d63 d64 d65 d66 d67 d68 d69 d70 d71 d72 d73 d74 d75 
    d76 d77 d78 d79 d80 d81 d82 d83 d84 d85 d86 d87 d88 d89 d90 d91 d92 d93 d94 d95 d96 d97 d98 d99 
    d100 d101 d102 d103 d104 d105 d106 d107 d108 d109 d110 d111 d112 d113 d114 d115 d116 d117 d118 d119 
    d120 d121 d122 d123 d124 d125 d126 d127 d128 d129 d130 d131 d132 d133 d134 d135 d136 d137 d138 d139 
    d140 d141 d142 d143 d144 d145 d146 d147 d148 d149 d150 d151 d152 d153 d154 d155 d156 d157 d158 d159 
    d160 d161 d162 d163 d164 d165 d166 d167 d168 d169 d170 d171 d172 d173 d174 d175 d176 d177 d178 d179 
    d180 d181 d182 d183 d184 d185 d186 d187 d188 d189 d190 d191 d192 d193 d194 d195 d196 d197 d198 d199 
    d200 d201 d202 d203 d204 d205 d206 d207 d208 d209 d210 d211 d212 d213 d214 d215 d216 d217 d218 d219 
    d220 d221 d222 d223 d224 d225 d226 d227 d228 d229 d230 d231 d232 d233 d234 d235 d236 d237 d238 d239 
    d240 d241 d242 d243 d244 d245 d246 d247 d248 d249 d250 d251 d252 d253 d254 d255 d256 d257 d258 d259 
    d260 d261 d262 d263 d264 d265 d266 d267 d268 d269 d270 d271 d272 d273 d274 d275 d276 d277 d278 d279 
    d280 d281 d282 d283 d284 d285 d286 d287 d288 d289 d290 d291 d292 d293 d294 d295 d296 d297 d298 d299 
    d300 d301 d302 d303 d304 d305 d306 d307 d308 d309 d310 d311 d312 d313 d314 d315 d316 d317 d318 d319 
    d320 d321 d322 d323 d324 d325 d326 d327 d328 d329 d330 d331 d332 d333 d334 d335 d336 d337 d338 d339 
    d340 d341 d342 d343 d344 d345 d346 d347 d348 d349 d350 d351 d352 d353 d354 d355 d356 d357 d358 d359 
    d360 d361 d362 d363 d364 d365 d366 d367 d368 d369 d370 d371 d372 d373 d374 d375 d376 d377 d378 d379 
    d380 d381 d382 d383 d384 d385 d386 d387 d388 d389 d390 d391 d392 d393 d394 d395 d396 d397 d398 d399 
    d400 d401 d402 d403 d404 d405 d406 d407 d408 d409 d410 d411 d412 d413 d414 d415 d416 d417 d418 d419 
    d420 d421 d422 d423 d424 d425 d426 d427 d428 d429 d430 d431 d432 d433 d434 d435 d436 d437 d438 d439 
    d440 d441 d442 d443 d444 d445 d446 d447 d448 d449 d450 d451 d452 d453 d454 d455 d456 d457 d458 d459 
    d460 d461 d462 d463 d464 d465 d466 d467 d468 d469 d470 d471 d472 d473 d474 d475 d476 d477 d478 d479 
    d480 d481 d482 d483 d484 d485 d486 d487 d488 d489 d490 d491 d492 d493 d494 d495 d496 d497 d498 d499 
    d500 d501 d502 d503 d504 d505 d506 d507 d508 d509 d510 d511 d512 d513 d514 d515 d516 d517 d518 d519 
    d520 d521 d522 d523 d524 d525 d526 d527 d528 d529 d530 d531 d532 d533 d534 d535 d536 d537 d538 d539 
    d540 d541 d542 d543 d544 d545 d546 d547 d548 d549 d550 d551 d552 d553 d554 d555 d556 d557 d558 d559 
    d560 d561 d562 d563 d564 d565 d566 d567 d568 d569 d570 d571 d572 d573 d574 d575 d576 d577 d578 d579 
    d580 d581 d582 d583 d584 d585 d586 d587 d588 d589 d590 d591 d592 d593 d594 d595 d596 d597 d598 d599 
    d600 d601 d602 d603 d604 d605 d606 d607 d608 d609 d610 d611 d612 d613 d614 d615 d616 d617 d618 d619 
    d620 d621 d622 d623 d624 d625 d626 d627 d628 d629 d630 d631 d632 d633 d634 d635 d636 d637 d638 d639 
    d640 d641 d642 d643 d644 d645 d646 d647 d648 d649 d650 d651 d652 d653 d654 d655 d656 d657 d658 d659 
    d660 d661 d662 d663 d664 d665 d666 d667 d668 d669 d670 d671 d672 d673 d674 d675 d676 d677 d678 d679 
    d680 d681 d682 d683 d684 d685 d686 d687 d688 d689 d690 d691 d692 d693 d694 d695 d696 d697 d698 d699 
    d700 d701 d702 d703 d704 d705 d706 d707 d708 d709 d710 d711 d712 d713 d714 d715 d716 d717 d718 d719 
    d720 d721 d722 d723 d724 d725 d726 d727 d728 d729 d730 d731 d732 d733 d734 d735 d736 d737 d738 d739 
    d740 d741 d742 d743 d744 d745 d746 d747 d748 d749 d750 d751 d752 d753 d754 d755 d756 d757 d758 d759 
    d760 d761 d762 d763 d764 d765 d766 d767 d768 d769 d770 d771 d772 d773 d774 d775 d776 d777 d778 d779 
    d780 d781 d782 d783 d784 d785 d786 d787 d788 d789 d790 d791 d792 d793 d794 d795 d796 d797 d798 d799 
    d800 d801 d802 d803 d804 d805 d806 d807 d808 d809 d810 d811 d812 d813 d814 d815 d816 d817 d818 d819 
    d820 d821 d822 d823 d824 d825 d826 d827 d828 d829 d830 d831 d832 d833 d834 d835 d836 d837 d838 d839 
    d840 d841 d842 d843 d844 d845 d846 d847 d848 d849 d850 d851 d852 d853 d854 d855 d856 d857 d858 d859 
    d860 d861 d862 d863 d864 d865 d866 d867 d868 d869 d870 d871 d872 d873 d874 d875 d876 d877 d878 d879 
    d880 d881 d882 d883 d884 d885 d886 d887 d888 d889 d890 d891 d892 d893 d894 d895 d896 d897 d898 d899 
    d900 d901 d902 d903 d904 d905 d906 d907 d908 d909 d910 d911 d912 d913 d914 d915 d916 d917 d918 d919 
    d920 d921 d922 d923 d924 d925 d926 d927 d928 d929 d930 d931 d932 d933 d934 d935 d936 d937 d938 d939 
    d940 d941 d942 d943 d944 d945 d946 d947 d948 d949 d950 d951 d952 d953 d954 d955 d956 d957 d958 d959 
    d960 d961 d962 d963 d964 d965 d966 d967 d968 d969 d970 d971 d972 d973 d974 d975 d976 d977 d978 d979 
    d980 d981 d982 d983 d984 d985 d986 d987 d988 d989 d990 d991 d992 d993 d994 d995 d996 d997 d998 d999 
    d1000 d1001 d1002 d1003 d1004 d1005 d1006 d1007 d1008 d1009 d1010 d1011 d1012 d1013 d1014 d1015 
    d1016 d1017 d1018 d1019 d1020 d1021 d1022 d1023 d1024 d1025 d1026 d1027 d1028 d1029 d1030 d1031 
    d1032 d1033 d1034 d1035 d1036 d1037 d1038 d1039 d1040 d1041 d1042 d1043 d1044 d1045 d1046 d1047 
    d1048 d1049 d1050 d1051 d1052 d1053 d1054 d1055 d1056 d1057 d1058 d1059 d1060 d1061 d1062 d1063 
    d1064 d1065 d1066 d1067 d1068 d1069 d1070 d1071 d1072 d1073 d1074 d1075 d1076 d1077 d1078 d1079 
    d1080 d1081 d1082 d1083 d1084 d1085 d1086 d1087 d1088 d1089 d1090 d1091 d1092 d1093 d1094 d1095 
    d1096 d1097 d1098 d1099 d1100 d1101 d1102 d1103 d1104 d1105 d1106 d1107 d1108 d1109 d1110 d1111 
    d1112 d1113 d1114 d1115 d1116 d1117 d1118 d1119 d1120 d1121 d1122 d1123 d1124 d1125 d1126 d1127 
    d1128 d1129 d1130 d1131 d1132 d1133 d1134 d1135 d1136 d1137 d1138 d1139 d1140 d1141 d1142 d1143 
    d1144 d1145 d1146 d1147 d1148 d1149 d1150 d1151 d1152 d1153 d1154 d1155 d1156 d1157 d1158 d1159 
    d1160 d1161 d1162 d1163 d1164 d1165 d1166 d1167 d1168 d1169 d1170 d1171 d1172 d1173 d1174 d1175 
    d1176 d1177 d1178 d1179 d1180 d1181 d1182 d1183 d1184 d1185 d1186 d1187 d1188 d1189 d1190 d1191 
    d1192 d1193 d1194 d1195 d1196 d1197 d1198 d1199 d1200 d1201 d1202 d1203 d1204 d1205 d1206 d1207 
    d1208 d1209 d1210 d1211 d1212 d1213 d1214 d1215 d1216 d1217 d1218 d1219 d1220 d1221 d1222 d1223 
    d1224 d1225 d1226 d1227 d1228 d1229 d1230 d1231 d1232 d1233 d1234 d1235 d1236 d1237 d1238 d1239 
    d1240 d1241 d1242 d1243 d1244 d1245 d1246 d1247 d1248 d1249 d1250 d1251 d1252 d1253 d1254 d1255 
    d1256 d1257 d1258 d1259 d1260 d1261 d1262 d1263 d1264 d1265 d1266 d1267 d1268 d1269 d1270 d1271 
    d1272 d1273 d1274 d1275 d1276 d1277 d1278 d1279 d1280 d1281 d1282 d1283 d1284 d1285 d1286 d1287 
    d1288 d1289 d1290 d1291 d1292 d1293 d1294 d1295 d1296 d1297 d1298 d1299 d1300 d1301 d1302 d1303 
    d1304 d1305 d1306 d1307 d1308 d1309 d1310 d1311 d1312 d1313 d1314 d1315 d1316 d1317 d1318 d1319 
    d1320 d1321 d1322 d1323 d1324 d1325 d1326 d1327 d1328 d1329 d1330 d1331 d1332 d1333 d1334 d1335 
    d1336 d1337 d1338 d1339 d1340 d1341 d1342 d1343 d1344 d1345 d1346 d1347 d1348 d1349 d1350 d1351 
    d1352 d1353 d1354 d1355 d1356 d1357 d1358 d1359 d1360 d1361 d1362 d1363 d1364 d1365 d1366 d1367 
    d1368 d1369 d1370 d1371 d1372 d1373 d1374 d1375 d1376 d1377 d1378 d1379 d1380 d1381 d1382 d1383 
    d1384 d1385 d1386 d1387 d1388 d1389 d1390 d1391 d1392 d1393 d1394 d1395 d1396 d1397 d1398 d1399 
    d1400 d1401 d1402 d1403 d1404 d1405 d1406 d1407 d1408 d1409 d1410 d1411 d1412 d1413 d1414 d1415 
    d1416 d1417 d1418 d1419 d1420 d1421 d1422 d1423 d1424 d1425 d1426 d1427 d1428 d1429 d1430 d1431 
    d1432 d1433 d1434 d1435 d1436 d1437 d1438 d1439 d1440 d1441 d1442 d1443 d1444 d1445 d1446 d1447 
    d1448 d1449 d1450 d1451 d1452 d1453 d1454 d1455 d1456 d1457 d1458 d1459 d1460 d1461 d1462 d1463 
    d1464 d1465 d1466 d1467 d1468 d1469 d1470 d1471 d1472 d1473 d1474 d1475 d1476 d1477 d1478 d1479 
    d1480 d1481 d1482 d1483 d1484 d1485 d1486 d1487 d1488 d1489 d1490 d1491 d1492 d1493 d1494 d1495 
    d1496 d1497 d1498 d1499 d1500 d1501 d1502 d1503 d1504 d1505 d1506 d1507 d1508 d1509 d1510 d1511 
    d1512 d1513 d1514 d1515 d1516 d1517 d1518 d1519 d1520 d1521 d1522 d1523 d1524 d1525 d1526 d1527 
    d1528 d1529 d1530 d1531 d1532 d1533 d1534 d1535 d1536 d1537 d1538 d1539 d1540 d1541 d1542 d1543 
    d1544 d1545 d1546 d1547 d1548 d1549 d1550 d1551 d1552 d1553 d1554 d1555 d1556 d1557 d1558 d1559 
    d1560 d1561 d1562 d1563 d1564 d1565 d1566 d1567 d1568 d1569 d1570 d1571 d1572 d1573 d1574 d1575 
    d1576 d1577 d1578 d1579 d1580 d1581 d1582 d1583 d1584 d1585 d1586 d1587 d1588 d1589 d1590 d1591 
    d1592 d1593 d1594 d1595 d1596 d1597 d1598 d1599 d1600 d1601 d1602 d1603 d1604 d1605 d1606 d1607 
    d1608 d1609 d1610 d1611 d1612 d1613 d1614 d1615 d1616 d1617 d1618 d1619 d1620 d1621 d1622 d1623 
    d1624 d1625 d1626 d1627 d1628 d1629 d1630 d1631 d1632 d1633 d1634 d1635 d1636 d1637 d1638 d1639 
    d1640 d1641 d1642 d1643 d1644 d1645 d1646 d1647 d1648 d1649 d1650 d1651 d1652 d1653 d1654 d1655 
    d1656 d1657 d1658 d1659 d1660 d1661 d1662 d1663 d1664 d1665 d1666 d1667 d1668 d1669 d1670 d1671 
    d1672 d1673 d1674 d1675 d1676 d1677 d1678 d1679 d1680 d1681 d1682 d1683 d1684 d1685 d1686 d1687 
    d1688 d1689 d1690 d1691 d1692 d1693 d1694 d1695 d1696 d1697 d1698 d1699 d1700 d1701 d1702 d1703 
    d1704 d1705 d1706 d1707 d1708 d1709 d1710 d1711 d1712 d1713 d1714 d1715 d1716 d1717 d1718 d1719 
    d1720 d1721 d1722 d1723 d1724 d1725 d1726 d1727 d1728 d1729 d1730 d1731 d1732 d1733 d1734 d1735 
    d1736 d1737 d1738 d1739 d1740 d1741 d1742 d1743 d1744 d1745 d1746 d1747 d1748 d1749 d1750 d1751 
    d1752 d1753 d1754 d1755 d1756 d1757 d1758 d1759 d1760 d1761 d1762 d1763 d1764 d1765 d1766 d1767 
    d1768 d1769 d1770 d1771 d1772 d1773 d1774 d1775 d1776 d1777 d1778 d1779 d1780 d1781 d1782 d1783 
    d1784 d1785 d1786 d1787 d1788 d1789 d1790 d1791 d1792 d1793 d1794 d1795 d1796 d1797 d1798 d1799 
    d1800 d1801 d1802 d1803 d1804 d1805 d1806 d1807 d1808 d1809 d1810 d1811 d1812 d1813 d1814 d1815 
    d1816 d1817 d1818 d1819 d1820 d1821 d1822 d1823 d1824 d1825 d1826 d1827 d1828 d1829 d1830 d1831 
    d1832 d1833 d1834 d1835 d1836 d1837 d1838 d1839 d1840 d1841 d1842 d1843 d1844 d1845 d1846 d1847 
    d1848 d1849 d1850 d1851 d1852 d1853 d1854 d1855 d1856 d1857 d1858 d1859 d1860 d1861 d1862 d1863 
    d1864 d1865 d1866 d1867 d1868 d1869 d1870 d1871 d1872 d1873 d1874 d1875 d1876 d1877 d1878 d1879 
    d1880 d1881 d1882 d1883 d1884 d1885 d1886 d1887 d1888 d1889 d1890 d1891 d1892 d1893 d1894 d1895 
    d1896 d1897 d1898 d1899 d1900 d1901 d1902 d1903 d1904 d1905 d1906 d1907 d1908 d1909 d1910 d1911 
    d1912 d1913 d1914 d1915 d1916 d1917 d1918 d1919 d1920 d1921 d1922 d1923 d1924 d1925 d1926 d1927 
    d1928 d1929 d1930 d1931 d1932 d1933 d1934 d1935 d1936 d1937 d1938 d1939 d1940 d1941 d1942 d1943 
    d1944 d1945 d1946 d1947 d1948 d1949 d1950 d1951 d1952 d1953 d1954 d1955 d1956 d1957 d1958 d1959 
    d1960 d1961 d1962 d1963 d1964 d1965 d1966 d1967 d1968 d1969 d1970 d1971 d1972 d1973 d1974 d1975 
    d1976 d1977 d1978 d1979 d1980 d1981 d1982 d1983 d1984 d1985 d1986 d1987 d1988 d1989 d1990 d1991 
    d1992 d1993 d1994 d1995 d1996 d1997 d1998 d1999 d2000 d2001 d2002 d2003 d2004 d2005 d2006 d2007 
    d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 d2022 d2023 
    d2024 d2025 d2026 d2027 d2028 d2029 d2030 d2031 d2032 d2033 d2034 d2035 d2036 d2037 d2038 d2039 
    d2040 d2041 d2042 d2043 d2044 d2045 d2046 d2047 d2048 d2049 d2050 d2051 d2052 d2053 d2054 d2055 
    d2056 d2057 d2058 d2059 d2060 d2061 d2062 d2063 d2064 d2065 d2066 d2067 d2068 d2069 d2070 d2071 
    d2072 d2073 d2074 d2075 d2076 d2077 d2078 d2079 d2080 d2081 d2082 d2083 d2084 d2085 d2086 d2087 
    d2088 d2089 d2090 d2091 d2092 d2093 d2094 d2095 d2096 d2097 d2098 d2099 d2100 d2101 d2102 d2103 
    d2104 d2105 d2106 d2107 d2108 d2109 d2110 d2111 d2112 d2113 d2114 d2115 d2116 d2117 d2118 d2119 
    d2120 d2121 d2122 d2123 d2124 d2125 d2126 d2127 d2128 d2129 d2130 d2131 d2132 d2133 d2134 d2135 
    d2136 d2137 d2138 d2139 d2140 d2141 d2142 d2143 d2144 d2145 d2146 d2147 d2148 d2149 d2150 d2151 
    d2152 d2153 d2154 d2155 d2156 d2157 d2158 d2159 d2160 d2161 d2162 d2163 d2164 d2165 d2166 d2167 
    d2168 d2169 d2170 d2171 d2172 d2173 d2174 d2175 d2176 d2177 d2178 d2179 d2180 d2181 d2182 d2183 
    d2184 d2185 d2186 d2187 d2188 d2189 d2190 d2191 d2192 d2193 d2194 d2195 d2196 d2197 d2198 d2199 
    d2200 d2201 d2202 d2203 d2204 d2205 d2206 d2207 d2208 d2209 d2210 d2211 d2212 d2213 d2214 d2215 
    d2216 d2217 d2218 d2219 d2220 d2221 d2222 d2223 d2224 d2225 d2226 d2227 d2228 d2229 d2230 d2231 
    d2232 d2233 d2234 d2235 d2236 d2237 d2238 d2239 d2240 d2241 d2242 d2243 d2244 d2245 d2246 d2247 
    d2248 d2249 d2250 d2251 d2252 d2253 d2254 d2255 d2256 d2257 d2258 d2259 d2260 d2261 d2262 d2263 
    d2264 d2265 d2266 d2267 d2268 d2269 d2270 d2271 d2272 d2273 d2274 d2275 d2276 d2277 d2278 d2279 
    d2280 d2281 d2282 d2283 d2284 d2285 d2286 d2287 d2288 d2289 d2290 d2291 d2292 d2293 d2294 d2295 
    d2296 d2297 d2298 d2299 d2300 d2301 d2302 d2303 d2304 d2305 d2306 d2307 d2308 d2309 d2310 d2311 
    d2312 d2313 d2314 d2315 d2316 d2317 d2318 d2319 d2320 d2321 d2322 d2323 d2324 d2325 d2326 d2327 
    d2328 d2329 d2330 d2331 d2332 d2333 d2334 d2335 d2336 d2337 d2338 d2339 d2340 d2341 d2342 d2343 
    d2344 d2345 d2346 d2347 d2348 d2349 d2350 d2351 d2352 d2353 d2354 d2355 d2356 d2357 d2358 d2359 
    d2360 d2361 d2362 d2363 d2364 d2365 d2366 d2367 d2368 d2369 d2370 d2371 d2372 d2373 d2374 d2375 
    d2376 d2377 d2378 d2379 d2380 d2381 d2382 d2383 d2384 d2385 d2386 d2387 d2388 d2389 d2390 d2391 
    d2392 d2393 d2394 d2395 d2396 d2397 d2398 d2399 d2400 d2401 d2402 d2403 d2404 d2405 d2406 d2407 
    d2408 d2409 d2410 d2411 d2412 d2413 d2414 d2415 d2416 d2417 d2418 d2419 d2420 d2421 d2422 d2423 
    d2424 d2425 d2426 d2427 d2428 d2429 d2430 d2431 d2432 d2433 d2434 d2435 d2436 d2437 d2438 d2439 
    d2440 d2441 d2442 d2443 d2444 d2445 d2446 d2447 d2448 d2449 d2450 d2451 d2452 d2453 d2454 d2455 
    d2456 d2457 d2458 d2459 d2460 d2461 d2462 d2463 d2464 d2465 d2466 d2467 d2468 d2469 d2470 d2471 
    d2472 d2473 d2474 d2475 d2476 d2477 d2478 d2479 d2480 d2481 d2482 d2483 d2484 d2485 d2486 d2487 
    d2488 d2489 d2490 d2491 d2492 d2493 d2494 d2495 d2496 d2497 d2498 d2499 d2500 d2501 d2502 d2503 
    d2504 d2505 d2506 d2507 d2508 d2509 d2510 d2511 d2512 d2513 d2514 d2515 d2516 d2517 d2518 d2519 
    d2520 d2521 d2522 d2523 d2524 d2525 d2526 d2527 d2528 d2529 d2530 d2531 d2532 d2533 d2534 d2535 
    d2536 d2537 d2538 d2539 d2540 d2541 d2542 d2543 d2544 d2545 d2546 d2547 d2548 d2549 d2550 d2551 
    d2552 d2553 d2554 d2555 d2556 d2557 d2558 d2559 d2560 d2561 d2562 d2563 d2564 d2565 d2566 d2567 
    d2568 d2569 d2570 d2571 d2572 d2573 d2574 d2575 d2576 d2577 d2578 d2579 d2580 d2581 d2582 d2583 
    d2584 d2585 d2586 d2587 d2588 d2589 d2590 d2591 d2592 d2593 d2594 d2595 d2596 d2597 d2598 d2599 
    d2600 d2601 d2602 d2603 d2604 d2605 d2606 d2607 d2608 d2609 d2610 d2611 d2612 d2613 d2614 d2615 
    d2616 d2617 d2618 d2619 d2620 d2621 d2622 d2623 d2624 d2625 d2626 d2627 d2628 d2629 d2630 d2631 
    d2632 d2633 d2634 d2635 d2636 d2637 d2638 d2639 d2640 d2641 d2642 d2643 d2644 d2645 d2646 d2647 
    d2648 d2649 d2650 d2651 d2652 d2653 d2654 d2655 d2656 d2657 d2658 d2659 d2660 d2661 d2662 d2663 
    d2664 d2665 d2666 d2667 d2668 d2669 d2670 d2671 d2672 d2673 d2674 d2675 d2676 d2677 d2678 d2679 
    d2680 d2681 d2682 d2683 d2684 d2685 d2686 d2687 d2688 d2689 d2690 d2691 d2692 d2693 d2694 d2695 
    d2696 d2697 d2698 d2699 d2700 d2701 d2702 d2703 d2704 d2705 d2706 d2707 d2708 d2709) 
  /DROP= d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15 d16 d17 d18 d19 d20 d21 d22 d23 d24 
    d25 d26 d27 d28 d29 d30 d31 d32 d33 d34 d35 d36 d37 d38 d39 d40 d41 d42 d43 d44 d45 d46 d47 d48 d49 
    d50 d51 d52 d53 d54 d55 d56 d57 d58 d59 d60 d61 d62 d63 d64 d65 d66 d67 d68 d69 d70 d71 d72 d73 d74 
    d75 d76 d77 d78 d79 d80 d81 d82 d83 d84 d85 d86 d87 d88 d89 d90 d91 d92 d93 d94 d95 d96 d97 d98 d99 
    d100 d101 d102 d103 d104 d105 d106 d107 d108 d109 d110 d111 d112 d113 d114 d115 d116 d117 d118 d119 
    d120 d121 d122 d123 d124 d125 d126 d127 d128 d129 d130 d131 d132 d133 d134 d135 d136 d137 d138 d139 
    d140 d141 d142 d143 d144 d145 d146 d147 d148 d149 d150 d151 d152 d153 d154 d155 d156 d157 d158 d159 
    d160 d161 d162 d163 d164 d165 d166 d167 d168 d169 d170 d171 d172 d173 d174 d175 d176 d177 d178 d179 
    d180 d181 d182 d183 d184 d185 d186 d187 d188 d189 d190 d191 d192 d193 d194 d195 d196 d197 d198 d199 
    d200 d201 d202 d203 d204 d205 d206 d207 d208 d209 d210 d211 d212 d213 d214 d215 d216 d217 d218 d219 
    d220 d221 d222 d223 d224 d225 d226 d227 d228 d229 d230 d231 d232 d233 d234 d235 d236 d237 d238 d239 
    d240 d241 d242 d243 d244 d245 d246 d247 d248 d249 d250 d251 d252 d253 d254 d255 d256 d257 d258 d259 
    d260 d261 d262 d263 d264 d265 d266 d267 d268 d269 d270 d271 d272 d273 d274 d275 d276 d277 d278 d279 
    d280 d281 d282 d283 d284 d285 d286 d287 d288 d289 d290 d291 d292 d293 d294 d295 d296 d297 d298 d299 
    d300 d301 d302 d303 d304 d305 d306 d307 d308 d309 d310 d311 d312 d313 d314 d315 d316 d317 d318 d319 
    d320 d321 d322 d323 d324 d325 d326 d327 d328 d329 d330 d331 d332 d333 d334 d335 d336 d337 d338 d339 
    d340 d341 d342 d343 d344 d345 d346 d347 d348 d349 d350 d351 d352 d353 d354 d355 d356 d357 d358 d359 
    d360 d361 d362 d363 d364 d365 d366 d367 d368 d369 d370 d371 d372 d373 d374 d375 d376 d377 d378 d379 
    d380 d381 d382 d383 d384 d385 d386 d387 d388 d389 d390 d391 d392 d393 d394 d395 d396 d397 d398 d399 
    d400 d401 d402 d403 d404 d405 d406 d407 d408 d409 d410 d411 d412 d413 d414 d415 d416 d417 d418 d419 
    d420 d421 d422 d423 d424 d425 d426 d427 d428 d429 d430 d431 d432 d433 d434 d435 d436 d437 d438 d439 
    d440 d441 d442 d443 d444 d445 d446 d447 d448 d449 d450 d451 d452 d453 d454 d455 d456 d457 d458 d459 
    d460 d461 d462 d463 d464 d465 d466 d467 d468 d469 d470 d471 d472 d473 d474 d475 d476 d477 d478 d479 
    d480 d481 d482 d483 d484 d485 d486 d487 d488 d489 d490 d491 d492 d493 d494 d495 d496 d497 d498 d499 
    d500 d501 d502 d503 d504 d505 d506 d507 d508 d509 d510 d511 d512 d513 d514 d515 d516 d517 d518 d519 
    d520 d521 d522 d523 d524 d525 d526 d527 d528 d529 d530 d531 d532 d533 d534 d535 d536 d537 d538 d539 
    d540 d541 d542 d543 d544 d545 d546 d547 d548 d549 d550 d551 d552 d553 d554 d555 d556 d557 d558 d559 
    d560 d561 d562 d563 d564 d565 d566 d567 d568 d569 d570 d571 d572 d573 d574 d575 d576 d577 d578 d579 
    d580 d581 d582 d583 d584 d585 d586 d587 d588 d589 d590 d591 d592 d593 d594 d595 d596 d597 d598 d599 
    d600 d601 d602 d603 d604 d605 d606 d607 d608 d609 d610 d611 d612 d613 d614 d615 d616 d617 d618 d619 
    d620 d621 d622 d623 d624 d625 d626 d627 d628 d629 d630 d631 d632 d633 d634 d635 d636 d637 d638 d639 
    d640 d641 d642 d643 d644 d645 d646 d647 d648 d649 d650 d651 d652 d653 d654 d655 d656 d657 d658 d659 
    d660 d661 d662 d663 d664 d665 d666 d667 d668 d669 d670 d671 d672 d673 d674 d675 d676 d677 d678 d679 
    d680 d681 d682 d683 d684 d685 d686 d687 d688 d689 d690 d691 d692 d693 d694 d695 d696 d697 d698 d699 
    d700 d701 d702 d703 d704 d705 d706 d707 d708 d709 d710 d711 d712 d713 d714 d715 d716 d717 d718 d719 
    d720 d721 d722 d723 d724 d725 d726 d727 d728 d729 d730 d731 d732 d733 d734 d735 d736 d737 d738 d739 
    d740 d741 d742 d743 d744 d745 d746 d747 d748 d749 d750 d751 d752 d753 d754 d755 d756 d757 d758 d759 
    d760 d761 d762 d763 d764 d765 d766 d767 d768 d769 d770 d771 d772 d773 d774 d775 d776 d777 d778 d779 
    d780 d781 d782 d783 d784 d785 d786 d787 d788 d789 d790 d791 d792 d793 d794 d795 d796 d797 d798 d799 
    d800 d801 d802 d803 d804 d805 d806 d807 d808 d809 d810 d811 d812 d813 d814 d815 d816 d817 d818 d819 
    d820 d821 d822 d823 d824 d825 d826 d827 d828 d829 d830 d831 d832 d833 d834 d835 d836 d837 d838 d839 
    d840 d841 d842 d843 d844 d845 d846 d847 d848 d849 d850 d851 d852 d853 d854 d855 d856 d857 d858 d859 
    d860 d861 d862 d863 d864 d865 d866 d867 d868 d869 d870 d871 d872 d873 d874 d875 d876 d877 d878 d879 
    d880 d881 d882 d883 d884 d885 d886 d887 d888 d889 d890 d891 d892 d893 d894 d895 d896 d897 d898 d899 
    d900 d901 d902 d903 d904 d905 d906 d907 d908 d909 d910 d911 d912 d913 d914 d915 d916 d917 d918 d919 
    d920 d921 d922 d923 d924 d925 d926 d927 d928 d929 d930 d931 d932 d933 d934 d935 d936 d937 d938 d939 
    d940 d941 d942 d943 d944 d945 d946 d947 d948 d949 d950 d951 d952 d953 d954 d955 d956 d957 d958 d959 
    d960 d961 d962 d963 d964 d965 d966 d967 d968 d969 d970 d971 d972 d973 d974 d975 d976 d977 d978 d979 
    d980 d981 d982 d983 d984 d985 d986 d987 d988 d989 d990 d991 d992 d993 d994 d995 d996 d997 d998 d999 
    d1000 d1001 d1002 d1003 d1004 d1005 d1006 d1007 d1008 d1009 d1010 d1011 d1012 d1013 d1014 d1015 
    d1016 d1017 d1018 d1019 d1020 d1021 d1022 d1023 d1024 d1025 d1026 d1027 d1028 d1029 d1030 d1031 
    d1032 d1033 d1034 d1035 d1036 d1037 d1038 d1039 d1040 d1041 d1042 d1043 d1044 d1045 d1046 d1047 
    d1048 d1049 d1050 d1051 d1052 d1053 d1054 d1055 d1056 d1057 d1058 d1059 d1060 d1061 d1062 d1063 
    d1064 d1065 d1066 d1067 d1068 d1069 d1070 d1071 d1072 d1073 d1074 d1075 d1076 d1077 d1078 d1079 
    d1080 d1081 d1082 d1083 d1084 d1085 d1086 d1087 d1088 d1089 d1090 d1091 d1092 d1093 d1094 d1095 
    d1096 d1097 d1098 d1099 d1100 d1101 d1102 d1103 d1104 d1105 d1106 d1107 d1108 d1109 d1110 d1111 
    d1112 d1113 d1114 d1115 d1116 d1117 d1118 d1119 d1120 d1121 d1122 d1123 d1124 d1125 d1126 d1127 
    d1128 d1129 d1130 d1131 d1132 d1133 d1134 d1135 d1136 d1137 d1138 d1139 d1140 d1141 d1142 d1143 
    d1144 d1145 d1146 d1147 d1148 d1149 d1150 d1151 d1152 d1153 d1154 d1155 d1156 d1157 d1158 d1159 
    d1160 d1161 d1162 d1163 d1164 d1165 d1166 d1167 d1168 d1169 d1170 d1171 d1172 d1173 d1174 d1175 
    d1176 d1177 d1178 d1179 d1180 d1181 d1182 d1183 d1184 d1185 d1186 d1187 d1188 d1189 d1190 d1191 
    d1192 d1193 d1194 d1195 d1196 d1197 d1198 d1199 d1200 d1201 d1202 d1203 d1204 d1205 d1206 d1207 
    d1208 d1209 d1210 d1211 d1212 d1213 d1214 d1215 d1216 d1217 d1218 d1219 d1220 d1221 d1222 d1223 
    d1224 d1225 d1226 d1227 d1228 d1229 d1230 d1231 d1232 d1233 d1234 d1235 d1236 d1237 d1238 d1239 
    d1240 d1241 d1242 d1243 d1244 d1245 d1246 d1247 d1248 d1249 d1250 d1251 d1252 d1253 d1254 d1255 
    d1256 d1257 d1258 d1259 d1260 d1261 d1262 d1263 d1264 d1265 d1266 d1267 d1268 d1269 d1270 d1271 
    d1272 d1273 d1274 d1275 d1276 d1277 d1278 d1279 d1280 d1281 d1282 d1283 d1284 d1285 d1286 d1287 
    d1288 d1289 d1290 d1291 d1292 d1293 d1294 d1295 d1296 d1297 d1298 d1299 d1300 d1301 d1302 d1303 
    d1304 d1305 d1306 d1307 d1308 d1309 d1310 d1311 d1312 d1313 d1314 d1315 d1316 d1317 d1318 d1319 
    d1320 d1321 d1322 d1323 d1324 d1325 d1326 d1327 d1328 d1329 d1330 d1331 d1332 d1333 d1334 d1335 
    d1336 d1337 d1338 d1339 d1340 d1341 d1342 d1343 d1344 d1345 d1346 d1347 d1348 d1349 d1350 d1351 
    d1352 d1353 d1354 d1355 d1356 d1357 d1358 d1359 d1360 d1361 d1362 d1363 d1364 d1365 d1366 d1367 
    d1368 d1369 d1370 d1371 d1372 d1373 d1374 d1375 d1376 d1377 d1378 d1379 d1380 d1381 d1382 d1383 
    d1384 d1385 d1386 d1387 d1388 d1389 d1390 d1391 d1392 d1393 d1394 d1395 d1396 d1397 d1398 d1399 
    d1400 d1401 d1402 d1403 d1404 d1405 d1406 d1407 d1408 d1409 d1410 d1411 d1412 d1413 d1414 d1415 
    d1416 d1417 d1418 d1419 d1420 d1421 d1422 d1423 d1424 d1425 d1426 d1427 d1428 d1429 d1430 d1431 
    d1432 d1433 d1434 d1435 d1436 d1437 d1438 d1439 d1440 d1441 d1442 d1443 d1444 d1445 d1446 d1447 
    d1448 d1449 d1450 d1451 d1452 d1453 d1454 d1455 d1456 d1457 d1458 d1459 d1460 d1461 d1462 d1463 
    d1464 d1465 d1466 d1467 d1468 d1469 d1470 d1471 d1472 d1473 d1474 d1475 d1476 d1477 d1478 d1479 
    d1480 d1481 d1482 d1483 d1484 d1485 d1486 d1487 d1488 d1489 d1490 d1491 d1492 d1493 d1494 d1495 
    d1496 d1497 d1498 d1499 d1500 d1501 d1502 d1503 d1504 d1505 d1506 d1507 d1508 d1509 d1510 d1511 
    d1512 d1513 d1514 d1515 d1516 d1517 d1518 d1519 d1520 d1521 d1522 d1523 d1524 d1525 d1526 d1527 
    d1528 d1529 d1530 d1531 d1532 d1533 d1534 d1535 d1536 d1537 d1538 d1539 d1540 d1541 d1542 d1543 
    d1544 d1545 d1546 d1547 d1548 d1549 d1550 d1551 d1552 d1553 d1554 d1555 d1556 d1557 d1558 d1559 
    d1560 d1561 d1562 d1563 d1564 d1565 d1566 d1567 d1568 d1569 d1570 d1571 d1572 d1573 d1574 d1575 
    d1576 d1577 d1578 d1579 d1580 d1581 d1582 d1583 d1584 d1585 d1586 d1587 d1588 d1589 d1590 d1591 
    d1592 d1593 d1594 d1595 d1596 d1597 d1598 d1599 d1600 d1601 d1602 d1603 d1604 d1605 d1606 d1607 
    d1608 d1609 d1610 d1611 d1612 d1613 d1614 d1615 d1616 d1617 d1618 d1619 d1620 d1621 d1622 d1623 
    d1624 d1625 d1626 d1627 d1628 d1629 d1630 d1631 d1632 d1633 d1634 d1635 d1636 d1637 d1638 d1639 
    d1640 d1641 d1642 d1643 d1644 d1645 d1646 d1647 d1648 d1649 d1650 d1651 d1652 d1653 d1654 d1655 
    d1656 d1657 d1658 d1659 d1660 d1661 d1662 d1663 d1664 d1665 d1666 d1667 d1668 d1669 d1670 d1671 
    d1672 d1673 d1674 d1675 d1676 d1677 d1678 d1679 d1680 d1681 d1682 d1683 d1684 d1685 d1686 d1687 
    d1688 d1689 d1690 d1691 d1692 d1693 d1694 d1695 d1696 d1697 d1698 d1699 d1700 d1701 d1702 d1703 
    d1704 d1705 d1706 d1707 d1708 d1709 d1710 d1711 d1712 d1713 d1714 d1715 d1716 d1717 d1718 d1719 
    d1720 d1721 d1722 d1723 d1724 d1725 d1726 d1727 d1728 d1729 d1730 d1731 d1732 d1733 d1734 d1735 
    d1736 d1737 d1738 d1739 d1740 d1741 d1742 d1743 d1744 d1745 d1746 d1747 d1748 d1749 d1750 d1751 
    d1752 d1753 d1754 d1755 d1756 d1757 d1758 d1759 d1760 d1761 d1762 d1763 d1764 d1765 d1766 d1767 
    d1768 d1769 d1770 d1771 d1772 d1773 d1774 d1775 d1776 d1777 d1778 d1779 d1780 d1781 d1782 d1783 
    d1784 d1785 d1786 d1787 d1788 d1789 d1790 d1791 d1792 d1793 d1794 d1795 d1796 d1797 d1798 d1799 
    d1800 d1801 d1802 d1803 d1804 d1805 d1806 d1807 d1808 d1809 d1810 d1811 d1812 d1813 d1814 d1815 
    d1816 d1817 d1818 d1819 d1820 d1821 d1822 d1823 d1824 d1825 d1826 d1827 d1828 d1829 d1830 d1831 
    d1832 d1833 d1834 d1835 d1836 d1837 d1838 d1839 d1840 d1841 d1842 d1843 d1844 d1845 d1846 d1847 
    d1848 d1849 d1850 d1851 d1852 d1853 d1854 d1855 d1856 d1857 d1858 d1859 d1860 d1861 d1862 d1863 
    d1864 d1865 d1866 d1867 d1868 d1869 d1870 d1871 d1872 d1873 d1874 d1875 d1876 d1877 d1878 d1879 
    d1880 d1881 d1882 d1883 d1884 d1885 d1886 d1887 d1888 d1889 d1890 d1891 d1892 d1893 d1894 d1895 
    d1896 d1897 d1898 d1899 d1900 d1901 d1902 d1903 d1904 d1905 d1906 d1907 d1908 d1909 d1910 d1911 
    d1912 d1913 d1914 d1915 d1916 d1917 d1918 d1919 d1920 d1921 d1922 d1923 d1924 d1925 d1926 d1927 
    d1928 d1929 d1930 d1931 d1932 d1933 d1934 d1935 d1936 d1937 d1938 d1939 d1940 d1941 d1942 d1943 
    d1944 d1945 d1946 d1947 d1948 d1949 d1950 d1951 d1952 d1953 d1954 d1955 d1956 d1957 d1958 d1959 
    d1960 d1961 d1962 d1963 d1964 d1965 d1966 d1967 d1968 d1969 d1970 d1971 d1972 d1973 d1974 d1975 
    d1976 d1977 d1978 d1979 d1980 d1981 d1982 d1983 d1984 d1985 d1986 d1987 d1988 d1989 d1990 d1991 
    d1992 d1993 d1994 d1995 d1996 d1997 d1998 d1999 d2000 d2001 d2002 d2003 d2004 d2005 d2006 d2007 
    d2008 d2009 d2010 d2011 d2012 d2013 d2014 d2015 d2016 d2017 d2018 d2019 d2020 d2021 d2022 d2023 
    d2024 d2025 d2026 d2027 d2028 d2029 d2030 d2031 d2032 d2033 d2034 d2035 d2036 d2037 d2038 d2039 
    d2040 d2041 d2042 d2043 d2044 d2045 d2046 d2047 d2048 d2049 d2050 d2051 d2052 d2053 d2054 d2055 
    d2056 d2057 d2058 d2059 d2060 d2061 d2062 d2063 d2064 d2065 d2066 d2067 d2068 d2069 d2070 d2071 
    d2072 d2073 d2074 d2075 d2076 d2077 d2078 d2079 d2080 d2081 d2082 d2083 d2084 d2085 d2086 d2087 
    d2088 d2089 d2090 d2091 d2092 d2093 d2094 d2095 d2096 d2097 d2098 d2099 d2100 d2101 d2102 d2103 
    d2104 d2105 d2106 d2107 d2108 d2109 d2110 d2111 d2112 d2113 d2114 d2115 d2116 d2117 d2118 d2119 
    d2120 d2121 d2122 d2123 d2124 d2125 d2126 d2127 d2128 d2129 d2130 d2131 d2132 d2133 d2134 d2135 
    d2136 d2137 d2138 d2139 d2140 d2141 d2142 d2143 d2144 d2145 d2146 d2147 d2148 d2149 d2150 d2151 
    d2152 d2153 d2154 d2155 d2156 d2157 d2158 d2159 d2160 d2161 d2162 d2163 d2164 d2165 d2166 d2167 
    d2168 d2169 d2170 d2171 d2172 d2173 d2174 d2175 d2176 d2177 d2178 d2179 d2180 d2181 d2182 d2183 
    d2184 d2185 d2186 d2187 d2188 d2189 d2190 d2191 d2192 d2193 d2194 d2195 d2196 d2197 d2198 d2199 
    d2200 d2201 d2202 d2203 d2204 d2205 d2206 d2207 d2208 d2209 d2210 d2211 d2212 d2213 d2214 d2215 
    d2216 d2217 d2218 d2219 d2220 d2221 d2222 d2223 d2224 d2225 d2226 d2227 d2228 d2229 d2230 d2231 
    d2232 d2233 d2234 d2235 d2236 d2237 d2238 d2239 d2240 d2241 d2242 d2243 d2244 d2245 d2246 d2247 
    d2248 d2249 d2250 d2251 d2252 d2253 d2254 d2255 d2256 d2257 d2258 d2259 d2260 d2261 d2262 d2263 
    d2264 d2265 d2266 d2267 d2268 d2269 d2270 d2271 d2272 d2273 d2274 d2275 d2276 d2277 d2278 d2279 
    d2280 d2281 d2282 d2283 d2284 d2285 d2286 d2287 d2288 d2289 d2290 d2291 d2292 d2293 d2294 d2295 
    d2296 d2297 d2298 d2299 d2300 d2301 d2302 d2303 d2304 d2305 d2306 d2307 d2308 d2309 d2310 d2311 
    d2312 d2313 d2314 d2315 d2316 d2317 d2318 d2319 d2320 d2321 d2322 d2323 d2324 d2325 d2326 d2327 
    d2328 d2329 d2330 d2331 d2332 d2333 d2334 d2335 d2336 d2337 d2338 d2339 d2340 d2341 d2342 d2343 
    d2344 d2345 d2346 d2347 d2348 d2349 d2350 d2351 d2352 d2353 d2354 d2355 d2356 d2357 d2358 d2359 
    d2360 d2361 d2362 d2363 d2364 d2365 d2366 d2367 d2368 d2369 d2370 d2371 d2372 d2373 d2374 d2375 
    d2376 d2377 d2378 d2379 d2380 d2381 d2382 d2383 d2384 d2385 d2386 d2387 d2388 d2389 d2390 d2391 
    d2392 d2393 d2394 d2395 d2396 d2397 d2398 d2399 d2400 d2401 d2402 d2403 d2404 d2405 d2406 d2407 
    d2408 d2409 d2410 d2411 d2412 d2413 d2414 d2415 d2416 d2417 d2418 d2419 d2420 d2421 d2422 d2423 
    d2424 d2425 d2426 d2427 d2428 d2429 d2430 d2431 d2432 d2433 d2434 d2435 d2436 d2437 d2438 d2439 
    d2440 d2441 d2442 d2443 d2444 d2445 d2446 d2447 d2448 d2449 d2450 d2451 d2452 d2453 d2454 d2455 
    d2456 d2457 d2458 d2459 d2460 d2461 d2462 d2463 d2464 d2465 d2466 d2467 d2468 d2469 d2470 d2471 
    d2472 d2473 d2474 d2475 d2476 d2477 d2478 d2479 d2480 d2481 d2482 d2483 d2484 d2485 d2486 d2487 
    d2488 d2489 d2490 d2491 d2492 d2493 d2494 d2495 d2496 d2497 d2498 d2499 d2500 d2501 d2502 d2503 
    d2504 d2505 d2506 d2507 d2508 d2509 d2510 d2511 d2512 d2513 d2514 d2515 d2516 d2517 d2518 d2519 
    d2520 d2521 d2522 d2523 d2524 d2525 d2526 d2527 d2528 d2529 d2530 d2531 d2532 d2533 d2534 d2535 
    d2536 d2537 d2538 d2539 d2540 d2541 d2542 d2543 d2544 d2545 d2546 d2547 d2548 d2549 d2550 d2551 
    d2552 d2553 d2554 d2555 d2556 d2557 d2558 d2559 d2560 d2561 d2562 d2563 d2564 d2565 d2566 d2567 
    d2568 d2569 d2570 d2571 d2572 d2573 d2574 d2575 d2576 d2577 d2578 d2579 d2580 d2581 d2582 d2583 
    d2584 d2585 d2586 d2587 d2588 d2589 d2590 d2591 d2592 d2593 d2594 d2595 d2596 d2597 d2598 d2599 
    d2600 d2601 d2602 d2603 d2604 d2605 d2606 d2607 d2608 d2609 d2610 d2611 d2612 d2613 d2614 d2615 
    d2616 d2617 d2618 d2619 d2620 d2621 d2622 d2623 d2624 d2625 d2626 d2627 d2628 d2629 d2630 d2631 
    d2632 d2633 d2634 d2635 d2636 d2637 d2638 d2639 d2640 d2641 d2642 d2643 d2644 d2645 d2646 d2647 
    d2648 d2649 d2650 d2651 d2652 d2653 d2654 d2655 d2656 d2657 d2658 d2659 d2660 d2661 d2662 d2663 
    d2664 d2665 d2666 d2667 d2668 d2669 d2670 d2671 d2672 d2673 d2674 d2675 d2676 d2677 d2678 d2679 
    d2680 d2681 d2682 d2683 d2684 d2685 d2686 d2687 d2688 d2689 d2690 d2691 d2692 d2693 d2694 d2695 
    d2696 d2697 d2698 d2699 d2700 d2701 d2702 d2703 d2704 d2705 d2706 d2707 d2708 d2709.
EXECUTE.
SAVE OUTFILE='N:\ARTICULOS\ART IG - fracturas - osteocalcina\estadística\BD-Reus-PREDIBONE-IG04052017.sav'
  /COMPRESSED.
*IG; carga glucémica.
COMPUTE media_CARGLUCE_Mod = 9999999. 
IF ((v_final_evento=10)) media_CARGLUCE_Mod = mean(CARGLUCE_Mod, CARGLUCE3_mod, CARGLUCE4_mod, CARGLUCE5_mod, CARGLUCE6_mod, CARGLUCE7_mod, CARGLUCE8_mod, CARGLUCE9_mod, CARGLUCE10_mod).
IF ((v_final_evento=9)) media_CARGLUCE_Mod = mean(CARGLUCE_Mod,  CARGLUCE3_mod, CARGLUCE4_mod, CARGLUCE5_mod, CARGLUCE6_mod, CARGLUCE7_mod, CARGLUCE8_mod).
IF ((v_final_evento=8)) media_CARGLUCE_Mod = mean(CARGLUCE_Mod,  CARGLUCE3_mod, CARGLUCE4_mod, CARGLUCE5_mod, CARGLUCE6_mod, CARGLUCE7_mod).
IF ((v_final_evento=7)) media_CARGLUCE_Mod = mean(CARGLUCE_Mod,  CARGLUCE3_mod, CARGLUCE4_mod, CARGLUCE5_mod, CARGLUCE6_mod).
IF ((v_final_evento=6)) media_CARGLUCE_Mod = mean(CARGLUCE_Mod,  CARGLUCE3_mod, CARGLUCE4_mod, CARGLUCE5_mod).
IF ((v_final_evento=5)) media_CARGLUCE_Mod = mean(CARGLUCE_Mod,  CARGLUCE3_mod, CARGLUCE4_mod).
IF ((v_final_evento=4)) media_CARGLUCE_Mod = mean(CARGLUCE_Mod,  CARGLUCE3_mod).
IF ((v_final_evento=3)) media_CARGLUCE_Mod = mean(CARGLUCE_Mod).
VARIABLE LABELS media_CARGLUCE_Mod 'CARGLUCE 31-08-2015 (ml/d’a)'.
execute.

COMPUTE media_IG_Mod = 9999999. 
IF ((v_final_evento=10)) media_IG_Mod = mean(IG_Mod, IG3_mod, IG4_mod, IG5_mod, IG6_mod, IG7_mod, IG8_mod, IG9_mod, IG10_mod).
IF ((v_final_evento=9)) media_IG_Mod = mean(IG_Mod,  IG3_mod, IG4_mod, IG5_mod, IG6_mod, IG7_mod, IG8_mod).
IF ((v_final_evento=8)) media_IG_Mod = mean(IG_Mod,  IG3_mod, IG4_mod, IG5_mod, IG6_mod, IG7_mod).
IF ((v_final_evento=7)) media_IG_Mod = mean(IG_Mod,  IG3_mod, IG4_mod, IG5_mod, IG6_mod).
IF ((v_final_evento=6)) media_IG_Mod = mean(IG_Mod,  IG3_mod, IG4_mod, IG5_mod).
IF ((v_final_evento=5)) media_IG_Mod = mean(IG_Mod,  IG3_mod, IG4_mod).
IF ((v_final_evento=4)) media_IG_Mod = mean(IG_Mod,  IG3_mod).
IF ((v_final_evento=3)) media_IG_Mod = mean(IG_Mod).
VARIABLE LABELS media_IG_Mod 'IG 31-08-2015 (ml/d’a)'.
execute.

*terciles de media de ferment acumulado ajust.
RANK VARIABLES= media_IG_Mod(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

RANK VARIABLES= media_CARGLUCE_Mod(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

RANK VARIABLES= CARGLUCE_Mod(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

*Ajuste de las variables de alimentacion.
Compute VitaminaD = vitD.
Compute VitaminaD3 = vitD3.
Compute VitaminaD4 = vitD4.
Compute VitaminaD5 = vitD5.
Compute VitaminaD6 = vitD6.
Compute VitaminaD7 = vitD7.
Compute VitaminaD8 = vitD8.
Compute VitaminaD9 = vitD9.
Compute VitaminaD10 = vitD10.
Compute Calci = calcio.
Compute Calci3 = Calcio3.
Compute Calci4 = Calcio4.
Compute Calci5 = Calcio5.
Compute Calci6 = Calcio6.
Compute Calci7 = Calcio7.
Compute Calci8 = Calcio8.
Compute Calci9 = Calcio9.
Compute Calci10 = Calcio10.
Compute FIBR = fibra.
Compute FIBR3 = fibra3.
Compute FIBR4 = fibra4.
Compute FIBR5 = fibra5.
Compute FIBR6 = fibra6.
Compute FIBR7 = fibra7.
Compute FIBR8 = fibra8.
Compute FIBR9 = fibra9.
Compute FIBR10 = fibra10.
Compute alcohol = alcoholg.
Compute alcohol3 = alcoholg3.
Compute alcohol4 = alcoholg4.
Compute alcohol5 = alcoholg5.
Compute alcohol6 = alcoholg6.
Compute alcohol7 = alcoholg7.
Compute alcohol8 = alcoholg8.
Compute alcohol9 = alcoholg9.
Compute alcohol10 = alcoholg10.
Compute ENERGIA = energiat.
Compute ENERGIA3 = energiat3.
Compute ENERGIA4 = energiat4.
Compute ENERGIA5 = energiat5.
Compute ENERGIA6 = energiat6.
Compute ENERGIA7 = energiat7.
Compute ENERGIA8 = energiat8.
Compute ENERGIA9 = energiat9.
Compute ENERGIA10 = energiat10.
*creación de la variable media_fibra.
COMPUTE media_fibra = fibr. 
IF ((v_final_evento=10)) media_fibra= mean(fibr, fibr3, fibr4, fibr5, fibr6, fibr7, fibr8, fibr9, fibr10).
IF ((v_final_evento=9)) media_fibra = mean(fibr, fibr3, fibr4, fibr5, fibr6, fibr7, fibr8).
IF ((v_final_evento=8)) media_fibra = mean(fibr, fibr3, fibr4, fibr5, fibr6, fibr7).
IF ((v_final_evento=7)) media_fibra = mean(fibr, fibr3, fibr4, fibr5, fibr6).
IF ((v_final_evento=6)) media_fibra = mean(fibr, fibr3, fibr4, fibr5).
IF ((v_final_evento=5)) media_fibra = mean(fibr, fibr3, fibr4).
IF ((v_final_evento=4)) media_fibra = mean(fibr, fibr3).
IF ((v_final_evento=3)) media_fibra = mean(fibr).
VARIABLE LABELS media_fibra 'media ingesta fibra  31-08-2015 (g/d’a)'.
execute.
delete variables fibr  .
delete variables fibr3  .
delete variables fibr4  .
delete variables fibr5  .
delete variables fibr6  .
delete variables fibr7  .
delete variables fibr8  .
delete variables fibr9  .
delete variables fibr10  .
*creación de la variable media_vitaminaD.
COMPUTE media_vitaminaD = vitaminaD. 
IF ((v_final_evento=10)) media_vitaminaD= mean(vitaminaD, vitaminaD3, vitaminaD4, vitaminaD5, vitaminaD6, vitaminaD7, vitaminaD8, vitaminaD9, vitaminaD10).
IF ((v_final_evento=9)) media_vitaminaD = mean(vitaminaD, vitaminaD3, vitaminaD4, vitaminaD5, vitaminaD6, vitaminaD7, vitaminaD8).
IF ((v_final_evento=8)) media_vitaminaD = mean(vitaminaD, vitaminaD3, vitaminaD4, vitaminaD5, vitaminaD6, vitaminaD7).
IF ((v_final_evento=7)) media_vitaminaD = mean(vitaminaD, vitaminaD3, vitaminaD4, vitaminaD5, vitaminaD6).
IF ((v_final_evento=6)) media_vitaminaD = mean(vitaminaD, vitaminaD3, vitaminaD4, vitaminaD5).
IF ((v_final_evento=5)) media_vitaminaD = mean(vitaminaD, vitaminaD3, vitaminaD4).
IF ((v_final_evento=4)) media_vitaminaD = mean(vitaminaD, vitaminaD3).
IF ((v_final_evento=3)) media_vitaminaD = mean(vitaminaD).
VARIABLE LABELS media_vitaminaD 'media ingesta vitaminaD 31-08-2015 (microg/d’a)'.
execute.

delete variables vitaminaD  .
delete variables vitaminaD3  .
delete variables vitaminaD4  .
delete variables vitaminaD5  .
delete variables vitaminaD6  .
delete variables vitaminaD7  .
delete variables vitaminaD8  .
delete variables vitaminaD9  .
delete variables vitaminaD10  .
*creacion variable media ingesta calci ajustada.
COMPUTE media_calci = calci. 
IF ((v_final_evento=10)) media_calci= mean(calci, calci3, calci4, calci5, calci6, calci7, calci8, calci9, calci10).
IF ((v_final_evento=9)) media_calci = mean(calci, calci3, calci4, calci5, calci6, calci7, calci8).
IF ((v_final_evento=8)) media_calci = mean(calci, calci3, calci4, calci5, calci6, calci7).
IF ((v_final_evento=7)) media_calci = mean(calci, calci3, calci4, calci5, calci6).
IF ((v_final_evento=6)) media_calci = mean(calci, calci3, calci4, calci5).
IF ((v_final_evento=5)) media_calci = mean(calci, calci3, calci4).
IF ((v_final_evento=4)) media_calci = mean(calci, calci3).
IF ((v_final_evento=3)) media_calci = mean(calci).
VARIABLE LABELS media_calci 'media ingesta calci  31-08-2015 (mg/d’a)'.
execute.

delete variables calci  .
delete variables calci3  .
delete variables calci4  .
delete variables calci5  .
delete variables calci6  .
delete variables calci7  .
delete variables calci8  .
delete variables calci9  .
delete variables calci10  .
*ajuste alcohol a energia.
REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol
  /METHOD=ENTER energia
  /SAVE PRED RESID. 

Compute alcohol_ajust= 8.799849 + RES_1.
EXECUTE.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol3
  /METHOD=ENTER energia3
  /SAVE PRED RESID. 

Compute alcohol_ajust3= 8.165100 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol4
  /METHOD=ENTER energia4
  /SAVE PRED RESID. 

Compute alcohol_ajust4= 8.050484 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol5
  /METHOD=ENTER energia5
  /SAVE PRED RESID. 

Compute alcohol_ajust5= 7.784848 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol6
  /METHOD=ENTER energia6
  /SAVE PRED RESID. 

Compute alcohol_ajust6= 7.486174 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol7
  /METHOD=ENTER energia7
  /SAVE PRED RESID. 

Compute alcohol_ajust7= 7.659402 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol8
  /METHOD=ENTER energia8
  /SAVE PRED RESID. 

Compute alcohol_ajust8= 7.733627 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol9
  /METHOD=ENTER energia9
  /SAVE PRED RESID. 

Compute alcohol_ajust9= 8.615975 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT alcohol10
  /METHOD=ENTER energia10
  /SAVE PRED RESID. 

Compute alcohol_ajust10= 3.666667 + RES_1.
execute.
DELETE VARIABLES PRE_1 RES_1.
execute.
*creacion variable media ingesta alcohol ajustada.
COMPUTE media_alcohol = alcohol_ajust. 
IF ((v_final_evento=10)) media_alcohol= mean(alcohol_ajust, alcohol_ajust3, alcohol_ajust4, alcohol_ajust5, alcohol_ajust6, alcohol_ajust7, alcohol_ajust8, alcohol_ajust9, alcohol_ajust10).
IF ((v_final_evento=9)) media_alcohol = mean(alcohol_ajust, alcohol_ajust3, alcohol_ajust4, alcohol_ajust5, alcohol_ajust6, alcohol_ajust7, alcohol_ajust8).
IF ((v_final_evento=8)) media_alcohol = mean(alcohol_ajust, alcohol_ajust3, alcohol_ajust4, alcohol_ajust5, alcohol_ajust6, alcohol_ajust7).
IF ((v_final_evento=7)) media_alcohol = mean(alcohol_ajust, alcohol_ajust3, alcohol_ajust4, alcohol_ajust5, alcohol_ajust6).
IF ((v_final_evento=6)) media_alcohol = mean(alcohol_ajust, alcohol_ajust3, alcohol_ajust4, alcohol_ajust5).
IF ((v_final_evento=5)) media_alcohol = mean(alcohol_ajust, alcohol_ajust3, alcohol_ajust4).
IF ((v_final_evento=4)) media_alcohol = mean(alcohol_ajust, alcohol_ajust3).
IF ((v_final_evento=3)) media_alcohol = mean(alcohol_ajust).
VARIABLE LABELS media_alcohol 'media ingesta alcohol ajustada 31-08-2015 (g/día)'.
execute.

delete variables alcohol  .
delete variables alcohol3  .
delete variables alcohol4  .
delete variables alcohol5  .
delete variables alcohol6  .
delete variables alcohol7  .
delete variables alcohol8  .
delete variables alcohol9  .
delete variables alcohol10  .
delete variables alcohol_ajust  .
delete variables alcohol_ajust3  .
delete variables alcohol_ajust4  .
delete variables alcohol_ajust5  .
delete variables alcohol_ajust6  .
delete variables alcohol_ajust7  .
delete variables alcohol_ajust8  .
delete variables alcohol_ajust9  .
delete variables alcohol_ajust10  .
 
*ajuste hidratos de carbono a energia.
Compute hidratos = HC.
Compute hidratos3 = HC3.
Compute hidratos4 = HC4.
Compute hidratos5 = HC5.
Compute hidratos6 = HC6.
Compute hidratos7 = HC7.
Compute hidratos8 = HC8.
Compute hidratos9 = HC9.
Compute hidratos10 = HC10.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos
  /METHOD=ENTER energia
  /SAVE PRED RESID. 

Compute hidratos_ajust= 231.665871 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos3
  /METHOD=ENTER energia3
  /SAVE PRED RESID. 

Compute hidratos_ajust3= 231.938071 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos4
  /METHOD=ENTER energia4
  /SAVE PRED RESID. 

Compute hidratos_ajust4= 227.291983 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos5
  /METHOD=ENTER energia5
  /SAVE PRED RESID. 

Compute hidratos_ajust5= 223.261680 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos6
  /METHOD=ENTER energia6
  /SAVE PRED RESID. 

Compute hidratos_ajust6= 216.358884 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos7
  /METHOD=ENTER energia7
  /SAVE PRED RESID. 

Compute hidratos_ajust7= 215.357294 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos8
  /METHOD=ENTER energia8
  /SAVE PRED RESID. 

Compute hidratos_ajust8= 210.472301 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos9
  /METHOD=ENTER energia9
  /SAVE PRED RESID. 

Compute hidratos_ajust9= 201.435309 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT hidratos10
  /METHOD=ENTER energia10
  /SAVE PRED RESID. 

Compute hidratos_ajust10= 155.160435 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.
*creacion variable media ingesta hidratos ajustada.
COMPUTE media_hidratos = hidratos_ajust. 
IF ((v_final_evento=10)) media_hidratos= mean(hidratos_ajust, hidratos_ajust3, hidratos_ajust4, hidratos_ajust5, hidratos_ajust6, hidratos_ajust7, hidratos_ajust8, hidratos_ajust9, hidratos_ajust10).
IF ((v_final_evento=9)) media_hidratos = mean(hidratos_ajust, hidratos_ajust3, hidratos_ajust4, hidratos_ajust5, hidratos_ajust6, hidratos_ajust7, hidratos_ajust8).
IF ((v_final_evento=8)) media_hidratos = mean(hidratos_ajust, hidratos_ajust3, hidratos_ajust4, hidratos_ajust5, hidratos_ajust6, hidratos_ajust7).
IF ((v_final_evento=7)) media_hidratos = mean(hidratos_ajust, hidratos_ajust3, hidratos_ajust4, hidratos_ajust5, hidratos_ajust6).
IF ((v_final_evento=6)) media_hidratos = mean(hidratos_ajust, hidratos_ajust3, hidratos_ajust4, hidratos_ajust5).
IF ((v_final_evento=5)) media_hidratos = mean(hidratos_ajust, hidratos_ajust3, hidratos_ajust4).
IF ((v_final_evento=4)) media_hidratos = mean(hidratos_ajust, hidratos_ajust3).
IF ((v_final_evento=3)) media_hidratos = mean(hidratos_ajust).
VARIABLE LABELS media_hidratos 'media ingesta hidratos ajustada 31-08-2015 (g/día)'.
execute.

delete variables hidratos  .
delete variables hidratos3  .
delete variables hidratos4  .
delete variables hidratos5  .
delete variables hidratos6  .
delete variables hidratos7  .
delete variables hidratos8  .
delete variables hidratos9  .
delete variables hidratos10  .
delete variables hidratos_ajust  .
delete variables hidratos_ajust3  .
delete variables hidratos_ajust4  .
delete variables hidratos_ajust5  .
delete variables hidratos_ajust6  .
delete variables hidratos_ajust7  .
delete variables hidratos_ajust8  .
delete variables hidratos_ajust9  .
delete variables hidratos_ajust10  .

*ajuste proteinas a energia.
Compute proteinas = PROT.
Compute proteinas3 = PROT3.
Compute proteinas4 = PROT4.
Compute proteinas5 = PROT5.
Compute proteinas6 = PROT6.
Compute proteinas7 = PROT7.
Compute proteinas8 = PROT8.
Compute proteinas9 = PROT9.
Compute proteinas10 = PROT10.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas
  /METHOD=ENTER energia
  /SAVE PRED RESID. 

Compute proteinas_ajust= 94.858311 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas3
  /METHOD=ENTER energia3
  /SAVE PRED RESID. 

Compute proteinas_ajust3= 95.700159 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas4
  /METHOD=ENTER energia4
  /SAVE PRED RESID. 

Compute proteinas_ajust4= 93.451668 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas5
  /METHOD=ENTER energia5
  /SAVE PRED RESID. 

Compute proteinas_ajust5= 88.864764 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas6
  /METHOD=ENTER energia6
  /SAVE PRED RESID. 

Compute proteinas_ajust6= 85.765993 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas7
  /METHOD=ENTER energia7
  /SAVE PRED RESID. 

Compute proteinas_ajust7= 84.864274 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas8
  /METHOD=ENTER energia8
  /SAVE PRED RESID. 

Compute proteinas_ajust8= 83.896632 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas9
  /METHOD=ENTER energia9
  /SAVE PRED RESID. 

Compute proteinas_ajust9= 81.702585 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT proteinas10
  /METHOD=ENTER energia10
  /SAVE PRED RESID. 

Compute proteinas_ajust10= 70.533156 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.
*creacion variable media ingesta proteinas ajustada.
COMPUTE media_proteinas = proteinas_ajust. 
IF ((v_final_evento=10)) media_proteinas= mean(proteinas_ajust, proteinas_ajust3, proteinas_ajust4, proteinas_ajust5, proteinas_ajust6, proteinas_ajust7, proteinas_ajust8, proteinas_ajust9, proteinas_ajust10).
IF ((v_final_evento=9)) media_proteinas = mean(proteinas_ajust, proteinas_ajust3, proteinas_ajust4, proteinas_ajust5, proteinas_ajust6, proteinas_ajust7, proteinas_ajust8).
IF ((v_final_evento=8)) media_proteinas = mean(proteinas_ajust, proteinas_ajust3, proteinas_ajust4, proteinas_ajust5, proteinas_ajust6, proteinas_ajust7).
IF ((v_final_evento=7)) media_proteinas = mean(proteinas_ajust, proteinas_ajust3, proteinas_ajust4, proteinas_ajust5, proteinas_ajust6).
IF ((v_final_evento=6)) media_proteinas = mean(proteinas_ajust, proteinas_ajust3, proteinas_ajust4, proteinas_ajust5).
IF ((v_final_evento=5)) media_proteinas = mean(proteinas_ajust, proteinas_ajust3, proteinas_ajust4).
IF ((v_final_evento=4)) media_proteinas = mean(proteinas_ajust, proteinas_ajust3).
IF ((v_final_evento=3)) media_proteinas = mean(proteinas_ajust).
VARIABLE LABELS media_proteinas 'media ingesta proteinas ajustada 31-08-2015 (g/día)'.
execute.

delete variables proteinas  .
delete variables proteinas3  .
delete variables proteinas4  .
delete variables proteinas5  .
delete variables proteinas6  .
delete variables proteinas7  .
delete variables proteinas8  .
delete variables proteinas9  .
delete variables proteinas10  .
delete variables proteinas_ajust  .
delete variables proteinas_ajust3  .
delete variables proteinas_ajust4  .
delete variables proteinas_ajust5  .
delete variables proteinas_ajust6  .
delete variables proteinas_ajust7  .
delete variables proteinas_ajust8  .
delete variables proteinas_ajust9  .
delete variables proteinas_ajust10  .

*ajuste grasas_totales a energia.
Compute grasas_totales = gratot.
Compute grasas_totales3 = gratot3.
Compute grasas_totales4 = gratot4.
Compute grasas_totales5 = gratot5.
Compute grasas_totales6 = gratot6.
Compute grasas_totales7 = gratot7.
Compute grasas_totales8 = gratot8.
Compute grasas_totales9 = gratot9.
Compute grasas_totales10 = gratot10.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales
  /METHOD=ENTER energia
  /SAVE PRED RESID. 

Compute grasas_totales_ajust= 104.812737 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales3
  /METHOD=ENTER energia3
  /SAVE PRED RESID. 

Compute grasas_totales_ajust3= 107.684773 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales4
  /METHOD=ENTER energia4
  /SAVE PRED RESID. 

Compute grasas_totales_ajust4= 106.455052 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales5
  /METHOD=ENTER energia5
  /SAVE PRED RESID. 

Compute grasas_totales_ajust5= 104.896641 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales6
  /METHOD=ENTER energia6
  /SAVE PRED RESID. 

Compute grasas_totales_ajust6= 101.696825 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales7
  /METHOD=ENTER energia7
  /SAVE PRED RESID. 

Compute grasas_totales_ajust7= 101.386729 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales8
  /METHOD=ENTER energia8
  /SAVE PRED RESID. 

Compute grasas_totales_ajust8= 99.675664 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales9
  /METHOD=ENTER energia9
  /SAVE PRED RESID. 

Compute grasas_totales_ajust9= 98.413381 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.

 REGRESSION
  /MISSING LISTWISE
  /STATISTICS COEFF OUTS R ANOVA
  /CRITERIA=PIN(.05) POUT(.10)
  /NOORIGIN
  /DEPENDENT grasas_totales10
  /METHOD=ENTER energia10
  /SAVE PRED RESID. 

Compute grasas_totales_ajust10= 96.773189 + RES_1.
DELETE VARIABLES PRE_1 RES_1.
execute.
*creacion variable media ingesta grasas_totales ajustada.
COMPUTE media_grasas_totales = grasas_totales_ajust. 
IF ((v_final_evento=10)) media_grasas_totales=mean(grasas_totales_ajust, grasas_totales_ajust3,grasas_totales_ajust4,grasas_totales_ajust5,grasas_totales_ajust6,grasas_totales_ajust7,grasas_totales_ajust8,grasas_totales_ajust9,grasas_totales_ajust10).
IF ((v_final_evento=9)) media_grasas_totales = mean(grasas_totales_ajust, grasas_totales_ajust3, grasas_totales_ajust4, grasas_totales_ajust5, grasas_totales_ajust6, grasas_totales_ajust7, grasas_totales_ajust8).
IF ((v_final_evento=8)) media_grasas_totales = mean(grasas_totales_ajust, grasas_totales_ajust3, grasas_totales_ajust4, grasas_totales_ajust5, grasas_totales_ajust6, grasas_totales_ajust7).
IF ((v_final_evento=7)) media_grasas_totales = mean(grasas_totales_ajust, grasas_totales_ajust3, grasas_totales_ajust4, grasas_totales_ajust5, grasas_totales_ajust6).
IF ((v_final_evento=6)) media_grasas_totales = mean(grasas_totales_ajust, grasas_totales_ajust3, grasas_totales_ajust4, grasas_totales_ajust5).
IF ((v_final_evento=5)) media_grasas_totales = mean(grasas_totales_ajust, grasas_totales_ajust3, grasas_totales_ajust4).
IF ((v_final_evento=4)) media_grasas_totales = mean(grasas_totales_ajust, grasas_totales_ajust3).
IF ((v_final_evento=3)) media_grasas_totales = mean(grasas_totales_ajust).
VARIABLE LABELS media_grasas_totales 'media ingesta grasas_totales ajustada 31-08-2015 (g/día)'.
execute.

delete variables grasas_totales  .
delete variables grasas_totales3  .
delete variables grasas_totales4  .
delete variables grasas_totales5  .
delete variables grasas_totales6  .
delete variables grasas_totales7  .
delete variables grasas_totales8  .
delete variables grasas_totales9  .
delete variables grasas_totales10  .
delete variables grasas_totales_ajust  .
delete variables grasas_totales_ajust3  .
delete variables grasas_totales_ajust4  .
delete variables grasas_totales_ajust5  .
delete variables grasas_totales_ajust6  .
delete variables grasas_totales_ajust7  .
delete variables grasas_totales_ajust8  .
delete variables grasas_totales_ajust9  .
delete variables grasas_totales_ajust10  .

*Tabla 1. Características basales por TIG.
ONEWAY edad0 imc1 getota_1 cint1 peso1 BY TIG
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

ONEWAY edad0 imc1 getota_1 cint1 peso1 BY TCG
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

CROSSTABS
  /TABLES=sexo grup_int escolar1 tabaco0 m_diuret m_insulin m_glucocort m_osteoporosis  
    ado1 diabetes0 fractura1 hormo1 BY TIG
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT COLUMN TOTAL
  /COUNT ROUND CELL.


CROSSTABS
  /TABLES=sexo grup_int escolar1 tabaco0 m_diuret m_insulin m_glucocort m_osteoporosis  
    ado1 diabetes0 fractura1 hormo1 BY TCG
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT COLUMN TOTAL
  /COUNT ROUND CELL.

*TABLA 2. Características basales nutricionales según TIG.
ONEWAY hc prot gratot mo sa po alcoholg energiat fibra verdutot frutatot legumbre grupocer lacteos carnicos pescados olivatot fsecos calcio ac_oliva ac_olivavir vitD p14_v1 cargluce_mod ig_mod BY TIG
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

ONEWAY hc prot gratot mo sa po alcoholg energiat fibra verdutot frutatot legumbre grupocer lacteos carnicos pescados olivatot fsecos calcio ac_oliva ac_olivavir vitD p14_v1 cargluce_mod ig_mod BY TCG
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

 *Tabla 3. Modelos de Cox.
*Modelos con terciles de indice glucémico y carga glucemica.
*T1.
COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER TIG 
 /CONTRAST (TIG)=Indicator (1)
 /PATTERN BY TiG
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_IG_Mod
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER TCG  
 /CONTRAST (TCG)=Indicator (1)
 /PATTERN BY TcG
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_CARGLUCE_Mod
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*T2.
COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER TIG sexo edad0 grup_int tabaco0 escolar1 imc1 media_getot  
 /CONTRAST (TIG)=Indicator (1)
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /PATTERN BY TIG
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_IG_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 media_getot
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator 
 /CONTRAST (tabaco0 )=Indicator
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER TCG sexo edad0 grup_int tabaco0 escolar1 imc1 media_getot 
 /CONTRAST (TCG)=Indicator(1) 
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator 
 /CONTRAST (tabaco0 )=Indicator  
 /PATTERN BY TcG
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_CARGLUCE_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 media_getot
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator 
 /CONTRAST (tabaco0 )=Indicator
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*T3.
COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER TIG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot m_insulin ado1 m_diuret  vitamin1
 /CONTRAST (TIG)=Indicator (1)
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY TIG
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_IG_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot m_insulin ado1 m_diuret vitamin1
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER TCG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot m_insulin ado1 m_diuret vitamin1
  /CONTRAST (tcg)=Indicator (1)
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY TcG
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER  media_CARGLUCE_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot m_insulin ado1 m_diuret vitamin1
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*T4.
COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER TIG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_PROT media_fibra
media_calci media_vitaminaD m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia vitamin1
 /CONTRAST (TIG)=Indicator (1)
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY TIG
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_IG_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 media_glu fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_PROT media_fibra
media_calci media_vitaminaD m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia vitamin1 
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).


COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER TCG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia vitamin1
  /CONTRAST (tcg)=Indicator (1)
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY TcG
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_CARGLUCE_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia vitamin1 sexo*media_CARGLUCE_Mod
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Fracturas por TIG.
CROSSTABS
  /TABLES=fract_osteo_new BY wTIG
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.

*Fracturas por TCG.
CROSSTABS
  /TABLES=fract_osteo_new BY wTCG
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.

*Fracturas por THC.
CROSSTABS
  /TABLES=fract_osteo_new BY THC
  /FORMAT=AVALUE TABLES
  /CELLS=COUNT COLUMN 
  /COUNT ROUND CELL.

*Media de IG por TIG.
ONEWAY media_IG_MOD BY TIG
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

*Media de CG por TCG.
ONEWAY media_CARGLUCE_Mod BY TCG
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

*Media de HC por THC.
ONEWAY media_hidratos BY THC
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

****CALCULO DE LAS P for TREND.
*AOVE.
EXAMINE VARIABLES= media_IG_mod BY TIG
  /PLOT BOXPLOT STEMLEAF
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

EXAMINE VARIABLES= media_CARGLUCE_mod BY TCG
  /PLOT BOXPLOT STEMLEAF
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.


**RECUERDA que se añade el valor MEDIANO del tercil para la variable de estudio.

if (TIG= 1) trend_TIG= 45.162167	. 
if (TIG= 2) trend_TIG = 47.876987.
if (TIG= 3) trend_TIG = 51.009323.
EXECUTE.

if (TCG= 1) trend_TCG= 83.574221	.
if (TCG= 2) trend_TCG = 104.707510.
if (TCG= 3) trend_TCG = 134.414589.
EXECUTE.

*TIG
*Opción 1.0,229.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TIG 
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /PATTERN BY trend_TIG
  /CONTRAST (trend_TIG)=POLYNOMIAL
  /METHOD=ENTER trend_TIG
  /PLOT SURVIVAL
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Opción 2.0,897.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TIG sexo edad0 grup_int tabaco0 escolar1 imc1 media_getot  
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /PATTERN BY trend_TIG  
  /CONTRAST (trend_TIG)=POLYNOMIAL
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
  /PLOT SURVIVAL 
  /METHOD=ENTER trend_TIG sexo edad0 grup_int tabaco0 escolar1 imc1 media_getot 
  /PLOT SURVIVAL
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Opción 3.0,997.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TIG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot m_insulin ado1 m_diuret
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /PATTERN BY trend_TIG  
  /CONTRAST (trend_TIG)=POLYNOMIAL
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
  /PLOT SURVIVAL 
  /METHOD=ENTER trend_TIG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot m_insulin ado1 m_diuret vitamin1
  /PLOT SURVIVAL
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Opción 4.0,051.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TIG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD   m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /PATTERN BY trend_TIG  
  /CONTRAST (trend_TIG)=POLYNOMIAL
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
  /PLOT SURVIVAL 
  /METHOD=ENTER trend_TIG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD   m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /PLOT SURVIVAL
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*TCG
*Opción 1.0,112.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TCG 
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /PATTERN BY trend_TcG
  /CONTRAST (trend_TcG)=POLYNOMIAL
  /METHOD=ENTER trend_TcG
  /PLOT SURVIVAL
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Opción 2.0,925.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TCG sexo edad0 grup_int tabaco0 escolar1 imc1 media_getot  
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /PATTERN BY trend_TcG  
  /CONTRAST (trend_TcG)=POLYNOMIAL
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
  /PLOT SURVIVAL 
  /METHOD=ENTER trend_TcG sexo edad0 grup_int tabaco0 escolar1 imc1 media_getot 
  /PLOT SURVIVAL
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Opción 3.0,809.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TCG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot m_insulin ado1 m_diuret
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /PATTERN BY trend_TcG  
  /CONTRAST (trend_TcG)=POLYNOMIAL
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
  /PLOT SURVIVAL 
  /METHOD=ENTER trend_TcG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot m_insulin ado1 m_diuret vitamin1
  /PLOT SURVIVAL
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Opción 4.0,000049.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TCG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD   m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /PATTERN BY trend_TcG  
  /CONTRAST (trend_TcG)=POLYNOMIAL
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
  /PLOT SURVIVAL 
  /METHOD=ENTER trend_TcG sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD   m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /PLOT SURVIVAL
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).
****************************************************************************************************************************************************************
*Analisis de sensibilidad (exclusión de fracturas previas al año). 
DATASET ACTIVATE Conjunto_de_datos1.
USE ALL.
COMPUTE filter_$=(T_cox_new > 1).
VARIABLE LABELS filter_$ 'T_cox_new > 1 (FILTER)'.
VALUE LABELS filter_$ 0 'Not Selected' 1 'Selected'.
FORMATS filter_$ (f1.0).
FILTER BY filter_$.
EXECUTE.

RANK VARIABLES= media_CARGLUCE_Mod(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

RANK VARIABLES= media_IG_Mod(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.


COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER Nmedia_C sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD   m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /CONTRAST (Nmedia_C)=Indicator (1)
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY Nmedia_C
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER NTI001 sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD   m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /CONTRAST (NTI001)=Indicator (1)
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (diabetes0)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY NTI001
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

****CALCULO DE LAS P for TREND.
*AOVE.
EXAMINE VARIABLES= media_IG_mod BY NTI001
  /PLOT BOXPLOT STEMLEAF
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.

EXAMINE VARIABLES= media_CARGLUCE_mod BY Nmedia_C
  /PLOT BOXPLOT STEMLEAF
  /COMPARE GROUPS
  /STATISTICS DESCRIPTIVES
  /CINTERVAL 95
  /MISSING LISTWISE
  /NOTOTAL.


**RECUERDA que se añade el valor MEDIANO del tercil para la variable de estudio.

if (NTI001= 1) trend_TIG_sensi= 45.185300	. 
if (NTI001= 2) trend_TIG_sensi = 47.877098.
if (NTI001= 3) trend_TIG_sensi = 50.996290.
EXECUTE.

if (Nmedia_C= 1) trend_TCG_sensi= 83.832136	.
if (Nmedia_C= 2) trend_TCG_sensi = 104.735855.
if (Nmedia_C= 3) trend_TCG_sensi = 134.089562.
EXECUTE.

*Opción 4.0,051.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TIG_sensi sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Opción 4.0,051.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TCG_sensi sexo edad0 grup_int tabaco0 escolar1 imc1 diabetes0 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD   m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

filter off.
DELETE variables Nmedia_C.
DELETE VARIABLES Rmedia_C.
DELETE variables NTI001.
DELETE VARIABLES Rmedia_I.
DELETE variables trend_TIG_sensi.
DELETE variables trend_TCG_sensi.

*Grupo de intervecion por terciles.
CROSSTABS
  /TABLES=TIG BY grup_int
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT
  /COUNT ROUND CELL.

CROSSTABS
  /TABLES=TCG BY grup_int
  /FORMAT=AVALUE TABLES
  /STATISTICS=CHISQ 
  /CELLS=COUNT
  /COUNT ROUND CELL.

*Media de peso1 por TIG.
ONEWAY peso1 BY TIG
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).

*Media de peso1 por TCG.
ONEWAY peso1 BY TCG
  /STATISTICS DESCRIPTIVES 
  /MISSING ANALYSIS
  /POSTHOC=BONFERRONI ALPHA(0.05).
************************************************************************************************************************************************************************************
*Review.

CORRELATIONS
  /VARIABLES=media_glu media_Hb1Ac TIG TCG media_IG_Mod media_CARGLUCE_Mod
  /PRINT=TWOTAIL NOSIG
  /STATISTICS DESCRIPTIVES
  /MISSING=PAIRWISE.

RANK VARIABLES= media_glu(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

RANK VARIABLES= media_Hb1Ac(A)
  /RANK
  /NTILES(3)
  /PRINT=YES
  /TIES=MEAN.

CORRELATIONS
  /VARIABLES=NTI001 NTI002 TIG TCG
  /PRINT=TWOTAIL NOSIG
  /STATISTICS DESCRIPTIVES
  /MISSING=PAIRWISE.

*Ajuste con media de glucosa.
COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER TIG sexo edad0 grup_int tabaco01 escolar100 imc1 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_PROT media_fibra
media_calci media_vitaminaD m_diuret media_PO media_SA media_MO media_energia media_glu 
 /CONTRAST (TIG)=Indicator (1)
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar100)=Indicator  
 /CONTRAST (tabaco01 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY TIG
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_IG_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_PROT media_fibra
media_calci media_vitaminaD m_diuret media_PO media_SA media_MO media_energia media_glu 
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).


COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER TCG sexo edad0 grup_int tabaco0 escolar1 imc1 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD m_diuret media_PO media_SA media_MO media_energia media_glu 
  /CONTRAST (tcg)=Indicator (1)
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY TcG
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_CARGLUCE_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD m_diuret media_PO media_SA media_MO media_energia media_glu 
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

if (TIG= 1) trend_TIG= 45.162167	. 
if (TIG= 2) trend_TIG = 47.876987.
if (TIG= 3) trend_TIG = 51.009323.
EXECUTE.

if (TCG= 1) trend_TCG= 83.574221	.
if (TCG= 2) trend_TCG = 104.707510.
if (TCG= 3) trend_TCG = 134.414589.
EXECUTE.

*Opción 4.0,051.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TIG sexo edad0 grup_int tabaco0 escolar1 imc1 media_glu fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Opción 4.0,051.
COXREG T_cox_new
  /STATUS=fract_osteo_new(1)
  /METHOD=ENTER trend_TCG sexo edad0 grup_int tabaco0 escolar1 imc1 media_glu fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD   m_insulin ado1 m_diuret media_PO media_SA media_MO media_energia
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

*Ajuste con media de glucosa + hemoglobina glicosilada.
COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER TIG sexo edad0 grup_int tabaco0 escolar1 imc1 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_PROT media_fibra
media_calci media_vitaminaD m_diuret media_PO media_SA media_MO media_energia media_glu media_Hb1Ac 
 /CONTRAST (TIG)=Indicator (1)
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator  
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY TIG
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
 /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_IG_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_PROT media_fibra
media_calci media_vitaminaD m_diuret media_PO media_SA media_MO media_energia media_glu  
 /CONTRAST (sexo)=Indicator
 /CONTRAST (grup_int)=Indicator 
 /CONTRAST (escolar1)=Indicator
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PLOT SURVIVAL 
 /PRINT=CI(95) CORR
 /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).


COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER TCG sexo edad0 grup_int tabaco0 escolar1 imc1 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD m_diuret media_PO media_SA media_MO media_energia media_glu media_Hb1Ac 
  /CONTRAST (tcg)=Indicator (1)
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
 /PATTERN BY TcG
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

COXREG T_cox_new
  /STATUS= fract_osteo_new(1)
/METHOD=ENTER media_CARGLUCE_Mod sexo edad0 grup_int tabaco0 escolar1 imc1 fractura1 hormo1 m_osteoporosis m_glucocort media_getot media_alcoholg ALC media_prot media_fibra 
media_calci media_vitaminaD m_diuret media_PO media_SA media_MO media_energia media_glu media_Hb1Ac 
  /CONTRAST (sexo)=Indicator
  /CONTRAST (grup_int)=Indicator 
  /CONTRAST (escolar1)=Indicator 
 /CONTRAST (tabaco0 )=Indicator 
 /CONTRAST (fractura1)=Indicator 
 /CONTRAST (hormo1)=Indicator 
 /CONTRAST (m_osteoporosis)=Indicator 
 /CONTRAST (m_glucocort)=Indicator 
  /PLOT SURVIVAL 
  /PRINT=CI(95) CORR
  /CRITERIA=PIN(.05) POUT(.10) ITERATE(20).

