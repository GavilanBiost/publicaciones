### Spanish

# Plasma Metabolite Profiles Associated with the Amount and Source of Meat and Fish Consumption and the Risk of Type 2 Diabetes

## Código y material de apoyo al artículo

[![DOI](https://img.shields.io/badge/DOI-10.1002%2Fmnfr.202200145-blue)](https://doi.org/10.1002/mnfr.202200145)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código estadístico en R, las figuras y el material suplementario utilizados en el estudio que identifica perfiles metabolómicos plasmáticos asociados al consumo de carne total, carne roja, carne procesada y pescado, y evalúa su asociación con el riesgo de diabetes tipo 2 (T2D) en participantes del estudio PREDIMED.

El objetivo principal fue avanzar desde la epidemiología nutricional clásica hacia un enfoque de nutrición de sistemas, identificando metabolitos que actúan como posibles mediadores biológicos entre la dieta y el riesgo cardiometabólico.

### Métodos estadísticos y analíticos

1. **Evaluación dietética**
   - Cuestionario de frecuencia alimentaria (FFQ) validado de 137 ítems
   - Cálculo de ingesta de:
     - Carne total (TM)
     - Carne roja (RM)
     - Carne roja procesada (PRM)
     - Pescado (blanco, azul y marisco)

2. **Perfilado metabolómico**
   - Muestras plasmáticas en ayunas (baseline y 1 año)
   - Análisis mediante LC–MS/MS de alta resolución
   - 385 metabolitos finales tras control de calidad
   - Estandarización usando muestras de referencia y escalado por desviación estándar

3. **Imputación y preprocesamiento**
   - Imputación de metabolitos con <20% de valores perdidos mediante random forest (missForest)
   - Autoscaling (centrado y escalado)

4. **Identificación de firmas metabolómicas**
   - Elastic Net Regression (glmnet)
   - Validación cruzada 10×10 (10 iteraciones de 10-fold CV)
   - Selección de parámetros óptimos (α y λ.min)
   - Construcción de perfiles metabolómicos específicos para TM, RM, PRM y pescado

5. **Validación interna**
   - Correlación de perfiles metabolómicos con la ingesta dietética:
     - Basal (población de descubrimiento)
     - Seguimiento a 1 año (población de validación)

6. **Análisis de asociación con diabetes tipo 2**
   - Modelos de Cox con pesos de Barlow (diseño caso-cohorte)
   - Varianza robusta
   - Resultados expresados como Hazard Ratios (HR) por incremento de 1 DE del perfil metabolómico

   Ajustes progresivos:
   - Modelo 1: edad, sexo y propensity scores (asignación al grupo de intervención)
   - Modelo 2: IMC, tabaquismo, alcohol (y término cuadrático), educación, actividad física, antecedentes familiares, energía total y grupos alimentarios
   - Modelo 3: modelo 2 + consumo dietético del alimento correspondiente

7. **Análisis estratificados y de sensibilidad**
   - Análisis por sexo
   - Análisis por grupo de intervención (dieta mediterránea vs control)
   - Exclusión de participantes con enfermedad cardiovascular incidente
   - Análisis alternativo con tertiles extremos (T1 vs T3)

Incluye:

- Scripts en R para:
  - Limpieza y preprocesamiento de datos
  - Modelos elastic net
  - Análisis de Cox
  - Figuras (diagramas de Venn, correlaciones, coeficientes)
- Material suplementario (tablas y figuras)
- Artículo original en PDF

> García-Gavilán J, Nishi SK, Paz-Graniel I, Guasch-Ferré M, Razquin C, Clish CB, Toledo E, Ruiz-Canela M, Corella D, Deik A, Drouin-Chartier JP, Wittenbecher C, Babio N, Estruch R, Ros E, Fitó M, Arós F, Fiol M, Serra-Majem L, Liang L, Martínez-González MA, Hu FB, Salas-Salvadó J. Plasma Metabolite Profiles Associated with the Amount and Source of Meat and Fish Consumption and the Risk of Type 2 Diabetes. Mol Nutr Food Res. 2022 Dec;66(23):e2200145. doi: 10.1002/mnfr.202200145. Epub 2022 Oct 26. PMID: 36214069; PMCID: PMC9722604.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- R ≥ 3.6
- Paquetes principales:
  - `glmnet` – Elastic net regression
  - `missForest` – Imputación de datos
  - `survival` – Modelos de Cox
  - `Hmisc`, `car` – Utilidades estadísticas
  - `ggplot2` – Visualización
  - `rms`, `splines` – Modelos avanzados y no lineales
  - `rio`, `readxl`, `haven` – Importación/exportación de datos

## Estructura del proyecto {#estructura-del-proyecto}

1. **Preparación de datos**
   - Limpieza de FFQ y covariables
   - Procesamiento de metabolómica

2. **Construcción de perfiles metabolómicos**
   - Elastic net para TM, RM, PRM y pescado
   - Validación cruzada y selección de metabolitos

3. **Validación de firmas**
   - Correlaciones dieta–metabolitos
   - Comparación basal vs 1 año

4. **Modelos de riesgo de T2D**
   - Cox con pesos de Barlow
   - Modelos ajustados y estratificados

5. **Análisis de sensibilidad**
   - Exclusiones
   - Tertiles extremos
   - Subanálisis por sexo e intervención

6. **Material suplementario**
   - Tablas suplementarias
   - Figuras (diagramas de Venn, coeficientes metabolómicos)

## Datos {#datos}

Por motivos de confidencialidad y protección de datos, las bases de datos del estudio PREDIMED no se incluyen en este repositorio.  
El acceso a los datos requiere autorización del Steering Committee del estudio PREDIMED, disponible a través de los autores de correspondencia del artículo original.

El artículo es Open Access y se incluye en formato PDF junto con el material suplementario.

## Licencia y citación {#licencia}

Licencia GNU GENERAL PUBLIC LICENSE (GPL).  
Si utilizas este código, por favor cita el artículo original.

---

### English

# Plasma Metabolite Profiles Associated with the Amount and Source of Meat and Fish Consumption and the Risk of Type 2 Diabetes

## Code and supporting material for the article

[![DOI](https://img.shields.io/badge/DOI-10.1002%2Fmnfr.202200145-blue)](https://doi.org/10.1002/mnfr.202200145)

## SHORT DESCRIPTION

This repository provides the R code, figures, and supplementary material used to identify plasma metabolite signatures of meat and fish consumption and to evaluate their association with incident type 2 diabetes (T2D) in participants from the PREDIMED trial.

The study applies a metabolomics-driven systems nutrition approach to explore biological pathways linking diet to cardiometabolic risk.

Includes:

- R scripts for elastic net regression, Cox models, and visualization  
- Supplementary tables and figures  
- Original Open Access PDF

## Data {#data}

PREDIMED datasets are not publicly available due to data protection policies. Access requires approval from the corresponding Steering Committee.

## License {#license}

GNU GENERAL PUBLIC LICENSE (GPL).
