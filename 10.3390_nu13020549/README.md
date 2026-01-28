# Circulating Metabolites Associated with Postprandial Satiety in Overweight/Obese Participants: The SATIN Study

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.3390%2Fnu13020549-blue)](https://doi.org/10.3390/nu13020549)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código y material de apoyo utilizados en el estudio “Circulating Metabolites Associated with Postprandial Satiety in Overweight/Obese Participants: The SATIN Study”, cuyo objetivo fue identificar un perfil metabolómico asociado con la saciedad postprandial en participantes con sobrepeso u obesidad.

En este estudio se emplearon los siguientes métodos estadísticos:

1. **Fase 1: Preparación y preprocesamiento de datos**
   - Limpieza de bases de datos clínicas, bioquímicas y metabolómicas.
   - Exclusión de metabolitos con más del 20% de valores perdidos.
   - Imputación de valores perdidos mediante *random forest*.
   - Normalización de metabolitos mediante transformación *rank-based inverse normal*.

2. **Fase 2: Cálculo de variables de saciedad**
   - Cálculo del área incremental bajo la curva (iAUC) de la escala visual analógica (VAS) de saciedad mediante integración trapezoidal.
   - Ajuste del iAUC por cambios de peso y grupo de intervención mediante modelos lineales.

3. **Fase 3: Modelado estadístico**
   - Modelos de regresión penalizada *elastic net*.
   - Validación cruzada (10-fold cross-validation).
   - Evaluación del rendimiento mediante correlaciones de Pearson, RMSE y R².

Incluye:

- Scripts en R para el preprocesamiento de datos, imputación, normalización y ajuste de modelos estadísticos.
- Código para el cálculo de iAUC de la saciedad.
- Resultados de validación interna y coeficientes de los modelos.
- Publicación científica en formato PDF (Open Access).

> **Cita (formato PubMed):**  
> Camacho-Barcia L, García-Gavilán J, Papandreou C, Hansen TT, Harrold JA, Finlayson G, Blundell JE, Sjödin A, Halford JCG, Bulló M. Circulating metabolites associated with postprandial satiety in overweight/obese participants: the SATIN study. Nutrients. 2021;13(2):549. doi:10.3390/nu13020549.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- R ≥ 3.6.1
- Paquetes utilizados:
  - `readxl`
  - `xlsx`
  - `missForest`
  - `RNOmni`
  - `caTools`
  - `glmnet`
  - `pROC`
  - `rio`
  - `haven`
  - `agricolae`
  - `tidyverse`
  - `caret`
  - `magrittr`
  - `dplyr` 
  - `pracma`  

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto se divide en las siguientes fases del análisis:

1. **Fase 1: Preparación de los datos**
   1. Importación de bases de datos clínicas, antropométricas y metabolómicas.
   2. Selección de metabolitos por visita (v8 y v14).
   3. Evaluación del porcentaje de valores perdidos.
   4. Eliminación de metabolitos con >20% de NA.
   5. Imputación de valores faltantes mediante *missForest*.

2. **Fase 2: Procesamiento de variables de saciedad**
   1. Escalado de las puntuaciones VAS.
   2. Cálculo del iAUC mediante integración trapezoidal.
   3. Ajuste del iAUC por cambios de peso y grupo de intervención.

3. **Fase 3: Análisis estadístico**
   1. Normalización de metabolitos (rankNorm).
   2. Ajuste de modelos *elastic net*.
   3. Validación cruzada interna (10×10 CV).
   4. Obtención de coeficientes y métricas de rendimiento.
   5. Análisis independiente para las visitas v8 y v14.

## Datos {#datos}

Por cuestiones de tratamiento y protección de datos, las bases de datos de dichas publicaciones solo son accesibles bajo petición estricta al Steering Committee (SC) de cada estudio respectivo.

En este repositorio no se incluyen las bases de datos originales de ninguno de los estudios. La forma de contacto con el SC de cada estudio está disponible dentro de cada publicación respectiva a través de los autores de correspondencia.

Las publicaciones publicadas bajo dominio Open Access están incluidas en su carpeta correspondiente en formato PDF.

## Licencia y citación {#licencia}

Licencia GNU GENERAL PUBLIC LICENSE

---

# Circulating Metabolites Associated with Postprandial Satiety in Overweight/Obese Participants: The SATIN Study

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.3390%2Fnu13020549-blue)](https://doi.org/10.3390/nu13020549)

## SHORT DESCRIPTION

This repository contains the code and supporting material used in the study **“Circulating Metabolites Associated with Postprandial Satiety in Overweight/Obese Participants: The SATIN Study”**, which aimed to identify a metabolomic profile associated with postprandial satiety in overweight and obese individuals.

The following statistical methods were applied in this study:

1. **Phase 1: Data preparation and preprocessing**
   - Cleaning of clinical, biochemical, and metabolomic datasets.
   - Exclusion of metabolites with more than 20% missing values.
   - Imputation of missing values using *random forest*.
   - Rank-based inverse normal transformation of metabolomic data.

2. **Phase 2: Satiety variable calculation**
   - Calculation of the incremental area under the curve (iAUC) for visual analogue scale (VAS) satiety scores using trapezoidal integration.
   - Adjustment of iAUC for weight changes and intervention group using linear models.

3. **Phase 3: Statistical modeling**
   - Penalized regression models using *elastic net*.
   - Internal validation using 10-fold cross-validation.
   - Model performance assessment using Pearson correlations, RMSE, and R².

Includes:

- R scripts for data preprocessing, imputation, normalization, and statistical modeling.
- Code for iAUC calculation of satiety responses.
- Internal validation results and model coefficients.
- Open Access scientific publication in PDF format.

> **Citation (PubMed format):**  
> Camacho-Barcia L, García-Gavilán J, Papandreou C, Hansen TT, Harrold JA, Finlayson G, Blundell JE, Sjödin A, Halford JCG, Bulló M. Circulating metabolites associated with postprandial satiety in overweight/obese participants: the SATIN study. Nutrients. 2021;13(2):549. doi:10.3390/nu13020549.

## Contents

1. [Requirements](#requisitos)
2. [Project structure](#estructura-del-proyecto)
3. [Data](#datos)
4. [License and citation](#licencia)

## Requirements {#requisitos}

- **R ≥ 3.6.1**
- Packages used:
  - `readxl`
  - `xlsx`
  - `missForest`
  - `RNOmni`
  - `caTools`
  - `glmnet`
  - `pROC`
  - `rio`
  - `haven`
  - `agricolae`
  - `tidyverse`
  - `caret`
  - `magrittr`
  - `dplyr` 
  - `pracma` 

## Project structure {#estructura-del-proyecto}

The project is organized into the following analytical phases:

1. **Phase 1: Data preparation**
   1. Import of clinical, anthropometric, and metabolomic datasets.
   2. Metabolite selection by visit (v8 and v14).
   3. Assessment of missing value percentages.
   4. Removal of metabolites with >20% missing values.
   5. Missing value imputation using *missForest*.

2. **Phase 2: Satiety processing**
   1. Scaling of VAS scores.
   2. iAUC calculation using trapezoidal integration.
   3. Adjustment of iAUC for weight change and intervention group.

3. **Phase 3: Statistical analysis**
   1. Rank-based normalization of metabolomic data.
   2. Elastic net regression modeling.
   3. Internal cross-validation (10×10 CV).
   4. Extraction of model coefficients and performance metrics.
   5. Separate analyses for visits v8 and v14.

## Data {#datos}

Due to data protection and ethical considerations, the datasets associated with these publications are only available upon strict request to the Steering Committee (SC) of each respective study.

This repository does not include the original study datasets. Contact information for the SC of each study can be found in the corresponding publication through the corresponding authors.

Publications released under Open Access are included in their respective folders in PDF format.

## License and citation {#licencia}

GNU GENERAL PUBLIC LICENSE
