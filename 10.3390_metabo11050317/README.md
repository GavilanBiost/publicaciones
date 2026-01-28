# Circulating Metabolites Associated with Body Fat and Lean Mass in Adults with Overweight/Obesity

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.3390%2Fmetabo11050317-blue)](https://doi.org/10.3390/metabo11050317)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código y material de apoyo utilizados en el estudio “Circulating Metabolites Associated with Body Fat and Lean Mass in Adults with Overweight/Obesity”, cuyo objetivo fue identificar perfiles metabolómicos asociados con la masa grasa y la masa magra en adultos con sobrepeso u obesidad pertenecientes al estudio SATIN.

En este estudio se emplearon los siguientes métodos estadísticos:

1. **Fase 1: Preparación y preprocesamiento de datos**
   - Integración de datos clínicos, de composición corporal (DXA) y metabolómica.
   - Selección de metabolitos comunes a todas las visitas (V0, V8, V14).
   - Detección y eliminación de metabolitos con más del 20% de valores perdidos.
   - Imputación de valores faltantes mediante *random forest* (*missForest*).
   - Normalización de metabolitos mediante transformación *rank-based inverse normal*.

2. **Fase 2: Construcción de variables de composición corporal**
   - Análisis transversal basal (V0) para masa grasa (% body fat) y masa magra (lean mass).
   - Cálculo de cambios longitudinales (deltas V8–V0 y V14–V8).
   - Ajuste por sexo y edad mediante modelos lineales de mínimos cuadrados.

3. **Fase 3: Modelado estadístico y validación**
   - Modelos de regresión penalizada *elastic net*.
   - Validación cruzada interna (10-fold cross-validation).
   - Evaluación del rendimiento mediante correlaciones de Pearson, RMSE y R².
   - Validación semi-externa utilizando modelos basales para predecir resultados en V8 y V14.

Incluye:

- Scripts en R para el preprocesamiento de datos metabolómicos y de composición corporal.
- Código para imputación, normalización y análisis longitudinal.
- Modelos *elastic net* para masa grasa y masa magra.
- Procedimientos de validación interna y semi-externa.
- Publicación científica en formato PDF (Open Access).

> **Cita (formato PubMed):**  
> Papandreou C, García-Gavilán J, Camacho-Barcia L, Hansen TT, Sjödin A, Harrold JA, Halford JCG, Bulló M. Circulating metabolites associated with body fat and lean mass in adults with overweight/obesity. Metabolites. 2021;11(5):317. doi:10.3390/metabo11050317.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- **R ≥ 3.6.1**
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
  - `gridExtra`

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto se divide en las siguientes fases del análisis:

1. **Fase 1: Preparación de los datos**
   1. Importación de bases de datos clínicas, de composición corporal y metabolómicas.
   2. Selección de metabolitos disponibles en todas las visitas.
   3. Evaluación del porcentaje de valores perdidos por metabolito.
   4. Eliminación de metabolitos con >20% de NA.
   5. Imputación de valores faltantes mediante *missForest*.

2. **Fase 2: Procesamiento de composición corporal**
   1. Análisis transversal basal (V0) de masa grasa y masa magra.
   2. Cálculo de deltas longitudinales (V8–V0 y V14–V8).
   3. Ajuste por sexo y edad mediante modelos lineales.
   4. Normalización de metabolitos (rankNorm).

3. **Fase 3: Análisis estadístico**
   1. Ajuste de modelos *elastic net* para masa grasa y masa magra.
   2. Validación cruzada interna (10×10 CV).
   3. Extracción de metabolitos seleccionados y coeficientes.
   4. Evaluación de rendimiento (Pearson, RMSE, R²).
   5. Validación semi-externa usando modelos basales en visitas posteriores.

## Datos {#datos}

Por cuestiones de tratamiento y protección de datos, las bases de datos de dichas publicaciones solo son accesibles bajo petición estricta al Steering Committee (SC) de cada estudio respectivo.

En este repositorio no se incluyen las bases de datos originales de ninguno de los estudios. La forma de contacto con el SC de cada estudio está disponible dentro de cada publicación respectiva a través de los autores de correspondencia.

Las publicaciones publicadas bajo dominio Open Access están incluidas en su carpeta correspondiente en formato PDF.

## Licencia y citación {#licencia}

Licencia GNU GENERAL PUBLIC LICENSE

---

# Circulating Metabolites Associated with Body Fat and Lean Mass in Adults with Overweight/Obesity

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.3390%2Fmetabo11050317-blue)](https://doi.org/10.3390/metabo11050317)

## SHORT DESCRIPTION

This repository contains the code and supporting material used in the study “Circulating Metabolites Associated with Body Fat and Lean Mass in Adults with Overweight/Obesity”, which aimed to identify metabolomic profiles associated with body fat and lean mass in adults with overweight or obesity from the SATIN study.

The following statistical methods were applied:

1. **Phase 1: Data preparation and preprocessing**
   - Integration of clinical, body composition (DXA), and metabolomic data.
   - Selection of metabolites available at all visits (V0, V8, V14).
   - Removal of metabolites with more than 20% missing values.
   - Missing value imputation using *random forest* (*missForest*).
   - Rank-based inverse normal transformation of metabolomic data.

2. **Phase 2: Body composition processing**
   - Cross-sectional baseline analysis (V0) of body fat and lean mass.
   - Longitudinal delta calculation (V8–V0 and V14–V8).
   - Adjustment for sex and age using linear models.

3. **Phase 3: Statistical modeling and validation**
   - Penalized regression models using *elastic net*.
   - Internal validation with 10-fold cross-validation.
   - Performance assessment using Pearson correlations, RMSE, and R².
   - Semi-external validation using baseline models to predict follow-up visits.

Includes:

- R scripts for metabolomic and body composition preprocessing.
- Code for imputation, normalization, and longitudinal analysis.
- Elastic net models for fat mass and lean mass.
- Internal and semi-external validation procedures.
- Open Access scientific publication in PDF format.

> **Citation (PubMed format):**  
> Papandreou C, García-Gavilán J, Camacho-Barcia L, Hansen TT, Sjödin A, Harrold JA, Halford JCG, Bulló M. Circulating metabolites associated with body fat and lean mass in adults with overweight/obesity. Metabolites. 2021;11(5):317. doi:10.3390/metabo11050317.

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
  - `gridExtra`

## Project structure {#estructura-del-proyecto}

The project is organized into the following analytical phases:

1. **Phase 1: Data preparation**
   1. Import of clinical, body composition, and metabolomic datasets.
   2. Selection of metabolites common across visits.
   3. Missing value assessment and filtering.
   4. Missing value imputation using *missForest*.

2. **Phase 2: Body composition processing**
   1. Baseline cross-sectional analysis (V0).
   2. Longitudinal delta calculations (V8–V0, V14–V8).
   3. Adjustment for sex and age.
   4. Rank-based normalization of metabolites.

3. **Phase 3: Statistical analysis**
   1. Elastic net regression modeling.
   2. Internal cross-validation (10×10 CV).
   3. Extraction of selected metabolites and coefficients.
   4. Model performance evaluation.
   5. Semi-external validation at follow-up visits.

## Data {#datos}

Due to ethical and data protection considerations, the datasets associated with these publications are only available upon strict request to the Steering Committee (SC) of each respective study.

This repository does not include the original study datasets. Contact information for the SC of each study can be found in the corresponding publication through the corresponding authors.

Open Access publications are included in their respective folders in PDF format.

## License and citation {#licencia}

GNU GENERAL PUBLIC LICENSE
