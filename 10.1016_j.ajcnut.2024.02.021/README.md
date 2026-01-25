# Effect of 1-year lifestyle intervention with energy-reduced Mediterranean diet and physical activity promotion on the gut metabolome and microbiota

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.ajcnut.2024.02.021-blue)](https://doi.org/10.1016/j.ajcnut.2024.02.021)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código y el material de apoyo utilizados en el análisis del estudio PREDIMED-Plus, cuyo objetivo fue evaluar el efecto de una intervención intensiva de estilo de vida basada en una dieta mediterránea hipocalórica y la promoción de la actividad física sobre el metaboloma fecal y la microbiota intestinal tras un año de seguimiento.

En este estudio se emplearon los siguientes métodos estadísticos:

1. **Fase 1: Preparación y preprocesado de los datos**
   - Fusión de bases de datos metabolómicas (HILIC-pos, HILIC-neg, C8-pos, C18-neg).
   - Eliminación de metabolitos de control de calidad (QC).
   - Eliminación de metabolitos y participantes con >20% de valores perdidos.
   - Imputación de valores perdidos mediante `random forest` (`missForest`).
   - Transformación logarítmica y escalado por la mediana.

2. **Fase 2: Análisis estadístico**
   - Modelos de regresión lineal multivariante para evaluar cambios a 1 año entre grupos.
   - Corrección por comparaciones múltiples mediante FDR (Benjamini–Hochberg).

3. **Fase 3: Análisis de redes metabolómicas**
   - Análisis de redes de coexpresión mediante WGCNA.
   - Identificación de subredes metabolómicas y metabolitos hub.
   - Asociación de cambios en subredes con factores de riesgo cardiovascular.

Incluye:

- Scripts en `R` para el preprocesado de datos, imputación, análisis estadístico y análisis de redes.
- Figuras finales del artículo.
- Material suplementario asociado al estudio.
 
> García-Gavilán JF, Atzeni A, Babio N, Liang L, Belzer C, Vioque J, Corella D, Fitó M, Vidal J, Moreno-Indias I, Torres-Collado L, Coltell O, Toledo E, Clish C, Hernando J, Yun H, Hernández-Cacho A, Jeanfavre S, Dennis C, Gómez-Pérez AM, Martínez MA, Ruiz-Canela M, Tinahones FJ, Hu FB, Salas-Salvadó J. Effect of 1-year lifestyle intervention with energy-reduced Mediterranean diet and physical activity promotion on the gut metabolome and microbiota: a randomized clinical trial. Am J Clin Nutr. 2024 May;119(5):1143-1154. doi: 10.1016/j.ajcnut.2024.02.021. Epub 2024 Feb 29. PMID: 38428742.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- **R ≥ 3.6**
- Paquetes utilizados:
  - `readxl`
  - `missForest`
  - `RNOmni`
  - `haven`
  - `rio`
  - `qvalue`
  - `dynamicTreeCut`
  - `cluster`
  - `flashClust`
  - `Hmisc`
  - `reshape`
  - `foreach`
  - `doParallel`
  - `WGCNA`
  - `corrplot`
  - `ppcor`

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto se divide en las siguientes fases:

1. **Fase 1: Preparación de los datos**
   1. Importación de bases de datos metabolómicas.
   2. Fusión de plataformas analíticas.
   3. Control de calidad y eliminación de metabolitos QC.
   4. Filtrado por valores perdidos.
   5. Imputación de valores faltantes.
   6. Transformación logarítmica y generación de deltas (1 año – basal).

2. **Fase 2: Análisis estadístico**
   1. Modelos de regresión lineal multivariante.
   2. Ajuste por covariables clínicas y demográficas.
   3. Corrección por FDR.

3. **Fase 3: Análisis de redes**
   1. Construcción de redes metabolómicas mediante WGCNA.
   2. Identificación de subredes y metabolitos hub.
   3. Asociación de subredes con la intervención y factores de riesgo cardiovascular.

## Datos {#datos}

Por cuestiones de tratamiento y protección de datos, las bases de datos de dichas publicaciones solo son accesibles bajo petición estricta al Steering Committee (SC) de cada estudio respectivo.  
En este repositorio no se incluyen las bases de datos originales de los estudios.

La forma de contacto con el SC de cada estudio está disponible dentro de cada respectiva publicación a través de los autores de correspondencia.  
Las publicaciones en Open Access se incluyen en este repositorio en formato PDF.

## Licencia y citación {#licencia}

Licencia GNU GENERAL PUBLIC LICENSE

---

### English

# Effect of 1-year lifestyle intervention with energy-reduced Mediterranean diet and physical activity promotion on the gut metabolome and microbiota

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.ajcnut.2024.02.021-blue)](https://doi.org/10.1016/j.ajcnut.2024.02.021)

## SHORT DESCRIPTION

This repository contains the code and supporting material used in the analysis of the PREDIMED-Plus study, which aimed to evaluate the effect of an intensive lifestyle intervention based on an energy-reduced Mediterranean diet and physical activity promotion on the fecal metabolome and gut microbiota after one year of follow-up.

The following statistical methods were applied in this study:

1. **Phase 1: Data preparation and preprocessing**
   - Integration of metabolomics datasets (HILIC-pos, HILIC-neg, C8-pos, C18-neg).
   - Removal of quality control (QC) metabolites.
   - Exclusion of metabolites and participants with more than 20% missing values.
   - Missing value imputation using a *random forest* approach (`missForest`).
   - Log-transformation and median scaling.

2. **Phase 2: Statistical analysis**
   - Multivariable linear regression models to assess 1-year changes between study groups.
   - Multiple testing correction using false discovery rate (FDR; Benjamini–Hochberg).

3. **Phase 3: Metabolomic network analysis**
   - Weighted Gene Co-expression Network Analysis (WGCNA).
   - Identification of metabolomic subnetworks and hub metabolites.
   - Association of subnetwork changes with cardiovascular risk factors.

Includes:

- R scripts for data preprocessing, imputation, statistical analyses, and network analyses.
- Final figures included in the manuscript.
- Supplementary material associated with the study.

> García-Gavilán JF, Atzeni A, Babio N, Liang L, Belzer C, Vioque J, Corella D, Fitó M, Vidal J, Moreno-Indias I, Torres-Collado L, Coltell O, Toledo E, Clish C, Hernando J, Yun H, Hernández-Cacho A, Jeanfavre S, Dennis C, Gómez-Pérez AM, Martínez MA, Ruiz-Canela M, Tinahones FJ, Hu FB, Salas-Salvadó J. Effect of 1-year lifestyle intervention with energy-reduced Mediterranean diet and physical activity promotion on the gut metabolome and microbiota: a randomized clinical trial. Am J Clin Nutr. 2024 May;119(5):1143-1154. doi: 10.1016/j.ajcnut.2024.02.021. Epub 2024 Feb 29. PMID: 38428742.

## Contents

1. [Requirements](#requirements)
2. [Project structure](#project-structure)
3. [Data](#data)
4. [License and citation](#license)

## Requirements {#requirements}

- **R ≥ 3.6**
- Required packages:
 - `readxl`
  - `missForest`
  - `RNOmni`
  - `haven`
  - `rio`
  - `qvalue`
  - `dynamicTreeCut`
  - `cluster`
  - `flashClust`
  - `Hmisc`
  - `reshape`
  - `foreach`
  - `doParallel`
  - `WGCNA`
  - `corrplot`
  - `ppcor`

## Project structure {#project-structure}

The project is organized into the following phases:

1. **Phase 1: Data preparation**
   1. Import of metabolomics datasets.
   2. Integration of analytical platforms.
   3. Quality control and removal of QC metabolites.
   4. Filtering based on missing values.
   5. Missing data imputation.
   6. Log-transformation and computation of deltas (1-year – baseline).

2. **Phase 2: Statistical analysis**
   1. Multivariable linear regression models.
   2. Adjustment for clinical and demographic covariates.
   3. False discovery rate correction.

3. **Phase 3: Network analysis**
   1. Construction of metabolomic networks using WGCNA.
   2. Identification of subnetworks and hub metabolites.
   3. Association of metabolomic subnetworks with the intervention and cardiovascular risk factors.

## Data {#data}

Due to data protection and privacy regulations, the databases used in these publications are only accessible upon strict request to the Steering Committee (SC) of each respective study.  
Therefore, no raw study datasets are included in this repository.

Contact information for each Steering Committee is available in the corresponding publication through the corresponding authors.  
Publications released under Open Access are included in this repository in PDF format.

## License and citation {#license}

Licensed under the GNU GENERAL PUBLIC LICENSE
