# Adherence to the Mediterranean diet and nuclear magnetic resonance spectroscopy biomarkers in older individuals at high cardiovascular disease risk

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.ajcnut.2023.11.003-blue)](https://doi.org/10.1016/j.ajcnut.2023.11.003)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código y material de apoyo utilizados en el estudio “Adherence to the Mediterranean diet and nuclear magnetic resonance spectroscopy biomarkers in older individuals at high cardiovascular disease risk”, cuyo objetivo fue evaluar la asociación transversal y longitudinal entre la adherencia a la dieta mediterránea (MEDAS) y perfiles avanzados de lipoproteínas, metabolitos relacionados con el metabolismo de la glucosa e inflamación, determinados mediante espectroscopía de resonancia magnética nuclear (NMR).

El estudio se realizó en participantes mayores con alto riesgo cardiometabólico del centro PREDIMED-Reus.

En este estudio se emplearon los siguientes métodos estadísticos:

1. **Fase 1: Preparación y preprocesamiento de datos**
   - Integración de bases de datos dietéticas (FFQ, MEDAS), clínicas y metabolómicas (NMR).
   - Cálculo de cambios a 1 año (deltas) en MEDAS y biomarcadores.
   - Evaluación de valores perdidos y decisión de no exclusión por >20% de NA.
   - Imputación de valores faltantes mediante *random forest* (*missForest*).
   - Normalización de biomarcadores mediante transformación *rank-based inverse normal* (Blom).

2. **Fase 2: Definición de exposición dietética**
   - Clasificación de la adherencia basal a la dieta mediterránea en categorías de MEDAS (<8, 8–10, >10).
   - Clasificación de los cambios a 1 año en MEDAS (<1, 1–3, >3).
   - Construcción de modelos categóricos y binomiales (T1 vs T3).

3. **Fase 3: Análisis estadístico**
   - Modelos ANCOVA para análisis transversal y longitudinal.
   - Ajuste por edad, sexo, IMC, tabaquismo, actividad física, diabetes, dislipemia, hipertensión, tratamiento con estatinas y grupo de intervención.
   - Comparaciones múltiples mediante método de Tukey.
   - Análisis de correlación de Pearson y Spearman.
   - Generación de figuras con medias e intervalos de confianza al 95%.

Incluye:

- Scripts en R para la creación de bases de datos analíticas.
- Código para imputación, normalización y generación de deltas.
- Análisis ANCOVA y modelos binomiales.
- Cálculo de correlaciones y tablas descriptivas.
- Código para la generación de figuras finales (barras con IC95%).
- Publicación científica en formato PDF (Open Access).

> **Cita (formato PubMed):**  
> Paz-Graniel I, García-Gavilán JF, Ros E, Connelly MA, Babio N, Mantzoros CS, Salas-Salvadó J. Adherence to the Mediterranean diet and nuclear magnetic resonance spectroscopy biomarkers in older individuals at high cardiovascular disease risk: cross-sectional and longitudinal analyses. Am J Clin Nutr. 2024;119:108–116. doi:10.1016/j.ajcnut.2023.11.003.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- R ≥ 4.2
- Paquetes utilizados:
  - `haven`
  - `labelled`
  - `rio`
  - `RNOmni`
  - `missForest`
  - `Hmisc`
  - `car`
  - `dplyr`
  - `ggplot2`
  - `scales`
  - `readxl`
  - `foreign`
  - `MASS` 

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto se divide en las siguientes fases del análisis:

1. **Fase 1: Preparación de los datos**
   1. Importación de bases de datos PREDIMED y OGTT.
   2. Integración de información clínica, dietética y metabolómica.
   3. Evaluación e imputación de valores faltantes.
   4. Normalización de biomarcadores mediante RankNorm.

2. **Fase 2: Procesamiento de variables**
   1. Cálculo de deltas de biomarcadores y MEDAS a 1 año.
   2. Creación de terciles de MEDAS basal y cambios en MEDAS.
   3. Generación de variables categóricas y binomiales.
   4. Construcción de bases de datos analíticas finales.

3. **Fase 3: Análisis estadístico**
   1. Análisis descriptivo de características basales.
   2. Modelos ANCOVA ajustados para biomarcadores NMR.
   3. Comparaciones múltiples entre categorías de MEDAS.
   4. Análisis de correlación basal y longitudinal.
   5. Generación de tablas y figuras finales del artículo.

## Datos {#datos}

Por cuestiones de tratamiento y protección de datos, las bases de datos del estudio PREDIMED solo son accesibles bajo petición estricta al Steering Committee (SC) del estudio.

En este repositorio no se incluyen las bases de datos originales. La información de contacto con el SC está disponible en la publicación original a través de los autores de correspondencia.

La publicación científica, distribuida bajo licencia Open Access, se incluye en formato PDF.

## Licencia y citación {#licencia}

Licencia GNU GENERAL PUBLIC LICENSE

---

# Adherence to the Mediterranean diet and nuclear magnetic resonance spectroscopy biomarkers in older individuals at high cardiovascular disease risk

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.ajcnut.2023.11.003-blue)](https://doi.org/10.1016/j.ajcnut.2023.11.003)

## SHORT DESCRIPTION

This repository contains the code and supporting material used in the study “Adherence to the Mediterranean diet and nuclear magnetic resonance spectroscopy biomarkers in older individuals at high cardiovascular disease risk”, which aimed to evaluate cross-sectional and longitudinal associations between Mediterranean diet adherence (MEDAS) and advanced lipoprotein subclasses, glucose metabolism, and inflammation biomarkers assessed by NMR spectroscopy.

The study was conducted in older adults at high cardiometabolic risk from the PREDIMED-Reus center.

The following statistical methods were applied:

1. **Phase 1: Data preparation**
   - Integration of dietary (FFQ, MEDAS), clinical, and NMR metabolomics datasets.
   - Calculation of 1-year changes (deltas) in MEDAS and biomarkers.
   - Missing data assessment and imputation using *missForest*.
   - Rank-based inverse normal transformation (Blom) of biomarkers.

2. **Phase 2: Dietary exposure definition**
   - Categorization of baseline MEDAS (<8, 8–10, >10).
   - Categorization of 1-year MEDAS changes (<1, 1–3, >3).
   - Construction of categorical and binomial exposure models.

3. **Phase 3: Statistical analysis**
   - ANCOVA models for cross-sectional and longitudinal analyses.
   - Adjustment for demographic, lifestyle, clinical variables and intervention group.
   - Multiple comparisons using Tukey’s method.
   - Pearson and Spearman correlation analyses.
   - Generation of figures with mean values and 95% confidence intervals.

Includes:

- R scripts for data preprocessing and normalization.
- ANCOVA and correlation analyses.
- Code for tables and figures presented in the article.
- Open Access scientific publication in PDF format.

> **Citation (PubMed format):**  
> Paz-Graniel I, García-Gavilán JF, Ros E, Connelly MA, Babio N, Mantzoros CS, Salas-Salvadó J. Adherence to the Mediterranean diet and nuclear magnetic resonance spectroscopy biomarkers in older individuals at high cardiovascular disease risk: cross-sectional and longitudinal analyses. Am J Clin Nutr. 2024;119:108–116. doi:10.1016/j.ajcnut.2023.11.003.

## Contents

1. [Requirements](#requisitos)
2. [Project structure](#estructura-del-proyecto)
3. [Data](#datos)
4. [License and citation](#licencia)

## Requirements {#requisitos}

- R ≥ 4.2
- Packages used:
  - `haven`
  - `labelled`
  - `rio`
  - `RNOmni`
  - `missForest`
  - `Hmisc`
  - `car`
  - `dplyr`
  - `ggplot2`
  - `scales`
  - `readxl`
  - `foreign`
  - `MASS`

## Project structure {#estructura-del-proyecto}

The project is organized into the following analytical phases:

1. **Phase 1: Data preparation**
   1. Import of PREDIMED and OGTT datasets.
   2. Integration of dietary, clinical, and NMR metabolomics data.
   3. Missing data imputation and normalization.

2. **Phase 2: Variable processing**
   1. Calculation of 1-year changes in MEDAS and biomarkers.
   2. Categorization of MEDAS adherence and changes.
   3. Construction of analytical datasets.

3. **Phase 3: Statistical analysis**
   1. Descriptive analyses of baseline characteristics.
   2. Adjusted ANCOVA models.
   3. Multiple comparisons and correlation analyses.
   4. Generation of tables and figures.

## Data {#datos}

Due to ethical and data protection considerations, PREDIMED datasets are only available upon strict request to the study Steering Committee (SC).

This repository does not include the original datasets. Contact details are provided in the original publication through the corresponding authors.

The Open Access publication is included as a PDF file.

## License and citation {#licencia}

GNU GENERAL PUBLIC LICENSE
