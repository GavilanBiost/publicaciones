# Vitamin K dietary intake is associated with cognitive function in an older adult Mediterranean population

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1093%2Fageing%2Fafab246-blue)](https://doi.org/10.1093/ageing/afab246)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código y material de apoyo utilizados en el estudio “Vitamin K dietary intake is associated with cognitive function in an older adult Mediterranean population”, cuyo objetivo fue evaluar la asociación entre los cambios a 2 años en la ingesta dietética de vitamina K y la función cognitiva en adultos mayores con sobrepeso u obesidad y síndrome metabólico del estudio PREDIMED-Plus.

En este estudio se emplearon los siguientes métodos estadísticos:

1. **Fase 1: Preparación y preprocesamiento de datos**
   - Integración de bases de datos dietéticas (FFQ), clínicas y neuropsicológicas.
   - Cálculo del cambio a 2 años (delta) en la ingesta de vitamina K.
   - Clasificación de los participantes en terciles de cambio de vitamina K.
   - Recodificación de variables clínicas, sociodemográficas y de estilo de vida.

2. **Fase 2: Definición de eventos cognitivos**
   - Evaluación de la función cognitiva mediante una batería de test neuropsicológicos.
   - Definición de deterioro cognitivo según puntuación MMSE ≤ 24.
   - Creación de variables dicotómicas de empeoramiento cognitivo para otros test (fluencia verbal, TMT, reloj).

3. **Fase 3: Modelado estadístico**
   - Modelos de regresión logística binomial.
   - Modelos crudos y modelos totalmente ajustados.
   - Ajuste por variables sociodemográficas, clínicas, dietéticas, estilo de vida y grupo de intervención.
   - Estratificación por centro e inclusión de efectos de clúster.
   - Análisis de tendencia lineal utilizando la mediana de cada tercil.
   - Análisis estratificado por presencia de diabetes.

Incluye:

- Scripts en R para la preparación de datos, recodificación de variables y análisis estadístico.
- Modelos de regresión logística para deterioro cognitivo y distintos test neuropsicológicos.
- Código para análisis por terciles y pruebas de tendencia.
- Figura final de *odds ratios* (Figura 1 del artículo).
- Publicación científica en formato PDF (Open Access).

> **Cita (formato PubMed):**  
> Camacho-Barcia L, García-Gavilán J, Martínez-González MÁ, Fernández-Aranda F, Galié S, Corella D, et al. Vitamin K dietary intake is associated with cognitive function in an older adult Mediterranean population. Age and Ageing. 2022;51:afab246. doi:10.1093/ageing/afab246.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- R ≥ 3.6
- Paquetes utilizados:
  - `gdata`
  - `Hmisc`
  - `car`
  - `effects`
  - `normtest`
  - `nortest`
  - `moments`
  - `haven`
  - `rms`
  - `splines`
  - `ggplot2`
  - `survival`
  - `xlsx`
  - `readxl`
  - `faraway`
  - `lmtest`
  - `psych`
  - `DescTools`
  - `foreign`
  - `MASS`
  - `dplyr` 

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto se divide en las siguientes fases del análisis:

1. **Fase 1: Preparación de los datos**
   1. Importación de bases de datos PREDIMED-Plus con información dietética y cognitiva.
   2. Integración de información de clúster y centro.
   3. Selección de participantes con FFQ y test cognitivos completos a 2 años.
   4. Creación de la base de datos analítica final.

2. **Fase 2: Procesamiento de variables**
   1. Cálculo del delta de ingesta de vitamina K a 2 años.
   2. Categorización en terciles de cambio de vitamina K.
   3. Definición de eventos de deterioro cognitivo (MMSE ≤ 24).
   4. Recodificación de covariables clínicas, dietéticas y de estilo de vida.

3. **Fase 3: Análisis estadístico**
   1. Modelos de regresión logística crudos.
   2. Modelos totalmente ajustados con efectos de clúster y estratificación por centro.
   3. Análisis de tendencia lineal.
   4. Análisis estratificado por diabetes.
   5. Generación de la Figura 1 (odds ratios e intervalos de confianza al 95%).

## Datos {#datos}

Por cuestiones de tratamiento y protección de datos, las bases de datos del estudio PREDIMED-Plus solo son accesibles bajo petición estricta al Steering Committee (SC) del estudio.

En este repositorio no se incluyen las bases de datos originales. La información de contacto con el SC está disponible en la publicación original a través de los autores de correspondencia.

La publicación científica, distribuida bajo licencia Open Access, se incluye en formato PDF.

## Licencia y citación {#licencia}

Licencia GNU GENERAL PUBLIC LICENSE

---

# Vitamin K dietary intake is associated with cognitive function in an older adult Mediterranean population

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1093%2Fageing%2Fafab246-blue)](https://doi.org/10.1093/ageing/afab246)

## SHORT DESCRIPTION

This repository contains the code and supporting material used in the study “Vitamin K dietary intake is associated with cognitive function in an older adult Mediterranean population”, which aimed to assess the association between 2-year changes in dietary vitamin K intake and cognitive function in older adults with overweight/obesity and metabolic syndrome from the PREDIMED-Plus study.

The following statistical methods were applied:

1. **Phase 1: Data preparation**
   - Integration of dietary (FFQ), clinical, and neuropsychological datasets.
   - Calculation of 2-year changes (delta) in dietary vitamin K intake.
   - Classification into tertiles of vitamin K intake change.
   - Recoding of clinical, lifestyle, and sociodemographic variables.

2. **Phase 2: Cognitive outcome definition**
   - Cognitive assessment using a battery of neuropsychological tests.
   - Definition of cognitive impairment based on MMSE score ≤ 24.
   - Creation of dichotomous outcomes for cognitive decline in other tests.

3. **Phase 3: Statistical analysis**
   - Binomial logistic regression models.
   - Crude and fully adjusted models.
   - Adjustment for sociodemographic, clinical, dietary, lifestyle variables, and intervention group.
   - Clustered standard errors and centre stratification.
   - Linear trend analyses using tertile medians.
   - Stratified analyses by diabetes status.

Includes:

- R scripts for data preprocessing and statistical analysis.
- Logistic regression models for cognitive impairment and neuropsychological tests.
- Tertile-based and trend analyses.
- Final odds ratio plot (Figure 1 of the article).
- Open Access scientific publication in PDF format.

> **Citation (PubMed format):**  
> Camacho-Barcia L, García-Gavilán J, Martínez-González MÁ, Fernández-Aranda F, Galié S, Corella D, et al. Vitamin K dietary intake is associated with cognitive function in an older adult Mediterranean population. Age and Ageing. 2022;51:afab246. doi:10.1093/ageing/afab246.

## Contents

1. [Requirements](#requisitos)
2. [Project structure](#estructura-del-proyecto)
3. [Data](#datos)
4. [License and citation](#licencia)

## Requirements {#requisitos}

- R ≥ 3.6
- Packages used:
  - `gdata`
  - `Hmisc`
  - `car`
  - `effects`
  - `normtest`
  - `nortest`
  - `moments`
  - `haven`
  - `rms`
  - `splines`
  - `ggplot2`
  - `survival`
  - `xlsx`
  - `readxl`
  - `faraway`
  - `lmtest`
  - `psych`
  - `DescTools`
  - `foreign`
  - `MASS`
  - `dplyr`

## Project structure {#estructura-del-proyecto}

The project is organized into the following analytical phases:

1. **Phase 1: Data preparation**
   1. Import of PREDIMED-Plus dietary and cognitive datasets.
   2. Integration of cluster and centre information.
   3. Selection of participants with complete FFQ and cognitive data.
   4. Creation of the final analytical dataset.

2. **Phase 2: Variable processing**
   1. Calculation of 2-year vitamin K intake changes.
   2. Categorisation into tertiles of vitamin K change.
   3. Definition of cognitive impairment events.
   4. Recoding of covariates.

3. **Phase 3: Statistical analysis**
   1. Crude logistic regression models.
   2. Fully adjusted models with clustering and stratification.
   3. Linear trend analyses.
   4. Stratified analyses by diabetes status.
   5. Generation of odds ratio plots.

## Data {#datos}

Due to ethical and data protection considerations, the PREDIMED-Plus datasets are only available upon strict request to the study Steering Committee (SC).

This repository does not include the original datasets. Contact details for data access are provided in the original publication through the corresponding authors.

The Open Access publication is included as a PDF file.

## License and citation {#licencia}

GNU GENERAL PUBLIC LICENSE
