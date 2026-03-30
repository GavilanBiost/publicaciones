# Metabolomics profiles of type 2 diabetes and insulin resistance and their associations with total mortality

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.metabol.2026.156599-blue)](https://doi.org/10.1016/j.metabol.2026.156599)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código y material de apoyo utilizados en el estudio **“Metabolomics profiles of type 2 diabetes and insulin resistance and their associations with total mortality”** (Metabolism, volumen 179, 2026, artículo 156599).

El objetivo del trabajo fue identificar perfiles metabolómicos asociados con diabetes tipo 2 (T2D) y resistencia a la insulina (HOMA-IR), y evaluar su relación con el riesgo de mortalidad total.

Métodos principales del estudio:

1. **Fase 1: Preparación y preprocesamiento de datos**
	- Curación de datos clínicos y metabolómicos.
	- Control de calidad, filtrado de metabolitos y gestión de valores faltantes.
	- Normalización y estandarización para análisis multivariante.

2. **Fase 2: Derivación de perfiles metabolómicos**
	- Construcción de puntuaciones multimetabolito para T2D y HOMA-IR.
	- Modelado con regresión penalizada (*elastic net*) y validación interna.

3. **Fase 3: Asociación con mortalidad y validación**
	- Modelos de riesgos proporcionales de Cox para mortalidad total.
	- Evaluación de metabolitos compartidos entre firmas de T2D y HOMA-IR.
	- Replicación/validación en PREDIMED y cohortes externas (NHS/HPFS).

Hallazgos destacados:

- Una firma de 31 metabolitos para T2D y otra de 105 metabolitos para HOMA-IR se asociaron con mayor riesgo de mortalidad.
- Nueve metabolitos compartidos entre ambas firmas mostraron asociación con mortalidad al combinarse en una puntuación compuesta.
- Metabolitos como glycine, SDMA, DMGV y phosphocreatine podrían mejorar la estratificación temprana del riesgo.

Incluye:

- Scripts en R/Quarto para preprocesado, control de calidad y análisis estadísticos.
- Código para modelos de *elastic net*, modelos de Cox y análisis de longevidad.
- Análisis complementarios (metabolitos comunes, MSEA y WGCNA) y scripts de figuras.
- Material suplementario del artículo.

> **Cita (formato PubMed):**  
> García-Gavilán JF, Paz-Graniel I, Pérez-Acosta JA, Ruiz-Canela M, Li J, Clish C, et al. Metabolomics profiles of type 2 diabetes and insulin resistance and their associations with total mortality. Metabolism. 2026;179:156599. doi:10.1016/j.metabol.2026.156599.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- R (versión recomendada: 4.2 o superior)
- Paquetes de uso habitual en este repositorio (según scripts):
  - `tidyverse`
  - `data.table`
  - `readxl`
  - `haven`
  - `rio`
  - `glmnet`
  - `survival`
  - `survminer`
  - `caret`
  - `pROC`
  - `WGCNA`

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto está organizado en scripts Quarto que cubren el flujo analítico completo:

1. **Preprocesado y calidad de datos**
	1. `1. Preprocesado.qmd`
	2. `2. QC.qmd`

2. **Desarrollo de modelos y validación**
	1. `3. ENR.qmd`
	2. `4. Validation.qmd`
	3. `5. Cox models.qmd`
	4. `6. Longevidad.qmd`

3. **Resultados, figuras y análisis complementarios**
	1. `7. Figuras.qmd`
	2. `8. Analisis con los metabolitos comunes.qmd`
	3. `9. Análisis MSEA y WGNA.qmd`
	4. `10. Características Basales.qmd`
	5. `11. Supplemental.qmd`
	6. `12. Analisis propuestos por coautores.qmd`

## Datos {#datos}

Por cuestiones éticas y de protección de datos, las bases originales del estudio no se distribuyen en este repositorio.

El acceso a datos del ensayo PREDIMED y cohortes relacionadas debe solicitarse siguiendo los procedimientos de los comités correspondientes y de los autores de correspondencia del artículo.

## Licencia y citación {#licencia}

Licencia GNU GENERAL PUBLIC LICENSE

---

# Metabolomics profiles of type 2 diabetes and insulin resistance and their associations with total mortality

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.metabol.2026.156599-blue)](https://doi.org/10.1016/j.metabol.2026.156599)

## SHORT DESCRIPTION

This repository contains code and supporting material for the article **“Metabolomics profiles of type 2 diabetes and insulin resistance and their associations with total mortality”** (Metabolism, volume 179, 2026, article 156599).

The study aimed to identify metabolomic profiles related to type 2 diabetes (T2D) and insulin resistance (HOMA-IR), and to evaluate their association with all-cause mortality risk.

Main analytical methods:

1. **Phase 1: Data preparation and preprocessing**
	- Clinical and metabolomics data curation.
	- Quality control, metabolite filtering, and missing-data handling.
	- Data normalization and standardization for multivariable analyses.

2. **Phase 2: Derivation of metabolomic profiles**
	- Multi-metabolite score construction for T2D and HOMA-IR.
	- Penalized regression (*elastic net*) with internal validation.

3. **Phase 3: Mortality associations and validation**
	- Cox proportional hazards models for all-cause mortality.
	- Evaluation of metabolites shared across T2D and HOMA-IR signatures.
	- Replication/validation in PREDIMED and external U.S. cohorts (NHS/HPFS).

Key findings:

- A 31-metabolite T2D score and a 105-metabolite HOMA-IR score were associated with higher mortality risk.
- Nine shared metabolites across both signatures were associated with mortality when combined into a composite score.
- Glycine, SDMA, DMGV, and phosphocreatine may improve early risk stratification.

Includes:

- R/Quarto scripts for preprocessing, quality control, and statistical analyses.
- Code for *elastic net* models, Cox models, and longevity-related analyses.
- Complementary analyses (common metabolites, MSEA, WGCNA) and figure generation scripts.
- Supplemental article material.

> **Citation (PubMed format):**  
> García-Gavilán JF, Paz-Graniel I, Pérez-Acosta JA, Ruiz-Canela M, Li J, Clish C, et al. Metabolomics profiles of type 2 diabetes and insulin resistance and their associations with total mortality. Metabolism. 2026;179:156599. doi:10.1016/j.metabol.2026.156599.

## Contents

1. [Requirements](#requisitos)
2. [Project structure](#estructura-del-proyecto)
3. [Data](#datos)
4. [License and citation](#licencia)

## Requirements {#requisitos}

- R (recommended version: 4.2 or newer)
- Commonly used packages in this repository (based on scripts):
  - `tidyverse`
  - `data.table`
  - `readxl`
  - `haven`
  - `rio`
  - `glmnet`
  - `survival`
  - `survminer`
  - `caret`
  - `pROC`
  - `WGCNA`

## Project structure {#estructura-del-proyecto}

The project is organized into Quarto scripts covering the full analytical workflow:

1. **Data preprocessing and quality control**
	1. `1. Preprocesado.qmd`
	2. `2. QC.qmd`

2. **Model development and validation**
	1. `3. ENR.qmd`
	2. `4. Validation.qmd`
	3. `5. Cox models.qmd`
	4. `6. Longevidad.qmd`

3. **Results, figures, and complementary analyses**
	1. `7. Figuras.qmd`
	2. `8. Analisis con los metabolitos comunes.qmd`
	3. `9. Análisis MSEA y WGNA.qmd`
	4. `10. Características Basales.qmd`
	5. `11. Supplemental.qmd`
	6. `12. Analisis propuestos por coautores.qmd`

## Data {#datos}

Due to ethical and data-protection constraints, the original study datasets are not distributed in this repository.

Access to data from PREDIMED and related cohorts should be requested through the corresponding study committees and corresponding authors of the publication.

## License and citation {#licencia}

GNU GENERAL PUBLIC LICENSE