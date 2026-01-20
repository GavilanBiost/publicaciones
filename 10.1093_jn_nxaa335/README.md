### Spanish

# U-shaped Association between Dietary Acid Load and Risk of Osteoporotic Fractures in Mediterranean Populations at High Cardiovascular Risk

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1093%2Fjn%2Fnxaa335-blue)](https://doi.org/10.1093/jn/nxaa335)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código y material suplementario utilizado en el estudio que evalúa la asociación entre la carga ácida de la dieta (estimada mediante PRAL (Potential Renal Acid Load) y NEAP (Net Endogenous Acid Production)) y el riesgo de fracturas osteoporóticas y la densidad mineral ósea (DMO) en dos poblaciones mediterráneas de alto riesgo cardiovascular.

### Métodos estadísticos principales

1. **Cálculo de la carga ácida de la dieta**
   - PRAL calculado según Remer & Manz a partir de proteína, fósforo, potasio, calcio y magnesio.
   - NEAP estimado según Frassetto et al. a partir de la ratio proteína/potasio.
   - Variables dietéticas derivadas de cuestionarios de frecuencia alimentaria (FFQ) validados y promediadas a lo largo del seguimiento.

2. **Clasificación en tertiles**
   - Los participantes se clasificaron en tertiles de PRAL y NEAP (bajo, medio, alto).
   - Para análisis no lineales se usaron tendencias cuadráticas y splines cúbicos restringidos.

3. **Análisis descriptivo**
   - Comparación de características basales por tertiles mediante ANOVA/ANCOVA y chi-cuadrado.

4. **Análisis longitudinal de fracturas**
   - Modelos de Cox con varianza robusta y clustering por hogar.
   - Ajustes progresivos por variables sociodemográficas, estilo de vida, comorbilidades, función renal (eGFR), medicación y factores dietéticos.
   - Evaluación de asunciones de riesgos proporcionales (Schoenfeld).

5. **Análisis transversal de DMO (PREDIMED-Plus)**
   - ANCOVA para DMO en fémur total, cuello femoral, trocánter, diáfisis femoral y columna lumbar.
   - Modelos ajustados por edad, sexo, IMC, actividad física, eGFR, suplementos y medicación.
   - Comparaciones múltiples con Tukey.

6. **Análisis de no linealidad**
   - Tendencia cuadrática (P q-trend).
   - Splines cúbicos restringidos para visualizar asociaciones en forma de U.

7. **Análisis de sensibilidad**
   - Exclusión de fracturas tempranas.
   - Exclusión de sujetos con fracturas previas u osteoporosis.
   - Ajustes adicionales (p. ej., calcio/vitamina D, cambios de peso).
   - Subanálisis por macronutrientes, proteína (total/animal/vegetal) y potasio.

Incluye:
- Scripts en R para limpieza de datos, cálculo de PRAL/NEAP, modelos de Cox y ANCOVA, análisis de sensibilidad y visualización.
- Material suplementario y resultados intermedios.

> García-Gavilán JF, Martínez A, Konieczna J, Mico-Perez R, García-Arellano A, Basora J, Barrubés L, Goday A, Canudas S, Salas-Salvadó J, Bulló M. U-Shaped Association between Dietary Acid Load and Risk of Osteoporotic Fractures in 2 Populations at High Cardiovascular Risk. J Nutr. 2021 Jan 4;151(1):152-161. doi: 10.1093/jn/nxaa335. PMID: 33296471.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- **R ≥ 3.3**
- Paquetes principales:
  - `haven`, `readxl`, `rio` – Importación/exportación de datos
  - `survival` – Modelos de Cox
  - `Hmisc`, `car`, `lmtest`, `effects` – Utilidades estadísticas
  - `ggplot2` – Visualización
  - `rms`, `splines` – Modelos no lineales
  - `agricolae` – Comparaciones múltiples
  - `gvlma` – Validación de supuestos

## Estructura del proyecto {#estructura-del-proyecto}

1. **Preparación de datos**
   - Limpieza y fusión de bases.
   - Cálculo de medias acumuladas a lo largo del seguimiento.

2. **Cálculo de PRAL y NEAP**
   - Implementación de fórmulas estándar.
   - Creación de tertiles y variables de tendencia.

3. **Análisis descriptivo**
   - Tablas basales por tertiles.

4. **Modelos de fracturas**
   - Cox crudos y ajustados.
   - Interacciones y análisis no lineales.

5. **Modelos de DMO**
   - ANCOVA por localización ósea.
   - Análisis de T-scores.

6. **Análisis de sensibilidad y subgrupos**
   - Exclusiones, ajustes adicionales y subanálisis nutricionales.

7. **Material suplementario**
   - Tablas y figuras adicionales.

## Datos {#datos}

Por razones de confidencialidad y protección de datos, las bases de datos no se incluyen en este repositorio. El acceso a los datos requiere autorización del Steering Committee correspondiente, disponible a través de los autores de correspondencia de la publicación. Los artículos en Open Access se incluyen en formato PDF.

## Licencia y citación {#licencia}

Licencia **GNU GENERAL PUBLIC LICENSE (GPL)**.  
Si utilizas este código, por favor cita el artículo original.

---

### English

# U-shaped Association between Dietary Acid Load and Risk of Osteoporotic Fractures in Mediterranean Populations at High Cardiovascular Risk

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1093%2Fjn%2Fnxaa335-blue)](https://doi.org/10.1093/jn/nxaa335)

## SHORT DESCRIPTION

This repository provides the code and supplementary material used to assess the association between dietary acid load—estimated by PRAL and NEAP—and osteoporotic fracture risk and bone mineral density (BMD) in two Mediterranean populations at high cardiovascular risk.

### Main statistical methods

1. **Dietary acid load calculation**
   - PRAL (Remer & Manz) and NEAP (Frassetto et al.) derived from validated FFQs and averaged over follow-up.

2. **Tertile classification**
   - Participants categorized into tertiles of PRAL and NEAP.
   - Quadratic trends and restricted cubic splines used to assess non-linearity.

3. **Descriptive analysis**
   - Baseline comparisons by tertiles using ANOVA/ANCOVA and chi-square tests.

4. **Longitudinal fracture analysis**
   - Cox proportional hazards models with robust variance and household clustering.
   - Progressive adjustment for demographics, lifestyle, comorbidities, eGFR, medications, and dietary factors.
   - Proportional hazards assumption checked.

5. **Cross-sectional BMD analysis**
   - ANCOVA for multiple skeletal sites with multivariable adjustment.
   - Multiple comparisons using Tukey tests.

6. **Non-linear analyses**
   - Quadratic trend tests and restricted cubic splines.

7. **Sensitivity analyses**
   - Exclusion of early fractures and prior osteoporosis.
   - Additional adjustments (e.g., calcium/vitamin D, weight change).
   - Sub-analyses by macronutrients and protein/potassium ratios.

Includes:
- R scripts for data processing, PRAL/NEAP computation, Cox and ANCOVA models, sensitivity analyses, and figures.
- Supplementary materials.

> García-Gavilán JF, Martínez A, Konieczna J, Mico-Perez R, García-Arellano A, Basora J, Barrubés L, Goday A, Canudas S, Salas-Salvadó J, Bulló M. U-Shaped Association between Dietary Acid Load and Risk of Osteoporotic Fractures in 2 Populations at High Cardiovascular Risk. J Nutr. 2021 Jan 4;151(1):152-161. doi: 10.1093/jn/nxaa335. PMID: 33296471.

## Contents

1. [Requirements](#requirements)
2. [Project structure](#project-structure)
3. [Data](#data)
4. [License and citation](#license)

## Requirements {#requirements}

- R ≥ 3.3
- Key packages: `haven`, `readxl`, `rio`, `survival`, `Hmisc`, `car`, `lmtest`, `effects`, `ggplot2`, `rms`, `splines`, `agricolae`, `gvlma`

## Project Structure {#project-structure}

1. Data preparation  
2. PRAL/NEAP calculation  
3. Descriptive analyses  
4. Fracture risk models (Cox)  
5. BMD models (ANCOVA)  
6. Sensitivity and subgroup analyses  
7. Supplementary material  

## Data {#data}

Due to data protection policies, datasets are not publicly available. Access requires approval from the respective Steering Committees. Open Access PDFs are included.

## License and Citation {#license}

GNU GENERAL PUBLIC LICENSE (GPL). Please cite the original article when using this code.
