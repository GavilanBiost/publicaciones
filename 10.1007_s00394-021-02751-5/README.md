### Spanish

# Asociación entre el índice inflamatorio de la dieta y la densidad mineral ósea

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1007%2Fs00394--021--02751--5-blue)](https://doi.org/10.1007/s00394-021-02751-5)

## DESCRIPCIÓN CORTA

En este estudio se emplearon los siguientes métodos estadísticos:

1. **Cálculo del Índice Inflamatorio de la Dieta (DII)**: Se calcularon puntuaciones z estandarizadas para 33 componentes dietéticos (nutrientes y compuestos bioactivos) a partir de cuestionarios de frecuencia alimentaria validados. Cada componente se estandarizó utilizando medias y desviaciones estándar de referencia globales. El DII final se obtuvo sumando los productos de cada puntuación z por su coeficiente inflamatorio específico.

2. **Estratificación en tertiles**: Los participantes se clasificaron en tres grupos (tertiles) según su puntuación DII: bajo (dieta antiinflamatoria), medio y alto (dieta proinflamatoria).

3. **Análisis descriptivo**: Se realizaron comparaciones de características basales entre tertiles mediante ANOVA para variables continuas y chi-cuadrado para variables categóricas.

4. **Modelos de regresión lineal**: Se utilizaron modelos lineales para analizar la asociación entre tertiles de DII (variable independiente) y densidad mineral ósea (DMO) en diferentes sitios anatómicos (fémur total, trocánter y columna lumbar L1-L4). Los modelos se ajustaron progresivamente por:
   - Modelo crudo: solo DII
   - Modelo ajustado: edad, sexo, índice de masa corporal, tabaquismo, nivel educativo, actividad física, ingesta energética total, nodo de reclutamiento, diabetes, tratamientos farmacológicos (insulina, metformina, otros antidiabéticos, terapia hormonal) y suplementación con calcio/vitamina D

5. **Modelos de regresión logística**: Se emplearon modelos logísticos para evaluar la asociación entre tertiles de DII y el riesgo de baja DMO (osteopenia o peor, definida como T-score ≤ -1), con los mismos ajustes que los modelos lineales.

6. **Análisis de subgrupos**: Se realizaron análisis estratificados por edad (>70 vs <70 años), sexo, índice de masa corporal (>30 vs <30 kg/m²) y presencia de diabetes, presentando los resultados mediante forest plots.

7. **Verificación de supuestos**: Se utilizó el paquete `gvlma` para validar los supuestos de los modelos lineales (normalidad, homocedasticidad, independencia).

Incluye:

- Scripts en **R** para pre-procesar datos, calcular el DII, ajustar modelos y generar las figuras del artículo.
- Imágenes finales (forest plots).

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- **R ≥ 3.6**
- Paquetes utilizados:
  - **`haven`:** Leer y escribir datos de SPSS, Stata y SAS conservando las etiquetas de las variables para un análisis reproducible
  - **`readxl`:** Leer hojas de cálculo de Excel (.xls y .xlsx) directamente sin necesidad de Java, con detección automática de tipos de columna
  - **`Hmisc`:** Resúmenes descriptivos, imputación, utilidades y gráficos de alto nivel para análisis estadísticos
  - **`agricolae`:** Diseño y análisis de experimentos agrícolas, incluyendo pruebas de comparación múltiple como el test de Tukey (HSD)
  - **`ggplot2`:** Implementa la gramática de gráficos (Grammar of Graphics) para crear visualizaciones elegantes, personalizables y componibles
  - **`forestplot`:** Crear gráficos de bosque (forest plots) de alta calidad para presentar resultados de meta-análisis y análisis de subgrupos con intervalos de confianza
  - **`rio`:** Importar y exportar datos en decenas de formatos (CSV, SPSS, Stata, SAS, Excel, JSON, etc.) con las funciones `import()` y `export()` de forma unificada y sencilla
  - **`gvlma`:** Validación global de supuestos de modelos lineales mediante pruebas de Peña y Slate (normalidad, homocedasticidad, independencia y especificación del modelo)

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto se divide en las siguientes fases:

1. **Fase 1: Cálculo de puntuaciones z**
   - Estandarización de 33 componentes dietéticos usando medias y desviaciones estándar de referencia
   - Cálculo de z-scores para cada nutriente y compuesto bioactivo

2. **Fase 2: Creación del índice DII**
   - Multiplicación de cada z-score por su coeficiente inflamatorio específico
   - Suma de todos los productos ponderados para obtener la puntuación DII final
   - Clasificación de participantes en tertiles de DII

3. **Fase 3: Análisis descriptivo (Tabla 1)**
   - Características basales de los participantes por tertiles de DII
   - Pruebas de comparación entre grupos (ANOVA, chi-cuadrado)

4. **Fase 4: Modelos de asociación con DMO (Tablas 2, 3 y 4)**
   - Regresión lineal para DMO en fémur total, trocánter y columna lumbar
   - Modelos crudos y ajustados por covariables

5. **Fase 5: Modelos de riesgo de baja DMO**
   - Regresión logística para osteopenia/osteoporosis (T-score ≤ -1)
   - Cálculo de odds ratios e intervalos de confianza

6. **Fase 6: Análisis de subgrupos y visualización (Figuras 1-3)**
   - Análisis estratificados por edad, sexo, IMC y diabetes
   - Generación de forest plots para cada sitio anatómico

7. **Fase 7: Material suplementario**
   - Distribución de componentes dietéticos por tertiles de DII
   - Visualización de coeficientes inflamatorios

## Datos {#datos}

Por cuestiones de tratamiento y protección de datos, las bases de datos de dichas publicaciones solo son accesibles bajo petición estricta al Steering Committee (SC) de cada estudio respectivo. En estos archivos no se muestran las bases de datos de ninguno de los estudios. La forma de contacto con el SC de cada estudio está disponible dentro de cada respectiva publicación a través de los autores de correspondencia. Así mismo, las publicaciones publicadas bajo dominio de Open Access están incluidas en su carpeta en formato PDF.

## Licencia y citación {#licencia}

Licencia **GNU GENERAL PUBLIC LICENSE**

---

### English

# Association between dietary inflammatory index and bone mineral density

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1007%2Fs00394--021--02751--5-blue)](https://doi.org/10.1007/s00394-021-02751-5)

## SHORT DESCRIPTION

The following statistical methods were used in this study:

1. **Dietary Inflammatory Index (DII) Calculation**: Standardized z-scores were calculated for 33 dietary components (nutrients and bioactive compounds) derived from validated food frequency questionnaires. Each component was standardized using global reference means and standard deviations. The final DII score was obtained by summing the products of each z-score by its specific inflammatory coefficient.

2. **Tertile Stratification**: Participants were classified into three groups (tertiles) according to their DII score: low (anti-inflammatory diet), medium, and high (pro-inflammatory diet).

3. **Descriptive Analysis**: Baseline characteristics were compared between tertiles using ANOVA for continuous variables and chi-square tests for categorical variables.

4. **Linear Regression Models**: Linear models were used to analyze the association between DII tertiles (independent variable) and bone mineral density (BMD) at different anatomical sites (total femur, trochanter, and lumbar spine L1-L4). Models were progressively adjusted for:
   - Crude model: DII only
   - Adjusted model: age, sex, body mass index, smoking status, educational level, physical activity, total energy intake, recruitment node, diabetes, pharmacological treatments (insulin, metformin, other antidiabetics, hormone therapy), and calcium/vitamin D supplementation

5. **Logistic Regression Models**: Logistic models were employed to assess the association between DII tertiles and the risk of low BMD (osteopenia or worse, defined as T-score ≤ -1), with the same adjustments as the linear models.

6. **Subgroup Analysis**: Stratified analyses were performed by age (>70 vs <70 years), sex, body mass index (>30 vs <30 kg/m²), and presence of diabetes, presenting results through forest plots.

7. **Assumption Verification**: The `gvlma` package was used to validate linear model assumptions (normality, homoscedasticity, independence).

Includes:

- **R** scripts to preprocess data, calculate DII, fit models, and generate the article's figures.
- Final images (forest plots).

## Contents

1. [Requirements](#requirements)
2. [Project structure](#project-structure)
3. [Data](#data)
4. [License and citation](#license)

## Requirements {#requirements}

- **R ≥ 3.6**
- Required packages:
  - **`haven`:** Read and write data from SPSS, Stata, and SAS while preserving variable labels for reproducible analysis
  - **`readxl`:** Read Excel spreadsheets (.xls and .xlsx) directly without Java, with automatic column type detection
  - **`Hmisc`:** Descriptive summaries, imputation, utilities, and high-level graphics for statistical analysis
  - **`agricolae`:** Design and analysis of agricultural experiments, including multiple comparison tests such as Tukey's HSD test
  - **`ggplot2`:** Implements the Grammar of Graphics to create elegant, customizable, and composable visualizations
  - **`forestplot`:** Create high-quality forest plots to present meta-analysis results and subgroup analyses with confidence intervals
  - **`rio`:** Import and export data in dozens of formats (CSV, SPSS, Stata, SAS, Excel, JSON, etc.) with the `import()` and `export()` functions in a unified and simple way
  - **`gvlma`:** Global validation of linear model assumptions through Peña and Slate tests (normality, homoscedasticity, independence, and model specification)

## Project Structure {#project-structure}

The project is divided into the following phases:

1. **Phase 1: Z-score Calculation**
   - Standardization of 33 dietary components using reference means and standard deviations
   - Calculation of z-scores for each nutrient and bioactive compound

2. **Phase 2: DII Index Creation**
   - Multiplication of each z-score by its specific inflammatory coefficient
   - Sum of all weighted products to obtain the final DII score
   - Classification of participants into DII tertiles

3. **Phase 3: Descriptive Analysis (Table 1)**
   - Baseline characteristics of participants by DII tertiles
   - Between-group comparison tests (ANOVA, chi-square)

4. **Phase 4: BMD Association Models (Tables 2, 3, and 4)**
   - Linear regression for BMD at total femur, trochanter, and lumbar spine
   - Crude and covariate-adjusted models

5. **Phase 5: Low BMD Risk Models**
   - Logistic regression for osteopenia/osteoporosis (T-score ≤ -1)
   - Calculation of odds ratios and confidence intervals

6. **Phase 6: Subgroup Analysis and Visualization (Figures 1-3)**
   - Stratified analyses by age, sex, BMI, and diabetes
   - Generation of forest plots for each anatomical site

7. **Phase 7: Supplementary Material**
   - Distribution of dietary components by DII tertiles
   - Visualization of inflammatory coefficients

## Data {#data}

For data processing and protection reasons, the databases of these publications are accessible only upon strict request to the Steering Committee (SC) of each respective study. The databases from any of the studies are not provided in these files. The contact form for each study's SC can be found within each publication through the corresponding authors. Publications released under Open Access are also included in their respective PDF folders.

## License and Citation {#license}

**GNU GENERAL PUBLIC LICENSE**
