### Spanish

# Nut consumption is associated with a shift of the NMR lipoprotein subfraction profile to a less atherogenic pattern among older individuals at high CVD risk

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1186%2Fs12933--022--01624--3-blue)](https://cardiab.biomedcentral.com/articles/10.1186/s12933-022-01624-3)

## DESCRIPCIÓN CORTA

En este estudio se emplearon los siguientes métodos estadísticos:

1.  **Transformación y normalización de datos**: Todas las variables de perfil lipoproteico y metabolitos fueron normalizadas usando la transformación inversa normal basada en el rango de Blom, y escaladas en unidades de desviación estándar para mejorar la normalidad de la distribución de los residuos
2.  **Definición de grupos y ajuste energético**: El consumo de frutos secos se cuantificó en gramos diarios (28 g = 1 porción) a partir de un cuestionario de frecuencia alimentaria validado. Para comparar de manera equitativa entre participantes, las ingestas se ajustaron por aporte energético total (método de residuos) y se dividieron en terciles de consumo bajo, medio y alto.
3.  **Modelos de comparación (ANCOVA)**:
    -   *Análisis transversal (baseline)*: se usaron modelos de AN COVA para comparar medias de parámetros lipoproteicos y metabolitos entre terciles al inicio, ajustando por: Edad, sexo, índice de masa corporal (kg/m²), Tabaquismo (fumador actual/antes/nunca), Actividad física (MET-min/día), Diabetes, dislipemia, hipertensión (sí/no), Tratamiento con estatinas
    -   *Análisis longitudinal (cambios a 1 año)*: mismos ajustes anteriores + valor basal de la variable estudiada, consumo basal de frutos secos e “brazo” de intervención (MedDiet + EVOO, MedDiet + nueces, dieta baja en grasa)
4.  **Contrastes múltiples y criterios de significación**: Para comparaciones post hoc entre tercil 1 y tercil 3 se aplicó la prueba de Tukey. Se consideraron estadísticamente significativos los valores de P \< 0.05 en todos los análisis.
5.  **Verificación de supuestos de los modelos**:
    -   Independencia de observaciones: diseño de subcohorte longitudinal.
    -   Homogeneidad de varianzas: comprobada con test de Levene (P \> 0.05).
    -   Normalidad de residuos: comprobada con test de Shapiro–Wilk (P \> 0.05).
    -   Todas las inspecciones gráficas y pruebas cuantitativas confirmaron el cumplimiento de estos supuestos.

Incluye:

-   Scripts en **R** para pre-procesar datos, ajustar modelos y generar las figuras del artículo.
-   Imágenes y tablas finales.

> García-Gavilán JF, Connelly MA, Babio N, Mantzoros CS, Ros E, Salas-Salvadó J. Nut consumption is associated with a shift of the NMR lipoprotein subfraction profile to a less atherogenic pattern among older individuals at high CVD risk. Cardiovasc Diabetol. 2022 Sep 20;21(1):189. doi: 10.1186/s12933-022-01624-3. Erratum in: Cardiovasc Diabetol. 2022 Oct 26;21(1):219. doi: 10.1186/s12933-022-01659-6. PMID: 36127725; PMCID: PMC9487141.

## Contenidos

1.  [Requisitos](#requisitos)
2.  [Estructura del proyecto](#estructura-del-proyecto)
3.  [Datos](#datos)
4.  [Licencia y citación](#licencia)

## Requisitos {#requisitos}

\- **R ≥ 3.6**\
- Paquetes utilizados:

    - **`haven`:** Leer y escribir datos de SPSS, Stata y SAS conservando las etiquetas de las variables para un análisis reproducible\
    - **`rio`:** Importar y exportar datos en decenas de formatos (CSV, SPSS, Stata, SAS, Excel, JSON, etc.) con las funciones `import()` y `export()` de forma unificada y sencilla\
    - **`RNOmni`:** Realizar pruebas de asociación genómica usando normalización por rangos inversa normal, ideal para datos de alto contenido genético\
    - **`missForest`:** Imputación no paramétrica de valores faltantes mediante bosques aleatorios, conservando relaciones no lineales entre variables\
    - **`readxl`:** Leer hojas de cálculo de Excel (.xls y .xlsx) directamente sin necesidad de Java, con detección automática de tipos de columna\
    - **`ggplot2`:** Implementa la gramática de gráficos (Grammar of Graphics) para crear visualizaciones elegantes, personalizables y componibles\
    - **`scales`:** Proporciona funciones para mapear, formatear y transformar datos en ejes y leyendas de gráficos (`continuous`, `discrete`, `date`, `percent`, etc.), facilitando el control de etiquetas y paletas de colores.

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto se divide en las siguientes fases:

1.  Fase 1: preparación de los datos
    1.  Preparación de los datos de metabolómica
    2.  Preparación de los metadatos
    3.  Creación de tablas descriptivas
2.  Fase 2: ANOVAS/ANCOVAS
3.  Fase 3: Figuras

## Datos {#datos}

Por cuestiones de tratamiento y protección de datos, las bases de datos de dichas publicaciones solo son accesibles bajo peticion estricta al Steering Committee (SC) de cada estudio respectivo. En estos archivos no se muestran las bases de datos de ninguno de los estudios. La forma de contacto con el SC de cada estudio está disponible dentro de cada respectiva publicacion a traves de los autores de correspondencia. Así mismo, las publicaciones publicadas bajo dominio de Open Access están incluidas en su carpeta en formato PDF.

## Licencia y citación

Licencia **GNU GENERAL PUBLIC LICENSE**

------------------------------------------------------------------------

### English

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1186%2Fs12933--022--01624--3-blue)](https://cardiab.biomedcentral.com/articles/10.1186/s12933-022-01624-3)

## SHORT DESCRIPTION

The following statistical methods were used in this study:

1.  **Data transformation and normalization**: All lipoprotein profiles and metabolite variables were normalized using the inverse normal transformation based on Blom's rank and scaled in standard deviation units to improve the normality of the residual distribution.
2.  **Group definition and energy adjustment**: Nut consumption was quantified in grams per day (28 g = 1 serving) using a validated food frequency questionnaire. To fairly compare participants, intakes were adjusted for total energy intake (residue method) and divided into low, medium, and high consumption tertiles.
3.  **Comparative Models (ANCOVA)**:
    1.  *Cross-sectional analysis (baseline)*: ANCOVA models were used to compare means of lipoprotein and metabolite parameters between tertiles at baseline, adjusting for: Age, sex, body mass index (kg/m²), Smoking status (current/former/never smoker), Physical activity (MET-min/day), Diabetes, dyslipidemia, hypertension (yes/no), Statin treatment
    2.  *Longitudinal analysis (changes at 1 year)*: same adjustments as above + baseline value of the study variable, baseline nut consumption, and intervention arm (MedDiet + EVOO, MedDiet + walnuts, low-fat diet)
4.  **Multiple contrasts and significance criteria**: For post hoc comparisons between tertiles 1 and 3, the Tukey test was applied. P values \< 0.05 were considered statistically significant in all analyses.
5.  **Verification of model assumptions**:
    -   Independence of observations: longitudinal subcohort design.
    -   Homogeneity of variances: verified with Levene's test (P \> 0.05).
    -   Normality of residuals: verified with Shapiro–Wilk test (P \> 0.05).
    -   All graphical inspections and quantitative tests confirmed compliance with these assumptions.

Includes:

\- **R** scripts to preprocess data, fit models, and generate the article's figures.

\- Final images and tables.

> García-Gavilán JF, Connelly MA, Babio N, Mantzoros CS, Ros E, Salas-Salvadó J. Nut consumption is associated with a shift of the NMR lipoprotein subfraction profile to a less atherogenic pattern among older individuals at high CVD risk. Cardiovasc Diabetol. 2022 Sep 20;21(1):189. doi: 10.1186/s12933-022-01624-3. Erratum in: Cardiovasc Diabetol. 2022 Oct 26;21(1):219. doi: 10.1186/s12933-022-01659-6. PMID: 36127725; PMCID: PMC9487141.

## Contents

1.  [Requirements](#requirements)
2.  [Project structure](#project-structure)
3.  [Data](#data)
4.  [License and citation](#license)

## Requirements: {#requirements}

\- **R ≥ 3.6**\
- Packages detailed in the libraries section of the `syntax` script:

    - **`haven`:** Read and write data from SPSS, Stata, and SAS while preserving variable labels for reproducible analysis\
    - **`rio`:** Import and export data in dozens of formats (CSV, SPSS, Stata, SAS, Excel, JSON, etc.) with the `import()` and `export()` functions in a unified and simple way\
    - **`RNOmni`:** Perform genome-wide association tests using inverse normal rank normalization, ideal for data with high genetic content\
    - **`missForest`:** Nonparametric imputation of missing values using random forests, preserving nonlinear relationships between variables\
    - **`readxl`:** Read Excel spreadsheets (.xls and .xlsx) directly without Java, with automatic column type detection\
    - **`ggplot2`:** Implements the Grammar of Graphics to create elegant, customizable, and interactive visualizations\
    - **`scales`:** Provides functions for mapping, formatting, and transforming data on chart axes and legends (`continuous`, `discrete`, `date`, `percent`, etc), making it easy to control labels and color palettes.

## Project Structure {#project-structure}

The project is divided into the following phases:

1.  Phase 1: Data Preparation
    1.  Metabolomics Data Preparation
    2.  Metadata Preparation
    3.  Descriptive Table Creation
2.  Phase 2: ANOVAs/ANCOVAS
3.  Phase 3: Figures

## Data {#data}

For data processing and protection reasons, the databases of these publications are accessible only upon strict request to the Steering Committee (SC) of each respective study. The databases from any of the studies are not provided in these files. The contact form for each study's SC can be found within each publication through the corresponding authors. Publications released under Open Access are also included in their respective PDF folders.

## License and Citation

**GNU GENERAL PUBLIC LICENSE**
