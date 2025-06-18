### Spanish

# Olive oil consumption, plasma metabolites, and risk of type 2 diabetes and cardiovascular disease

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1186%2Fs12933--023--02066--1-blue)](https://doi.org/10.1186/s12933-023-02066-1)

## DESCRIPCIÓN CORTA

En este estudio se emplearon modelos de aprendizaje automático para identificar conjuntos de metabolitos asociados al consumo de aceite de oliva y sus variedades, generando una puntuación capaz de predecir dicho consumo. Los valores faltantes se imputaron con *missForest* y las concentraciones metabolómicas se normalizaron mediante la transformación de Blom. Para seleccionar los metabolitos se aplicó un modelo Elastic Net con validación cruzada estratificada 10 × 10-fold. Los conjuntos identificados se validaron con los datos del primer año y se resumieron mediante PCA. Por último, la relación entre la puntuación metabolómica y la incidencia de diabetes tipo 2 o enfermedad cardiovascular se evaluó mediante modelos de Cox ponderados (pesos de Barlow), ajustados progresivamente por variables demográficas, estilo de vida y dieta.

Incluye:

-   Scripts en **R** para pre-procesar datos, ajustar modelos y generar las figuras del artículo.
-   Imágenes y tablas finales.

> García-Gavilán JF, Babio N, Toledo E, et al. Olive oil consumption, plasma metabolites, and risk of type 2 diabetes and cardiovascular disease. Cardiovasc Diabetol. 2023 Dec 13;22(1):340. doi: 10.1186/s12933-023-02066-1. PMID: 38093289; PMCID: PMC10720204.

## Contenidos

1.  [Requisitos](#requisitos)
2.  [Estructura del proyecto](#estructura-del-proyecto)
3.  [Datos](#datos)
4.  [Licencia y citación](#licencia)

## Requisitos

\-   **R ≥ 4.3**\
\-   Paquetes detallados en el apartado librerias del script `sintaxis`:
- **`abind`:** Para combinar matrices o data frames a lo largo de nuevas dimensiones de forma flexible.  
- **`agricolae`:** Diseño y análisis de experimentos agrícolas (ANOVA, pruebas de comparación múltiple, etc.).  
- **`Amelia`:** Imputación múltiple de datos faltantes mediante bootstrapping y EM.  
- **`car`:** Diagnósticos, visualizaciones y pruebas para modelos de regresión (“Companion to Applied Regression”).  
- **`caret`:** Flujo unificado de preprocesado, entrenamiento y validación cruzada de modelos de *machine learning*.  
- **`chillR`:** Modelización de horas-frío y fenómenos de dormancia en frutales.  
- **`cluster`:** Algoritmos y validaciones para análisis de agrupamiento (k-means, PAM, CLARA, etc.).  
- **`colorspace`:** Paletas perceptualmente uniformes y conversión entre espacios de color.  
- **`compare`:** Comparar objetos (p. ej., data frames) y detectar diferencias.  
- **`cvAUC`:** Cálculo y comparación del AUC promedio con validación cruzada.  
- **`dendextend`:** Editar, colorear y mejorar dendrogramas de *clustering* jerárquico.  
- **`dplyr`:** Manipulación de datos (filtrar, seleccionar, agrupar, resumir) con sintaxis legible.  
- **`factoextra`:** Extraer y visualizar resultados de análisis multivariantes (PCA, CA, *clustering*, etc.).  
- **`foreach`:** Bucle flexible (secuencial o paralelo) para iteraciones elegantes.  
- **`foreign`:** Importar archivos de SPSS, SAS, Stata y otros formatos estadísticos.  
- **`ggplot2`:** Gramática de gráficos para crear visualizaciones elegantes y personalizables.  
- **`ggrepel`:** Etiquetas de texto que evitan superposiciones en gráficos de ggplot2.  
- **`glmnet`:** Regresión penalizada (Lasso, Ridge, Elastic Net) para GLM y modelos de Cox.  
- **`gmodels`:** Tablas de contingencia y contrastes de hipótesis (*CrossTable*, etc.).  
- **`gtools`:** Funciones auxiliares de programación (permutaciones, combinaciones, manejo de ficheros).  
- **`haven`:** Lectura/escritura de datos SPSS, Stata y SAS conservando etiquetas.  
- **`Hmisc`:** Resúmenes descriptivos, imputación, utilidades y gráficos de alto nivel.  
- **`iterators`:** Crear iteradores que alimentan *foreach* y bucles perezosos.  
- **`knitr`:** Generar informes dinámicos (R Markdown, LaTeX, HTML) con código y resultados integrados.  
- **`lattice`:** Sistema Trellis para gráficos de paneles con datos multivariantes.  
- **`lmtest`:** Pruebas de diagnóstico y contraste en modelos lineales y series temporales.  
- **`MASS`:** Funciones clásicas del libro *Modern Applied Statistics with S* (p. ej., *stepAIC*, *mvrnorm*).  
- **`mclust`:** Agrupamiento y densidad basados en mezclas gaussianas con selección BIC.  
- **`missForest`:** Imputación no paramétrica de datos faltantes mediante bosques aleatorios.  
- **`mixOmics`:** Integración y análisis multivariante de datos ómicos (PLS, sPLS, DIABLO).  
- **`nortest`:** Pruebas de normalidad (Anderson–Darling, Cramér–von Mises, Lilliefors, etc.).  
- **`PerformanceAnalytics`:** Métricas de retorno/riesgo y gráficos para carteras financieras.  
- **`pls`:** Regresión de mínimos cuadrados parciales y análisis de componentes.  
- **`plyr`:** Estrategia dividir-aplicar-combinar para listas, matrices y data frames.  
- **`pROC`:** Trazar y comparar curvas ROC con intervalos de confianza y AUC.  
- **`Publish`:** Presentar resultados de modelos en tablas y *forest plots* listos para publicar.  
- **`pvclust`:** *Clustering* jerárquico con valores p mediante *bootstrap* multiescala.  
- **`QuantPsyc`:** Herramientas de psicología cuantitativa (betas estandarizadas, análisis de media).  
- **`randomForest`:** Implementación de bosques aleatorios para clasificación y regresión.  
- **`Rcpp`:** Integrar y compilar código C++ de alto rendimiento dentro de R.  
- **`readr`:** Lectura y escritura rápidas de archivos delimitados (CSV, TSV).  
- **`readxl`:** Leer hojas de cálculo Excel (.xls y .xlsx) sin necesidad de Java.  
- **`reshape`:** Transformar datos entre formatos ancho/largo (*melt*, *cast*).  
- **`RNOmni`:** Pruebas de asociación genómica usando normalización por rangos.  
- **`rgl`:** Gráficos 3D interactivos basados en OpenGL.  
- **`rio`:** Importar y exportar datos en decenas de formatos con `import()` y `export()`.  
- **`rJava`:** Puente de bajo nivel para ejecutar código Java desde R.  
- **`Rmisc`:** Estadística descriptiva, multiplots y otras utilidades varias.  
- **`ropls`:** Análisis OPLS-DA, PLS y PCA para metabolómica con validación.  
- **`rpart`:** Árboles de clasificación y regresión (CART) con podado y visualización.  
- **`shiny`:** Crear aplicaciones web interactivas directamente desde R.  
- **`survival`:** Análisis de supervivencia (Kaplan–Meier, modelos de Cox, modelos paramétricos).  
- **`tibble`:** Data frames modernos con impresión amigable y columnas anidadas.  
- **`tictoc`:** Medir tiempos de ejecución con `tic()` y `toc()`.  
- **`tidyr`:** Dar formato *tidy* a los datos (*pivot_longer*, *pivot_wider*, *separate*, *unite*).  
- **`tis`:** Series temporales con calendarios financieros y funciones de índice de tiempo.  
- **`varhandle`:** Conversión y manejo de tipos (factor ↔ numérico, etc.).  
- **`xlsx`:** Leer y escribir archivos Excel usando Apache POI (requiere Java).  
- **`xlsxjars`:** Archivos .jar necesarios para el paquete **xlsx**.  
- **`doParallel`:** Backend *foreach* para paralelizar en múltiples núcleos o clústeres.  
- **`qvalue`:** Corrección por FDR y cálculo de *q-values* en análisis múltiples.  
- **`EnhancedVolcano`:** Gráficos Volcano claros y anotados para resultados de expresión génica.  
- **`airway`:** Conjunto de datos de RNA-seq de vías respiratorias para ejemplos/tutoriales.  
- **`magrittr`:** Operadores de tubería `%>%` y utilidades de programación funcional.  
- **`parallel`:** Funciones base para cómputo paralelo (*mclapply*, *makeCluster*, etc.).  
- **`limma`:** Análisis diferencial en microarrays y RNA-seq mediante modelos lineales con *empirical Bayes*.  
- **`VennDiagram`:** Crear diagramas de Venn y Euler altamente personalizables.  
- **`corrplot`:** Visualizar matrices de correlación con múltiples estilos de celdas.  
- **`stats`:** Funciones estadísticas base de R (modelos lineales, distribuciones, pruebas, etc.).  

## Estructura del proyecto

El proyecto se divide en las siguientes fases:

1.  Fase 1: preparación de los datos
    1.  Preparación de los metadatos
    2.  Creación de tablas descriptivas
    3.  Preparación de los datos de metabolómica
2.  Fase 2: ejecución de la elastic net
    1.  Creación de las puntuaciones
    2.  Validación visita al año
3.  Fase 3: inferencia con incidencia de DT2 y con incidencia de ECV
4.  Fase 4: analisis secundarios
    1.  Diagrama de Venn
    2.  Correlaciones entre las puntuaciones y el consumo estimado
    3.  Estatrificación por grupos de intervención
    4.  PCA

## Datos

Por cuestiones de tratamiento y protección de datos, las bases de datos de dichas publicaciones solo son accesibles bajo peticion estricta al Steering Committee (SC) de cada estudio respectivo. En estos archivos no se muestran las bases de datos de ninguno de los estudios. La forma de contacto con el SC de cada estudio está disponible dentro de cada respectiva publicacion a traves de los autores de correspondencia. Así mismo, las publicaciones publicadas bajo dominio de Open Access están incluidas en su carpeta en formato PDF.

## Licencia y citación

Licencia **GNU GENERAL PUBLIC LICENSE**

-------------------------------------------------------------------------------------------------------------------------------------------------

### English

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1186%2Fs12933--023--02066--1-blue)](https://doi.org/10.1186/s12933-023-02066-1)

## SHORT DESCRIPTION

In this study, machine learning models were employed to identify sets of metabolites associated with olive oil consumption and its varieties, producing a score that can predict such consumption. Missing values were imputed using *missForest*, and metabolomic concentrations were normalized with Blom's transformation. An Elastic Net model with 10 × 10-fold stratified cross-validation was utilized to select the metabolites. The identified sets were validated using data from the first year and summarized through principal component analysis (PCA). Finally, the relationship between the metabolomic score and the incidence of type 2 diabetes or cardiovascular disease was analyzed using weighted Cox models (Barlow weights), progressively adjusted for demographic, lifestyle, and dietary variables.

Includes:

- **R** scripts to preprocess data, fit models, and generate the article's figures.
- Final images and tables.

> García-Gavilán JF, Babio N, Toledo E, et al. Olive oil consumption, plasma metabolites, and risk of type 2 diabetes and cardiovascular disease. Cardiovasc Diabetol. 2023 Dec 13;22(1):340. doi: 10.1186/s12933-023-02066-1. PMID: 38093289; PMCID: PMC10720204.

## Contents

1. [Requirements](#requirements)
2. [Project structure](#project-structure)
3. [Data](#data)
4. [License and citation](#license)

## Requirements:

\- **R ≥ 4.3**\
\- Packages detailed in the libraries section of the `syntax` script:

- **`abind`:** Flexibly combine arrays or data frames along new dimensions.  
- **`agricolae`:** Design and analysis of agricultural experiments (ANOVA, multiple-comparison tests, etc.).  
- **`Amelia`:** Multiple imputation of missing data via bootstrapping and the EM algorithm.  
- **`car`:** Diagnostics, visualisations and tests for regression models (“Companion to Applied Regression”).  
- **`caret`:** Unified workflow for preprocessing, training, and cross-validating machine-learning models.  
- **`chillR`:** Modelling chill accumulation and dormancy in fruit trees.  
- **`cluster`:** Algorithms and validation for cluster analysis (k-means, PAM, CLARA, etc.).  
- **`colorspace`:** Perceptually uniform palettes and colour-space conversion.  
- **`compare`:** Compare objects (e.g., data frames) and detect differences.  
- **`cvAUC`:** Compute and compare cross-validated area under the ROC curve (AUC).  
- **`dendextend`:** Edit, colour, and enhance hierarchical-clustering dendrograms.  
- **`dplyr`:** Data manipulation (filter, select, group, summarise) with readable syntax.  
- **`factoextra`:** Extract and visualise results of multivariate analyses (PCA, CA, clustering, etc.).  
- **`foreach`:** Flexible (sequential or parallel) looping for elegant iterations.  
- **`foreign`:** Import SPSS, SAS, Stata, and other statistical file formats.  
- **`ggplot2`:** Grammar of graphics for creating elegant, customisable plots.  
- **`ggrepel`:** Text labels that repel each other to avoid overlap in ggplot2 graphics.  
- **`glmnet`:** Penalised regression (Lasso, Ridge, Elastic Net) for GLMs and Cox models.  
- **`gmodels`:** Contingency tables and hypothesis contrasts (*CrossTable*, etc.).  
- **`gtools`:** Programming helpers (permutations, combinations, file handling).  
- **`haven`:** Read/write SPSS, Stata, and SAS data while preserving labels.  
- **`Hmisc`:** Descriptive summaries, imputation utilitie,s and high-level graphics.  
- **`iterators`:** Create iterators feeding *foreach* and lazy loops.  
- **`knitr`:** Generate dynamic reports (R Markdown, LaTeX, HTML) integrating code and output.  
- **`lattice`:** Trellis system for multi-panel graphics of multivariate data.  
- **`lmtest`:** Diagnostic and hypothesis tests for linear models and time series.  
- **`MASS`:** Functions from *Modern Applied Statistics with S* (e.g., *stepAIC*, *mvrnorm*).  
- **`mclust`:** Gaussian mixture-model clustering and density estimation with BIC selection.  
- **`missForest`:** Non-parametric imputation of missing data using random forests.  
- **`mixOmics`:** Integration and multivariate analysis of omics data (PLS, sPLS, DIABLO).  
- **`nortest`:** Normality tests (Anderson–Darling, Cramér–von Mises, Lilliefors, etc.).  
- **`PerformanceAnalytics`:** Return/risk metrics and charts for financial portfolios.  
- **`pls`:** Partial least-squares regression and component analysis.  
- **`plyr`:** Split-apply-combine strategy for lists, matrices, and data frames.  
- **`pROC`:** Draw and compare ROC curves with confidence intervals and AUC.  
- **`Publish`:** Present model results in publication-ready tables and forest plots.  
- **`pvclust`:** Hierarchical clustering with p-values via multiscale bootstrap.  
- **`QuantPsyc`:** Tools for quantitative psychology (standardised betas, mean analysis).  
- **`randomForest`:** Random forest implementation for classification and regression.  
- **`Rcpp`:** Integrate and compile high-performance C++ code within R.  
- **`readr`:** Fast reading and writing of delimited files (CSV, TSV).  
- **`readxl`:** Read Excel spreadsheets (.xls and .xlsx) without Java.  
- **`reshape`:** Transform data between wide and long formats (*melt*, *cast*).  
- **`RNOmni`:** Genome-wide association tests using rank-normalisation.  
- **`rgl`:** Interactive 3-D graphics based on OpenGL.  
- **`rio`:** Import and export data in many formats with a single `import()`/`export()` call.  
- **`rJava`:** Low-level bridge to run Java code from R.  
- **`Rmisc`:** Descriptive statistics, multiplots, and miscellaneous utilities.  
- **`ropls`:** OPLS-DA, PLS, and PCA for metabolomics with validation.  
- **`rpart`:** Classification and regression trees (CART) with pruning and visualisation.  
- **`shiny`:** Build interactive web applications directly from R.  
- **`survival`:** Survival analysis (Kaplan–Meier, Cox models, parametric models).  
- **`tibble`:** Modern data frames with friendly printing and nested columns.  
- **`tictoc`:** Measure execution time with `tic()` and `toc()`.  
- **`tidyr`:** Make data tidy (*pivot_longer*, *pivot_wider*, *separate*, *unite*).  
- **`tis`:** Time-series objects with financial calendars and index functions.  
- **`varhandle`:** Convert and handle types (factor ↔ numeric, etc.).  
- **`xlsx`:** Read and write Excel files using Apache POI (requires Java).  
- **`xlsxjars`:** .jar files required by the **xlsx** package.  
- **`doParallel`:** *foreach* backend to parallelise across multiple cores or clusters.  
- **`qvalue`:** False-Discovery-Rate adjustment and *q-value* calculation in multiple testing.  
- **`EnhancedVolcano`:** Produce clear, annotated volcano plots for gene-expression results.  
- **`airway`:** RNA-seq airway dataset for examples and tutorials.  
- **`magrittr`:** Pipe operator `%>%` and functional-programming helpers.  
- **`parallel`:** Base functions for parallel computing (*mclapply*, *makeCluster*, etc.).  
- **`limma`:** Differential-expression analysis for microarrays and RNA-seq with empirical Bayes.  
- **`VennDiagram`:** Create highly customisable Venn and Euler diagrams.  
- **`corrplot`:** Visualise correlation matrices with multiple cell styles.  
- **`stats`:** Base R statistical functions (linear models, distributions, tests, etc.).  

## Project Structure

The project is divided into the following phases:

1. Phase 1: Data Preparation
1. Metadata Preparation
2. Creation of Descriptive Tables
3. Metabolomics Data Preparation
2. Phase 2: Elastic Net Execution
1. Creation of Scores
2. Validation at One-Year Visit
3. Phase 3: Inference with T2D Incidence and CVD Incidence
4. Phase 4: Secondary Analyses
1. Venn Diagram
2. Correlations between Scores and Estimated Consumption
3. Stratification by Intervention Group
4. PCA

## Data

For data processing and protection reasons, the databases of these publications are accessible only upon strict request to the Steering Committee (SC) of each respective study. The databases from any of the studies are not provided in these files. The contact form for each study's SC can be found within each publication through the corresponding authors. Publications released under Open Access are also included in their respective PDF folders.

## License and Citation

**GNU GENERAL PUBLIC LICENSE**
