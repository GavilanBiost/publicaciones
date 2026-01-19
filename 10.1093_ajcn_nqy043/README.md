### Spanish

# Higher dietary glycemic index and glycemic load values increase the risk of osteoporotic fracture in the PREDIMED-Reus trial

## Código y material de apoyo al artículo

[![DOI](https://img.shields.io/badge/DOI-10.1093%2Fajcn%2Fnqy043-blue)](https://doi.org/10.1093/ajcn/nqy043)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código estadístico, la sintaxis y el material suplementario utilizados en el análisis del estudio que evalúa la asociación entre el Índice Glucémico Dietético (DGI), la Carga Glucémica Dietética (DGL) y el riesgo de fractura osteoporótica en una población mediterránea de edad avanzada con alto riesgo cardiovascular.

En este estudio se emplearon los siguientes métodos estadísticos:

1. **Cálculo del Índice Glucémico Dietético (DGI)**
   - Estimación a partir de cuestionarios de frecuencia alimentaria (FFQ) validados de 137 ítems  
   - Asignación de valores de índice glucémico internacionales (glucosa como referencia)  
   - Cálculo del DGI como la razón entre la carga glucémica total y la ingesta total de hidratos de carbono, multiplicado por 100  

2. **Cálculo de la Carga Glucémica Dietética (DGL)**  
   - Multiplicación del contenido de hidratos de carbono disponibles por el índice glucémico específico de cada alimento  
   - Agregación diaria y cálculo de promedios acumulados durante el seguimiento  

3. **Promedios acumulados de exposición**  
   - DGI y DGL estimados como medias acumuladas desde el inicio hasta el evento, fallecimiento o fin del seguimiento  

4. **Estratificación en tertiles y cuartiles**  
   - Clasificación de los participantes en tertiles (análisis principal)  
   - Análisis de sensibilidad utilizando cuartiles  

5. **Análisis descriptivo basal**  
   - Comparación de características antropométricas, clínicas y dietéticas entre tertiles  
   - ANOVA y pruebas chi-cuadrado  

6. **Modelos de regresión de Cox**  
   - Estimación del riesgo de fractura osteoporótica incidente  
   - Resultados expresados como Hazard Ratios (HR) e IC 95% con ajustes progresivos:
        - Modelo 1: edad, sexo, IMC, nivel educativo, actividad física, grupo de intervención y tabaquismo  
        - Modelo 2: modelo 1 + diabetes, fracturas previas y medicación relevante  
        - Modelo 3: modelo 2 + ingesta energética y nutrientes  

7. **Análisis de tendencia y variables continuas**  
   - Tendencia cuadrática por tertiles  
   - Incremento de riesgo por aumento de 1 punto en DGI y DGL  

8. **Análisis de sensibilidad**  
   - Exclusión de eventos tempranos  
   - Análisis alternativo por cuartiles  

Incluye:

- Sintaxis en SPSS
- Tablas y figuras suplementarias
- Artículo original en PDF

> García-Gavilán JF, Bulló M, Camacho-Barcia L, Rosique-Esteban N, Hernández-Alonso P, Basora J, Martínez-González MA, Estruch R, Fitó M, Salas-Salvadó J. Higher dietary glycemic index and glycemic load values increase the risk of osteoporotic fracture in the PREvención con DIeta MEDiterránea (PREDIMED)-Reus trial. Am J Clin Nutr. 2018 Jun 1;107(6):1035-1042. doi: 10.1093/ajcn/nqy043. PMID: 29746627.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- **SPSS ≥ 21.0**
- Software utilizado: IBM SPSS Statistics

## Estructura del proyecto {#estructura-del-proyecto}

1. **Preparación de datos dietéticos**
2. **Cálculo de DGI y DGL**
3. **Análisis descriptivo basal**
4. **Modelos de riesgo de fractura**
5. **Análisis de tendencia y sensibilidad**
6. **Material suplementario**

## Datos {#datos}

Las bases de datos del estudio no se incluyen por motivos de confidencialidad. El acceso a los datos está sujeto a solicitud al Steering Committee del estudio.  
El material suplementario y el artículo original se incluyen en formato PDF con fines de reproducibilidad metodológica.

## Licencia y citación {#licencia}

Licencia **GNU GENERAL PUBLIC LICENSE**

---

### English

# Higher dietary glycemic index and glycemic load values increase the risk of osteoporotic fracture in the PREDIMED-Reus trial

## Code and supporting material for the article

[![DOI](https://img.shields.io/badge/DOI-10.1093%2Fajcn%2Fnqy043-blue)](https://doi.org/10.1093/ajcn/nqy043)

## SHORT DESCRIPTION

This repository contains the statistical code, syntax, and supplementary material used to assess the association between dietary glycemic index (DGI), dietary glycemic load (DGL), and osteoporotic fracture risk in elderly participants.

Includes:

- SPSS syntax files  
- Supplementary tables and figures  
- Original article in PDF format  

## Data {#data}

Due to data protection policies, PREDIMED databases are not publicly available and can only be accessed upon request to the Steering Committee.

## License {#license}

**GNU GENERAL PUBLIC LICENSE**