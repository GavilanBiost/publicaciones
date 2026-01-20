### Spanish

# Association between DDR1 variants and processing speed: linear mixed-effects model analysis

## Código y material de apoyo al artículo

[![DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.jpsychires.2022.12.020-blue)](https://doi.org/10.1016/j.jpsychires.2022.12.020)

## DESCRIPCIÓN CORTA

Este repositorio contiene el código y el material analítico correspondiente al análisis longitudinal mediante modelos lineales mixtos, utilizado para evaluar la asociación entre variantes del gen DDR1 y la velocidad de procesamiento cognitivo, medida mediante el Trail Making Test A (TMT-A), en pacientes con psicosis temprana y controles sanos.

El contenido se limita únicamente al análisis longitudinal y no incluye los análisis transversales (ANCOVA) presentados en el artículo original.

## DESCRIPCIÓN DEL ANÁLISIS ESTADÍSTICO

### Variable dependiente
- Velocidad de procesamiento (PS) evaluada mediante el Trail Making Test A (TMT-A)
- Se emplea la transformación logarítmica de TMT-A para reducir la asimetría de la distribución

### Diseño longitudinal
- Seguimiento de hasta 10 años
- Mediciones repetidas en baseline, 1, 3 y 10 años
- Inclusión de participantes con al menos una medición disponible
- Manejo implícito de datos faltantes mediante el enfoque de modelos mixtos

### Modelos lineales mixtos
- Análisis realizado con modelos lineales mixtos (LMM) usando el paquete `nlme` de R
- Consideración de la correlación intraindividuo derivada de las medidas repetidas
- Inclusión de efectos aleatorios a nivel de sujeto

### Análisis estratificado por grupo
Los modelos se ajustaron de forma independiente para:
- Controles sanos
- Pacientes con psicosis temprana

### Variables genéticas evaluadas
- Genotipo combinado de riesgo DDR1
- rs1264323
- rs2267641 (o SNP proxy en desequilibrio de ligamiento)

Cada variante genética se analizó mediante modelos separados.

### Covariables incluidas

**Controles sanos**
- Tiempo de seguimiento
- Sexo
- Edad
- Años de educación
- Consumo de cannabis

**Pacientes con psicosis temprana**
- Tiempo de seguimiento
- Sexo
- Edad
- Años de educación
- Consumo de cannabis
- Síntomas negativos (SANS)
- Síntomas positivos (SAPS)
- Dosis antipsicótica (equivalentes de clorpromazina)
- Años de evolución de la enfermedad

### Evaluación de interacciones
- Se exploraron interacciones entre genotipo y tiempo
- No se identificaron interacciones significativas, por lo que el efecto genético se interpretó como estable a lo largo del seguimiento

### Selección y evaluación del modelo
- Comparación de modelos anidados mediante pruebas de razón de verosimilitudes
- Evaluación de efectos marginales
- Selección basada en parsimonia y coherencia clínica

### Verificación de supuestos
- Inspección gráfica de residuos
- Evaluación de normalidad
- Evaluación de homocedasticidad

## Incluye

- Scripts en R para:
  - Preparación de la base de datos longitudinal
  - Ajuste de modelos lineales mixtos
  - Comparación de modelos
  - Diagnóstico de supuestos
- Figura longitudinal correspondiente al análisis de modelos mixtos
- Artículo original en PDF

> Gas C, Ayesa-Arriola R, Vázquez-Bourgon J, Crespo-Facorro B, García-Gavilán J, Labad J, Martorell L, Muntané G, Sanchez-Gistau V, Vilella E. Cross-sectional and longitudinal assessment of the association between DDR1 variants and processing speed in patients with early psychosis and healthy controls. J Psychiatr Res. 2023 Feb;158:49-55. doi: 10.1016/j.jpsychires.2022.12.020. Epub 2022 Dec 20. PMID: 36571911.

## Contenidos

1. [Requisitos](#requisitos)  
2. [Datos](#datos)  
3. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- R ≥ 4.0
- Paquetes utilizados:
  - `nlme` – Modelos lineales y no lineales de efectos mixtos
  - `readxl`, `rio` – Importación y exportación de datos
  - `ggplot2` – Visualización
  - `car` – Evaluación de supuestos

## Datos {#datos}

Por motivos de confidencialidad y protección de datos, las bases de datos clínicas no se incluyen en este repositorio.  
El acceso a los datos requiere autorización del Steering Committee correspondiente, disponible a través de los autores de correspondencia del artículo.

El artículo está disponible en Open Access.

## Licencia y citación {#licencia}

Licencia GNU GENERAL PUBLIC LICENSE (GPL).  
Si utilizas este código, por favor cita el artículo original.

### English

# Association between DDR1 variants and processing speed: linear mixed-effects model analysis

## Code and supporting material for the article

[![DOI](https://img.shields.io/badge/DOI-10.1016%2Fj.jpsychires.2022.12.020-blue)](https://doi.org/10.1016/j.jpsychires.2022.12.020)

## SHORT DESCRIPTION

This repository contains only the code and material related to the longitudinal linear mixed-effects model analysis assessing the association between DDR1 gene variants and processing speed (Trail Making Test A) in early psychosis patients and healthy controls.

The repository focuses exclusively on mixed models for repeated measures and does not include cross-sectional analyses.

## Data {#data}

Study datasets are not publicly available due to data protection regulations. Access requires approval from the corresponding Steering Committees.

## License {#license}

GNU GENERAL PUBLIC LICENSE (GPL).
