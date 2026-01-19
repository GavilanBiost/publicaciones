### Spanish

# Extra virgin olive oil consumption reduces the risk of osteoporotic fractures in the PREDIMED trial

## Código y material de apoyo al artículo

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.clnu.2016.12.030-blue)](https://doi.org/10.1016/j.clnu.2016.12.030)

## DESCRIPCIÓN CORTA

En este estudio se emplearon los siguientes métodos estadísticos:

1. **Descripción de la muestra**: Las características basales de los participantes se describieron mediante medias (DE) y porcentajes (n), y se presentaron por tertiles de consumo (Tablas 1 y 2).

2. **Construcción de la exposición dietética longitudinal**: Para aprovechar las evaluaciones dietéticas anuales (FFQ), se calculó el promedio acumulado del consumo de alimentos desde el inicio hasta el final del seguimiento, o hasta el último FFQ previo al evento de fractura.

3. **Ajuste por energía**: Las variables dietéticas se ajustaron por ingesta energética total con el método de residuos y los resultados se presentaron según terciles ajustados por energía (total de aceite de oliva, AOVE/EVOO y aceite común).

4. **Tiempo a evento y censura**: El tiempo de seguimiento se definió desde el inicio del estudio hasta la fecha de fractura, fallecimiento o fin de seguimiento, lo que ocurriera primero.

5. **Modelos de supervivencia (Cox)**: La asociación entre los terciles de consumo (ajustados por energía) y el riesgo de fractura osteoporótica se evaluó con modelos de Cox multivariables y se reportó HR e IC 95%:
   - Modelo 1: edad, sexo, IMC, nivel educativo, actividad física (MET-min/día), tabaquismo y grupo de intervención.
   - Modelo 2: Modelo 1 + diabetes, fracturas previas, uso de medicación relevante (p. ej., insulina, antidiabéticos orales, diuréticos, glucocorticoides, fármacos antiosteoporóticos, anticoagulantes, estrógenos) y adherencia basal a MedDiet (puntuación de 12 ítems).

6. **Comprobación de supuestos**: Se evaluó la proporcionalidad de riesgos (p. ej., mediante contraste tipo log-rank según el artículo).

7. **Curvas y análisis de sensibilidad**:
   - Se estimaron curvas de riesgo acumulado con el estimador de Nelson–Aalen (Fig. 1).
   - Se realizó análisis de sensibilidad excluyendo los casos ocurridos durante el primer año de intervención.
   - Se evaluaron asociaciones adicionales para MUFA, PUFA y el cociente MUFA:PUFA (en modelos ajustados).

Incluye:

- Sintaxis de **SPSS** (`.sps`) para limpieza, derivación de variables, tertiles y modelos de Cox.
- Exportaciones de la figura final (curvas Nelson–Aalen).

> García-Gavilan JF, Bulló M, Canudas S, Martínez-Gonzalez MA, Estruch R, Giardina S, Fitó M, Corella D, Ros E, Salas-Salvadó J. Extra virgin olive oil consumption reduces the risk of osteoporotic fractures in the PREDIMED trial. Clinical Nutrition. 2018;37:329–335. doi: 10.1016/j.clnu.2016.12.030.

## Contenidos

1. [Requisitos](#requisitos)
2. [Estructura del proyecto](#estructura-del-proyecto)
3. [Datos](#datos)
4. [Licencia y citación](#licencia)

## Requisitos {#requisitos}

- **IBM SPSS Statistics 21.0 (Windows)** (software principal del análisis).
- **Stata 14** (si se desea replicar/contrastar alguna parte del pipeline, según el artículo).

## Estructura del proyecto {#estructura-del-proyecto}

El proyecto se divide en las siguientes fases:

1. **Fase 1: Preparación y limpieza de datos**
   - Importación de FFQ anuales, covariables clínicas y eventos.
   - Revisión de rangos, valores perdidos y consistencia longitudinal.

2. **Fase 2: Derivación de variables de exposición**
   - Cálculo del consumo (g/día) de:
     - Aceite de oliva total
     - AOVE/EVOO (extra virgin)
     - Aceite de oliva común (refinado + orujo, según definiciones del FFQ)

3. **Fase 3: Ajuste por energía**
   - Ajuste por energía total mediante método de residuos.
   - Generación de variables ajustadas para análisis.

4. **Fase 4: Exposición acumulada y categorización**
   - Cálculo del promedio desde basal hasta último FFQ pre-evento/fin de seguimiento.
   - Clasificación en terciles (T1–T3) para cada tipo de aceite.

5. **Fase 5: Descriptivos basales (Tablas 1 y 2)**
   - Medias (DE) y porcentajes por terciles de AOVE (u otras exposiciones).

6. **Fase 6: Modelos de Cox (Tabla 3 y suplementarios)**
   - Modelos dependientes del tiempo con HR e IC 95%.
   - Ajustes por bloques (Modelo 1/Modelo 2).

7. **Fase 7: Curvas y sensibilidad (Figura 1)**
   - Nelson–Aalen para riesgo acumulado por tertiles de AOVE.
   - Sensibilidad excluyendo eventos del primer año.

8. **Fase 8: Material suplementario**
   - Resultados por grupo de intervención y exposiciones secundarias (MUFA, PUFA, ratio).

## Datos {#datos}

Por motivos de protección y tratamiento de datos, la base de datos del ensayo PREDIMED no se incluye en este repositorio.
El acceso a los datos se gestiona mediante solicitud al estudio/consorcio correspondiente y/o autores de correspondencia, según se describe en la publicación. El ensayo está registrado como **ISRCTN35739639**.

## Licencia y citación {#licencia}

Licencia **GNU GENERAL PUBLIC LICENSE**

Si utilizas este repositorio, por favor cita el artículo original:

> García-Gavilan JF, Bulló M, Canudas S, Martínez-Gonzalez MA, Estruch R, Giardina S, Fitó M, Corella D, Ros E, Salas-Salvadó J. Extra virgin olive oil consumption reduces the risk of osteoporotic fractures in the PREDIMED trial. Clinical Nutrition. 2018;37:329–335. doi: 10.1016/j.clnu.2016.12.030.

---

### English

# Extra virgin olive oil consumption reduces the risk of osteoporotic fractures in the PREDIMED trial

## Code and supporting material for the article

[![](https://img.shields.io/badge/DOI-10.1016%2Fj.clnu.2016.12.030-blue)](https://doi.org/10.1016/j.clnu.2016.12.030)

## SHORT DESCRIPTION

The following statistical methods were used in this study:

1. **Sample description**: Baseline characteristics were summarized using means (SD) and percentages (n), and reported across intake tertiles (Tables 1 and 2).

2. **Longitudinal dietary exposure**: To leverage yearly FFQ measurements, cumulative average intake was computed from baseline to end of follow-up or to the last FFQ before the fracture event.

3. **Energy adjustment**: Dietary variables were adjusted for total energy intake using the residual method, and results were presented using energy-adjusted tertiles (total olive oil, extra-virgin olive oil, and common olive oil).

4. **Time-to-event definition**: Follow-up time was defined from baseline to fracture date, death, or end of follow-up, whichever occurred first.

5. **Survival analysis (Cox models)**: Associations between energy-adjusted tertiles and fracture risk were evaluated using multivariable Cox proportional hazards models, reporting HRs and 95% CIs:
   - Model 1: age, sex, BMI, education, physical activity (MET-min/day), smoking, and intervention group.
   - Model 2: Model 1 + diabetes, prior fractures, relevant medications (e.g., insulin, oral antidiabetics, diuretics, glucocorticoids, anti-osteoporotic drugs, anticoagulants, estrogens), and baseline MedDiet adherence (12-point score).

6. Assumption checks: Proportional hazards assumptions were assessed (as reported in the article).

7. **Curves and sensitivity analyses**:
   - Nelson–Aalen estimator was used to plot cumulative hazard curves (Fig. 1).
   - A sensitivity analysis excluded early cases during the first intervention year.
   - Additional exposures (MUFA, PUFA, MUFA:PUFA ratio) were assessed within adjusted models.

Includes:

- SPSS syntax files (`.sps`) for data cleaning, variable derivation, tertiles, and Cox models.
- Exported final figure (Nelson–Aalen curves).

> García-Gavilan JF, Bulló M, Canudas S, Martínez-Gonzalez MA, Estruch R, Giardina S, Fitó M, Corella D, Ros E, Salas-Salvadó J. Extra virgin olive oil consumption reduces the risk of osteoporotic fractures in the PREDIMED trial. Clinical Nutrition. 2018;37:329–335. doi: 10.1016/j.clnu.2016.12.030.

## Contents

1. [Requirements](#requirements)
2. [Project structure](#project-structure)
3. [Data](#data)
4. [License and citation](#license)

## Requirements {#requirements}

- **IBM SPSS Statistics 21.0 (Windows)** (primary analysis software).
- **Stata 14** (optional; used alongside SPSS as reported in the article).

## Project Structure {#project-structure}

The project is divided into the following phases:

1. **Phase 1: Data preparation and cleaning**
2. **Phase 2: Exposure derivation (total / EVOO / common olive oil, g/day)**
3. **Phase 3: Energy adjustment (residual method)**
4. **Phase 4: Cumulative average exposure + tertiles (T1–T3)**
5. **Phase 5: Descriptive tables (Tables 1–2)**
6. **Phase 6: Time-dependent Cox models (Table 3 + supplementary)**
7. **Phase 7: Nelson–Aalen curves + sensitivity analysis (Fig. 1)**
8. **Phase 8: Supplementary analyses (intervention groups, MUFA/PUFA metrics)**

## Data {#data}

Due to data governance and privacy, PREDIMED trial data are not included in this repository.
Data access must be requested through the appropriate study governance channels and/or corresponding authors, as described in the publication. Trial registration: **ISRCTN35739639**.

## License and Citation {#license}

**GNU GENERAL PUBLIC LICENSE**

If you use this repository, please cite the original article:

> García-Gavilan JF, Bulló M, Canudas S, Martínez-Gonzalez MA, Estruch R, Giardina S, Fitó M, Corella D, Ros E, Salas-Salvadó J. Extra virgin olive oil consumption reduces the risk of osteoporotic fractures in the PREDIMED trial. Clinical Nutrition. 2018;37:329–335. doi: 10.1016/j.clnu.2016.12.030.
