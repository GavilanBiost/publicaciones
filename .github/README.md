# Auto-Update System / Sistema de Actualización Automática

## 🇪🇸 Español

Este repositorio cuenta con un sistema automático que actualiza el archivo `index.md` cada vez que se añade una nueva carpeta de publicación.

### Cómo funciona

1. **GitHub Actions Workflow**: Cada vez que se hace push al repositorio (especialmente cuando se añaden nuevas carpetas con archivos README.md), se ejecuta automáticamente un workflow.

2. **Script Python**: El script `scripts/update_readme.py` escanea todas las carpetas del repositorio que siguen el patrón de DOI (comienzan con `10.`).

3. **Extracción de Metadatos**: Para cada carpeta encontrada:
   - Extrae el DOI del nombre de la carpeta
   - Lee el archivo README.md dentro de la carpeta
   - Extrae el título de la publicación
   
4. **Generación del Index**: Actualiza automáticamente `index.md` con la lista completa de todas las publicaciones encontradas.

### Añadir una nueva publicación

Para añadir una nueva publicación:

1. Crea una nueva carpeta con el formato DOI: `10.XXXX_YYYY` (reemplaza `/` con `_` y `.` según el patrón existente)
2. Añade el código R, materiales suplementarios y un archivo `README.md` con:
   - Un título principal (línea que comienza con `#`)
   - La descripción de la publicación
3. Haz commit y push de los cambios
4. El sistema actualizará automáticamente `index.md` en unos segundos

### Ejecutar manualmente

Si necesitas actualizar `index.md` manualmente:

```bash
python scripts/update_readme.py
```

---

## 🇬🇧 English

This repository has an automatic system that updates the `index.md` file every time a new publication folder is added.

### How it works

1. **GitHub Actions Workflow**: Every time a push is made to the repository (especially when new folders with README.md files are added), a workflow automatically runs.

2. **Python Script**: The `scripts/update_readme.py` script scans all repository folders following the DOI pattern (starting with `10.`).

3. **Metadata Extraction**: For each folder found:
   - Extracts the DOI from the folder name
   - Reads the README.md file inside the folder
   - Extracts the publication title
   
4. **Index Generation**: Automatically updates `index.md` with the complete list of all found publications.

### Adding a new publication

To add a new publication:

1. Create a new folder with DOI format: `10.XXXX_YYYY` (replace `/` with `_` and `.` according to existing pattern)
2. Add R code, supplementary materials, and a `README.md` file with:
   - A main title (line starting with `#`)
   - The publication description
3. Commit and push the changes
4. The system will automatically update `index.md` in a few seconds

### Run manually

If you need to update `index.md` manually:

```bash
python scripts/update_readme.py
```
