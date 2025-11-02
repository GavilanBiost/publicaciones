# GitHub Pages Setup Instructions / Instrucciones de Configuración de GitHub Pages

## 🇪🇸 Español

### ¿Qué se ha hecho?

Se han agregado los siguientes archivos para habilitar GitHub Pages:

1. **`_config.yml`** - Configuración de Jekyll para GitHub Pages
2. **`index.md`** - Página principal del sitio web
3. **`.github/workflows/pages.yml`** - Workflow de GitHub Actions para desplegar automáticamente
4. **`.gitignore`** - Excluye archivos de compilación de Jekyll
5. **`README.md`** en cada carpeta de publicación

### Pasos para activar GitHub Pages

Para que el sitio web se publique en `https://gavilanbiost.github.io`, es necesario:

#### Opción A: Usar este repositorio para GitHub Pages (Recomendado si es su sitio principal)

1. Ir a la configuración del repositorio: `https://github.com/GavilanBiost/publicaciones/settings/pages`

2. En la sección **"Build and deployment"**:
   - **Source**: Seleccionar `GitHub Actions`
   - Esto permitirá que el workflow `pages.yml` despliegue automáticamente el sitio

3. Guardar los cambios

4. Hacer push de estos cambios a la rama `main`:
   ```bash
   git checkout main
   git merge copilot/fix-page-publishing-issue
   git push origin main
   ```

5. El workflow se ejecutará automáticamente y el sitio estará disponible en unos minutos

#### Opción B: Usar el repositorio gavilanbiost.github.io (Si existe)

Si ya tiene un repositorio llamado `gavilanbiost.github.io`, puede:

1. Copiar los archivos de configuración (`_config.yml`, `index.md`, `.github/workflows/pages.yml`) a ese repositorio
2. Seguir los mismos pasos de configuración en ese repositorio
3. Este repositorio `publicaciones` puede usarse como un proyecto adicional en `https://gavilanbiost.github.io/publicaciones`

### Verificación

Una vez activado:
- El sitio estará disponible en `https://gavilanbiost.github.io` (si es el sitio principal)
- O en `https://gavilanbiost.github.io/publicaciones` (si es un proyecto)
- Cada push a `main` actualizará automáticamente el sitio

### Características del sitio

✅ **Bilingüe**: Contenido en Español e Inglés  
✅ **Responsive**: Se adapta a móviles y escritorio  
✅ **Tema Cayman**: Tema elegante y profesional de GitHub  
✅ **Navegación**: Enlaces a cada publicación por DOI  
✅ **Actualización automática**: Con cada push a main  

---

## 🇬🇧 English

### What has been done?

The following files have been added to enable GitHub Pages:

1. **`_config.yml`** - Jekyll configuration for GitHub Pages
2. **`index.md`** - Main website homepage
3. **`.github/workflows/pages.yml`** - GitHub Actions workflow for automatic deployment
4. **`.gitignore`** - Excludes Jekyll build artifacts
5. **`README.md`** in each publication folder

### Steps to activate GitHub Pages

For the website to be published at `https://gavilanbiost.github.io`, you need to:

#### Option A: Use this repository for GitHub Pages (Recommended if it's your main site)

1. Go to repository settings: `https://github.com/GavilanBiost/publicaciones/settings/pages`

2. In the **"Build and deployment"** section:
   - **Source**: Select `GitHub Actions`
   - This will allow the `pages.yml` workflow to automatically deploy the site

3. Save the changes

4. Push these changes to the `main` branch:
   ```bash
   git checkout main
   git merge copilot/fix-page-publishing-issue
   git push origin main
   ```

5. The workflow will run automatically and the site will be available in a few minutes

#### Option B: Use the gavilanbiost.github.io repository (If it exists)

If you already have a repository named `gavilanbiost.github.io`, you can:

1. Copy the configuration files (`_config.yml`, `index.md`, `.github/workflows/pages.yml`) to that repository
2. Follow the same configuration steps in that repository
3. This `publicaciones` repository can be used as an additional project at `https://gavilanbiost.github.io/publicaciones`

### Verification

Once activated:
- The site will be available at `https://gavilanbiost.github.io` (if it's the main site)
- Or at `https://gavilanbiost.github.io/publicaciones` (if it's a project)
- Each push to `main` will automatically update the site

### Site features

✅ **Bilingual**: Content in Spanish and English  
✅ **Responsive**: Adapts to mobile and desktop  
✅ **Cayman Theme**: Elegant and professional GitHub theme  
✅ **Navigation**: Links to each publication by DOI  
✅ **Automatic updates**: With each push to main  

---

## Troubleshooting / Solución de problemas

### The site is not publishing / El sitio no se publica

1. Check that GitHub Pages is enabled in repository settings
2. Verify that the workflow has permissions to deploy (Settings → Actions → General → Workflow permissions → Read and write)
3. Check the Actions tab for any workflow errors
4. Make sure you pushed to the `main` branch

### 404 Error

- If using the main user site (`gavilanbiost.github.io`), make sure this is your username.github.io repository
- If using as a project site, access it at `https://gavilanbiost.github.io/publicaciones`

### Workflow fails / El workflow falla

Check the Actions tab for detailed error messages. Common issues:
- Permissions not set correctly
- YAML syntax errors (already validated)
- GitHub Pages not enabled in settings

## Support / Soporte

For issues related to this code, contact: gavilanbiost@gmail.com
