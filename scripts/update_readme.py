#!/usr/bin/env python3
"""
Script to automatically update index.md with all publication folders.
This script scans all directories in the repository, extracts metadata from their
README files, and updates the main index.md with a complete list of publications.
"""

import os
import re
from pathlib import Path
from typing import List, Dict, Optional


def extract_title_from_readme(readme_path: Path) -> Optional[str]:
    """Extract the main title from a README file."""
    try:
        with open(readme_path, 'r', encoding='utf-8') as f:
            content = f.read()
            # Look for the first # heading after "Spanish" or at the beginning
            match = re.search(r'^#\s+(.+?)$', content, re.MULTILINE)
            if match:
                title = match.group(1).strip()
                # Remove markdown links and extra formatting
                title = re.sub(r'\[([^\]]+)\]\([^\)]+\)', r'\1', title)
                return title
    except Exception as e:
        print(f"Warning: Could not read {readme_path}: {e}")
    return None


def extract_doi_from_folder_name(folder_name: str) -> str:
    """Convert folder name to DOI format."""
    # Replace underscores with appropriate characters
    doi = folder_name.replace('_', '/', 1)  # First underscore to /
    doi = doi.replace('_', '.', 1)  # Second underscore to .
    return doi


def get_publication_folders(repo_path: Path) -> List[Dict[str, str]]:
    """
    Get all publication folders and their metadata.
    Returns a list of dictionaries with folder name, DOI, and title.
    """
    publications = []
    
    # Get all directories that match DOI pattern (start with 10.)
    for item in sorted(repo_path.iterdir()):
        if item.is_dir() and not item.name.startswith('.') and item.name.startswith('10.'):
            folder_name = item.name
            doi = extract_doi_from_folder_name(folder_name)
            
            # Try to extract title from README
            readme_path = item / 'README.md'
            title = None
            if readme_path.exists():
                title = extract_title_from_readme(readme_path)
            
            publications.append({
                'folder': folder_name,
                'doi': doi,
                'title': title
            })
    
    return publications


def generate_readme_content(publications: List[Dict[str, str]]) -> str:
    """Generate the complete README.md content."""
    
    # Spanish section
    spanish_list = ""
    for i, pub in enumerate(publications, 1):
        if pub['title']:
            spanish_list += f"{i}. **[{pub['doi']}](./{pub['folder']}/)** - {pub['title']}\n"
        else:
            spanish_list += f"{i}. **[{pub['doi']}](./{pub['folder']}/)** - Código R y materiales suplementarios\n"
    
    # English section
    english_list = ""
    for i, pub in enumerate(publications, 1):
        if pub['title']:
            english_list += f"{i}. **[{pub['doi']}](./{pub['folder']}/)** - {pub['title']}\n"
        else:
            english_list += f"{i}. **[{pub['doi']}](./{pub['folder']}/)** - R code and supplementary materials\n"
    
    readme_content = f"""# Publicaciones Científicas / Scientific Publications

## 🇪🇸 Español

Bienvenido al repositorio de código en R utilizado en las publicaciones científicas en las que he trabajado.

### Publicaciones Disponibles

En cada carpeta identificada con un DOI, se puede encontrar el código de la publicación:

{spanish_list}
### 📊 Sobre los Datos

Por cuestiones de tratamiento y protección de datos, las bases de datos de dichas publicaciones solo son accesibles mediante petición estricta al Steering Committee (SC) de cada estudio. 

En estos archivos **no se muestran las bases de datos** de ningún estudio. La forma de contacto con el SC de cada estudio está disponible en la respectiva publicación, a través de los autores de correspondencia.

Las publicaciones bajo el modelo de **Open Access** están incluidas en su carpeta en formato PDF. Los archivos de imagen que se indican en el código también están adjuntos y se identifican de la misma manera en que aparecen en la publicación.

### 📧 Contacto

Para cuestiones relacionadas con el código, contactar por correo electrónico a: [gavilanbiost@gmail.com](mailto:gavilanbiost@gmail.com)

---

## 🇬🇧 English

Welcome to the repository containing the R code utilized in the scientific publications with which I have been involved.

### Available Publications

The publication code is accessible within each folder identified by its DOI:

{english_list}
### 📊 About the Data

For reasons related to data processing and protection, the databases associated with these publications are available only upon a formal request to the Steering Committee (SC) of each respective study.

These files **do not include the databases** for any of the studies. The contact information for the SC of each study can be found in its respective publication, via the corresponding author.

Publications made available under **Open Access** are included in their respective folders in PDF format. The image files referenced in the code are also provided and are identified in the same manner as they appear in the publication.

### 📧 Contact

For inquiries related to the code, please reach out via email at: [gavilanbiost@gmail.com](mailto:gavilanbiost@gmail.com)

---

## 📝 License

This repository is licensed under the GNU General Public License v3.0. Please take a look at individual publication folders for specific licensing information.

---

*This README is automatically updated by GitHub Actions whenever a new publication folder is added.*
"""
    
    return readme_content


def main():
    """Main function to update index.md"""
    repo_path = Path(__file__).parent.parent
    
    print("Scanning repository for publication folders...")
    publications = get_publication_folders(repo_path)
    print(f"Found {len(publications)} publication folders")
    
    print("Generating README content...")
    readme_content = generate_readme_content(publications)
    
    # Update index.md (the main file for this repository)
    index_path = repo_path / 'index.md'
    print(f"Writing to {index_path}...")
    with open(index_path, 'w', encoding='utf-8') as f:
        f.write(readme_content)
    
    print("index.md updated successfully!")


if __name__ == '__main__':
    main()
