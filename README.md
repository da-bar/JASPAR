# JASPAR <img src="https://jaspar.elixir.no/static/img/jaspar_logo.png" align="right" width="140"/>

[![License: GPL v2](https://img.shields.io/badge/License-GPL_v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)

**JASPAR** is a data package that provides programmatic access to transcription factor (TF) binding profiles from the [JASPAR database](https://jaspar.elixir.no/).  
The JASPAR databases are SQLite files that are registered as resources in [AnnotationHub](https://bioconductor.org/packages/AnnotationHub). The package selects the release you ask for by its version, lets AnnotationHub download it once, and returns the path of the cached file. Open the file with RSQLite, or use it with [TFBSTools](https://bioconductor.org/packages/TFBSTools) to get curated **position frequency matrices (PFMs)**.

---

## 📦 Package overview

[JASPAR](https://jaspar.elixir.no/) has provided **open-access**, **manually curated**, and **non-redundant** DNA binding profiles for TFs for over 20 years.  

This R package makes the content of the main JASPAR database accessible for downstream analyses in R/Bioconductor pipelines. The releases that are registered in AnnotationHub are listed by `getAvailableJASPARVersions()`. The default version is `JASPAR2024`.

---

## 🔧 Installation

You can install the package from this repository:

```r
# install.packages("remotes")
remotes::install_github("da-bar/JASPAR")
```

## 🚀 Quick start

```r
library(JASPAR)

getAvailableJASPARVersions()              # releases registered in AnnotationHub
jaspar <- JASPAR(version = "JASPAR2024")  # downloaded once, then cached
db(jaspar)                                # path of the SQLite file

# search the profiles with TFBSTools: CORE profiles of S. cerevisiae
con <- RSQLite::dbConnect(RSQLite::SQLite(), db(jaspar),
                          flags = RSQLite::SQLITE_RO)
pfms <- TFBSTools::getMatrixSet(con, list(species = 4932, collection = "CORE"))
RSQLite::dbDisconnect(con)
```

The search step needs the packages RSQLite and TFBSTools, which are only suggested: `BiocManager::install(c("RSQLite", "TFBSTools"))`.

The first call of `AnnotationHub()` downloads the hub database (about 140 MB); later calls take a few seconds. To retrieve several versions, create the hub once, `ah <- AnnotationHub::AnnotationHub()`, and pass it with `JASPAR(version, hub = ah)`.

## 📖 Vignettes

For detailed usage examples, see the package vignettes:

```r
browseVignettes("JASPAR")
```
---

## 🛠 Bug reports & contributions

- Report issues here: [**GitHub Issues**](https://github.com/da-bar/JASPAR/issues)
- Contributions are welcome via pull requests.
- For database-related inquiries, visit the [JASPAR helpdesk](https://jaspar.elixir.no/).

---

## 📄 License

This package is licensed under **GPL-2**.  
Upstream data licensing and citation requirements apply.

---

## 🔗 Useful links

- **JASPAR database** → [https://jaspar.elixir.no/](https://jaspar.elixir.no/)    
- **Bug reports** → [https://github.com/da-bar/JASPAR/issues](https://github.com/da-bar/JASPAR/issues)

---

*Maintainer*: **Damir Baranašić** [✉️](mailto:damir.baranasic@lms.mrc.ac.uk)  
*ORCID*: [0000-0001-5948-0932](https://orcid.org/0000-0001-5948-0932)
