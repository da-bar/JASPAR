# JASPAR <img src="https://jaspar.elixir.no/static/img/jaspar_logo.png" align="right" width="140"/>

[![License: GPL v2](https://img.shields.io/badge/License-GPL_v2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)

**JASPAR** is a data package that provides programmatic access to transcription factor (TF) binding profiles from the [JASPAR database](https://jaspar.elixir.no/).  
This package integrates all major JASPAR releases (latest: **2026**) and exposes curated **position frequency matrices (PFMs)**, **deep learning-based TF binding models**, and **associated genomic annotations** for computational and experimental genomics research.

---

## 📦 Package overview

[JASPAR](https://jaspar.elixir.no/) has provided **open-access**, **manually curated**, and **non-redundant** DNA binding profiles for TFs for over 20 years.  

This R package mirrors the content of the main JASPAR database and makes it programmatically accessible for downstream analyses in R/Bioconductor pipelines.

---

## 🔧 Installation

You can install the package from this repository:

```r
# install.packages("remotes")
remotes::install_github("da-bar/JASPAR")
```

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

