# biospheremetrics


*The goal of biospheremetrics is to provide functions to calculate and plot 
the biosphere integrity metrics BioCol and EcoRisk in an R package based on 
outputs of [LPJmL](https://gitlab.pik-potsdam.de/lpjml/LPJmL_internal).
biospheremetrics utilizes the read functions of the 
[lpjmlkit package](https://gitlab.pik-potsdam.de/lpjml/lpjmlkit).*

## Installation lpjmlkit
The dependency package lpjmlkit is currently not available from CRAN, therefore it needs to be installed manually.

Either install from PIK repository:
```R
install.packages('lpjmlkit', repos = c('https://pik-piam.r-universe.dev', 'https://cloud.r-project.org'))
```

or checkout and install from github (https://github.com/PIK-LPJmL/lpjmlkit).

## Installation biospheremetrics

The easiest way to install biospheremetrics is by directly loading it from github/gitlab depending on what you have access to:

```R
devtools::install_git("git@github.com:stenzelf/biospheremetrics.git")
library(biospheremetrics)
```
or
```R
devtools::install_git("git@gitlab.pik-potsdam.de:stenzel/biospheremetrics.git")
library(biospheremetrics)
```

Alternatively, you can install `biospheremetrics` by git cloning this repository:

```bash
git clone https://gitlab.pik-potsdam.de/stenzel/biospheremetrics.git <path_to_biospheremetrics>
```

and install via  [`devtools`](https://rawgit.com/rstudio/cheatsheets/master/package-development.pdf):

```R
devtools::install("<path_to_biospheremetrics>")
library("biospheremetrics")
```

alternatively, you can also load it from source:

```R
devtools::load_all("<path_to_biospheremetrics>"")
```

## Scripts

The `./scripts` folder contains scripts to be used on the PIK cluster to 
compute longer timeseries with higher RAM demand.

## Example
For examples, see the vignette.
