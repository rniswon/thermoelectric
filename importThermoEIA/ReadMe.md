# importThermoEIA

The importThermoEIA package pulls data on thermoelectric plants,
including forms 923, 860, 767, 906/920, and 759 from the EIA website;
and formats data in a standard format including stable table and
column names regardless of year data was reported.  The
importThermoEIA package includes tools to assist the user in
developing crosswalks for EIA import. The importThermoEIA package
formats data to be used by the EIAplantAssociation and condenseryDuty
packages.

## Reporting Bugs

Please consider reporting bugs and asking questions on the [Issues
page](https://github.com/rniswon/Thermoelectric/issues).

## Required Software and Dependencies

The importThermoEIA package must be run on an R version
[4.0](https://cran.microsoft.com/snapshot/2021-01-17/bin/windows/base/old/4.0.0).
Download the latest version R here [R download for
Windows](https://cran.r-project.org/bin/windows/base/).  An
installation of Rtools (either
[rtools40](https://github.com/r-windows/rtools-installer/releases/download/2022-02-06/rtools40-x86_64.exe)
for R versions 4.0.x through 4.1.x or
[rtools42](https://cran.microsoft.com/bin/windows/Rtools/rtools42/rtools.html)
for R version 4.2.x is required to install prerequisite packages.
Microsoft Excel is also required. The following prerequisite R
packages listed in the `DESCRIPTION` file will be installed when
importThermoEIA is installed:

1. [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
2. [foreign](https://cran.r-project.org/web/packages/foreign/index.html)
3. [knitr](https://cran.r-project.org/web/packages/knitr/index.html)
4. [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)
5. [openxlsx](https://cran.r-project.org/web/packages/openxlsx/)
6. [readxl](https://cran.r-project.org/web/packages/readxl/index.html)
7. [rmarkdown](https://cran.r-project.org/web/packages/rmarkdown/index.html)
8. [testthat](https://cran.r-project.org/web/packages/testthat/index.html)

## How to Load the importThermoEIA Package

To load the importThermoEIA package the user must use a method that
installs R packages from [GitHub](https://github.com/) like
`remotes::install_github()`. The `remotes` package must be installed
first and then the `importThermoEIA` package using the code below.
Note that the `subdir` argument is necessary to load this package
using `remotes::install_github()`. This is a one-time process for
each version of R or version of the `importThermoEIA` package. After
the `importThermoEIA` package is installed, you can use it like any
other R package with `library(importThermoEIA)`.

```{r, echo=TRUE, eval=FALSE}
install.packages("remotes")

# install the importThermoEIA package (one-time process)
remotes::install_github(
    repo = "https://github.com/rniswon/Thermoelectric",
    subdir = "importThermoEIA", build_vignettes = T, upgrade = "never"
)

# load the importThermoEIA package (necessary each time the package is
# used in a given R session)
library(importThermoEIA)
```

When the importThermoEIA package is installed, all prerequisite
packages above will be installed and the process will take a few
minutes, but once the package is installed all dependencies will load
with `library(importThermoEIA)`.

## Documentation for the importThermoEIA Package

Detailed documentation of the importThermoEIA package functions is
included in the package vignette. To view the vignette use the code
below *after* installing the importThermoEIA package and select the
"HTML" option.

```{r echo=TRUE,eval=FALSE}
# to open vignette
library(importThermoEIA)
browseVignettes("importThermoEIA")

# to get all package documentation
??importThermoEIA
```

## Disclaimer

This software is in the public domain because it contains materials
that originally came from the U.S. Geological Survey, an agency of the
United States Department of Interior. For more information, see the
[USGS copyright policy](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits).

Although this software program has been used by the U.S. Geological
Survey (USGS), no warranty, expressed or implied, is made by the USGS
or the U.S. Government as to the accuracy and functioning of the
program and related program material nor shall the fact of
distribution constitute any such warranty, and no responsibility is
assumed by the USGS in connection therewith. This software is provided
"AS IS."
