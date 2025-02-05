EIAplantAssociation
==========
  
  The `EIAplantAssociation` package includes methods to generate boiler-generator and boiler-generator-cooling associations using data reported to the EIA from thermoelectric plants found here [EIA website](https://www.eia.gov/electricity/).  Data used by the `EIAplantAssociation` package comes from the following EIA forms: Form-923, Form-860, Form-767, Form-906920, and Form-759. Boiler-generator  and boiler-generator-cooling associations are critical to calculating condenser duty at thermoelectric plants because the data required to calculate fuel heat is reported by boiler and net generation is reported by generator. The output of the `EIAplantAssociation` package is formatted to be used by the `condenserDuty` package which calculates monthly condenser duty by cooling type.  The `EIAplantAssociation` package expects the input data used for associations to be formatted according to the `importThermoEIA` package output.

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
  [EIAplantAssociation issues](https://github.com/rniswon/Thermoelectric/issues)

## Required Software and Dependencies

The `EIAplantAssociation` package must be run on a R-version  [4.0](https://cran.microsoft.com/snapshot/2021-01-17/bin/windows/base/old/4.0.0).  Download the latest version R here [R download for Windows](https://cran.r-project.org/bin/windows/base/).  An installation of rtools (either [rtools40](https://github.com/r-windows/rtools-installer/releases/download/2022-02-06/rtools40-x86_64.exe) for R-version 4.0.x-4.1.x or [rtools42](https://cran.microsoft.com/bin/windows/Rtools/rtools42/rtools.html) for R version 4.2.x is required to install dependency packages.  Microsoft Excel® is also required.  The following dependency R-packages listed in the DESCRIPTION file will be installed when `EIAplantAssociation` is installed:
  
1. [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
2. [igraph](https://cran.r-project.org/web/packages/igraph/index.html)
3. [importThermoEIA](https://github.com/rniswon/Thermoelectric/tree/master/importThermoEIA)
4. [knitr](https://cran.r-project.org/web/packages/knitr/index.html)
5. [magrittr](https://cran.r-project.org/web/packages/magrittr/index.html)
6. [plyr](https://cran.microsoft.com/snapshot/2019-03-01/web/packages/plyr/index.html)
7. [rmarkdown](https://cran.r-project.org/web/packages/rmarkdown/index.html)
8. [reshape2](https://cran.r-project.org/web/packages/reshape2/)
9. [testthat](https://cran.r-project.org/web/packages/testthat/index.html)

## How to load the EIAplantAssociation package

To load the `EIAplantAssociation` package the user must use a method that installs R-packages from [GitHub](https://github.com/) like `remotes::install_github()`.  The `remotes` package must be installed first and then the `importThermoEIA` package using the code below.  Note that the `subdir` argument is necessary to load this package using `remotes::install_github()`.  This is a one-time process for each version of R or version of the `EIAplantAssociation` package.  After the `EIAplantAssociation` package is installed, you can use it like any other R-package with `library(EIAplantAssociation)`.

```{r, echo=TRUE, eval=FALSE}
#install the remotes package (one-time process)
install.packages("remotes")

#install the importThermoEIA package (one-time process)
remotes::install_github(repo="https://github.com/rniswon/Thermoelectric",
                        subdir="importThermoEIA",build_vignettes = T,upgrade = "never")
                        
#install the EIAplantAssociation package (one-time process)
remotes::install_github(repo="https://github.com/rniswon/Thermoelectric",
                        subdir="EIAplantAssociation",build_vignettes = T,upgrade = "never")

#load the EIAplantAssociation package (necessary each time the package is used in a given R session)
library(EIAplantAssociation)
```

When the `EIAplantAssociation` package is installed, all dependency packages above will be installed and the process will take a few minutes, but once the package is installed all dependencies will load with `library(EIAplantAssociation)`.

## Documentation for the EIAplantAssociation package

A detailed documentation of the EIAplantAssociation package functions is included in the package vignette.  To view the vignette use the code below AFTER installing the `EIAplantAssociation` package and select the "HTML" option.

```{r echo=TRUE,eval=FALSE}
#to open vignette
library(EIAplantAssociation)
browseVignettes("EIAplantAssociation")

#To get all package documentation
??EIAplantAssociation
```

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright) <br /> <br />
                                                                                                                                                                                                                      Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith. This software is provided "AS IS."