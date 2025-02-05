towerWaterUse
==========
  
  The `towerWaterUse` package .....

### Reporting bugs

Please consider reporting bugs and asking questions on the Issues page:
  [importThermoEIA issues](https://github.com/rniswon/Thermoelectric/issues)

## Required Software and Dependencies

The `towerWaterUse` package must be run on a R-version  [4.0](https://cran.microsoft.com/snapshot/2021-01-17/bin/windows/base/old/4.0.0).  Download the latest version R here [R download for Windows](https://cran.r-project.org/bin/windows/base/).  An installation of rtools (either [rtools40](https://github.com/r-windows/rtools-installer/releases/download/2022-02-06/rtools40-x86_64.exe) for R-version 4.0.x-4.1.x or [rtools42](https://cran.microsoft.com/bin/windows/Rtools/rtools42/rtools.html) for R version 4.2.x is required to install dependency packages.  Microsoft Excel® is also required.  The following dependency R-packages listed in the DESCRIPTION file will be installed when `towerWaterUse` is installed:
  
fill this out

## How to load the towerWaterUse package

To load the `towerWaterUse` package the user must use a method that installs R-packages from [GitHub](https://github.com/) like `remotes::install_github()`.  The `remotes` package must be installed first and then the `towerWaterUse` package using the code below.  Note that the `subdir` argument is necessary to load this package using `remotes::install_github()`.  This is a one-time process for each version of R or version of the `towerWaterUse` package.  After the `towerWaterUse` package is installed, you can use it like any other R-package with `library(towerWaterUse)`.

```{r, echo=TRUE, eval=FALSE}
#install the remotes package (one-time process)
install.packages("remotes")

#install the towerWaterUse package (one-time process)
remotes::install_github(repo="https://github.com/rniswon/Thermoelectric",
                        subdir="towerWaterUse",build_vignettes = T,upgrade = "never")

#load the towerWaterUse package (necessary each time the package is used in a given R session)
library(towerWaterUse)
```

When the `towerWaterUse` package is installed, all dependency packages above will be installed and the process will take a few minutes, but once the package is installed all dependencies will load with `library(towerWaterUse)`.

## Documentation for the towerWaterUse package

A detailed documentation of the towerWaterUse package functions is included in the package vignette.  To view the vignette use the code below AFTER installing the `towerWaterUse` package and select the "HTML" option.

```{r echo=TRUE,eval=FALSE}
#to open vignette
library(towerWaterUse)
browseVignettes("towerWaterUse")

#To get all package documentation
??towerWaterUse
```

## Disclaimer

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the official USGS copyright policy at [https://www.usgs.gov/visual-id/credit_usgs.html#copyright](https://www.usgs.gov/visual-id/credit_usgs.html#copyright) <br /> <br />
                                                                                                                                                                                                                      Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith. This software is provided "AS IS."