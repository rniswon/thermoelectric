# install required packages, only need to do this part once.
install.packages("here")
install.packages("dplyr")
install.packages("purrr")
install.packages("qdapTools")
install.packages("igraph")
install.packages("sjmisc") #not available for 3.3.0
install.packages("reshape2")
install.packages("RColorBrewer")
install.packages("ggmap")
install.packages("ggplot2") #non zero exit status
install.packages("knitr")
install.packages("testthat")
install.packages("drake")
install.packages("devtools") # trying an older versoon 

install.packages("cachem")
install.packages("memoise")
install.packages("git2r")
install.packages("devtools", repo = 'devtools_1.13.6.tar.gz')
packageurl <- "https://cran.r-project.org/src/contrib/Archive/devtools/devtools_1.13.6.tar.gz"
install.packages(packageurl, repos=NULL, type="source")


# load the packages, need to do this every time you open up R
library(assertthat)
library(data.table)
library(fastmatch)
library(fpCompare)
library(gWidgets2)
library(gWidgets2tcltk)
library(ggplot2)
library(grDevices)
library(openxlsx)
library(readxl)
library(rio)
library(stringi)
library(tcltk)
library(utils)
library(here)
library(maps)

# 5/6/21
install.packages('tidyverse')
