# example proteomics analysis to generate differential abunbance data. 

if (!requireNamespace("BiocManager", quietly=TRUE))
  install.packages("BiocManager")
BiocManager::install("DEP")

library("DEP")

library(dplyr)

data <- UbiLength

data <- filer(data, Reverse != "+" & Potential.contaminant != "+")
