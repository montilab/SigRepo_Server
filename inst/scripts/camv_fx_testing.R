
library(RMySQL)
library(DBI)
library(devtools)

devtools::install_github("montilab/SigRepo", auth_token = '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')

SigRepo::getSignatures(conn)


SigRepo