
# SigRepo

## Contacts

Reina Chau - `rchau88@bu.edu`  
Vanessa Mengze Li - `vmli@bu.edu`  
Stefano Monti - `smonti@bu.edu`

# Installation

- Using `devtools` package

``` r
library(devtools)
devtools::install_github("montilab/SigRepo")
```

# How to connect to SigRepo database

1.  Load packages

``` r
library(RMySQL)
library(DBI)
library(SigRepo)
```

2.  Connect to SigRepo database using RMySQL driver for ‘guest’ access

``` r
conn <- SigRepo::newConnHandler(
  driver = RMySQL::MySQL(),
  dbname = "sigrepo",
  host = "montilab.bu.edu",
  port = 3306,
  user = "guest",
  password = "guest"
)
```

3.  See user connection information

``` r
conn_info <- DBI::dbGetInfo(conn)
conn_info
$host
[1] "montilab.bu.edu"

$user
[1] "guest"

$dbname
[1] "sigrepo"

$conType
[1] "montilab.bu.edu via TCP/IP"

$serverVersion
[1] "8.3.0"

$protocolVersion
[1] 10

$threadId
[1] 124

$rsId
list()
```

# Database Schemas are stored at:

inst/init/00-sigrepo-initiate-table-schemas.R

# Shiny Application is stored at:

inst/shiny/app.R

# Plumber API is stored at:

inst/shiny/sigrepo_api.R
