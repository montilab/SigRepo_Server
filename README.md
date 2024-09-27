README
================
Callen & Vanessa
12/10/2020

# SigRepoR

## Contacts

Reina Chau - `rchau88@bu.edu`  
Vanessa Mengze Li - `vmli@bu.edu`

## How to connect to SigRepo database

1.  Load packages

``` r
library(RMySQL)
```

    ## Loading required package: DBI

``` r
library(DBI)
```

2.  Connect to Sigrepo database using RMySQL driver

``` r
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "sigrepo",
  host = "montilab.bu.edu",
  port = 3306,
  username = "guest",
  password = "guest"
)
```

3.  See connection information

``` r
conn_info <- DBI::dbGetInfo(conn)
conn_info
```

    ## $host
    ## [1] "montilab.bu.edu"
    ## 
    ## $user
    ## [1] "guest"
    ## 
    ## $dbname
    ## [1] "sigrepo"
    ## 
    ## $conType
    ## [1] "montilab.bu.edu via TCP/IP"
    ## 
    ## $serverVersion
    ## [1] "8.3.0"
    ## 
    ## $protocolVersion
    ## [1] 10
    ## 
    ## $threadId
    ## [1] 227
    ## 
    ## $rsId
    ## list()

# Database Schema Location:

inst/init/00-sigrepo-initiate-table-schemas.R

# Shiny Application Location:

inst/shiny/app.R

# Plumber API Location:

inst/shiny/sigrepo_api.R
