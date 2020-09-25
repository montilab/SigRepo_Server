# SigRepoR version 0.1.0
## OmicSignature & OmicSignatureCollection

### last update
09/24/2020
contact: `vmli@bu.edu`

### installation
`devtools::install_github(repo = "Vanessa104/SigRepoR", auth_token = "...")`  
`library(SigRepoR)`  
`SigRepoR::testRepo(10) # a simple function to test if it’s working`  

### description
**Please go to "/vignettes/OmicSig-vignette.Rmd" to see the vignettes of _OmicSignature_ and _OmicSignatureCollection_ R6 objects**
As high-throughput technology evolves, a large number of omics-signatures are being derived. This package contains _OmicSignature_ and _OmicSignatureCollection_ R6 objects to store signatures efficiently and retrieve them conveniently.

### development notes
Run `devtools::document()`, and `.Rd` description will be automatically generated. Do not manually edit `.Rd` files.  
Run `devtools::check()` to check the package.  
Run `devtools::build()` to build the package locally.  

