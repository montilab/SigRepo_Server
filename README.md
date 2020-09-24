### last updated
09/24/2020
contact: `vmli@bu.edu`

### installation
`devtools::install_github(repo = "Vanessa104/SigRepoR", auth_token = "...")`  
`library(SigRepoR)`  
`SigRepoR::repoFun(5) # a simple function to test if it’s working`  

### files
* R: functions
* man: manual (documentation of functions)
* NAMESPACE: don't know what it does yet
* DESCRIPTION: description of this package
* README.md: manually created

### notes
Put functions into R folder with documentation (it's okay to have multiple functions in one `.R` file. As long as their documentation are properly formated, each function will have its own `.Rd` file in man folder).  
Run `devtools::document()`, and `.Rd` description will be automatically generated. Do not manually edit `.Rd` files. 
