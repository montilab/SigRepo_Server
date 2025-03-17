
# SigRepo

The `SigRepo` package includes a suite of functions for easily storing
and managing biological signatures and its constituents. Currently,
`Sigrepo` is capable of storing, searching, and retrieving signatures
and its signature collections from a MySQL Database of choice. See
<a href="">here</a> for documentation of how set-up the MySQL database
with the appropriate schema.

In order to interact with a suite of functions in `SigRepo` package, the
input data must be in a format of R6 objects for the representation of
signatures and signature collections, and they can be created using our
proprietary package,
<a href="https://github.com/montilab/OmicSignature">OmicSignature</a>.

For more information click the links below.

- [Overview of the object
  structure](https://montilab.github.io/OmicSignature/articles/ObjectStructure.html)
- [Create an OmicSignature
  (OmS)](https://montilab.github.io/OmicSignature/articles/CreateOmS.html)
- [Create an OmicSignatureCollection
  (OmSC)](https://montilab.github.io/OmicSignature/articles/CreateOmSC.html)

For demonstrations, we will walk through the steps of how to use
`SigRepo` package to store, retrieve, and interact with a list of
signatures stored in our MySQL SigRepo Database.

# Contacts

Reina Chau - `rchau88@bu.edu` <br>  
Cameron Vicnaire - `camv@bu.edu` <br> Vanessa Mengze Li - `vmli@bu.edu`
<br> Stefano Monti - `smonti@bu.edu` <br>

# Installation

- Using `devtools` package

``` r
# Load devtools package
library(devtools)

# Install SigRepo
devtools::install_github(repo = 'montilab/SigRepo')

# Install OmicSignature
devtools::install_github(repo = 'montilab/OmicSignature')
```

# Load packages

    # Load tidyverse package
    library(tidyverse)

    # Load SigRepo package
    library(SigRepo)

    # Load OmicSignature package
    library(OmicSignature)

# Connect to SigRepo Database

We adopted a MySQL Database structure for efficiently storing,
searching, and retrieving the biological signatures and its
constituents. To access the signatures stored in our database, you MUST
<a href="">register here</a> to create an account or contact our
<a href="">admin</a> to be added.

There are three types of user accounts:<br> - `admin` has <b>READ</b>
and <b>WRITE</b> access to all signatures in the database.<br> -
`editor` has <b>READ</b> and <b>WRITE</b> access to ONLY their own
uploaded signatures in the database.<br> - `viewer` has <b>ONLY READ</b>
access to see a list of signatures in the database but <b>DO NOT HAVE
WRITE</b> access to the database.<br>

Once you have a valid account, to connection to our SigRepo Database,
one can use `newConnHandler()` function to create a handler which will
contain appropriate credentials to establish connection to our database.

``` r
# Create a connection handler
conn_handler <- SigRepo::newConnHandler(
  dbname = "sigrepo", 
  host = "142.93.67.157", 
  port = 3306, 
  user = "montilab", 
  password = "sigrepo"
)
```

# Load Signatures

Here, we provided two signature objects that came with the package for
demonstrations:

1.  omic_signature_AGS_OmS
2.  omic_signature_MDA_CYP

``` r
# Getting the signature path
signature_path <- base::system.file("inst/data/signatures", package = "SigRepo")

# Read in the signature object
omic_signature_AGS_OmS <- base::readRDS(base::file.path(signature_path, "omic_signature_AGS_OmS.RDS"))
omic_signature_MDA_CYP <- base::readRDS(base::file.path(signature_path, "omic_signature_MDA_CYP.RDS"))
```

# Upload a signature

The function `addSignature()` allows users to upload a signature to the
database.

**IMPORTANT NOTE:** The user **MUST HAVE** an `editor` or `admin` access
to use this function.

## **Example 1**: Create an omic signature using **OmicSignature** package and upload to database

``` r
# Create signature metadata
metadata <- base::list(
  # required attributes:
  signature_name = "Myc_reduce_mice_liver_24m",
  organism = "Mus Musculus",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = "Myc_reduce",
  
  # optional and recommended:
  covariates = "none",
  description = "mice MYC reduced expression",
  platform = "GPL6246", # use GEO platform ID
  sample_type = "liver", # use BRENDA ontology
  
  # optional cut-off attributes.
  # specifying them can facilitate the extraction of signatures.
  logfc_cutoff = NULL,
  p_value_cutoff = NULL,
  adj_p_cutoff = 0.05,
  score_cutoff = 5,
  
  # other optional built-in attributes:
  keywords = c("Myc", "KO", "longevity"),
  cutoff_description = NULL,
  author = NULL,
  PMID = 25619689,
  year = 2015,
  
  # example of customized attributes:
  others = list("animal_strain" = "C57BL/6")
)

# Create difexp object
difexp <- base::readRDS(file.path(system.file("extdata", package = "OmicSignature"), "difmatrix_Myc_mice_liver_24m.rds")) %>% dplyr::rename(feature_name = ensembl)
colnames(difexp) <- OmicSignature::replaceDifexpCol(colnames(difexp))

# Create signature object
signature <- difexp %>%
  dplyr::filter(abs(score) > metadata$score_cutoff & adj_p < metadata$adj_p_cutoff) %>%
  dplyr::select(probe_id, feature_name, score) %>%
  dplyr::mutate(direction = ifelse(score > 0, "+", "-"))

# Create signature object 
omic_signature <- OmicSignature::OmicSignature$new(
  metadata = metadata,
  signature = signature,
  difexp = difexp
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
```

``` r

# Add signature to database
SigRepo::addSignature(
  conn_handler = conn_handler,        # A handler contains user credentials to establish connection to a remote database
  omic_signature = omic_signature,    # An R6 object obtained from OmicSignature::OmicSignature()
  return_signature_id = FALSE,        # Whether to return the uploaded signature id
  verbose = TRUE                      # Whether to print diagnostic messages
)
#> Uploading signature metadata into the database...
#> Saving difexp to the database...
#> now dyn.load("/usr/local/lib/R/site-library/curl/libs/curl.so") ...
#> Adding user to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 12
```

## **Example 2**: Upload `omic_signature_AGS_OmS` signature

``` r
SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = omic_signature_AGS_OmS
)
#>  You already uploaded a signature with the name = 'LLFS_Aging_Gene_2023' to the SigRepo Database.
#>  ID of the uploaded signature: 3
```

## **Example 3**: Upload `omic_signature_MDA_CYP` signature

``` r
SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = omic_signature_MDA_CYP
)
#> Uploading signature metadata into the database...
#> Saving difexp to the database...
#> Adding user to the signature access table of the database...
#> Adding signature feature set to the database...
#> Error in value[[3L]](cond): Error in SigRepo::showTranscriptomicsErrorMessage(db_table_name = ref_table, : 
#> The following features do not existed in the 'transcriptomics_features' table of the database:
#> 'ENSG00000281508'
#> 'ENSG00000199900'
#> 'ENSG00000247844'
#> 'ENSG00000258777'
#> 'ENSG00000230836'
#> 'ENSG00000204282'
#> 'ENSG00000179979'
#> 'ENSG00000198384'
#> 'ENSG00000277203'
#> 'ENSG00000250889'
#> 'ENSG00000170647'
#> 'ENSG00000276797'
#> 'ENSG00000237975'
#> 'ENSG00000241990'
#> 'ENSG00000155640'
#> 'ENSG00000199404'
#> 'ENSG00000230641'
#> 'ENSG00000227895'
#> 'ENSG00000150526'
#> 'ENSG00000277555'
#> 'ENSG00000274744'
#> 'ENSG00000250588'
#> 'ENSG00000223414'
#> 'ENSG00000184258'
#> 'ENSG00000228265'
#> 'ENSG00000146521'
#> 'ENSG00000232224'
#> 'ENSG00000256045'
#> 'ENSG00000240875'
#> 'ENSG00000182584'
#> 'ENSG00000239332'
#> 'ENSG00000186354'
#> 'ENSG00000200649'
#> 'ENSG00000225163'
#> 'ENSG00000255145'
#> 'ENSG00000228439'
#> 'ENSG00000201126'
#> 'ENSG00000225986'
#> 'ENSG00000238648'
#> 'ENSG00000228393'
#> 'ENSG00000112096'
#> 'ENSG00000170590'
#> 'ENSG00000269028'
#> 'ENSG00000280524'
#> 'ENSG00000238266'
#> 'ENSG00000235825'
#> 'ENSG00000243587'
#> 'ENSG00000203441'
#> 'ENSG00000207770'
#> 'ENSG00000132832'
#> 'ENSG00000236850'
#> 'ENSG00000235884'
#> 'ENSG00000249860'
#> 'ENSG00000215271'
#> 'ENSG00000256164'
#> 'ENSG00000215067'
#> 'ENSG00000223797'
#> 'ENSG00000244349'
#> 'ENSG00000208035'
#> 'ENSG00000255090'
#> 'ENSG00000242349'
#> You can use 'searchFeature()' to see a list of available features in the database.
#> To add these features to our database, please contact our admin for support.
```

# Search for a list of signatures

The `searchSignature()` function allows users to search for all or a
specific set of signatures that are available in the database.

## Example 1: Search for all signatures

``` r
signature_tbl <- SigRepo::searchSignature(conn_handler = conn_handler)

if(nrow(signature_tbl) > 0){
  knitr::kable(
    signature_tbl, 
    row.names = FALSE
  )
}
```

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_id | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|:---|
| 1 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | GPLXXXXX | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 11159 | 882 | 957 | root | 2025-03-17 16:55:45 | 7fdecac66691beed1703efc25487768c |
| 3 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | GPLXXXXX | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 11159 | 882 | 957 | montilab | 2025-03-17 16:56:20 | b35b7c1d387440d474bfcb3cb162c9a6 |
| 5 | Myc_reduce_mice_liver_24m_v1 | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPL6246 | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-03-17 17:02:10 | d6f0247a37eec0e3a6b6285dc7394d88 |
| 6 | Myc_reduce_mice_liver_24m_v2 | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPL6246 | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-03-17 17:02:11 | e67a6b45899dc0f8d7eaa2cedec4d622 |
| 12 | Myc_reduce_mice_liver_24m | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPL6246 | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-03-17 17:17:21 | 1873c11dbbd138281361bed28899c17d |

## Example 2: Search for a specific signature, e.g., **signature_name = “LLFS_Aging_Gene_2023”**.

``` r
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)

if(nrow(signature_tbl) > 0){
  knitr::kable(
    signature_tbl, 
    row.names = FALSE
  )
}
```

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_id | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|:---|
| 1 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | GPLXXXXX | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 11159 | 882 | 957 | root | 2025-03-17 16:55:45 | 7fdecac66691beed1703efc25487768c |
| 3 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | GPLXXXXX | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 11159 | 882 | 957 | montilab | 2025-03-17 16:56:20 | b35b7c1d387440d474bfcb3cb162c9a6 |

# Retrieve a list of omic signatures

The `getSignature()` function allows users to retrieve a list of omic
signature objects that they previously uploaded to the database.

**IMPORTANT NOTE:**

- The user **MUST HAVE** an `editor` or `admin` access to use this
  function.
- Furthermore, the user can **ONLY RETRIEVE** their own uploaded
  signatures or was given an `editor` permission from other users in the
  database to access their signatures.

## Example 1: Retrieve all signatures

``` r
signature_list <- SigRepo::getSignature(conn_handler = conn_handler)
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v1 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v2 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
```

## Example 2: Retrieve a specific signature, e.g., **signature_name = “LLFS_Aging_Gene_2023”**

``` r
LLFS_oms <- SigRepo::getSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
```

# Update a signature

The `updateSignature()` function allows users to update a specific
signature in the SigRepo database.

**IMPORTANT NOTE:**

- The user **MUST HAVE** an `editor` or `admin` access to use this
  function.
- Furthermore, the user can **ONLY UPDATE** their own uploaded
  signatures or was given an `editor` permission from other users in the
  database to access and edit their signatures.

**For example:** If the `platform` information in the previous uploaded
signature, **“Myc_reduce_mice_liver_24m”**, is incorrect, and you wish
to update the platform information with a correct value, e.g.,
**platform = “GPLXXXXX”**. You can use the `updateSignature()` function
as follows:

``` r
# Revise the metadata object with new platform = GPLXXXXX
metadata_revised <- base::list(
  # required attributes:
  signature_name = "Myc_reduce_mice_liver_24m",
  organism = "Mus Musculus",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = "Myc_reduce",
  
  # optional and recommended:
  covariates = "none",
  description = "mice MYC reduced expression",
  platform = "GPLXXXXX", # use GEO platform ID
  sample_type = "liver", # use BRENDA ontology
  
  # optional cut-off attributes.
  # specifying them can facilitate the extraction of signatures.
  logfc_cutoff = NULL,
  p_value_cutoff = NULL,
  adj_p_cutoff = 0.05,
  score_cutoff = 5,
  
  # other optional built-in attributes:
  keywords = c("Myc", "KO", "longevity"),
  cutoff_description = NULL,
  author = NULL,
  PMID = 25619689,
  year = 2015,
  
  # example of customized attributes:
  others = list("animal_strain" = "C57BL/6")
)

# Create difexp object
difexp <- readRDS(file.path(system.file("extdata", package = "OmicSignature"), "difmatrix_Myc_mice_liver_24m.rds")) %>% dplyr::rename(feature_name = ensembl)
colnames(difexp) <- OmicSignature::replaceDifexpCol(colnames(difexp))

# Create signature object
signature <- difexp %>%
  dplyr::filter(abs(score) > metadata_revised$score_cutoff & adj_p < metadata_revised$adj_p_cutoff) %>%
  dplyr::select(probe_id, feature_name, score) %>%
  dplyr::mutate(direction = ifelse(score > 0, "+", "-"))

# Create the updated OmicSignature object
updated_omic_signature <- OmicSignature::OmicSignature$new(
  signature = signature,
  metadata = metadata_revised,
  difexp = difexp
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
```

``` r
# Now search for Myc_reduce_mice_liver_24m in the database
# in which we would like to revise the value of platform to GPLXXXXX
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = metadata_revised$signature_name
)

# Updating the signature with the revised omic_signature object
if(nrow(signature_tbl) > 0){
  SigRepo::updateSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id, 
    omic_signature = updated_omic_signature
  )
}
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
#>  signature_id = '12' has been updated.
```

Now look up **signature_name = “Myc_reduce_mice_liver_24m”** and see if
the value of platform has been changed.

``` r
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "Myc_reduce_mice_liver_24m"
)

if(nrow(signature_tbl) > 0){
  knitr::kable(
    signature_tbl,
    row.names = FALSE
  )
}
```

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_id | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|:---|
| 12 | Myc_reduce_mice_liver_24m | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPLXXXXX | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-03-17 17:17:27 | 1873c11dbbd138281361bed28899c17d |

# Delete a signature

The `deleteSignature()` function allows users to delete a signature from
the database.

**IMPORTANT NOTE:**

- The user **MUST HAVE** an `editor` or `admin` access to use this
  function.
- Furthermore, the user can **ONLY DELETE** their own uploaded
  signatures or was given an `editor` permission from other users to
  access and delete their signatures.

**For example:** You want to remove **signature_name =
“Myc_reduce_mice_liver_24m”** from the database.

``` r
# Search for Myc_reduce_mice_liver_24m in the database and remove it
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "Myc_reduce_mice_liver_24m"
)

# Remove signature from the database
if(nrow(signature_tbl) > 0){
  SigRepo::deleteSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id
  )
}
#> Remove signature_id = '12' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '12' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '12' from 'signature_access' table of the database.
#> Remove signature_id = '12' from 'signature_collection_access' table of the database.
#> signature_id = '12' has been removed.
```
