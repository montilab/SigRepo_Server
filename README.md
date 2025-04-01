
# SigRepo

The `SigRepo` package includes a suite of functions for easily storing
and managing biological signatures and its constituents. Currently,
`Sigrepo` is capable of storing, searching, and retrieving signatures
and its signature collections from a MySQL Database of choice. See
<a href="">here</a> for how set-up the MySQL database with the
appropriate schema.

In order to interact with a suite of functions in `SigRepo` package, the
input data must represent an R6 object for the representation of
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

# Installation

- Using `devtools` package

<!-- -->

    # Load devtools package
    library(devtools)

    # Install SigRepo
    devtools::install_github(repo = 'montilab/SigRepo')

    # Install OmicSignature
    devtools::install_github(repo = 'montilab/OmicSignature')

    # Load tidyverse package
    library(tidyverse)

    # Load SigRepo package
    library(SigRepo)

    # Load OmicSignature package
    library(OmicSignature)

- Using `Github` clone

1.  Clone `SigRepo` repository to your \$HOME directory

<!-- -->

    cd $HOME
    git clone https://github.com/montilab/SigRepo

2.  Open **SigRepo.Rproj** file with your **RStudio Server**

3.  Load `SigRepo` package with `devtools`

<!-- -->

    # Load devtools package
    library(devtools)

    # Load SigRepo package
    devtools::load_all()

    # Load tidyverse package
    library(tidyverse)

    # Load OmicSignature package
    library(OmicSignature)

# Connect to SigRepo Database

We adopt a MySQL Database structure for efficiently storing, searching,
and retrieving the biological signatures and its constituents. To access
the signatures stored in our database, you <a href="">MUST register
here</a> to create an account or <a href="">contact our admin</a> to be
added.

There are three types of user accounts:<br> - `admin` has <b>READ</b>
and <b>WRITE</b> access to all signatures in the database.<br> -
`editor` has <b>READ</b> and <b>WRITE</b> access to ONLY their own
uploaded signatures in the database.<br> - `viewer` has <b>ONLY READ</b>
access to see a list of signatures that are publicly available in the
database but <b>DO NOT HAVE WRITE</b> access to the database.<br>

Once you have a valid account, to connect to our SigRepo database, one
can use the `newConnHandler()` function to create a handler which
contains user credentials to establish connection to our database.

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

Here, we provide two signature objects that comes with the package for
demonstrations:

1.  omic_signature_AGS_OmS (**LLFS_Aging_Gene_2023**)
2.  omic_signature_MDA_CYP (**CYP181 knockdown in breast cancer cell
    line**)

``` r
# Getting the signature path
signature_path <- base::system.file("inst/data/signatures", package = "SigRepo")

# Read in the signature object
omic_signature_AGS_OmS <- base::readRDS(base::file.path(signature_path, "omic_signature_AGS_OmS.RDS"))
omic_signature_MDA_CYP <- base::readRDS(base::file.path(signature_path, "omic_signature_MDA_CYP.RDS"))
```

# Upload a signature

The `addSignature()` function allows users to upload a signature to the
database.

**IMPORTANT NOTE:**

- The user **MUST HAVE** an `editor` or `admin` account to use this
  function.
- A signature **MUST BE** an R6 object obtained from
  **OmicSignature::OmicSignature()**

## **Example 1**: Create an omic signature using **OmicSignature** package and upload to the database

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
  visibility = FALSE,                 # Whether to make signature public or private. Default is FALSE.
  return_signature_id = FALSE,        # Whether to return the uploaded signature id. Default is FALSE.
  verbose = TRUE                      # Whether to print diagnostic messages. Default is TRUE.
)
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> now dyn.load("/usr/local/lib/R/site-library/curl/libs/curl.so") ...
#> Adding user to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 102
```

## **Example 2**: Upload `omic_signature_AGS_OmS` signature

``` r
SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = omic_signature_AGS_OmS
)
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> Adding user to the signature access table of the database...
#> Adding signature feature set to the database...
#> Finished uploading.
#> ID of the uploaded signature: 103
```

## **Example 3**: Upload `omic_signature_MDA_CYP` signature

``` r
SigRepo::addSignature(
  conn_handler = conn_handler, 
  omic_signature = omic_signature_MDA_CYP,
  return_missing_features = TRUE                # Whether to return a list of missing features during upload. Default is FALSE.
)
#> Uploading signature metadata to the database...
#> Saving difexp to the database...
#> Adding user to the signature access table of the database...
#> Adding signature feature set to the database...
#> Warning in SigRepo::showTranscriptomicsErrorMessage(db_table_name = ref_table, : 
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
#>                       table  unknown_values
#> 1  transcriptomics_features ENSG00000281508
#> 2  transcriptomics_features ENSG00000199900
#> 3  transcriptomics_features ENSG00000247844
#> 4  transcriptomics_features ENSG00000258777
#> 5  transcriptomics_features ENSG00000230836
#> 6  transcriptomics_features ENSG00000204282
#> 7  transcriptomics_features ENSG00000179979
#> 8  transcriptomics_features ENSG00000198384
#> 9  transcriptomics_features ENSG00000277203
#> 10 transcriptomics_features ENSG00000250889
#> 11 transcriptomics_features ENSG00000170647
#> 12 transcriptomics_features ENSG00000276797
#> 13 transcriptomics_features ENSG00000237975
#> 14 transcriptomics_features ENSG00000241990
#> 15 transcriptomics_features ENSG00000155640
#> 16 transcriptomics_features ENSG00000199404
#> 17 transcriptomics_features ENSG00000230641
#> 18 transcriptomics_features ENSG00000227895
#> 19 transcriptomics_features ENSG00000150526
#> 20 transcriptomics_features ENSG00000277555
#> 21 transcriptomics_features ENSG00000274744
#> 22 transcriptomics_features ENSG00000250588
#> 23 transcriptomics_features ENSG00000223414
#> 24 transcriptomics_features ENSG00000184258
#> 25 transcriptomics_features ENSG00000228265
#> 26 transcriptomics_features ENSG00000146521
#> 27 transcriptomics_features ENSG00000232224
#> 28 transcriptomics_features ENSG00000256045
#> 29 transcriptomics_features ENSG00000240875
#> 30 transcriptomics_features ENSG00000182584
#> 31 transcriptomics_features ENSG00000239332
#> 32 transcriptomics_features ENSG00000186354
#> 33 transcriptomics_features ENSG00000200649
#> 34 transcriptomics_features ENSG00000225163
#> 35 transcriptomics_features ENSG00000255145
#> 36 transcriptomics_features ENSG00000228439
#> 37 transcriptomics_features ENSG00000201126
#> 38 transcriptomics_features ENSG00000225986
#> 39 transcriptomics_features ENSG00000238648
#> 40 transcriptomics_features ENSG00000228393
#> 41 transcriptomics_features ENSG00000112096
#> 42 transcriptomics_features ENSG00000170590
#> 43 transcriptomics_features ENSG00000269028
#> 44 transcriptomics_features ENSG00000280524
#> 45 transcriptomics_features ENSG00000238266
#> 46 transcriptomics_features ENSG00000235825
#> 47 transcriptomics_features ENSG00000243587
#> 48 transcriptomics_features ENSG00000203441
#> 49 transcriptomics_features ENSG00000207770
#> 50 transcriptomics_features ENSG00000132832
#> 51 transcriptomics_features ENSG00000236850
#> 52 transcriptomics_features ENSG00000235884
#> 53 transcriptomics_features ENSG00000249860
#> 54 transcriptomics_features ENSG00000215271
#> 55 transcriptomics_features ENSG00000256164
#> 56 transcriptomics_features ENSG00000215067
#> 57 transcriptomics_features ENSG00000223797
#> 58 transcriptomics_features ENSG00000244349
#> 59 transcriptomics_features ENSG00000208035
#> 60 transcriptomics_features ENSG00000255090
#> 61 transcriptomics_features ENSG00000242349
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

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_id | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 56 | Myc_reduce_mice_liver_24m_v1 | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPL6246 | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-04-01 13:26:04 | 0 | d6f0247a37eec0e3a6b6285dc7394d88 |
| 62 | Myc_reduce_mice_liver_24m_v2 | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPL6246 | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-04-01 13:44:05 | 0 | e67a6b45899dc0f8d7eaa2cedec4d622 |
| 64 | Myc_reduce_mice_liver_24m_v3 | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPL6246 | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-04-01 14:09:49 | 0 | 3a6d590d99bdf0ab38c742b1d6dc5ac9 |
| 89 | Myc_reduce_mice_liver_24m_v4 | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPL6246 | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-04-01 15:55:07 | 0 | f08919100db811a8b117ce6602dd31e4 |
| 102 | Myc_reduce_mice_liver_24m | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPL6246 | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-04-01 19:25:55 | 0 | 1873c11dbbd138281361bed28899c17d |
| 103 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | GPLXXXXX | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 11159 | 882 | 957 | montilab | 2025-04-01 19:25:56 | 0 | b35b7c1d387440d474bfcb3cb162c9a6 |

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

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_id | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 103 | LLFS_Aging_Gene_2023 | Homo sapiens | bi-directional | transcriptomics | Aging | GPLXXXXX | blood | sex,fc,education,percent_intergenic,PC1-4,GRM | NA | 6 | NA | NA | 0.01 | NA | human,aging,LLFS | NA | 2023 | NA | 1 | 11159 | 882 | 957 | montilab | 2025-04-01 19:25:56 | 0 | b35b7c1d387440d474bfcb3cb162c9a6 |

# Retrieve a list of omic signatures

The `getSignature()` function allows users to retrieve a list of omic
signature objects that they are **PUBLICLY** available in the database.

**IMPORTANT NOTE:**

- Users **MUST HAVE** an `editor` or `admin` account to use this
  function.
- Users can **ONLY RETRIEVE** a list of signatures that are publicly
  available in the database including their own uploaded signatures.
- If a signature is `PRIVATE` and belongs to other user in the database,
  users will need to be given an `editor` permission from its owner in
  order to access, retrieve, and edit their signatures.

## Example 1: Retrieve all signatures that are publicly available or owned by the user in the database

``` r
signature_list <- SigRepo::getSignature(conn_handler = conn_handler)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v1 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v2 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v3 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m_v4 created.
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
```

## Example 2: Retrieve a specific signature that is publicly available or owned by the user in the database, e.g., **signature_name = “LLFS_Aging_Gene_2023”**

``` r
LLFS_oms <- SigRepo::getSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)
#>   [Success] OmicSignature object LLFS_Aging_Gene_2023 created.
```

# Delete a signature

The `deleteSignature()` function allows users to delete a signature from
the database.

**IMPORTANT NOTE:**

- Users **MUST HAVE** an `editor` or `admin` account to use this
  function.
- Users can **ONLY DELETE** their own uploaded signatures or were given
  an `editor` permission from its owner to access, retrieve, and edit
  their signatures.
- Users can **ONLY DELETE** a signature one at a time.

**For example:** You want to remove **signature_name =
“LLFS_Aging_Gene_2023”** from the database.

``` r
# 1. Let's search for signature_name = "LLFS_Aging_Gene_2023" in the database
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = "LLFS_Aging_Gene_2023"
)

# 2. If the signature exists, remove it from the database
if(nrow(signature_tbl) > 0){
  SigRepo::deleteSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id
  )
}
#> Remove signature_id = '103' from 'signatures' table of the database.
#> Remove features belongs to signature_id = '103' from 'signature_feature_set' table of the database.
#> Remove user access to signature_id = '103' from 'signature_access' table of the database.
#> Remove signature_id = '103' from 'signature_collection_access' table of the database.
#> signature_id = '103' has been removed.
```

# Update a signature

The `updateSignature()` function allows users to update a specific
signature in the SigRepo database.

**IMPORTANT NOTE:**

- Users **MUST HAVE** an `editor` or `admin` account to use this
  function.
- Users can **ONLY UPDATE** their own uploaded signatures or were given
  an `editor` permission from its owner to access, retrieve, and edit
  their signatures.
- Users can **ONLY UPDATE** a signature one at a time.

**For example:** If the `platform` information in the previous uploaded
signature, **“Myc_reduce_mice_liver_24m”**, is incorrect, and you wish
to update the `platform` information with the correct value, e.g.,
**platform = “GPLXXXXX”**. You can use the `updateSignature()` function
as follows:

``` r
# 1. Revise the metadata object with new platform = GPLXXXXX
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

# 2. Create difexp object
difexp <- readRDS(file.path(system.file("extdata", package = "OmicSignature"), "difmatrix_Myc_mice_liver_24m.rds")) %>% dplyr::rename(feature_name = ensembl)
colnames(difexp) <- OmicSignature::replaceDifexpCol(colnames(difexp))

# 3. Create signature object
signature <- difexp %>%
  dplyr::filter(abs(score) > metadata_revised$score_cutoff & adj_p < metadata_revised$adj_p_cutoff) %>%
  dplyr::select(probe_id, feature_name, score) %>%
  dplyr::mutate(direction = ifelse(score > 0, "+", "-"))

# 4. Create the updated OmicSignature object
updated_omic_signature <- OmicSignature::OmicSignature$new(
  signature = signature,
  metadata = metadata_revised,
  difexp = difexp
)
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
```

``` r
# Now, let's search for Myc_reduce_mice_liver_24m in the database
# in which we would like to revise the value of platform to GPLXXXXX
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = metadata_revised$signature_name
)

# Update the signature with the revised omic_signature object
if(nrow(signature_tbl) > 0){
  SigRepo::updateSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id, 
    omic_signature = updated_omic_signature
  )
}
#>   [Success] OmicSignature object Myc_reduce_mice_liver_24m created.
#>  signature_id = '102' has been updated.
```

Finally, let’s look up **signature_name = “Myc_reduce_mice_liver_24m”**
and see if the value of `platform` has been changed.

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

| signature_id | signature_name | organism | direction_type | assay_type | phenotype | platform_id | sample_type | covariates | description | score_cutoff | logfc_cutoff | p_value_cutoff | adj_p_cutoff | cutoff_description | keywords | PMID | year | others | has_difexp | num_of_difexp | num_up_regulated | num_down_regulated | user_name | date_created | visibility | signature_hashkey |
|---:|:---|:---|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|---:|:---|:---|---:|---:|:---|---:|---:|---:|---:|:---|:---|---:|:---|
| 102 | Myc_reduce_mice_liver_24m | Mus musculus | bi-directional | transcriptomics | Myc_reduce | GPLXXXXX | liver | none | mice MYC reduced expression | 5 | NA | NA | 0.05 | NA | Myc,KO,longevity | 25619689 | 2015 | animal_strain: \<C57BL/6\> | 1 | 884 | 5 | 10 | montilab | 2025-04-01 19:26:03 | 0 | 1873c11dbbd138281361bed28899c17d |

# Additional Guides

- [How to install SigRepo via
  Docker](https://montilab.github.io/SigRepo/articles/install-sigrepo-locally.html)
