README
================
Callen & Vanessa
April 14, 2021

SigRepoR
========

### Last update

April 14, 2021

Contacts
--------

Callen Bragdon - `cjoseph@bu.edu`

Vanessa Mengze Li - `vmli@bu.edu`

SigRepo Components
------------------

-   Back-end: MariaDB
-   R functions to interact with DB: **This package `SigRepoR`**. See *Installation* below.
-   R6 objects: `OmicSignature` and `OmicSignatureCollection` designed to store signatures. See *Installation* below. Click [here for the vignette](https://montilab.github.io/OmicSignature/articles/OmicSig_vignette.html)
-   User-control API
-   Front-end: R-Shiny interface

*See our development [Shiny app server](http://sigrepo.bu.edu:3838/app) (BU VPN required)*

*Installation*
--------------

`devtools::install_github(repo = "montilab/OmicSignature")`

`devtools::install_github(repo = "montilab/SigRepoR", auth_token = "...")`

------------------------------------------------------------------------

Manual of uploading signatures into Database
--------------------------------------------

#### Configuration and Setup

Before proceeding with any interactions, it's important that you first configure your session to point to which database to upload to, along with where to write files to in your file system.

``` r
configureSigRepo(
  signatureDirectory="/opt/shiny-server/challenge_project/miscellanea/signatures/",
  databaseServer="sigrepo.bu.edu",
  databasePort="4253",
  applicationPort="",
  signatureServer="sigrepo.bu.edu"
)
```

Now, for downstream queries, you'll be able to establish connections in the future without needing to specify which server to query repeatedly.

The final step in your setup will be to establish your connection handle,
described below:

```r
myHandle <- newConnHandle("cjoseph", usePassword="NO")
```

You will be prompted to (safely) enter your password in another dialogue
window in order to establish a connection to your database.

However, if you don't need any particular permissions, and just want to 
"read only", you can create a handle with the "guest" account, which only 
has read privileges and are the default parameters for establishing this handle, shown below.

```r
myHandle <- newConnHandle()
```

For certain R querying functions in this package, the guest account is
used by default, and the handle disconnects when accomplishing the query
by default. 


Assuming you already have your OmicSignature object created, let's work with uploading.

#### Uploading OmicSignature Object

To upload your object completely to your back-end

``` r
addSignatureWrapper(
    yourObjectOrFileOfObject,
    thisHandle=yourConnectionHandle,
    # uploadPath=sys.getenv("signatureDirectory"), # default to configuration settings
    user="your SigRepo Username"
)
```

executing the above:

-   writes your object and differential expression files to disk
-   checks the phenotype of the signature object. if it doesn't exist already in the phenotypes table, that new phenotype will get added.
-   inserts signature metadata into signatures table in the database(addSignature)
-   inserts level2 data from that object into the features\_signatures table in the database(addLevel2).
-   inserts signature-keyword pairs into the keyword\_signatures table in the database(addSignatureKeywords).

#### Uploading OmicSignatureCollection Objects

OmicSignatureCollection Objects are simply a group of Omic Signature objects. You can upload such objects like this:

``` r
addSignatureCollection(
    OmicSignatureCollectionObj, 
    connHandle,
  uploadPath=sys.getenv("signatureDirectory"), 
  thisUser="your SigRepo User Name"
)
```

This function:

-   "unpacks" the OmicSignatureCollection by getting the "OmicSigList" property
-   runs an lapply of the function "addSignatureWrapper" on this list
-   inserts signature-to-collection pairs into the signature-to-collection table in the database(addCollectionSignatures)

If the collection you're uploading doesn't exist as an entry in the collections table of the database, the addCollectionSignatures function will add that collection as a new entry to the collections table before uploading the pairs.

#### Requesting Upload Privileges

If you don't have "write" access





