organism_tbl <- organisms %>% 
  dplyr::mutate(
    biomart_db = ifelse(organism == "Arabidopsis thaliana", "", "genes"),
    biomart_dataset = base::sapply(
      base::seq_along(organism),
      function(s){
        if(organism[s] == "Arabidopsis thaliana"){
          return("")
        }else if(organism[s] == "Caenorhabditis elegans"){
          return("celegans_gene_ensembl")
        }else if(organism[s] == "Danio rerio"){
          return("drerio_gene_ensembl")
        }else if(organism[s] == "Drosophila melanogaster"){
          return("dmelanogaster_gene_ensembl")
        }else if(organism[s] == "Heterocephalus glaber"){
          return("hgfemale_gene_ensembl")
        }else if(organism[s] == "Homo sapiens"){
          return("hsapiens_gene_ensembl")
        }else if(organism[s] == "Mus musculus"){
          return("mmusculus_gene_ensembl")
        }else if(organism[s] == "Rattus norvegicus"){
          return("rnorvegicus_gene_ensembl")
        }
      }
    ),
    biomart_description = base::sapply(
      base::seq_along(organism),
      function(s){
        if(organism[s] == "Arabidopsis thaliana"){
          return("")
        }else if(organism[s] == "Caenorhabditis elegans"){
          return("Caenorhabditis elegans (Nematode, N2) genes (WBcel235)")
        }else if(organism[s] == "Danio rerio"){
          return("Zebrafish genes (GRCz11)")
        }else if(organism[s] == "Drosophila melanogaster"){
          return("Drosophila melanogaster - (Fruit fly) genes (BDGP6.54)")
        }else if(organism[s] == "Heterocephalus glaber"){
          return("Naked mole-rat female genes (Naked_mole-rat_maternal)")
        }else if(organism[s] == "Homo sapiens"){
          return("Human genes (GRCh38.p14)")
        }else if(organism[s] == "Mus musculus"){
          return("Mouse genes (GRCm39)")
        }else if(organism[s] == "Rattus norvegicus"){
          return("Norway rat - BN/NHsdMcwi genes (GRCr8)")
        }
      }
    ),
    biomart_version = ifelse(organism == "Arabidopsis thaliana", "", 113),
    biomart_updated_date = ifelse(organism == "Arabidopsis thaliana", NA, base::as.Date(base::Sys.Date(), format = "%Y-%m-%d"))
  )

organism_tbl <- organism_tbl %>% 
  dplyr::mutate(
    prot_organism_code = ifelse(organism == "Homo sapiens", "HUMAN", ""),
    prot_organism_taxid = ifelse(organism == "Homo sapiens", 9606, ""),
    prot_updated_date = ifelse(organism == "Homo sapiens", base::as.Date(base::Sys.Date(), format = "%Y-%m-%d"), NA)
  )

organism_tbl <- organism_tbl %>% 
  dplyr::mutate(
    biomart_updated_date = base::as.Date(biomart_updated_date, format = "%Y-%m-%d"),
    prot_updated_date = base::as.Date(prot_updated_date, format = "%Y-%m-%d")
  )

readr::write_csv(organism_tbl, "~/Connect/SigRepo_Server/mysql/data/organisms.csv")












