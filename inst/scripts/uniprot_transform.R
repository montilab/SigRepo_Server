# script to transform uniprot data




# human data #### 
url <- "https://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/by_organism/HUMAN_9606_idmapping_selected.tab.gz"

# downloading the file

if (!file.exists("HUMAN_9606_idmapping_selected.tab.gz")) {
  download.file(url, destfile = "HUMAN_9606_idmapping_selected.tab.gz", method = "curl")
}

# reading in the file

human_prot_ids <- read_tsv("HUMAN_9606_idmapping_selected.tab.gz", col_names = FALSE)

# grabbing only the relevant cols

refs_prot_ids <- human_prot_ids %>%
  rename("feature_name" = "X1",
         "gene_symbol" = "X2") %>% 
  select(feature_name, gene_symbol) %>%
  mutate(organism = "Homo sapiens",
         version = 07242025,
         is_current = 1) # date created
                            