# Script to create 4 different example omic signature objects



library(devtools)
library(dplyr)
devtools::install_github(repo = "montilab/OmicSignature")

load_all()


# Grabbing the data path

data_path <- system.file("data", package="SigRepo")


# reading in the data 
brca_diffanal<- base::readRDS(file.path(data_path, "signatures/ahr_cyp1b1_brca_diffanal.RDS"))




OmS_MDA_CYP <- brca_diffanal$MDA.CYP

OmS_MDA_AhR <- brca_diffanal$MDA.AhR

OmS_SUM_CYP <- brca_diffanal$SUM.CYP

OmS_SUM_AhR <- brca_diffanal$SUM.AhR

# proteomics example

prot_OmS_MDA_CYP <- brca_diffanal$MDA.CYP


# filtering out the depricated ids

filter_list <- read.csv(file.path(data_path,"feature_tables/Transcriptomics_HomoSapiens.csv"))

filter_list <- filter_list[1]


OmS_MDA_CYP <- dplyr::filter(OmS_MDA_CYP, ensembl_gene_id %in% filter_list$feature_name)

OmS_MDA_AhR <- dplyr::filter(OmS_MDA_AhR, ensembl_gene_id %in% filter_list$feature_name)

OmS_SUM_CYP <- dplyr::filter(OmS_SUM_CYP, ensembl_gene_id %in% filter_list$feature_name)

OmS_SUM_AhR <- dplyr::filter(OmS_SUM_AhR, ensembl_gene_id %in% filter_list$feature_name)

# proteomics example

prot_OmS_MDA_CYP <-  dplyr::filter(prot_OmS_MDA_CYP, ensembl_gene_id %in% filter_list$feature_name)


# transcriptomics example metadata 

metadata_MDA_CYP <- OmicSignature::createMetadata(
  signature_name = "CYP181 knockdown in breast cancer cell line", 
  organism = "Homo sapiens",
  assay_type = "transcriptomics", 
  phenotype = "CYP181 knockdown",
  sample_type = "MDA-MB-231 cell",
  direction_type = "bi-directional", 
  platform = "GPL17930", 
  covariates = "none", 
  year = 2016, 
  keywords = c("breast cancer", "CYP181 knockdown"), 
  description = "Profiles of the transcriptional response of CYP181 knockdown in breast cancer cell ines", 
  adj_p_cutoff = "0.01")

metadata_MDA_AhR <- OmicSignature::createMetadata( 
  signature_name = "AhR knockdown in breast cancer cell line", 
  organism = "Homo sapiens",
  assay_type = "transcriptomics", 
  phenotype = "AhR knockdown",
  sample_type = "MDA-MB-231 cell",
  direction_type = "bi-directional", 
  platform = "GPL17930", 
  covariates = "none", 
  year = 2016, 
  keywords = c("breast cancer", "AhR knockdown"), 
  description = "Profiles of the transcriptional response of AhR knockdown in breast cancer cell ines", 
  adj_p_cutoff = "0.01")

metadata_SUM_CYP <- OmicSignature::createMetadata(
  signature_name = "CYP181 knockdown in breast cancer cell line", 
  organism = "Homo sapiens",
  assay_type = "transcriptomics", 
  phenotype = "AhR knockdown",
  sample_type = "SUM-149PT cell",
  direction_type = "bi-directional", 
  platform = "GPL17930", 
  covariates = "none", 
  year = 2016, 
  keywords = c("breast cancer", "CYP181 knockdown"), 
  description = "Profiles of the transcriptional response of CYP181 knockdown in breast cancer cell ines", 
  adj_p_cutoff = "0.01")

metadata_SUM_AhR <- OmicSignature::createMetadata(signature_name = "AhR knockdown in breast cancer cell line", 
                                                  organism = "Homo sapiens",
                                                  assay_type = "transcriptomics", 
                                                  phenotype = "AhR knockdown",
                                                  sample_type = "SUM-149PT cell",
                                                  direction_type = "bi-directional", 
                                                  platform = "GPL17930", 
                                                  covariates = "none", 
                                                  year = 2016, 
                                                  keywords = c("breast cancer", "AhR knockdown"), 
                                                  description = "Profiles of the transcriptional response of AhR knockdown in breast cancer cell ines", 
                                                  adj_p_cutoff = "0.01")


# proteomics example metadata

prot_metadata_MDA_CYP <- OmicSignature::createMetadata(
  signature_name = "CYP181 knockdown in breast cancer cell line", 
  organism = "Homo sapiens",
  assay_type = "proteomics", 
  phenotype = "CYP181 knockdown",
  sample_type = "MDA-MB-231 cell",
  direction_type = "bi-directional", 
  platform = "GPL17930", 
  covariates = "none", 
  year = 2016, 
  keywords = c("breast cancer", "CYP181 knockdown"), 
  description = "Profiles of the transcriptional response of CYP181 knockdown in breast cancer cell ines", 
  adj_p_cutoff = "0.01")


# Transriptomics example difexp

difexp_SUM_Ahr <- OmS_SUM_AhR %>%
  dplyr::rename( score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val
  ) %>%
  select(feature_name, adj_p, score )

difexp_SUM_CYP <- OmS_SUM_CYP %>%
  dplyr::rename( score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val
  ) %>%
  select(feature_name, adj_p, score )


difexp_MDA_AhR <- OmS_MDA_AhR %>%
  dplyr::rename( score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val
  ) %>%
  select(feature_name, adj_p, score )


difexp_MDA_CYP <- OmS_MDA_CYP %>%
  dplyr::rename( score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val
  ) %>%
  select(feature_name, adj_p, score )

# proteomics example difex

prot_difexp_MDA_CYP <- prot_OmS_MDA_CYP %>%
  dplyr::rename(score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val) %>%
  select(feature_name, adj_p, score)

# creating fake probe_ids for now, still not working for me

difexp_SUM_Ahr$probe_id <- paste0("SUM_AhR_", 1:nrow(difexp_SUM_Ahr))
difexp_SUM_CYP$probe_id <- paste0("SUM_CYP_", 1:nrow(difexp_SUM_CYP))
difexp_MDA_AhR$probe_id <- paste0("MDA_AhR_", 1:nrow(difexp_MDA_AhR))
difexp_MDA_CYP$probe_id <- paste0("MDA_CYP_", 1:nrow(difexp_MDA_CYP))

# proteomics example fake probe ids

prot_difexp_MDA_CYP$probe_id <- paste0("prot_MDA_CYP", 1:nrow(prot_difexp_MDA_CYP))

# Creating Signatures, 

sig_SUM_AhR <- difexp_SUM_Ahr %>%
  select(feature_name, score, probe_id) 


sig_SUM_CYP <- difexp_SUM_CYP %>%
  select(feature_name, score, probe_id) 


sig_MDA_AhR <- difexp_MDA_AhR %>%
  select(feature_name, score, probe_id) 


sig_MDA_CYP <- difexp_MDA_CYP %>%
  select(feature_name, score, probe_id)

# proteomics example signature

prot_sig_MDA_CYP <- prot_difexp_MDA_CYP %>%
  select(feature_name, score, probe_id)

# adding in the direction column

sig_SUM_AhR$direction <- ifelse(sig_SUM_AhR$score > 0, "+", "-")

sig_SUM_CYP$direction <- ifelse(sig_SUM_CYP$score > 0, "+", "-")

sig_MDA_AhR$direction <- ifelse(sig_MDA_AhR$score > 0, "+", "-")

sig_MDA_CYP$direction <- ifelse(sig_MDA_CYP$score > 0, "+", "-")

# adding direction column

prot_sig_MDA_CYP$direction <- ifelse(prot_sig_MDA_CYP$score > 0, "+","-")



# Creating the Omic Signature Objects

omic_signature_SUM_Ahr <- OmicSignature::OmicSignature$new(
  signature = sig_SUM_AhR,
  metadata = metadata_SUM_AhR,
  difexp = difexp_SUM_Ahr
)

omic_signature_SUM_CYP <- OmicSignature::OmicSignature$new(
  signature = sig_SUM_CYP,
  metadata = metadata_SUM_CYP,
  difexp = difexp_SUM_CYP
)

omic_signature_MDA_AhR <- OmicSignature::OmicSignature$new(
  signature = sig_MDA_AhR,
  metadata = metadata_MDA_AhR,
  difexp = difexp_MDA_AhR
)

omic_signature_MDA_CYP <- OmicSignature::OmicSignature$new(
  signature = sig_MDA_CYP,
  metadata = metadata_MDA_CYP,
  difexp = difexp_MDA_CYP
)

# Creating Proteomics signature object

prot_omic_signature_MDA_CYP <- OmicSignature::OmicSignature$new(
  signature = prot_sig_MDA_CYP,
  metadata = prot_metadata_MDA_CYP,
  difexp = prot_difexp_MDA_CYP
)

# Creating an OmicSignature Collection


colMeta <- list(
  'collection_name' = 'example',
  'description' = 'example of signature collection',
  'author' = 'me')

OmSC <- OmicSignature::OmicSignatureCollection$new(
  OmicSigList = list(omic_signature_MDA_AhR,omic_signature_MDA_CYP, omic_signature_SUM_CYP, omic_signature_SUM_Ahr),
  metadata = colMeta
)

# saving the RDS objcts to the siganture folder

saveRDS(OmSC, file = file.path(data_path, "signatures/OmSC_example.RDS"))

saveRDS(omic_signature_MDA_AhR, file = file.path(data_path, "signatures/omic_signature_MDA_AhR.RDS"))

saveRDS(omic_signature_MDA_CYP, file = file.path(data_path, "signatures/omic_signature_MDA_CYP.RDS"))

saveRDS(omic_signature_SUM_Ahr, file = file.path(data_path, "signatures/omic_signature_SUM_AhR.RDS"))

saveRDS(omic_signature_SUM_CYP, file = file.path(data_path, "signatures/omic_signature_SUM_CYP.RDS"))

saveRDS(prot_omic_signature_MDA_CYP, file = file.path(data_path, "signatures/prot_omic_signature_MDA_CYP.RDS"))
