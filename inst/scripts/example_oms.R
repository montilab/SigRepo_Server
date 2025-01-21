# Script to create 4 different example omic signature objects



library(devtools)
library(dplyr)
devtools::install_github(repo = "montilab/OmicSignature")


# Grabbing the data path
data_path <- system.file("data", package="SigRepo")


# reading in the data 
brca_diffanal<- base::readRDS(file.path(data_path, "signatures/ahr_cyp1b1_brca_diffanal.RDS"))




OmS_MDA_CYP <- brca_diffanal$MDA.CYP

OmS_MDA_AhR <- brca_diffanal$MDA.AhR

OmS_SUM_CYP <- brca_diffanal$SUM.CYP

OmS_SUM_AhR <- brca_diffanal$SUM.AhR

# Creating metadata for all 4, The xxxx is either AhR or CYP1B1. "MDA-MB-231 cell" or "SUM-149PT cell",

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

# Creating difexp's

difexp_SUM_Ahr <- OmS_SUM_AhR %>%
  rename( score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val
  ) %>%
  select(feature_name, adj_p, score )

difexp_SUM_CYP <- OmS_SUM_CYP %>%
  rename( score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val
  ) %>%
  select(feature_name, adj_p, score )


difexp_MDA_AhR <- OmS_MDA_AhR %>%
  rename( score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val
  ) %>%
  select(feature_name, adj_p, score )


difexp_MDA_CYP <- OmS_MDA_CYP %>%
  rename( score = t, feature_name = ensembl_gene_id, adj_p = adj.P.Val
  ) %>%
  select(feature_name, adj_p, score )


# creating fake probe_ids for now, still not working for me

difexp_SUM_Ahr$probe_id <- paste0("SUM_AhR_", 1:nrow(difexp_SUM_Ahr))
difexp_SUM_CYP$probe_id <- paste0("SUM_CYP_", 1:nrow(difexp_SUM_CYP))
difexp_MDA_AhR$probe_id <- paste0("MDA_AhR_", 1:nrow(difexp_MDA_AhR))
difexp_MDA_CYP$probe_id <- paste0("MDA_CYP_", 1:nrow(difexp_MDA_CYP))

# Creating Signatures, 

sig_SUM_AhR <- difexp_SUM_Ahr %>%
  select(feature_name, score, probe_id) 


sig_SUM_CYP <- difexp_SUM_CYP %>%
  select(feature_name, score, probe_id) 


sig_MDA_AhR <- difexp_MDA_AhR %>%
  select(feature_name, score, probe_id) 


sig_MDA_CYP <- difexp_MDA_CYP %>%
  select(feature_name, score, probe_id)


# adding in the direction column

sig_SUM_AhR$direction <- ifelse(sig_SUM_AhR$score > 0, "+", "-")

sig_SUM_CYP$direction <- ifelse(sig_SUM_CYP$score > 0, "+", "-")

sig_MDA_AhR$direction <- ifelse(sig_MDA_AhR$score > 0, "+", "-")

sig_MDA_CYP$direction <- ifelse(sig_MDA_CYP$score > 0, "+", "-")



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

# creating an RDS of the omic signatures 


