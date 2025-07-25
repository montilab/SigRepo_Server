library(dplyr)




# script to process the reference tables to cnform to our database


# proteomics ids



proteomics_refs <- read_delim('HUMAN_9606_idmapping_selected.tab', col_names = FALSE)


# cleaning the data
# need to add a is_current column with 1, a version column, and an organism column

proteomics_ids <- proteomics_refs %>%
  select(X1, X2) %>%
  rename(feature_name = X1, gene_symbol = X2) %>%
  mutate(
    is_current = 1,
    version =20250618, # using the date for now, can find the actualy version (dont think they keep track of that?) Also ids are updated every 8 weeks.
    organism = 'Homo Sapiens'
  )

# save proteomics ids dataframe to a csv file


# write.csv(proteomics_ids, file = 'prot_ids.csv')

# transcriptomics reference change




