#### ensembl id download ####

#### 1. Download GTF #### 
## Manually download ##
# https://useast.ensembl.org/info/data/ftp/index.html
# Select Download Genesets -> GTF of a species. 
# e.g. 
# Homo_sapiens.GRCh38.113.chr.gtf.gz
# Mus_musculus.GRCm39.112.chr.gtf.gz
# note: Homo_sapiens.GRCh38.113.gtf.gz contains the scaffold. 

## Download using curl ##
curl https://ftp.ensembl.org/pub/release-113/gtf/homo_sapiens/Homo_sapiens.GRCh38.113.chr.gtf.gz --output Homo_sapiens.GRCh38.113.chr.gtf.gz
gunzip Homo_sapiens.GRCh38.113.chr.gtf.gz

#### 2. Filter rows #### 
## Keep only the rows where the 3rd column is "gene"
# Can take a minute since the file is large. 
awk '{if($3=="gene"){print $0}}' Homo_sapiens.GRCh38.113.chr.gtf > gene1.gtf

## Remove the entries on scaffold, i.e. only keep entries on each chromosome. 
# Note: do not need to do this if downloaded chr.gtf in the previous step. 
# For mouse: 
# awk '$1 ~ /^([1-9]|1[0-9]|X|Y|MT)$/ {print $0}' gene1.gtf > gene1.gtf
# For human: 
# awk '$1 ~ /^([1-9]|1[0-9]|2[0-2]|X|Y|MT)$/ {print $0}' gene1.gtf > gene1.gtf

#### 3. Extract relevent columns #### 
# 1. For genes with a symbol: 
# select: column 10 (column "gene_id") 
#         column 14 (column "gene_name") which is gene symbol 
# optional: column 18 or 16 (column "gene_biotype")
awk '{if($13=="gene_name") {print $10 $14}}' gene1.gtf > gene2_1.txt

# 2. For genes without a symbol, manually add an empty column as a place holder for symbol: 
awk '{if($13=="gene_source") {print $10 "\"\";"}}' gene1.gtf > gene2_2.txt

# 3. Bind the two together. Manually add column name. Replace all ";" to ","
echo 'feature_name, ensembl_id' > gene3.txt 
cat gene2_1.txt gene2_2.txt >> gene3.txt
sed 's/;/,/g' gene3.txt > gene4.csv
