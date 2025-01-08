# Load required libraries ----
library(qiime2R)     # For importing QIIME2 artifacts
library(phyloseq)    # For microbiome data analysis
library(dplyr)       # For data manipulation
library(tidyr)       # For data tidying
library(ggplot2)     # For plotting

# === 16S rRNA Analysis ===
# Import data using qiime2R
ps_16s <- qiime2R::qza_to_phyloseq(
  features = "table_16s.qza",
  tree = "rooted-tree_16s.qza",
  taxonomy = "classification_16s.qza",
  metadata = "16s_manifest.tsv"
)

# Convert to table format
ps_tbl_16s <- psmelt(ps_16s)

# Convert columns to factors
ps_tbl_16s <- ps_tbl_16s %>%
  mutate(
    Sample = factor(Sample),
    Season = factor(Season),
    Kingdom = factor(Kingdom),
    Phylum = factor(Phylum),
    Class = factor(Class),
    Order = factor(Order),
    Family = factor(Family),
    Genus = factor(Genus),
    Species = factor(Species)
  )

# === 18S rRNA Analysis ===
# Import data using qiime2R
ps_18s <- qiime2R::qza_to_phyloseq(
  features = "table_18s.qza",
  tree = "rooted-tree_18s.qza",
  taxonomy = "classification_18s.qza",
  metadata = "18s_manifest.tsv"
)

# Convert to table format
ps_tbl_18s <- psmelt(ps_18s)

# Convert columns to factors
ps_tbl_18s <- ps_tbl_18s %>%
  mutate(
    Sample = factor(Sample),
    Season = factor(Season),
    Kingdom = factor(Kingdom),
    Phylum = factor(Phylum),
    Class = factor(Class),
    Order = factor(Order),
    Family = factor(Family),
    Genus = factor(Genus),
    Species = factor(Species)
  )

# === ITS Analysis ===
# Import data using qiime2R
ps_its <- qiime2R::qza_to_phyloseq(
  features = "table_its.qza",
  tree = "rooted-tree_its.qza",
  taxonomy = "classification_its.qza",
  metadata = "its_manifest.tsv"
)

# Convert to table format
ps_tbl_its <- psmelt(ps_its)

# Convert columns to factors
ps_tbl_its <- ps_tbl_its %>%
  mutate(
    Sample = factor(Sample),
    Season = factor(Season),
    Kingdom = factor(Kingdom),
    Phylum = factor(Phylum),
    Class = factor(Class),
    Order = factor(Order),
    Family = factor(Family),
    Genus = factor(Genus),
    Species = factor(Species)
  )

# Check taxonomic levels (example for each dataset)
# 16S
levels(ps_tbl_16s$Phylum)
levels(ps_tbl_16s$Class)
levels(ps_tbl_16s$Order)

# 18S
levels(ps_tbl_18s$Phylum)
levels(ps_tbl_18s$Class)
levels(ps_tbl_18s$Order)

# ITS
levels(ps_tbl_its$Phylum)
levels(ps_tbl_its$Class)
levels(ps_tbl_its$Order)






