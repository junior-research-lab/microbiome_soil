library(readr)
library(dplyr)

# Importing the tables to merge and skip the first line for feature

taxonomy_table <- read_tsv("ITS2_taxonomy_jrl2020.tsv")
View(taxonomy_table)
feature_table <- read_tsv("ITS2_feature-table.tsv", skip = 1)
View(feature_table)

# Merging the tables

assignment_table <- merge(taxonomy_table, feature_table, by.x = "Feature ID", "#OTU ID")
View(assignment_table)

# Changing names

assignment_table <- rename(assignment_table, "Feature_ID"="Feature ID")

# Selecting only the taxon and samples columns

(assignment_table <- select(assignment_table, -Feature_ID, -Confidence))



