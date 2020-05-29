library(readr)
library(dplyr)
install.packages("vegan")
library(vegan)
library(tidyr)

# Importing the tables to merge and skip the first line for feature

taxonomy_table <- read_tsv("~/Documentos/SupAgro - Facul/JRL/microbiome_soil-master/microbiome_soil-master/sequences_processing/16S/16S_export_output_jrl2020/16S_taxonomy_jrl2020.tsv")
View(taxonomy_table)
feature_table <- read_tsv("~/Documentos/SupAgro - Facul/JRL/microbiome_soil-master/microbiome_soil-master/sequences_processing/16S/16S_export_output_jrl2020/16S_feature-table_jrl2020.tsv", skip = 1)
View(feature_table)

# Merging the tables

assignment_table <- merge(taxonomy_table, feature_table, by.x = "Feature ID", "#OTU ID")
View(assignment_table)

# Changing names

assignment_table <- rename(assignment_table, "Feature_ID"="Feature ID")

# Selecting only the taxon and samples columns + Removing B-2-3, EstCont and MockCom

(assignment_table <- select(assignment_table, -Feature_ID, -PCRCont, -Confidence, -ExtCont, -MockCom))

# To do the rarefaction curve it's needed to switch the columns and lines from the table

(t_assignment_table <- data.frame(t(assignment_table[-1])))
colnames(t_assignment_table) <- assignment_table[,1]
rownames(t_assignment_table) <- assignment_table[1,]
View(t_assignment_table)

# Rarefaction curve

S <- specnumber(t_assignment_table)
raremax <- min(rowSums(t_assignment_table))
Srare <- rarefy(t_assignment_table, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
abline(0,1)
rarecurve(t_assignment_table, step = 20, sample = raremax, col = "blue", cex = 0.6)

