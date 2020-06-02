library(readr)
library(dplyr)
library(vegan)
library(tidyr)
install.packages("openxlsx")
install.packages("rio")
library(rio)
library(openxlsx)

# Importing the tables to merge and skip the first line for feature

taxonomy_table <- read_tsv("~/Documentos/SupAgro - Facul/JRL/microbiome_soil-master/microbiome_soil-master/sequences_processing/16S/16S_export_output_jrl2020/16S_taxonomy_jrl2020.tsv")
View(taxonomy_table)
feature_table <- read_tsv("~/Documentos/SupAgro - Facul/JRL/microbiome_soil-master/microbiome_soil-master/sequences_processing/16S/16S_export_output_jrl2020/16S_feature-table_jrl2020.tsv", skip = 1)
View(feature_table)

# Merging the tables

assignment_table <- merge(taxonomy_table, feature_table, by.x = "Feature ID", "#OTU ID")
View(assignment_table)
export(assignment_table, "assignment.table.xlsx")
getwd()

# Changing names

assignment_table <- rename(assignment_table, "Feature_ID"="Feature ID")
assignment_table <- rename(assignment_table, "B_3_1"="B-3-1")
assignment_table <- rename(assignment_table, "B_5_2"="B-5-2")

# Selecting only the taxon and samples columns +  EstCont and MockCom + B_3_1 + B_5_2

(assignment_table <- select(assignment_table, -Feature_ID, -PCRCont, -Confidence, -ExtCont, -MockCom, -B_3_1, -B_5_2))

# To do the rarefaction curve it's needed to switch the columns and lines from the table

(t_assignment_table <- data.frame(t(assignment_table[-1])))
colnames(t_assignment_table) <- assignment_table[,1]
rownames(t_assignment_table) <- assignment_table[1,]
View(t_assignment_table)

# Rarefaction curve

S <- specnumber(t_assignment_table)
raremax <- min(rowSums(t_assignment_table))
Srare <- rarefy(t_assignment_table, raremax)
plot(S, Srare, xlab = "Observed No. of Species", ylab = "Rarefied of OTU")
abline(0,1)
rarecurve(t_assignment_table, step = 20, sample = raremax, col = "blue", cex = 0.6)

