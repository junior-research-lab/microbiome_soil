library(readr)
library(dplyr)
library(vegan)
library(tidyr)

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
assignment_table <- rename(assignment_table, "B_2_3"="B-2-3")

# Selecting only the taxon and samples columns + Removing B-2-3, EstCont and MockCom

(assignment_table <- select(assignment_table, -Feature_ID, -Confidence, -B_2_3, -ExtCont, -MockCom))

taxa_assignment_table <- separate(data = assignment_table, col = Taxon, sep = "([pcofgs]__)", into = c("Kingdom","Phylum","Class","Order","Family","Gender","Species"))
taxa_assignment_table$Phylum <-  gsub(";","",taxa_assignment_table$Phylum)
taxa_assignment_table$Class <-  gsub(";","",taxa_assignment_table$Class)
taxa_assignment_table$Order <-  gsub(";","",taxa_assignment_table$Order)
taxa_assignment_table$Family <-  gsub(";","",taxa_assignment_table$Family)
taxa_assignment_table$Gender <-  gsub(";","",taxa_assignment_table$Gender)
taxa_assignment_table$Kingdom <-  gsub(";","",taxa_assignment_table$Kingdom)
taxa_assignment_table$Kingdom <-  gsub("k__","",taxa_assignment_table$Kingdom)

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
rarecurve(t_assignment_table, step = 20, sample = raremax, col = "blue", cex = 0.6, ylab = "OTU number")


