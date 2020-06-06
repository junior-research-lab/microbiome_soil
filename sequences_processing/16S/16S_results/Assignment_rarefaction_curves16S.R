---
  title: "Assignment rarefaction curves 16S"
author: "JRL"
date: "06/06/2020"
output: html_document
---

library(readr)
library(dplyr)
library(vegan)
library(tidyr)
library(openxlsx)
library(rio)

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
(assignment_table <- select(assignment_table, -Feature_ID, -Confidence, -ExtCont, -MockCom))
taxa_assignment_table <- separate(data = assignment_table, col = Taxon, sep = "([pcofgs]__)", into = c("Domain","Phylum","Class","Order","Family","Gender","Species"))
taxa_assignment_table$Phylum <-  gsub(";","",taxa_assignment_table$Phylum)
taxa_assignment_table$Class <-  gsub(";","",taxa_assignment_table$Class)
taxa_assignment_table$Order <-  gsub(";","",taxa_assignment_table$Order)
taxa_assignment_table$Family <-  gsub(";","",taxa_assignment_table$Family)
taxa_assignment_table$Gender <-  gsub(";","",taxa_assignment_table$Gender)
taxa_assignment_table$Domain <-  gsub(";","",taxa_assignment_table$Domain)
taxa_assignment_table$Domain <-  gsub("d__","",taxa_assignment_table$Domain)
View(taxa_assignment_table)

# Total number of reads for unidentified taxa
(unidentified_taxa <- taxa_assignment_table %>% 
    filter(Phylum == "unidentified"))
unidentified_taxa <- select( unidentified_taxa, -c(1:7))
sum(rowSums(unidentified_taxa))
#
all_taxa <- select( taxa_assignment_table, -c(1:7))
sum(rowSums(all_taxa))


```{r rarefaction}
# To do the rarefaction curve it's needed to switch the columns and lines from the table
(t_assignment_table <- data.frame(t(assignment_table[-1])))
colnames(t_assignment_table) <- assignment_table[,1]
rownames(t_assignment_table) <- assignment_table[1,]
View(t_assignment_table)

# We also need to delete PCRCont and ExtCont from the dataframe
assignment_table_rare1 <-  t_assignment_table[
  grep("[\\-]", rownames(t_assignment_table)),]
# Rarefaction curve
OTU <- specnumber(assignment_table_rare1) # It counts the number of OTU
raremax <- min(rowSums(assignment_table_rare1)) #Look at the minimum number of reads in the   samples
print(rowSums(assignment_table_rare1))
Srare <- rarefy(assignment_table_rare1, raremax) #Function rarefy gives the expected species richness in random subsamples of size 'raremax' from the community

# Plot the rarefaction curve
plot(OTU, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
rarecurve(assignment_table_rare1, step = 20, sample = raremax, col = "blue", cex = 0.6, ylab = "OTU number")


As we can see on the graph and int the printed rowSums, B-3-1 and B-5-2 should be deleted, so that we can have a correct rarefied table

```{r rarefied_df}
#Delete  B-3-1 and B-5-2
assignment_table_rare2 <- assignment_table_rare1[
  -c(grep("B-3-1", rownames(t_assignment_table), fixed = TRUE), 
     grep("B-5-2", rownames(t_assignment_table), fixed = TRUE),]
# Rarefaction curve
OTU <- specnumber(assignment_table_rare2) # It counts the number of OTU
raremax <- min(rowSums(assignment_table_rare2)) #Look at the minimum number of reads in the   samples
print(raremax)
Srare <- rarefy(assignment_table_rare2, raremax) #Function rarefy gives the expected species richness in random subsamples of size 'raremax' from the community

# Plot the rarefaction curve
plot(OTU, Srare, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
rarecurve(assignment_table_rare2, step = 20, sample = raremax, col = "blue", cex = 0.6, ylab = "OTU number")
#Rarefy the dataframe
set.seed(seed = 222029)
rarefied_df <- rrarefy(assignment_table_rare2, raremax)
