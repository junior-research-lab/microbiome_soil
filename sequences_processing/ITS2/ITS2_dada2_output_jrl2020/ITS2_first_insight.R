# Loading of the packages

library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# Importing the basic statistics data from DADA2 ITS2 process and change name of variables

(data_frame <- read_tsv("ITS2_stats_jrl2020.tsv") %>%
    slice(-c(1)) %>%
    rename(c( "ID"= "sample-id")) %>%
    rename(c("nb_non_chimeric_reads" = "non-chimeric")) %>%
    rename(c("pct_input_non_chimeric" = "percentage of input non-chimeric")) %>%
    rename(c("pct_filtered_reads" = "percentage of input passed filter"))
)

# convert character variables into numeric variables

data_frame$pct_filtered_reads <- as.numeric(data_frame$pct_filtered_reads)
data_frame$pct_input_non_chimeric <- as.numeric(data_frame$pct_input_non_chimeric)
data_frame$nb_non_chimeric_reads <- as.numeric(data_frame$nb_non_chimeric_reads)

# Plot a first graph to see the number of read for each replicate
ggplot(data=data_frame) + geom_col(mapping = aes(x=ID, y = pct_input_non_chimeric, fill = ID))


# remove the Mock, ExtC and PCR lines
(no_test_df <- filter(data_frame, grepl("*-*-", ID)))

# plot a second graph without the three test samples (mock, PCR and EXternal contaminants)
ggplot(data=no_test_df) + geom_col(mapping = aes(x=ID, y = pct_input_non_chimeric, fill = ID))


# Separate the ID column so that we see the replicate number and the sample number of each sample ID

(no_test_df <- separate(data = no_test_df, col = "ID", sep = 3, into = c("sample_number", "replicate_number"), remove = FALSE) %>% 
    group_by(sample_number) %>%
    summarise(total_reads = sum(nb_non_chimeric_reads)))# Here we have the total_reads for each sample

# This graph is a bonus : we can see wich sample have the lowest/higher number of reads
  ggplot(data = no_test_df, mapping = aes(x = reorder(sample_number,total_reads, FUN = median), y = total_reads)) + geom_point()

# I think we should remove B-2-3, B-5-1, B-7-3 and B-9-3 from the dataset beacause they
# have few reads


  



      