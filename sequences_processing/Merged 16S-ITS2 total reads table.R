library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

# 16S DATA

(data_frame <- read_tsv("C:/Users/amapo/Documents/GitHub/microbiome_soil/sequences_processing/16S/16S_export_output_jrl2020/16S_stats_jrl2020.tsv") %>%
    slice(-c(1)) %>%
    rename(c( "ID"= "sample-id")) %>%
    rename(c("nb_non_chimeric_reads" = "non-chimeric")) %>%
    rename(c("pct_input_non_chimeric" = "percentage of input non-chimeric")) %>%
    rename(c("pct_filtered_reads" = "percentage of input passed filter"))%>%
    rename(c("pct_input" = "percentage of input merged"))
)

View(data_frame)

# convert character variables into numeric variables

data_frame$pct_filtered_reads <- as.numeric(data_frame$pct_filtered_reads)
data_frame$pct_input_non_chimeric <- as.numeric(data_frame$pct_input_non_chimeric)
data_frame$nb_non_chimeric_reads <- as.numeric(data_frame$nb_non_chimeric_reads)

view(data_frame)

# remove the Mock, ExtC and PCR lines

(no_test_df <- filter(data_frame, grepl("*-*-", ID)))

# Separate the ID column so that we see the replicate number and the sample number of each sample ID

(no_test_df_16S <- separate(data = no_test_df, col = "ID", sep = 3, into = c("sample_number", "replicate_number_16S"), remove = FALSE) %>% 
    group_by(sample_number) %>%
    summarise(total_reads_16S = sum(nb_non_chimeric_reads)))

View(no_test_df_16S)


# ITS2 DATA

(data_frame <- read_tsv("C:/Users/amapo/Documents/GitHub/microbiome_soil/sequences_processing/ITS2/ITS2_dada2_output_jrl2020/ITS2_stats_jrl2020.tsv") %>%
    slice(-c(1)) %>%
    rename(c( "ID"= "sample-id")) %>%
    rename(c("nb_non_chimeric_reads" = "non-chimeric")) %>%
    rename(c("pct_input_non_chimeric" = "percentage of input non-chimeric")) %>%
    rename(c("pct_filtered_reads" = "percentage of input passed filter"))%>%
    rename(c("pct_input" = "percentage of input merged"))
)

View(data_frame)

# convert character variables into numeric variables

data_frame$pct_filtered_reads <- as.numeric(data_frame$pct_filtered_reads)
data_frame$pct_input_non_chimeric <- as.numeric(data_frame$pct_input_non_chimeric)
data_frame$nb_non_chimeric_reads <- as.numeric(data_frame$nb_non_chimeric_reads)

view(data_frame)

# remove the Mock, ExtC and PCR lines

(no_test_df <- filter(data_frame, grepl("*-*-", ID)))

# Separate the ID column so that we see the replicate number and the sample number of each sample ID

(no_test_df_ITS2 <- separate(data = no_test_df, col = "ID", sep = 3, into = c("sample_number", "replicate_number"), remove = FALSE) %>% 
    group_by(sample_number) %>%
    summarise(total_reads_ITS2 = sum(nb_non_chimeric_reads)))

View(no_test_df_ITS2)


#Merging the 2 tables

total_reads_table <- merge(no_test_df_16S, no_test_df_ITS2, by.x = "sample_number", "sample_number")
View(total_reads_table)



