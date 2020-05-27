library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)

(data_frame <- read_tsv("ITS2_stats_jrl2020.tsv") %>%
    slice(-c(1)) %>%
    rename(c( "ID"= "sample-id")) %>%
    rename(c("non_chimeric" = "non-chimeric")) %>%
    rename(c("pct_of_non_chimeric" = "percentage of input non-chimeric")) %>%
    rename(c("filtered_pct" = "percentage of input passed filter"))
)

data_frame$filtered_pct <- as.numeric(data_frame$filtered_pct)
data_frame$non_chimeric <- as.numeric(data_frame$non_chimeric)

arrange(data_frame, filtered_pct)

ggplot(data=data_frame) + geom_col(mapping = aes(x=ID, y = non_chimeric, fill = ID))

# remove the Mock, ExtC and PCR lines

(no_test_df <- filter(data_frame, grepl("*-*-", ID)))

# Group the lines given the sample they come from

(no_test_df <- separate(data = no_test_df, col = "ID", sep = 3, into = c("sample_number", "primer_number"), remove = FALSE) %>% 
    group_by(sample_number) %>%
    mutate(prop_reads_each_primer = (non_chimeric/sum(non_chimeric))*100) %>%
    select(ID, non_chimeric, prop_reads_each_primer, everything())
)

(no_test_df <- group_by(no_test_df, sample_number)) %>%
  summarise(total_reads = sum(non_chimeric)) %>%
  ggplot(mapping = aes(x = reorder(sample_number,total_reads, FUN = median), y = total_reads)) + geom_point()
)

filter(no_test_df, prop_reads_each_primer < 5)

# I think we should remove B-2-3, B-5-1, B-7-3 and B-9-3 from the dataset beacause they
# have few reads


ggplot(data = no_test_df) + geom_boxplot(mapping=aes(x = primer_number, y = prop_reads_each_primer))
ggplot(data = no_test_df) + geom_col(mapping = aes(x = ID, y = prop_reads_each_primer))

  



      