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

View(data_frame)

data_frame$filtered_pct <- as.numeric(data_frame$filtered_pct)

arrange(data_frame, filtered_pct)




ggplot(data = data_frame) +
  geom_col(mapping = aes(x= ID, y = pct_of_non_chimeric))




      