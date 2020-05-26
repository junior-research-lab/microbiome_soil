library(dplyr)
(data_frame <- read_tsv ("ITS2_stats_jrl2020.tsv") %>% slice(-c(1)))
View(data_frame)
require(reshape)
names(data_frame)
names(data_frame)[1] = "sample_id"
names(data_frame)[9] = "percentage_of_input_non_chimeric"
names(data_frame)[8] = "non_chimeric"
names(data_frame)[4] = "percentage_of_input_passed_filter"
names(data_frame)[7] = "percentage_of_input_merged"
library(ggplot2)
ggplot(data = data_frame) + geom_col(mapping = aes(x = sample_id, y = percentage_of_input_non_chimeric))
arrange(data_frame, percentage_of_input_non_chimeric)
arrange(data_frame, percentage_of_input_passed_filter)
data_frame$percentage_of_input_passed_filter <- as.numeric(data_frame$percentage_of_input_passed_filter)
arrange(data_frame, percentage_of_input_passed_filter)
