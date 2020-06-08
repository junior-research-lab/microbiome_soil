library(readr)
library(dplyr)
library(openxlsx)
library(expss)
library(tidyverse)

# Import the assignment table

assignment_table <- read.xlsx("assignment.table.ITS2.(II).xlsx")
View(assignment_table)


# Add the column of % of OTU on the PCR

assignment_table <- read.xlsx("assignment.table.ITS2.(II).xlsx") %>% 
  rename(`B_*_*` = `B-*-*`)

(tot_read_samples <- select(assignment_table, `B-*-*`) %>% colSums())

(assignment_table <- (assignment_table %>% mutate(pct_cont_OTU_PCR = (PCRCont/tot_read_PCR)*100)))
View(assignment_table)
