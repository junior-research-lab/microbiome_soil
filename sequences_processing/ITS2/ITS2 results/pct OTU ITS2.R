library(readr)
library(dplyr)
library(openxlsx)
library(expss)
library(tidyverse)

# Import the assignment table

assignment_table <- read.xlsx("ITS2_assignment_table.xlsx")
View(assignment_table)

# Add the column of % of OTU on the PCR

colnames(assignment_table) <- gsub("-","_",colnames(assignment_table))

#We create a vector containing the name of the columns for the percentage of contamination. So the vector is 47 length
# varnames <- paste(colnames(assignment_table)[-1], "pct","cont", sep = "_")

#We calculate the percentage of reads provided by the OTU compared to the total number of read for each sample
calculated_pct <- apply(assignment_table[,8:ncol(assignment_table)], 2, function(x){x/sum(x)*100} )

#We rename the columns of the data frame 'calculated_pct' with the names we put in varnames
names(calculated_pct) <- varnames

#The result is a data frame with the name of the contaminatde OTU and the percentage of contamination for each sample
(res <- cbind(assignment_table[,c(1:7)],
              calculated_pct)
)

(tot_read_samples <- select(assignment_table, `B-*-*`) %>% colSums())

(assignment_table <- (assignment_table %>% mutate(pct_cont_OTU_PCR = (PCRCont/tot_read_PCR)*100)))
View(assignment_table)
