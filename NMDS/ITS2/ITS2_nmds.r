#Load packages
library(vegan)
library(openxlsx)
library(ggplot2)

#Set the seed
set.seed(222029)

###### NMDS FOR PHYLUM #######

#Attach data
df_phylum <- read.xlsx("ITS2_phylum_reads.xlsx")
attach(df_phylum)
str(df_phylum)

#Add a column specifying if the sample is bulk or rhizospheric soil
df_phylum <- mutate(df_phylum, Type_soil = ifelse(grepl("B", df_phylum$Samples),"Bulk","Rhizosphere" ) ) %>%
  select(Samples, Type_soil, everything()) %>% 
  select(-unidentified)

#subset the dataframe on which to base the ordination (data_taxa)
p_data_taxa <- df_phylum[,3:length(df_phylum)]
rownames(p_data_taxa) <- df$Samples

#Identify the columns that contains the descriptive/environmental data (data_sample)
p_data_sample <- df_phylum[,1:2]

#ordination by NMDS. WRITE DOWN the 20th stress indicator
NMDS_phylum <- metaMDS(p_data_taxa, distance = "bray", k = 2)

######Data visualisation

#Create a table with the results of the NMDS for the samples
p_sample_scores <- as.data.frame(scores(NMDS_phylum))
p_sample_scores$site <- rownames(p_sample_scores)
p_sample_scores$Habitat <- df_phylum$Type_soil

#Create a table with the results of the NMDS for the phylum
p_scores <- as.data.frame(scores(NMDS_phylum, "species"))
p_scores$Phylum <- rownames(p_scores)

ggplot() + 
  stat_ellipse(data = p_sample_scores, aes(x=NMDS1,y=NMDS2,colour = Habitat), size = 1) +
  geom_point(data = p_sample_scores,mapping = aes(x = NMDS1,y = NMDS2, shape = Habitat, colour = Habitat),size=4) + # add the point markers
  geom_text(data = p_scores,aes(x=NMDS1,y=NMDS2,label = Phylum),alpha=0.5) +  # add the species labels
  geom_text(data = p_sample_scores,aes(x=NMDS1,y=NMDS2,label = site),size = 3,vjust=0,hjust=0) +  # add the site labels
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=10), # remove x-axis labels
        axis.title.y = element_text(size=10), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

########Statistical proof 
#Bootstrapping and testing for differences between the groups
p_fit <- adonis(p_data_taxa ~ Type_soil, data=p_data_sample, permutations=999, method="bray")


#Check assumption of homogeneity of multivariate dispersion
p_distances_data <- vegdist(p_data_taxa)
anova(betadisper(p_distances_data, p_data_sample$Type_soil))

###### NMDS FOR ORDER #######

#Attach data
df_order <- read.xlsx("ITS2_order_reads.xlsx")
attach(df_order)
str(df_order)

#Add a column specifying if the sample is bulk or rhizospheric soil
df_order <- mutate(df_order, Type_soil = ifelse(grepl("B", df_order$Samples),"Bulk","Rhizosphere" ) ) %>%
  select(Samples, Type_soil, everything()) %>% 
  select(-unidentified)

#subset the dataframe on which to base the ordination (data_taxa)
o_data_taxa <- df_order[,3:length(df_phylum)]
rownames(o_data_taxa) <- df_order$Samples

#Identify the columns that contains the descriptive/environmental data (data_sample)
o_data_sample <- df_phylum[,1:2]

#ordination by NMDS. WRITE DOWN the 20th stress indicator
NMDS_order <- metaMDS(o_data_taxa, distance = "bray", k = 2)

######Data visualisation

#Create a table with the results of the NMDS for the samples
o_sample_scores <- as.data.frame(scores(NMDS_order))
o_sample_scores$site <- rownames(o_sample_scores)
o_sample_scores$Habitat <- df_order$Type_soil

#Create a table with the results of the NMDS for the phylum
o_scores <- as.data.frame(scores(NMDS_order, "species"))
o_scores$Order <- rownames(o_scores)

ggplot() + 
  stat_ellipse(data = o_sample_scores, aes(x=NMDS1,y=NMDS2,colour = Habitat), size = 1) +
  geom_point(data = o_sample_scores,mapping = aes(x = NMDS1,y = NMDS2, shape = Habitat, colour = Habitat),size=4) + # add the point markers
  geom_text(data = o_scores,aes(x=NMDS1,y=NMDS2,label = Order),alpha=0.5) +  # add the species labels
  geom_text(data = o_sample_scores,aes(x=NMDS1,y=NMDS2,label = site),size = 3,vjust=0,hjust=0) +  # add the site labels
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=10), # remove x-axis labels
        axis.title.y = element_text(size=10), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

########Statistical proof 
#Bootstrapping and testing for differences between the groups
o_fit <- adonis(o_data_taxa ~ Type_soil , data = o_data_sample, permutations=999, method="bray")
o_fit

#Check assumption of homogeneity of multivariate dispersion
o_distances_data <- vegdist(o_data_taxa)
anova(betadisper(o_distances_data, o_data_sample$Type_soil)) ##PB

###### NMDS FOR FAMILY #######

#Attach data
df_family <- read.xlsx("ITS2_family_reads.xlsx")
attach(df_family)
str(df_family)

#Add a column specifying if the sample is bulk or rhizospheric soil
df_family <- mutate(df_family, Type_soil = ifelse(grepl("B", df_family$Samples),"Bulk","Rhizosphere" ) ) %>%
  select(Samples, Type_soil, everything()) %>% 
  select(-unidentified)

#subset the dataframe on which to base the ordination (data_taxa)
f_data_taxa <- df_family[,3:length(df_family)]
rownames(f_data_taxa) <- df_family$Samples

#Identify the columns that contains the descriptive/environmental data (data_sample)
f_data_sample <- df_family[,1:2]

#ordination by NMDS. WRITE DOWN the 20th stress indicator
NMDS_family <- metaMDS(f_data_taxa, distance = "bray", k = 2)

######Data visualisation

#Create a table with the results of the NMDS for the samples
f_sample_scores <- as.data.frame(scores(NMDS_family))
f_sample_scores$site <- rownames(f_sample_scores)
f_sample_scores$Habitat <- df_family$Type_soil

#Create a table with the results of the NMDS for the phylum
f_scores <- as.data.frame(scores(NMDS_family, "species"))
f_scores$Order <- rownames(f_scores)

ggplot() + 
  stat_ellipse(data = f_sample_scores, aes(x=NMDS1,y=NMDS2,colour = Habitat), size = 1) +
  geom_point(data = f_sample_scores,mapping = aes(x = NMDS1,y = NMDS2, shape = Habitat, colour = Habitat),size=4) + # add the point markers
  geom_text(data = f_scores,aes(x=NMDS1,y=NMDS2,label = Order),alpha=0.5) +  # add the species labels
  geom_text(data = f_sample_scores,aes(x=NMDS1,y=NMDS2,label = site),size = 3,vjust=0,hjust=0) +  # add the site labels
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=10), # remove x-axis labels
        axis.title.y = element_text(size=10), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

########Statistical proof 
#Bootstrapping and testing for differences between the groups
f_fit <- adonis(f_data_taxa ~ Type_soil , data = f_data_sample, permutations=999, method="bray")
f_fit

#Check assumption of homogeneity of multivariate dispersion
distances_data <- vegdist(data_taxa)
anova(betadisper(distances_data, data_sample$Type_soil))

###### NMDS FOR FAMILY #######

#Attach data
df_family <- read.xlsx("ITS2_family_reads.xlsx")
attach(df_family)
str(df_family)

#Add a column specifying if the sample is bulk or rhizospheric soil
df_family <- mutate(df_family, Type_soil = ifelse(grepl("B", df_family$Samples),"Bulk","Rhizosphere" ) ) %>%
  select(Samples, Type_soil, everything()) %>% 
  select(-unidentified)

#subset the dataframe on which to base the ordination (data_taxa)
f_data_taxa <- df_family[,3:length(df_family)]
rownames(f_data_taxa) <- df_family$Samples

#Identify the columns that contains the descriptive/environmental data (data_sample)
f_data_sample <- df_family[,1:2]

#ordination by NMDS. WRITE DOWN the 20th stress indicator
NMDS_family <- metaMDS(f_data_taxa, distance = "bray", k = 2)

######Data visualisation

#Create a table with the results of the NMDS for the samples
f_sample_scores <- as.data.frame(scores(NMDS_family))
f_sample_scores$site <- rownames(f_sample_scores)
f_sample_scores$Habitat <- df_family$Type_soil

#Create a table with the results of the NMDS for the phylum
f_scores <- as.data.frame(scores(NMDS_family, "species"))
f_scores$Order <- rownames(f_scores)

ggplot() + 
  stat_ellipse(data = f_sample_scores, aes(x=NMDS1,y=NMDS2,colour = Habitat), size = 1) +
  geom_point(data = f_sample_scores,mapping = aes(x = NMDS1,y = NMDS2, shape = Habitat, colour = Habitat),size=4) + # add the point markers
  geom_text(data = f_scores,aes(x=NMDS1,y=NMDS2,label = Order),alpha=0.5) +  # add the species labels
  geom_text(data = f_sample_scores,aes(x=NMDS1,y=NMDS2,label = site),size = 3,vjust=0,hjust=0) +  # add the site labels
  coord_equal() +
  theme_bw() + 
  theme(axis.text.x = element_blank(),  # remove x-axis text
        axis.text.y = element_blank(), # remove y-axis text
        axis.ticks = element_blank(),  # remove axis ticks
        axis.title.x = element_text(size=10), # remove x-axis labels
        axis.title.y = element_text(size=10), # remove y-axis labels
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())

########Statistical proof 
#Bootstrapping and testing for differences between the groups
o_fit <- adonis(o_data_taxa ~ Type_soil , data = o_data_sample, permutations=999, method="bray")
o_fit

#Check assumption of homogeneity of multivariate dispersion
distances_data <- vegdist(data_taxa)
anova(betadisper(distances_data, data_sample$Type_soil))
