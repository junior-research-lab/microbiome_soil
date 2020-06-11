#Attach data
dataNMDS <- read.table("NMDS.txt",h=T)
attach(dataNMDS)
str(dataNMDS)


#Install packages
install.packages("vegan")
library("vegan")


#########################
#subset the dataframe on which to base the ordination (dataframe 1)
data_1 <- dataNMDS[,3:11]

#Identify the columns that contains the descriptive/environmental data (dataframe 2)
data_2 <- dataNMDS[,1:2]


#ordination by NMDS
NMDS <- metaMDS(data_1, distance = "bray", k = 2)



#########################
#Data visualisation

#Plot ordination so that points are coloured and shaped according to the groups of interest
co=c("red","blue")
shape=c(18,16)
plot(NMDS$points, col=co[data_2$Habitat],  pch = shape[data_2$Habitat], 
     cex=1.2, main="Vegetation community composition",  xlab = "axis 1", ylab = "axis 2")

#Connect the points that belong to the same treatment with ordispider
ordispider(NMDS, groups = data_2$Habitat,  label = TRUE)

#Add legend
txt <- c("Grassland","Marsh")
legend('topleft', txt , pch=c(18,16),col=c("red","blue"),cex=1, bty = "y")



#####################
#Bootstrapping and testing for differences between the groups
fit <- adonis(data_1 ~ Habitat, data=data_2, permutations=999, method="bray")
fit


#####################
#Check assumption of homogeneity of multivariate dispersion
distances_data <- vegdist(data_1)
anova(betadisper(distances_data, data_2$Habitat))

