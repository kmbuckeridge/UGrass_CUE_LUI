setwd("/Users/katebuckeridge/Dropbox/R/cue")
setwd("C:/Users/kbuckeri/Dropbox/R/Ugrass/cue") ## at work

library(indicspecies)

##################################################################################

otu <- read.csv("otu_ITS.csv", header=TRUE, sep=",", row.names = 1) #spp in columns

#getrid of all of the all-zero rows - these are here because I removed sites when I subsampled my sites from the whole UGrass collection
otu1 <- subset(otu, sum != 0)
otu1 <- otu1[-grep('sum',colnames(otu1))]
head(otu1)

tax <- otu1$taxonomy # makes a taxonomy only object

otu2 <- otu1[,1:130] # makes a file without taxonomy and sum column
head(otu2, 3)

##transpose table, need sites in rows and spp in columns
otu3 <- as.data.frame(t(otu2))

otuR <- read.csv("otu_ITS.csv", header=TRUE, sep=",")
head(otuR)
#added this file so OTU.ID was not a row name, instead a column that I can subset

##################################################################################

###just 2 groups

#make CUE N groups 
meta <- read.csv("metaotu2017.csv", header=TRUE, sep=",", row.names = 1)
cuen <- meta$CUE_N
cuen1 <- scale(cuen) # standardize variables
# Determine number of clusters
wss <- (nrow(cuen1)-1)*sum(apply(cuen1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cuen1, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(cuen1, 2) # 2 cluster solution based on plot
# get cluster means 
aggregate(cuen,by=list(fit$cluster),FUN=mean)
# append cluster assignment
cuen2 <- data.frame(cuen1, fit$cluster)
N2 <- cuen2$fit.cluster

#make CUE G groups
cueg <- meta$CUE_G
cueg1 <- scale(cueg) # standardize variables
# Determine number of clusters
wss <- (nrow(cueg1)-1)*sum(apply(cueg1,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(cueg1, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
# K-Means Cluster Analysis
fit <- kmeans(cueg1, 2) # 2 cluster solution based on plot
# get cluster means 
aggregate(cueg1,by=list(fit$cluster),FUN=mean)
# append cluster assignment
cueg2 <- data.frame(cueg1, fit$cluster)
G2 <- cueg2$fit.cluster

####################################################################################
# run indicator species
#A is the probability that the surveyed site belongs to the target site group given the fact that the species has been found (specificity or predictive value of Ind Sp)
#B is the probability of finding the species in sites belonging to the site group (fidelity or sensitivity of spp as IndSp)

indvalN = multipatt(otu3, N2, duleg = TRUE, control = how(nperm=999))
options(max.print = 99999999)
sink("indvalN_ITS.txt", append=TRUE, split=FALSE)
summary(indvalN, indvalcomp=TRUE)
sink()

indvalG = multipatt(otu3, G2, duleg = TRUE, control = how(nperm=999))
options(max.print = 99999999)
sink("indvalG_ITS.txt", append=TRUE, split=FALSE)
summary(indvalG, indvalcomp=TRUE)
sink()


####################################################################################
## extract indicator species OTUs from multipatt and make new relative abundance charts for each cluster of low, mid and high CUE. To do this, need 3 columns: the OTU names for all groups, the group assignment, and the associated taxonomy in a data frame. First make csv table with OTUs and group numbers (*), from output txt file, then subset matching tax from otuR
## * cut and paste the otu column from the indval.txt output and add a group affliation (1, 2, 3) column and save this as CUE[G or N]indicspp_ITS.csv

cuegIS <- read.csv("cueGindicspp_ITS.csv", header=TRUE, sep=",")
# assign tax to OTUs
ids <- cuegIS$OTU.ID
head(ids)
head(otuR$taxonomy)

taxIS <- character(length(ids))
for (i in 1:length(ids)){
  taxIS[i] <- as.character(otuR$taxonomy[otuR$OTU == as.character(ids[i])])
}

### bind together the original file and the taxonomy column and write to .csv
cuegIS2 <- cbind(cuegIS, taxIS)

write.csv(cuegIS2, "cueGindicspp2_ITS.csv")


##### CUE N indic spp

cuenIS <- read.csv("cueNindicspp_ITS.csv", header=TRUE, sep=",")
# assign tax to OTUs
idsN <- cuenIS$OTU.ID
head(idsN)

taxISn <- character(length(idsN))
for (i in 1:length(idsN)){
  taxISn[i] <- as.character(otuR$taxonomy[otuR$OTU == as.character(idsN[i])])
}

cuenIS2 <- cbind(cuenIS, taxISn)
head(cuenIS2)

cuen.test <- otuR$taxonomy[otuR$OTU == "OTU4521"] 
cuen.test 

write.csv(cuenIS2, "cueNindicspp2_ITS.csv")

