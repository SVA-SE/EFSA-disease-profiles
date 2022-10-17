### DACRAH3# - E -  GEOGRAPHICAL DISTRIBUTION (GD) ###

## This script is for data cleaning of the whole dataset of geographical distribution

## working directory assumed to be the folder where this file lives
## setwd("location")
## getwd()
## Loading necessary packages
library(readr)
library(dplyr)
library(stringr)

#source('GeoDistribution_definitions.R')

## Loading dataset retrieved from Distiller ----
dfgd <- read.csv("data/DistillerData_raw/GeoDistribution.csv",
                 header=T, sep=",", stringsAsFactors = F, dec = ".",encoding  = "UTF-8")

## Distiller exports the data with large text or with codes.
## The use of such column names would be impractical.
## Thus, a dataset with column names was created.
columnsGD <- read.csv("SLR_objectives/GeoDistribution_columnNames.csv",
                      header=F, sep=",", stringsAsFactors = F)
#columnsGD <- read.csv2("SLR_objectives/GeoDistribution_columnNamesCSV2.csv",header=F, stringsAsFactors = F)
#write.table(columnsGD,file="SLR_objectives/GeoDistribution_columnNames.csv",col.names=FALSE,row.names=FALSE,sep=",")

# Naming columns in our df
if (dim(columnsGD)[1]==dim(dfgd)[2]){
  colnames(dfgd)<- columnsGD[,2]
}else{print("ERROR!")}
rm(columnsGD)

#a row count used to match data in some operations
dfgd$rowID <- 1:dim(dfgd)[1]

dfgd$uniqueID <- paste0(dfgd$refID,dfgd$groupID)

## Unnecessary information in our df will be excluded
## Proper classes will be given for variables that we will use
## for descriptives and plotting in R.
## If the class of the variable is numerical,
## missing values not treated as NA will be converted to NA.
## If the class of the variable is categorical,
## missing values will be converted to some categorical expression
## according to information found in the DACRAH 2 final report

## Many columns may be completely empty, since we chose to export ALL columns
## in Distiller (not hide empty columns).
## This was done for consistency (so that we would always have the same dataset out).
## Removing columns with only NA:
dfgd[sapply(dfgd, function(x) all(is.na(x)))] <- NULL

##refID
#any(is.na(dfgd$refID))

dfgd$user <- NULL
dfgd$level <- NULL

## groupID
colnames(dfgd)[which(names(dfgd) == "studyID")] <- "groupID"

## studyContext
dfgd$studyContext [dfgd$studyContext == "Other: specify"] <- "Other"
dfgd$studyContext <- factor (dfgd$studyContext)
#dfgd %>% count (studyContext, sort = T)

## studyContext_C
dfgd$studyContext_C [dfgd$studyContext_C == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (studyContext_C, sort = T)
#dfgd %>% select( refID, studyContext_C) %>% filter (studyContext_C == "Article")

## sampStrategy
dfgd$sampStrategy [dfgd$sampStrategy == ""] <- "not investigated/not given/not relevant"
dfgd$sampStrategy [dfgd$sampStrategy == "Unspecified"] <- "not investigated/not given/not relevant"
dfgd$sampStrategy <- str_replace(dfgd$sampStrategy,": specify","")
dfgd$sampStrategy <- factor (dfgd$sampStrategy)
#dfgd %>% count (sampStrategy, sort = T)

## sampStrategy_C
dfgd$sampStrategy_C [dfgd$sampStrategy_C == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (sampStrategy_C, sort = T)

## sampPoint
dfgd$sampPoint <- gsub ('Vet/Clinic ', "Vet/Clinic", dfgd$sampPoint, fixed = T)
dfgd$sampPoint [dfgd$sampPoint == ""] <- "not investigated/not given/not relevant"
dfgd$sampPoint [dfgd$sampPoint == "Unknown"] <- "not investigated/not given/not relevant"
dfgd$sampPoint [dfgd$sampPoint == "Unspecified"] <- "not investigated/not given/not relevant"
dfgd$sampPoint [dfgd$sampPoint == "Other: specify"] <- "Other"
dfgd$sampPoint <- factor (dfgd$sampPoint)
#dfgd %>% count (sampPoint, sort = T)

## sampPoint_C
dfgd$sampPoint_C [dfgd$sampPoint_C == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (sampPoint_C, sort = T)

# ## progType
# dfgd$progType <- gsub ('Survey ', "Survey", dfgd$progType, fixed = T)
# dfgd$progType [dfgd$progType == ""] <- "not investigated/not given/not relevant"
# dfgd$progType [dfgd$progType == "Unspecified"] <- "not investigated/not given/not relevant"
# dfgd$progType [dfgd$progType == "Monitoring &ndash;active"] <- "Monitoring - active"
# dfgd$progType [dfgd$progType == "Monitoring &ndash; passive"] <- "Monitoring - passive"
# dfgd$progType [dfgd$progType == "Other: specify"] <- "Other"
# dfgd$progType <- factor (dfgd$progType)
# #dfgd %>% count (progType, sort = T)
#
# ## progType_C
# dfgd$progType_C [dfgd$progType_C == ""] <- "not investigated/not given/not relevant"
# #dfgd %>% count (progType_C, sort =T)

## minAgeMonths
dfgd$minAgeMonths [dfgd$minAgeMonths == -1] <- NA
#summary(dfgd$minAgeMonths)

## maxAgeMonths
dfgd$maxAgeMonths [dfgd$maxAgeMonths == -1] <- NA
dfgd$maxAgeMonths <- as.numeric(dfgd$maxAgeMonths)
#summary(dfgd$maxAgeMonths)

## sampUnit
dfgd$sampUnit [dfgd$sampUnit == "Other: specify"] <- "Other"
dfgd$sampUnit <- factor (dfgd$sampUnit)
#dfgd %>% count (sampUnit, sort = T)

## sampUnit_C
dfgd$sampUnit_C [dfgd$sampUnit_C == ""] <- "not investigated/not given/not relevant"
dfgd$sampUnit_C [dfgd$sampUnit_C == "Unknown"] <- "not investigated/not given/not relevant"
#dfgd %>% count (sampUnit_C, sort = T)

## sampUnitSize
dfgd$sampUnitSize <- as.numeric (dfgd$sampUnitSize)
#summary(dfgd$sampUnitSize)
#dfgd %>% select (refID, sampUnit, sampUnitSize) %>% filter (is.na (dfgd$sampUnitSize))

## country
dfgd$country [dfgd$country == ""] <- "not investigated/not given/not relevant"
dfgd$country [dfgd$country == "Unknown"] <- "not investigated/not given/not relevant"
dfgd$country <- factor (dfgd$country)
#dfgd %>% count (country, sort = T) %>% print (n=110)

## sampArea
dfgd$sampArea [dfgd$sampArea == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (sampArea, sort = T) %>% print (n = 312)

## startYear
dfgd$startYear [dfgd$startYear == -1] <- NA
dfgd$startYear [dfgd$startYear == 0] <- NA
#summary (dfgd$startYear)

## StartMonth
dfgd$StartMonth [dfgd$StartMonth == -1] <- NA
dfgd$StartMonth <- ifelse (dfgd$StartMonth <= 12, dfgd$StartMonth, NA)
#summary (dfgd$StartMonth)

## duration (months)
dfgd$duration [dfgd$duration == -1] <- NA
#summary (dfgd$duration)

## phase
dfgd$phase [dfgd$phase == ""] <- "not investigated/not given/not relevant"
dfgd$phase [dfgd$phase == "Not reported"] <- "not investigated/not given/not relevant"
dfgd$phase <- factor (dfgd$phase)
#dfgd %>% count (phase, sort = T)

## targetSpecies
dfgd$targetSpecies [dfgd$targetSpecies == ""] <- "not investigated/not given/not relevant"
dfgd$targetSpecies <- gsub ('Pig (Sus scrofa domesticus) ', "Pig (Sus scrofa domesticus)", dfgd$targetSpecies, fixed = T)
dfgd$targetSpecies <- gsub ('Bats ', "Bats", dfgd$targetSpecies, fixed = T)
dfgd$targetSpecies <- str_replace(dfgd$targetSpecies,": specify","")
dfgd$targetSpecies <- factor (dfgd$targetSpecies)
#dfgd %>% count (targetSpecies, sort = T) %>% print (n=25)
#dfgd %>% select (refID, targetSpecies) %>% filter (dfgd$targetSpecies =="not investigatedd/not given/not relevant")

## matrix
dfgd$matrix [dfgd$matrix == "Other: specify"] <- "Other"
dfgd$matrix [dfgd$matrix == ""] <- "not investigated/not given/not relevant"
dfgd$matrix [dfgd$matrix == "Unspecified"] <- "not investigated/not given/not relevant"
dfgd$matrix <- gsub ('Skin ', "Skin", dfgd$matrix, fixed = T)
dfgd$matrix <- gsub ('Lymph node ', "Lymph node", dfgd$matrix, fixed = T)
dfgd$matrix <- factor (dfgd$matrix)
#dfgd %>% count (matrix, sort = T)

## matrix_C
dfgd$matrix_C [dfgd$matrix_C == ""] <- "not investigated/not given/not relevant"
dfgd$matrix_C [dfgd$matrix_C == "n/a"] <- "not investigated/not given/not relevant"
dfgd$matrix_C [dfgd$matrix_C == "Unknow n"] <- "not investigated/not given/not relevant"
dfgd$matrix_C [dfgd$matrix_C == "unknown"] <- "not investigated/not given/not relevant"
#dfgd %>% count (matrix_C, sort = T)

## analysisYear
dfgd$analysisYear [dfgd$analysisYear == -1] <- NA
dfgd$analysisYear [dfgd$analysisYear == 0] <- NA
summary(dfgd$analysisYear)

## labTarget
dfgd$labTarget [dfgd$labTarget  == ""] <- "not investigated/not given/not relevant"
dfgd$labTarget [dfgd$labTarget  == "Unspecified"] <- "not investigated/not given/not relevant"
dfgd$labTarget <- factor (dfgd$labTarget)
#dfgd %>% count (labTarget, sort = T)

## labTarget_C
dfgd$labTarget_C [dfgd$labTarget_C == ""] <- "not investigated/not given/not relevant"
dfgd$labTarget_C [dfgd$labTarget_C == "Unknown"] <- "not investigated/not given/not relevant"
dfgd$labTarget_C [dfgd$labTarget_C == "n/a"] <- "not investigated/not given/not relevant"
#dfgd %>% count (labTarget_C, sort = T)

## agent
dfgd$agent <- factor (dfgd$agent)
#dfgd %>% count (agent, sort = T) %>% print (n= 25)

## agentSubtype
dfgd$agentSubtype [dfgd$agentSubtype == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (agentSubtype, sort = T)

## agentDetails
dfgd$agentDetails [dfgd$agentDetails == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (agentDetails, sort = T)

## agentSubtypeDC1
dfgd$agentSubtypeDC1 [dfgd$agentSubtypeDC1 == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (agentSubtypeDC1, sort = T)

## labTest
dfgd$labTest [dfgd$labTest == ""] <- "not investigated/not given/not relevant"
dfgd$labTest [dfgd$labTest == "Unspecified"] <- "not investigated/not given/not relevant"
dfgd$labTest <- gsub ('Enzymatic tests ', "Enzymatic tests", dfgd$labTest, fixed = T)
dfgd$labTest <- str_replace(dfgd$labTest,": specify","")
dfgd$labTest <- factor (dfgd$labTest)
#dfgd %>% count (labTest, sort = T) %>% print (n= 28)

## labTest_C
dfgd$labTest_C [dfgd$labTest_C == ""] <- "not investigated/not given/not relevant"
dfgd$labTest_C [dfgd$labTest_C == "n/a"] <- "not investigated/not given/not relevant"
dfgd$labTest_C [dfgd$labTest_C == "Unknown"] <- "not investigated/not given/not relevant"
#dfgd %>% count (labTest_C, sort =T)

## labDescription
dfgd$labDescription [dfgd$labDescription == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (labDescription, sort =T) %>% print (n= 131)

## nPositive
#summary(dfgd$nPositive)

## nNegative
dfgd$nNegative <- as.numeric (dfgd$nNegative)
#summary(dfgd$nNegative)

## sensitivity
#summary(dfgd$sensitivity)

## specificity
#summary(dfgd$specificity)

## prevalence
#summary (dfgd$prevalence)
#dfgd %>% select (refID, prevalence, sampUnitSize, nPositive,nNegative, incidence, R0) %>%
#  filter (is.na(dfgd$prevalence) & is.na (dfgd$nPositive) & is.na (dfgd$nNegative) & is.na (dfgd$incidence) & is.na (dfgd$R0))

## UCI_prev
#summary(dfgd$UCI_prev)

## LCI_prev
#summary(dfgd$LCI_prev)

## incidence
#summary(dfgd$incidence)

## UCI_incid
#summary(dfgd$incidence)

## LCI_incid
#summary(dfgd$LCI_incid)

## incidUnit
dfgd$incidUnit [dfgd$incidUnit == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (incidUnit)

## incidUnit2
dfgd$incidUnit2 [dfgd$incidUnit2 == ""] <- "not investigated/not given/not relevant"
#dfgd %>% count (incidUnit2)

## R0
#summary(dfgd$R0)

## lowerR0
#summary(dfgd$lowerR0)

## upperR0
#summary(dfgd$upperR0)

# ## resInfo
# dfgd$resInfo [dfgd$resInfo == ""] <- "not investigated/not given/not relevant"
# #dfgd %>% count (resInfo, sort = T)

## Delete colum with unique values, e.g. labTest_C
uniquelength <- sapply(dfgd,function(x) length(unique(x)))
ischr <- sapply(dfgd,inherits,"character")
dfgd <- subset(dfgd, select=!ischr | uniquelength>1)






references <- read.csv("data/DistillerData_raw/GeoDistribution_refid_distiller.csv",
            header=T, sep=",", stringsAsFactors = F,encoding  = "UTF-8")
colnames(references)<-  c("Refid","ShortBibliography","Author","Title", "Abstract", "publicationYear", "User", "Level","agent","Country","counry_c","targetSpecies","species_c")
references$FullBibliography <- paste(references$Author,
                                     references$publicationYear,references$Title,sep=".")

references$FullBibliography <- as.character(references$FullBibliography)
for (r in 1:dim(references)[1]){
    references$FullBibliography[r]<- dfgd$FullReference[which(dfgd$refID==references$Refid[r])[1]]
}
references <- references[,c("Refid","agent","Country","targetSpecies","ShortBibliography", "FullBibliography","Author", "Title", "Abstract", "publicationYear")]
references <- references[!duplicated(references[,c(1,2,3,4)]),]

write.csv(references, file="data/FilesDownload/GeoDistribution_refid.csv", row.names = F)
#write.csv2(references, file="data/FilesDownload/GeoDistribution_refid2.csv", row.names = F)



ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
ref.merge <- unique(ref.merge)
dfgd <- merge(dfgd,ref.merge,by.x="refID",by.y="Refid")

## To export data as .csv
write.csv(dfgd, file = "data/FilesDownload/GeoDistribution_cleaned.csv", row.names = F)
#write.csv2(dfgd, file = "data/FilesDownload/GeoDistribution_cleaned2.csv", row.names = F)

# ### END OF DATA CLEANING #######################################################################################################


# #code to update the "refid" file when downloaded directly from Distiller,
# #instead of using the one on the folder

  # references <- read.csv("GeoDistribution_refid_distiller.csv",
  #                        header=T, sep=",", stringsAsFactors = F)

 #colnames(references)<-  c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear" ,"user", "level","country","targetSpecies","agent","X")
 #references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
# references$FullBibliography <- as.character(references$FullBibliography)
# for (r in 1:dim(references)[1]){
#   references$FullBibliography[r]<- dfgd$FullReference[which(dfgd$refID==references$Refid[r])[1]]
# }
 #references <- references[,c("Refid","country","agent","targetSpecies","ShortBibliography","FullBibliography","Author","Title","Abstract","publicationYear")]
 #references$targetSpecies [references$targetSpecies == ""] <- "not investigated/not given/not relevant"
 # references <- references[!duplicated(references[,c(1,2,3,4)]),]

# write.csv(references, file="GeoDistribution_refid.csv", row.names = F)
# write.csv2(references, file="GeoDistribution_refid2.csv", row.names = F)
#
#
#
#
#
#
