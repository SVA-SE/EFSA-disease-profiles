### DACRAH3# - D - PATHOGEN SURVIVAL (PS) ###

## This script is for data cleaning of the whole dataset of pathogen survival

## working directory assumed to be the folder where this file lives
## getwd()
## Loading necessary packages
library(readr)
library(dplyr)
library(stringr)

#source('PathogenSurvival_definitions.R')

## Loading dataset retrieved from Distiller ----
dfps <- read.csv("data/DistillerData_raw/PathogenSurvival.csv",
                 header=T, sep=",", stringsAsFactors = F, dec = ".",encoding  = "UTF-8")

## Distiller exports the data with large text or with codes.
## The use of such column names would be impractical.
## Thus, a dataset with column names was created.
columnsPS <- read.csv("SLR_objectives/PathogenSurvival_columnNames.csv",
                      header=T, sep=",", stringsAsFactors = F)
if(dim(columnsPS)[2]>2)(columnsPS[,1]<-NULL)



# Naming columns in our df
if (dim(columnsPS)[1]==dim(dfps)[2]){
  colnames(dfps)<- columnsPS[,2]
}else{print("ERROR!")}
rm(columnsPS)

#a row count used to match data in some operations
dfps$rowID <- 1:dim(dfps)[1]

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
dfps[sapply(dfps, function(x) all(is.na(x)))] <- NULL

##refID
#dfps$refID
#any(is.na(dfps$refID))

dfps$user <- NULL
dfps$level <- NULL

dfps$studyID <- NULL

## groupID
dfps$groupID <- as.character (dfps$groupID)
dfps$groupID <- (str_sub(dfps$groupID, start = -2))
dfps$groupID <- as.integer (dfps$groupID)


## uniqueID ----
## "uniqueID" is the merge of refID and groupID.
## this is created so that we can detect multiple rows of data referring to the same group, globally
## without having to refer to the refID columns everytime.
dfps$uniqueID <- paste(dfps$refID, dfps$groupID, sep="")

## agent
#table(dfps$agent)
#dfps$agent[dfps$agent=="African Swine fever"]<-"African swine fever virus"

dfps$agent <- factor (dfps$agent)
#dfps %>% count (agent, sort =T)

## agent subtype
dfps$agentSubtype [dfps$agentSubtype == ""] <- "not investigated/not given/not relevant"
dfps$agentSubtype <- factor (dfps$agentSubtype)
#dfps %>% count (agentSubtype, sort = T)

## agentDetails
dfps$agentDetails [dfps$agentDetails == "not given"] <- "not investigated/not given/not relevant"
dfps$agentDetails [dfps$agentDetails == ""] <- "not investigated/not given/not relevant"
dfps$agentDetails <- factor (dfps$agentDetails)
#dfps %>% count (agentDetails, sort = T) %>% print (n = 32)

## agentSubtypeDC1
dfps$agentSubtypeDC1 [dfps$agentSubtypeDC1 == ""] <- "not investigated/not given/not relevant"
dfps$agentSubtypeDC1 <- factor (dfps$agentSubtypeDC1)
#dfps %>% count (agentSubtypeDC1, sort = T)

## targetSpecies
dfps$targetSpecies <- gsub('Pig (Sus scrofa domesticus) ', "Pig (Sus scrofa domesticus)",
                           dfps$targetSpecies, fixed=TRUE)
dfps$targetSpecies <- gsub('Human (homo sapiens) ', "Human (homo sapiens)",
                           dfps$targetSpecies, fixed=TRUE)
dfps$targetSpecies [dfps$targetSpecies == "Not applicable"] <- "not investigated/not given/not relevant"
dfps$targetSpecies <- factor (dfps$targetSpecies)
#dfps %>% count (targetSpecies, sort = T)

## sampUnitSize
dfps$sampUnitSize [dfps$sampUnitSize == -1] <- NA
dfps$sampUnitSize <- as.numeric(dfps$sampUnitSize)

## matrix
dfps$matrix <- factor (dfps$matrix)
#dfps %>% count (matrix, sort = T) %>% print (n= 33)

## matrixDetails
dfps$matrixDetails [dfps$matrixDetails == ""] <- "not investigated/not given/not relevant"
dfps$matrixDetails <- factor (dfps$matrixDetails)
#dfps %>% count (matrixDetails, sort = T) %>% print (n= 61)

## temperature (celsius)
#dfps$temperature

## humidity (%)
dfps$humidity <- as.numeric(dfps$humidity)

## labTest
dfps$labTest <- gsub('Haemadsorption assay (HAT test) ', "Haemadsorption assay (HAT test)",
                     dfps$labTest, fixed=TRUE)
dfps$labTest <- factor (dfps$labTest)
#dfps %>% count (labTest, sort =T)

## targetLab
dfps$targetLab <- factor (dfps$targetLab)
#dfps %>% count (targetLab, sort = T)

## maxDetect (days)
dfps$maxDetect [dfps$maxDetect == -1] <- NA
#summary(dfps$maxDetect)

## truncated
dfps$truncated [dfps$truncated == ""] <- "not investigated/not given/not relevant"
dfps$truncated <- factor (dfps$truncated)
#dfps %>% count (truncated, sort =T)

## halfLife
#summary(dfps$halfLife)

## LCI_halfLife
#dfps$LCI_halfLife

## UCI_halfLife
#dfps$UCI_halfLife
# what to do with "inf"
#dfps %>% select (refID, UCI_halfLife) %>% filter (UCI_halfLife == "inf")
dfps$UCI_halfLife <- gsub(",", ".", dfps$UCI_halfLife, fixed=TRUE)
dfps$UCI_halfLife <- as.numeric (as.character(dfps$UCI_halfLife))

## strengths
dfps$strengths <- NULL

## weaknesses
dfps$weaknesses <- NULL


# refs ----

references <- read.csv("data/DistillerData_raw/PathogenSurvival_refid_distiller.csv",
                       header=T, sep=",", stringsAsFactors = F,encoding  = "UTF-8")
colnames(references)<-  c("Refid","ShortBibliography","Author","Title","Abstract", "publicationYear", "user", "level","agent")
references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
references <- references[,c("Refid", "agent", "ShortBibliography", "FullBibliography", "Author","Title","Abstract", "publicationYear")]
references <- references[!duplicated(references[,c(1,2)]),]
references$FullBibliography <- as.character(references$FullBibliography)
for (r in 1:dim(references)[1]){
    references$FullBibliography[r]<- dfps$FullReference[which(dfps$refID==references$Refid[r])[1]]
}
write.csv(references, file="data/FilesDownload/PathogenSurvival_refid.csv", row.names = F)
#write.csv2(references, file="data/FilesDownload/DiagnosticTests_refid2.csv", row.names = F)


ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
ref.merge <- unique(ref.merge)
dfps <- merge(dfps,ref.merge,by.x="refID",by.y="Refid")

dfps$humidity <- str_remove(dfps$humidity,"\n")

## To export data as .csv
write.csv(dfps, file = "data/FilesDownload/PathogenSurvival_cleaned.csv", row.names = F)
#write.csv2(dfps, file = "data/FilesDownload/PathogenSurvival_cleaned2.csv", row.names = F)


#
#
#
#
#
# references <- read.csv("data/DistillerData_raw/PathogenSurvival_refid.csv")
# ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
# ref.merge <- unique(ref.merge)
# dfps <- merge(dfps,ref.merge,by.x="refID",by.y="Refid")
#
# ## To export data as .csv
# write.csv(dfps, file = "DistillerData_cleaned/PathogenSurvival_cleaned.csv", row.names = F)
# write.csv2(dfps, file = "DistillerData_cleaned/PathogenSurvival_cleaned2.csv", row.names = F)
#
# # #code to update the "refid" file when downloaded directly from Distiller,
# # #instead of using the one on the folder
# #
# # references <- read.csv("DistillerData_raw/PathogenSurvival_refid_distiller.csv",
# #                      header=T, sep=",", stringsAsFactors = F)
# # colnames(references)<-  c("Refid", "ShortBibliography","Author","Title","Abstract", "URL","publicationYear", "User", "level","agent")#,"targetSpecies","X")
# # references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
# # references <- references[,c("Refid","agent","Title", "ShortBibliography","FullBibliography","Author","Abstract", "publicationYear")]
# # references <- references[!duplicated(references[,c(1,2,3)]),]
# # references$FullBibliography <- as.character(references$FullBibliography)
# # for (r in 1:dim(references)[1]){
# #   references$FullBibliography[r]<- dfps$FullReference[which(dfps$refID==references$Refid[r])[1]]
# # }
# # write.csv(references, file="DistillerData_cleaned/PathogenSurvival_refid.csv", row.names = F)
# # write.csv2(references, file="DistillerData_cleaned/PathogenSurvival_refid2.csv", row.names = F)
