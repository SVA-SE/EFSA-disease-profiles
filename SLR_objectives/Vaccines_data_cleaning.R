### DACRAH3# - F -  VACCINE CHALLENGES (VC) ###

## This script is for data cleaning of the whole dataset of vaccine challenges

## working directory assumed to be the folder where this file lives
## setwd("location")
## getwd()
## Loading necessary packages
library(readr)
library(dplyr)
library(stringr)

#source('Vaccines_0_definitions.R')

## Loading dataset retrieved from Distiller ----
dfva <- read.csv("data/DistillerData_raw/Vaccines.csv",
                 header=T, sep=",", stringsAsFactors = F, dec = ".",encoding  = "UTF-8")

## Distiller exports the data with large text or with codes.
## The use of such column names would be impractical.
## Thus, a dataset with column names was created.
columnsVA <- read.csv("SLR_objectives/Vaccines_columnNames.csv",
                      header=F, sep=",", stringsAsFactors = F)
#columnsVA <- read.csv2("SLR_objectives/Vaccines_columnNamesCSV2.csv",header=F, stringsAsFactors = F)
#write.table(columnsVA,file="SLR_objectives/Vaccines_columnNames.csv",col.names=FALSE,row.names=FALSE,sep=",")
#rm(list=ls())




# Naming columns in our df
if (dim(columnsVA)[1]==dim(dfva)[2]){
  colnames(dfva)<- columnsVA[,2]
}else{print("ERROR!")}
rm(columnsVA)

#a row count used to match data in some operations
dfva$rowID <- 1:dim(dfva)[1]

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
dfva[sapply(dfva, function(x) all(is.na(x)))] <- NULL

##refID
#any(is.na(dfva$refID))

dfva$user <- NULL
dfva$level <- NULL

## groupID
colnames(dfva)[which(names(dfva) == "studyID")] <- "groupID"

## uniqueID ----
## "uniqueID" is the merge of refID and groupID.
## this is created so that we can detect multiple rows of data referring to the same group, globally
## without having to refer to the refID columns everytime.
dfva$uniqueID <- paste(dfva$refID, dfva$groupID, sep="")


## country
dfva$country <- factor (dfva$country)
#dfva %>% count (country, sort = T)

## year
#summary(dfva$year)

## agent
dfva$agent <- factor (dfva$agent)
#dfva  %>% count (agent, sort = T)

## agentSubtypeType
if(!is.null(dfva$agentSubtypeType)){
dfva$agentSubtypeType [dfva$agentSubtypeType == ""] <- "not investigated/not given/not relevant"
dfva$agentSubtypeType <- factor (dfva$agentSubtypeType)
}
#dfva %>% count (agentSubtypeType, sort = T)

## agentDetails
#dfva %>% count (agentDetails, sort = T)

## agentSubtypeDC1
#dfva %>% count (agentSubtypeDC1)
## columns agentDetails and agentSubtypeDC1 seems to have the same information, so far BTV-8
## Columns will be merged
if(!is.null(dfva$agentSubtypeType)){
dfva$agentDetails <- ifelse (dfva$agentDetails != "", dfva$agentDetails, dfva$agentSubtypeDC1)
}else{
  dfva$agentDetails <- dfva$agentSubtypeDC1
}
dfva$agentSubtypeDC1 <- NULL
dfva$agentDetails [dfva$agentDetails == ""] <- "not investigated/not given/not relevant"
dfva$agentDetails [dfva$agentDetails == "BTV8"] <- "BTV-8"
dfva$agentDetails <- factor (dfva$agentDetails)

## targetSpecies
dfva$targetSpecies <- factor (dfva$targetSpecies)
#dfva %>% count (targetSpecies, sort = T)


## ageMonths
dfva$ageMonths [dfva$ageMonths == -1] <- NA


## sampUnit
dfva$sampUnit <- factor (dfva$sampUnit)
#dfva %>% count (sampUnit)

## sampUnitSize
dfva$sampUnitSize <- as.numeric (dfva$sampUnitSize)

## route
dfva$route [dfva$route == ""] <- "not investigated/not given/not relevant"
dfva$route <- factor (dfva$route)
#dfva %>% count (route, sort = T)

## testSubstance - Column to be excluded.
#dfva$testSubstance [dfva$testSubstance == ""] <- "not investigated/not given/not relevant"
#dfva$testSubstance <- gsub ('BTV\tBTVPUR Alsap 8', "BTV - tBTVPUR Alsap 8", dfva$testSubstance, fixed = T)
#dfva$testSubstance <- gsub ('SBV\tZulvac SBV', "SBV - tZulvac SBV", dfva$testSubstance, fixed = T)
#dfva$testSubstance <- factor (dfva$testSubstance)
#dfva %>% count (testSubstance, sort = T)

# ## testSubstanceCAT
# #dfva$testSubstanceCAT [dfva$testSubstanceCAT == ""] <- "not investigated/not given/not relevant"
# dfva$testSubstanceCAT <- gsub ('BTV\tBTVPUR AlSap 2-4', "BTV-BTVPUR AlSap 2-4", dfva$testSubstanceCAT, fixed = T)
# dfva$testSubstanceCAT <- gsub ('BTV\tBTVPUR Alsap 8', "BTV-BTVPUR Alsap 8", dfva$testSubstanceCAT, fixed = T)
# dfva$testSubstanceCAT <- gsub ('WNV\tEquip WNV (previously Duvaxyn WNV)', "WNV-Equip WNV (previously Duvaxyn WNV)", dfva$testSubstanceCAT, fixed = T)
# dfva$testSubstanceCAT <- gsub ('SBV\tZulvac SBV', "SBV-Zulvac SBV", dfva$testSubstanceCAT, fixed = T)
# dfva$testSubstanceCAT <- factor (dfva$testSubstanceCAT)
# #dfva %>% count (testSubstanceCAT, sort = T)

#dfva$testSubstance <- NULL


#to be able to separate control groups from intervention easily
dfva$interventionType <- ifelse(
  dfva$testSubstanceCAT=="Placebo"|dfva$testSubstanceCAT=="Unvaccinated control",
  "control",
  "vaccine"
)


# ## dosageFreq
# dfva$dosageFreq [dfva$dosageFreq == -1] <- NA
# dfva$dosageFreq <- as.numeric (dfva$dosageFreq)
#
# ## dosageInterval
# if(!is.null(dfva$dosageInterval)){
# dfva$dosageInterval [dfva$dosageInterval == -1] <- NA
# dfva$dosageInterval <- as.numeric (dfva$dosageInterval)
# }
#
# ## dose
# dfva$dose [dfva$dose == -1] <- NA
# dfva$dose <- as.numeric (dfva$dose)
# #dfva %>% select (refID, groupID, dose, testSubstanceCAT) %>% filter (dose == 0)
#
# ## doseUnits
# dfva$doseUnits [dfva$doseUnits == ""] <- "not investigated/not given/not relevant"
# dfva$doseUnits <- factor (dfva$doseUnits)
# #dfva %>% count (count = doseUnits, sort = T)
#
# ## dayDose1
# dfva$dayDose1 <- as.numeric (dfva$dayDose1)
#
# ## dayDose2
# if(!is.null(dfva$dayDose2)){
# dfva$dayDose2 <- as.numeric(dfva$dayDose2)
# }

## challengeType
if(!is.null(dfva$challengeType)){
dfva$challengeType [dfva$challengeType == ""] <- "not investigated/not given/not relevant"
dfva$challengeType <- factor (dfva$challengeType)
}
#dfva %>% count (challengeType, sort = T)

## challengeSubstance
if(!is.null(dfva$challengeSubstance)){
dfva$challengeSubstance [dfva$challengeSubstance == ""] <- "not investigated/not given/not relevant"
dfva$challengeSubstance <- factor (dfva$challengeSubstance)
}
#dfva %>% count (challengeSubstance)

## challengeDose
dfva$challengeDose [dfva$challengeDose == -1] <- NA

# ## challengeDoseUnits
# dfva$challengeDoseUnits [dfva$challengeDoseUnits == ""] <- "not investigated/not given/not relevant"
# dfva$challengeDoseUnits <- gsub("mililiter", "milliliter", dfva$challengeDoseUnits, fixed = T)
# dfva$challengeDoseUnits <- factor (dfva$challengeDoseUnits)
# #dfva %>% count (challengeDoseUnits, sort = T)

## challengeDay
dfva$challengeDay <- as.numeric(dfva$challengeDay)

# ## lastVaccine
# #which(dfva$lastVaccine > dfva$challengeDay)
# dfva$lastVaccine <- as.numeric(dfva$lastVaccine)

## timePoint
dfva$timePoint <- as.numeric (dfva$timePoint)

## experimentStatus
dfva$experimentStatus [dfva$experimentStatus == ""] <- "not investigated/not given/not relevant"
dfva$experimentStatus <- factor (dfva$experimentStatus)
#dfva %>% count (experimentStatus)

## labTest
dfva$labTest [dfva$labTest == "Not applicable"] <- "not investigated/not given/not relevant"
dfva$labTest <- factor (dfva$labTest)
#dfva %>% count (labTest, sort = T)

## labDescription
dfva$labDescription [dfva$labDescription == ""] <- "not investigated/not given/not relevant"
#dfva %>% count (labDescription)

## labTarget
dfva$labTarget [dfva$labTarget == "Not applicable"] <- "not investigated/not given/not relevant"
dfva %>% count (labTarget)

## matrix
dfva$matrix <- factor (dfva$matrix)
#dfva %>% count (matrix, sort = T)

## nTested
dfva$nTested <- as.numeric (dfva$nTested)

## nPositive
dfva$nPositive <- as.numeric (dfva$nPositive)

## testValue
#dfva$testValue
#dfva %>% select (refID, groupID, nPositive, testValue) %>% filter (is.na(dfva$nPositive))

## testValueUnits
dfva$testValueUnits <- as.character(dfva$testValueUnits)
dfva$testValueUnits [dfva$testValueUnits == ""] <- "not investigated/not given/not relevant"

for (r in 1:dim(dfva)[1]){
  if(is.na(dfva$testValue[r])&!is.na(dfva$nPositive[r])){
    dfva$testValue[r] <- dfva$nPositive[r]/dfva$nTested[r]*100
    dfva$testValueUnits[r] <- paste0("% POS, ", dfva$labTest[r])
  }
}


dfva$testValueUnits <- factor (dfva$testValueUnits)
#dfva %>% count (testValueUnits)

## scaleMin
dfva$scaleMin <- as.numeric (dfva$scaleMin)

## ScaleMax
dfva$ScaleMax <- as.numeric (dfva$scaleMin)

## deadUnits
dfva$deadUnits <- as.numeric (dfva$deadUnits)

## mortalityTime
if(!is.null(dfva$mortalityTime)){
dfva$mortalityTime <- as.numeric (dfva$mortalityTime)
}

## mortalityTimeUnits
if(!is.null(dfva$mortalityTimeUnits)){
dfva$mortalityTimeUnits [dfva$mortalityTimeUnits == ""] <- "not investigated/not given/not relevant"
dfva$mortalityTimeUnits <- factor (dfva$mortalityTimeUnits)
}
#dfva %>% count (mortalityTimeUnits)

## efficacy
if(!is.null(dfva$efficacy)){
dfva$efficacy <- as.numeric (dfva$efficacy)
}

# ## UCI_efficacy
# if(!is.null(dfva$UCI_efficacy)){
# dfva$UCI_efficacy <- as.numeric (dfva$UCI_efficacy)
# }
#
# ## LCI_efficacy
# if(!is.null(dfva$LCI_efficacy)){
# dfva$LCI_efficacy <- as.numeric (dfva$LCI_efficacy)
# }

dfva$quality <- NULL

# adjusted timelines
# every study reported a timeline differently,
# to be able to plot and compare results, we standardize all timelines according to the
# CHALLENGE DAY

offsetDay <- dfva$challengeDay
dfva$dayDose1Std <- dfva$dayDose1 - offsetDay
dfva$dayDose2Std <- dfva$dayDose2 - offsetDay
dfva$challengeDayStd <- 0
dfva$timePointStd <- dfva$timePoint - offsetDay

dfva$experimentStatus2 <- as.character(dfva$experimentStatus)
dfva$experimentStatus2[dfva$experimentStatus2=="1st vaccination day"]<- "Not vaccinated/Before vaccination"
dfva$experimentStatus2[dfva$experimentStatus2=="2nd vaccination day"]<- "Vaccinated, not challenged"
dfva$experimentStatus2[dfva$experimentStatus2=="Between vaccination doses"]<- "Vaccinated, not challenged"
#table(dfva$experimentStatus2)





references <- read.csv("data/DistillerData_raw/Vaccines_refid_distiller.csv",
                       header=T, sep=",", stringsAsFactors = F,encoding  = "UTF-8")
colnames(references)<-  c("Refid","ShortBibliography","Author","Title","Abstract", "publicationYear", "user", "level","agent","targetSpecies")
references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
references <- references[,c("Refid", "agent", "targetSpecies", "ShortBibliography", "FullBibliography", "Author","Title","Abstract", "publicationYear")]
references <- references[!duplicated(references[,c(1,2,3)]),]
references$FullBibliography <- as.character(references$FullBibliography)
for (r in 1:dim(references)[1]){
  references$FullBibliography[r]<- dfva$FullReference[which(dfva$refID==references$Refid[r])[1]]
}
write.csv(references, file="data/FilesDownload/Vaccines_refid.csv", row.names = F)
#write.csv2(references, file="data/FilesDownload/DiagnosticTests_refid2.csv", row.names = F)




ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
ref.merge <- unique(ref.merge)
dfva <- merge(dfva,ref.merge,by.x="refID",by.y="Refid")

## To export data as .csv
write.csv(dfva, file = "data/FilesDownload/Vaccines_cleaned.csv", row.names = F)
#write.csv2(dfdt, file = "data/FilesDownload/Vaccines_cleaned2.csv", row.names = F)















references <- read.csv("data/FilesDownload/Vaccines_refid.csv")
ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
ref.merge <- unique(ref.merge)
dfva <- merge(dfva,ref.merge,by.x="refID",by.y="Refid")

## To export data as .csv
write.csv(dfva, file = "data/FilesDownload/Vaccines_cleaned.csv", row.names = F)
#write.csv2(dfva, file = "data/FilesDownload/Vaccines_cleaned2.csv", row.names = F)

# ### END OF DATA CLEANING #######################################################################################################

#
# #code to update the "refid" file when downloaded directly from Distiller,
# #instead of using the one on the folder

# references <- read.csv("Vaccines_refid_distiller.csv",
#                      header=T, sep=",", stringsAsFactors = F)
#
# colnames(references)<-  c("Refid", "ShortBibliography", "Author","Title","Abstract", "publicationYear","user", "level","agent","targetSpecies","X")
#
# references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
# references$FullBibliography <- as.character(references$FullBibliography)
# for (r in 1:dim(references)[1]){
#   references$FullBibliography[r]<- dfva$FullReference[which(dfva$refID==references$Refid[r])[1]]
# }
#
#
# references <- references[,c("Refid", "agent", "targetSpecies", "ShortBibliography", "FullBibliography", "Author","Title","Abstract", "publicationYear")]
# references <- references[!duplicated(references[,c(1,2,3)]),]
#
# write.csv(references, file="Vaccines_refid.csv", row.names = F)
# write.csv2(references, file="Vaccines_refid2.csv", row.names = F)

