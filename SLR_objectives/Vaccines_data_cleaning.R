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
#####dfva[sapply(dfva, function(x) all(is.na(x)))] <- NULL
#####this was removed in 2022 because efficacy was empty. Also

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

## Columns will be merged
if(!is.null(dfva$agentSubtypeType)){
dfva$agentDetails <- ifelse(dfva$agentDetails != "", dfva$agentDetails, dfva$agentSubtypeDC1)
}else{
  dfva$agentDetails <- dfva$agentSubtypeDC1
}
dfva$agentDetails[dfva$agentDetails == ""] <- "not investigated/not given/not relevant"
dfva$agentDetails[dfva$agentDetails == "BTV8"] <- "BTV-8"
dfva$agentDetails <- factor(dfva$agentDetails)

## targetSpecies
dfva$targetSpecies <- factor(dfva$targetSpecies)
#dfva %>% count (targetSpecies, sort = T)


## ageMonths
dfva$ageMonths [dfva$ageMonths == -1] <- NA


## sampUnit
dfva$sampUnit <- factor (dfva$sampUnit)
#dfva %>% count (sampUnit)

## sampUnitSize
dfva$sampUnitSize <- as.numeric (dfva$sampUnitSize)

#table(dfva$animalStatus)
#table(dfva$animalStatus_C)
dfva$animalStatus[dfva$animalStatus_C=="Not described"]<-"Not given"
dfva$animalStatus <- factor(dfva$animalStatus)


#table(dfva$testSubstance)
#table(dfva$testSubstance_C)
#table(dfva$testSubstance_C[dfva$testSubstance=="Other: specify"])

dfva$testSubstance[dfva$testSubstance_C=="A/chicken/Vietnam/ C58/2004 (H5N3)"] <- "A/chicken/Vietnam/C58/2004 (H5N3)"
dfva$testSubstance[dfva$testSubstance_C=="A/gyrfalcon/Washington/41088/2014 (H5N8)"] <-"A/Gyrfalcon/Washington/40188-6/2014 (H5N8), Merck Animal Health"
dfva$testSubstance[dfva$testSubstance_C=="A/TK/OR/71 (TK/OR/71) (H7N3)"] <-"A/TK/OR/71 (TK/OR/71) (H7N3)"
dfva$testSubstance[dfva$testSubstance_C=="H5N2: A/chicken/ Mexico/232/1994(H5N2); manufactured in Mexico"] <-"A/chicken/Mexico/232/1994 (H5N2), licensed in Indonesia"
dfva$testSubstance[dfva$testSubstance_C=="H5N3: A/chicken/Vietnam/ C58/2004"] <- "A/chicken/Vietnam/C58/2004 (H5N3)"
dfva$testSubstance[dfva$testSubstance_C=="Poulvac i-AI H5N9 and H7N1 bivalent vaccine"] <-"Poulvac i-AI"
dfva$testSubstance[dfva$testSubstance_C=="Re-4"] <- "H5N1 Re-4"
  dfva$testSubstance[dfva$testSubstance_C=="RE-5"] <- "H5N1 Re-5"
  dfva$testSubstance[dfva$testSubstance_C=="Re-6"] <- "H5N1 Re-6"
  dfva$testSubstance[dfva$testSubstance_C=="RE-6"] <- "H5N1 Re-6"
  dfva$testSubstance[dfva$testSubstance_C=="Reassortant trivalent vaccine (H5N2 Re-11 strain + Re-12 strain, H7N9 H7-Re-2 strain)"] <-"Reassortant trivalent vaccine (H5N2 Re-11 + Re-12 + H7N9 H7-Re-2)"
  dfva$testSubstance[str_detect(dfva$testSubstance_C,"Vectormune AI rHVT-AI")] <- "Vectormune HVT-AIV"
  dfva$testSubstance[str_detect(dfva$testSubstance_C,"Vectormune AI rHVT-H5")] <- "Vectormune AI (rHVT-H5)"
  dfva$testSubstance[dfva$testSubstance_C=="Volvac AI KV"] <- "Volvac AI KV"
  

  
    #table(dfva$vaccineType)
  #table(dfva$vaccineType_C)


  
  
    
## route
#table(dfva$route)
  #table(dfva$route_C)

  
  ##CHECK
  table(dfva$testSubstance[dfva$route==""])
  
  
  dfva$route [dfva$route == ""] <- "not investigated/not given/not relevant"
dfva$route <- factor (dfva$route)
#dfva %>% count (route, sort = T)



## challengeType
#table(dfva$challengeType)
dfva$challengeType [dfva$challengeType == ""] <- "not investigated/not given/not relevant"
dfva$challengeType <- factor (dfva$challengeType)




## challengeRoute
#table(dfva$challengeRoute)
#table(dfva$challengeRoute_C)
dfva$challengeRoute[dfva$challengeRoute == ""] <- "not investigated/not given/not relevant"


dfva$challengeRoute_C <- str_to_lower(dfva$challengeRoute_C)
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"choanal")] <- "intrachoanal"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"direct contact")] <- "direct contact"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"eye drop and intranasal routes")] <- "oculo-nasal"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"intranasal and oral")] <- "oro-nasal"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"intraocular, intranasal, oral")] <- "oculo-oro-nasal"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"nose/eye drop")] <- "oculo-nasal"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"not mentioned")] <- "not investigated/not given/not relevant"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"oculo-oronasal")] <- "oculo-oro-nasal"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"oculo nasal")] <- "oculo-nasal"
dfva$challengeRoute_C[str_detect(dfva$challengeRoute_C,"oronasal")] <- "oro-nasal"


dfva$challengeRoute[dfva$challengeRoute=="OTHER: specify"] <- dfva$challengeRoute_C[dfva$challengeRoute=="OTHER: specify"]
dfva$challengeRoute[dfva$challengeRoute=="MULTIPLE: specify"] <- "MULTIPLE"
  
#table(dfva$challengeRoute)
dfva$challengeRoute <- factor (dfva$challengeRoute)


## challengeSubstance
#table(dfva$challengeSubstance)
if(!is.null(dfva$challengeSubstance)){
dfva$challengeSubstance [dfva$challengeSubstance == ""] <- "not investigated/not given/not relevant"
dfva$challengeSubstance <- factor (dfva$challengeSubstance)
}
#dfva %>% count (challengeSubstance)

## challengeDose
dfva$challengeDose [dfva$challengeDose == -1] <- NA


## challengeDay
dfva$challengeDay <- as.numeric(dfva$challengeDay)

# ## lastVaccine
# #which(dfva$lastVaccine > dfva$challengeDay)
# dfva$lastVaccine <- as.numeric(dfva$lastVaccine)



## deadUnits
dfva$deadUnits <- as.numeric (dfva$deadUnits)
dfva$deadUnits[dfva$deadUnits == -1] <- NA

## mortalityTime
dfva$mortalityTime <- as.numeric (dfva$mortalityTime)
dfva$mortalityTime[dfva$mortalityTime == -1] <- NA

## mortalityTimeUnits
  dfva$mortalityTimeUnits <- factor (dfva$mortalityTimeUnits)

  
  ## mortalityTime
  dfva$mortalityTimeMax <- as.numeric (dfva$mortalityTimeMax)
  dfva$mortalityTimeMax[dfva$mortalityTimeMax == -1] <- NA
  
  ## mortalityTimeUnits
  dfva$mortalityTimeUnitsMax <- factor(dfva$mortalityTimeUnitsMax)

    
  
#dfva %>% count (mortalityTimeUnits)

## efficacy
if(!is.null(dfva$efficacy)){
  dfva$efficacy <- as.numeric (dfva$efficacy)
}

  ## coverage
  if(!is.null(dfva$coverage)){
    dfva$coverage <- as.numeric (dfva$coverage)
  }


  dfva$vaccCost
  dfva$vaccCostIndirect
  dfva$withdrawal
  dfva$storage

  
  
## timePoint
dfva$timePoint <- as.numeric (dfva$timePoint)

## experimentStatus
#table(dfva$experimentStatus)
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
#table(dfva$labTarget)
#table(dfva$labTarget_C)

dfva$labTarget[dfva$labTarget == "Not applicable"] <- "not investigated/not given/not relevant"
dfva$labTarget[dfva$labTarget == "Other: specify"] <- dfva$labTarget_C[dfva$labTarget == "Other: specify"]
dfva$labTarget <- factor(dfva$labTarget)


## matrix
#table(dfva$matrix)
#table(dfva$matrix_C)

dfva$matrix[dfva$matrix_C=="lung tissue"] <- "lung"
dfva$matrix[dfva$matrix_C=="Organ tissues"] <- "organ tissue (not specified)"
dfva$matrix[dfva$matrix_C=="spleen tissue"] <- "spleen"
dfva$matrix[dfva$matrix_C=="tracheal"] <- "tracheal swab"

dfva$matrix[dfva$matrix_C=="Bursa Fabricius tissue"] <- "Bursa Fabricius tissue"
dfva$matrix[dfva$matrix_C=="cloacal swab"] <- "cloacal swab"
dfva$matrix[dfva$matrix_C=="intestine"] <- "intestine"
dfva$matrix[dfva$matrix_C=="liver"] <- "liver"
dfva$matrix[dfva$matrix_C=="lung"] <- "lung"
dfva$matrix[dfva$matrix_C=="organ tissue (not spedified)"] <- "organ tissue (not spedified)"
dfva$matrix[dfva$matrix_C=="oronasal swab"] <- "oronasal swab"
dfva$matrix[dfva$matrix_C=="oropharyngeal swab"] <- "oropharyngeal swab"
dfva$matrix[dfva$matrix_C=="orophgaryngeal swab"] <- "oropharyngeal swab"
dfva$matrix[dfva$matrix_C=="tracheal swab"] <- "tracheal swab"



dfva$matrix[dfva$matrix_C=="Oropharyngeal and cloacal swabs"] <- "MULTIPLE"
dfva$matrix[dfva$matrix_C=="oropharyngeal/cloacal swabs"] <- "MULTIPLE"
dfva$matrix[dfva$matrix_C=="tracheal and cloacal swab"] <- "MULTIPLE"
dfva$matrix[dfva$matrix_C=="tracheal/cloacal swab"] <- "MULTIPLE"

dfva$matrix[dfva$matrix=="MULTIPLE: specify"] <- "MULTIPLE"

dfva$matrix <- factor(dfva$matrix)


## nTested
dfva$nTested <- as.numeric (dfva$nTested)
dfva$nTested[dfva$nTested == -1] <- NA


## nPositive
dfva$nPositive <- as.numeric (dfva$nPositive)
dfva$nPositive[dfva$nPositive == -1] <- NA


## testValue
#dfva$testValue
#dfva %>% select (refID, groupID, nPositive, testValue) %>% filter (is.na(dfva$nPositive))

## testValueUnits
#dfva$testValueUnits <- as.character(dfva$testValueUnits)
#dfva$testValueUnits [dfva$testValueUnits == ""] <- "not investigated/not given/not relevant"

#for (r in 1:dim(dfva)[1]){
#  if(is.na(dfva$testValue[r])&!is.na(dfva$nPositive[r])){
#    dfva$testValue[r] <- dfva$nPositive[r]/dfva$nTested[r]*100
#    dfva$testValueUnits[r] <- paste0("% POS, ", dfva$labTest[r])
#  }
#}


#dfva$testValueUnits <- factor (dfva$testValueUnits)
#dfva %>% count (testValueUnits)

# ## scaleMin
# dfva$scaleMin <- as.numeric (dfva$scaleMin)
# 
# ## ScaleMax
# dfva$ScaleMax <- as.numeric (dfva$scaleMin)




dfva$quality <- NULL


# adjusted timelines
# every study reported a timeline differently,
# to be able to plot and compare results, we standardize all timelines according to the
# CHALLENGE DAY

dfva$dayDose1 <- as.numeric(dfva$dayDose1)
dfva$dayDose1[dfva$dayDose1 == -1] <- NA

dfva$dayDose2 <- as.numeric(dfva$dayDose2)
dfva$dayDose2[dfva$dayDose2 == -1] <- NA



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

