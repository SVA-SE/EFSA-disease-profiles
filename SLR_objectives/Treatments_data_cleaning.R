### DACRAH3# - E - TREATMENT INVESTIGATIONS (TT) ###

## This script is for data cleaning of the whole dataset of treatment investigations

## working directory assumed to be the folder where this file lives
## setwd("location")
## getwd()
## Loading necessary packages
library(readr)
library(dplyr)
library(stringr)

#source('Treatments_definitions.R')

## Loading dataset retrieved from Distiller ----
dftt <- read.csv("../DistillerData_raw/Treatments.csv",
                 header=T, sep=",", stringsAsFactors = F, dec = ".",encoding  = "UTF-8")

## Distiller exports the data with large text or with codes.
## The use of such column names would be impractical.
## Thus, a dataset with column names was created.
columnsTT <- read.csv("Treatments_columnNames.csv",
                      header=F, sep=",", stringsAsFactors = F)



# Naming columns in our df
if (dim(columnsTT)[1]==dim(dftt)[2]){
  colnames(dftt)<- columnsTT[,2]
}else{print("ERROR!")}
rm(columnsTT)

#a row count used to match data in some operations
dftt$rowID <- 1:dim(dftt)[1]

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
dftt[sapply(dftt, function(x) all(is.na(x)))] <- NULL

##refID
#any(is.na(dftt$refID))

dftt$user <- NULL
dftt$level <- NULL

## groupID
colnames(dftt)[which(names(dftt) == "studyID")] <- "groupID"

## uniqueID ----
## "uniqueID" is the merge of refID and groupID.
## this is created so that we can detect multiple rows of data referring to the same group, globally
## without having to refer to the refID columns everytime.
dftt$uniqueID <- paste(dftt$refID, dftt$groupID, sep="")
dftt$uniqueID <- gsub ("NA", "", dftt$uniqueID)

## country
dftt$country [dftt$country == ""] <- "not investigated/not given/not relevant"
dftt$country <- factor (dftt$country)
#dftt %>% count (country, sort = T)

## year
dftt$year [dftt$year == -1] <- NA

## agent
dftt$agent <- factor (dftt$agent)
#dftt %>% count (agent, sort = T)

## agent subtype Dacrah 1
dftt$agentSuptypeDC1 [dftt$agentSuptypeDC1 == ""] <- "not investigated/not given/not relevant"
dftt$agentSuptypeDC1 <- factor (dftt$agentSuptypeDC1)
#dftt %>% count (agentSuptypeDC1, sort = T)

## target host
dftt$targetHost <- gsub ('Human (homo sapiens) ', "Human (homo sapiens)",
                         dftt$targetHost, fixed=T)
dftt$targetHost <- factor (dftt$targetHost)
#dftt %>% count (targetHost, sort = T)

## target vector
dftt$targetVector [dftt$targetVector == ""] <- "not investigated/not given/not relevant"
dftt$targetVector <- factor (dftt$targetVector)
#dftt %>% count (targetVector, sort = T)

## age months
dftt$ageMonths [dftt$ageMonths == -1] <- NA

## age DC1
dftt$ageDC1 [dftt$ageDC1 == ""] <- "not investigated/not given/not relevant"
dftt$ageDC1 <- factor (dftt$ageDC1)

## sampStrategy
dftt$sampStrategy [dftt$sampStrategy == ""] <- "not investigated/not given/not relevant"
dftt$sampStrategy [dftt$sampStrategy == "Unspecified"] <- "not investigated/not given/not relevant"
dftt$sampStrategy  <- str_replace(dftt$sampStrategy,": specify","")
dftt$sampStrategy <- factor (dftt$sampStrategy)
#dftt %>% count (sampStrategy, sort =T)

## sampUnit
dftt$sampUnit <- factor (dftt$sampUnit)
#dftt %>% count (sampUnit, sort = T)

## sampUnitSize
dftt$sampUnitSize [dftt$sampUnitSize == -1] <- NA
#summary(dftt$sampUnitSize)

## Route
dftt$route [dftt$route == ""] <- "not investigated/not given/not relevant"
dftt$route <- factor (dftt$route)
#dftt %>% count (route, sort = T)

## route_C
dftt$route_C [dftt$route_C == ""] <- "not investigated/not given/not relevant"
#dftt %>% count (route_C, sort = T)

## Intervention
dftt$intervention <- factor (dftt$intervention)
#dftt %>% count (intervention, sort = T)


# when intervention is marked as "contol" or "placebo", it is very hard later
# to filter all study groups that referred to "pharmaceutical treatment" for instance
# because the controls won't come. and if filtering by "control", then can't separate which
# were control for pharmaceutical treatment, versus insecticide
# so created interventionStudy, which classified all study groups in the same stud as either
# "Pharmaceutical treatment" or "Insectide treatment"

dftt$interventionStudy <- ifelse(dftt$intervention=="Insecticide treatment"|
                                   dftt$intervention=="Pharmaceutical treatment",
                                 as.character(dftt$intervention),
                                 NA)
for (r in 1:dim(dftt)[1]){
  if (is.na(dftt$interventionStudy[r])){
    dftt$interventionStudy[r] <- sort(dftt$interventionStudy[dftt$refID==dftt$refID[r]])[1]
  }
}


## testSubstance
#dftt$testSubstance
#dftt %>% count (testSubstance, sort = T) %>% print (n=80)

## testSubstance2
dftt$testSubstance2 [dftt$testSubstance2 == ""] <- "not investigated/not given/not relevant"
#dftt %>% count (testSubstance2, sort = T)

## SubstanceCat
dftt$SubstanceCat <- factor (dftt$SubstanceCat)
#dftt %>% count (SubstanceCat, sort = T) %>% print (n = 29)
#any(is.na(dftt$SubstanceCat))

## SubstanceCat2
dftt$substance2Cat [dftt$substance2Cat == ""] <- "not investigated/not given/not relevant"
dftt$substance2Cat <- factor (dftt$substance2Cat)
#dftt %>% count (substance2Cat)

## SubstancePerc
dftt$substancePerc <- as.numeric (dftt$substancePerc)
dftt$substancePerc [dftt$substancePerc == -1] <- NA

## Substance2Perc
dftt$substance2Perc [dftt$substance2Perc == -1] <- NA

## dosagefreq
dftt$dosageFreq <- as.numeric (dftt$dosageFreq)
dftt$dosageFreq [dftt$dosageFreq == -1] <- NA

## dosafreq2
dftt$dosageFreq2 [dftt$dosageFreq2 == -1] <- NA
#dftt %>% select (refID, testSubstance2, dosageFreq2) %>% filter (!is.na(dftt$dosageFreq2))

## dosageinterval (days)
dftt$dosageInterval [dftt$dosageInterval == -1] <- NA

## dosageInterval2
dftt$dosageInterval2 [dftt$dosageInterval2 == -1] <- NA
#dftt %>% select (refID, testSubstance2, dosageFreq2, dosageInterval2) %>% filter (!is.na(dftt$dosageFreq2))

## Dose
dftt$dose [dftt$dose == -1] <- NA

#dftt %>% select (refID, substancePerc, dose, doseUnits) %>%
 # filter (doseUnits == "percentage")

dftt$substancePerc <- ifelse (is.na(dftt$substancePerc) & dftt$doseUnits == "percentage",
                                  dftt$dose, dftt$substancePerc)

idx <- which(dftt$doseUnits == "percentage" & dftt$substancePerc == dftt$dose)
for(i in 1:length(idx))
  dftt$dose[idx[i]]<- NA

## dose2
dftt$dose2 [dftt$dose2 == -1] <- NA

#dftt %>% select (refID, substance2Perc, dose2, doseUnits2) %>%
 #filter (doseUnits2 == "percentage")

dftt$substance2Perc <- ifelse (is.na(dftt$substance2Perc) & dftt$doseUnits2 == "percentage",
                              dftt$dose2, dftt$substance2Perc)

idx <- which(dftt$doseUnits2 == "percentage" & dftt$substance2Perc == dftt$dose2)
for(i in 1:length(idx))
  dftt$dose2[idx[i]]<- NA

## doseUnits
dftt$doseUnits [dftt$doseUnits == ""] <- "not investigated/not given/not relevant"
dftt$doseUnits [dftt$doseUnits == "percentage"] <- "not investigated/not given/not relevant"
#dftt %>% count (doseUnits, sort = T)

## doseUnits2
dftt$doseUnits2 [dftt$doseUnits2 == ""] <- "not investigated/not given/not relevant"
dftt$doseUnits2 [dftt$doseUnits2 == "percentage"] <- "not investigated/not given/not relevant"
#dftt %>% count (doseUnits2, sort = T)

## substanceDose_C
dftt$substanceDose_C [dftt$substanceDose_C == ""] <- "not investigated/not given/not relevant"
#dftt %>% count (substanceDose_C, sort = T)

## matrix
dftt$matrix [dftt$matrix == ""] <- "not investigated/not given/not relevant"
dftt$matrix <- factor (dftt$matrix)
#dftt %>% count (matrix, sort = T)

## labTest
dftt$labTest [dftt$labTest == "Not applicable"] <- "not investigated/not given/not relevant"
dftt$labTest <- factor (dftt$labTest)
#dftt %>% count (labTest, sort = T)

## labTest_Description
dftt$labTest_Description [dftt$labTest_Description == ""] <- "not investigated/not given/not relevant"
#dftt %>% count (labTest_Description)

## targetLab
dftt$targetLab [dftt$targetLab == ""] <- "not investigated/not given/not relevant"
dftt$targetLab <- factor (dftt$targetLab)
#dftt %>% count (targetLab, sort = T)

## timePoint (days)
dftt$timePoint [dftt$timePoint == -1] <- NA
dftt$timePoint <- as.numeric (dftt$timePoint)

## nAnimals
dftt$nAnimals [dftt$nAnimals == -1] <- NA
dftt$nAnimals <- as.numeric (dftt$nAnimals)

## nClinical
dftt$nClinical [dftt$nClinical == -1] <- NA
dftt$nClinical <- as.numeric (dftt$nClinical)

## titres
dftt$titres [dftt$titres == -1] <- NA

## nTested
#dftt$nTested
dftt$nTested [dftt$nTested == -1] <- NA
dftt$nTested <- as.numeric (dftt$nTested)

## nPositive
dftt$nPositive [dftt$nPositive == -1] <- NA
dftt$nPositive <- as.numeric (dftt$nPositive)

## nNegative
dftt$nNegative [dftt$nNegative == -1] <- NA
dftt$nNegative <- as.numeric (dftt$nNegative)

## check if we always have nPositive and/or nNegative when nTested is given
dftt %>% select (refID, intervention, sampUnitSize, nTested, nPositive, nNegative) %>%
  filter (is.na(dftt$nPositive)| is.na(dftt$nNegative))

## dead, it will be split into "mortalityNr" and "mortalityPerc", see below:
#dftt$dead

## deadUnit
dftt$deadUnit [dftt$deadUnit == ""] <- "not investigated/not given/not relevant"
#dftt %>% count (deadUnit, sort = T)

## Giving mortality as number and as percentage
dftt$mortalityNr <- ifelse (dftt$deadUnit %in% c("individuals (animals or vectors)"), dftt$dead, (dftt$dead/100)*dftt$sampUnitSize)
dftt$mortalityPerc <- ifelse(dftt$deadUnit %in% c ("percentage"), dftt$dead, (dftt$mortalityNr/dftt$sampUnitSize)*100)
dftt$mortalityPerc <- round(dftt$mortalityPerc, 1)
#dftt %>% select (refID, sampUnitSize, deadUnit, dead, mortalityNr, mortalityPerc)
#dftt$dead <- NULL

## efficacy
dftt$efficacy [dftt$efficacy == -1] <- NA

## LCI_efficacy
dftt$LCI_efficacy [dftt$LCI_efficacy == -1] <- NA

## UCI_efficacy
dftt$UCI_efficacy [dftt$UCI_efficacy == -1] <- NA

## otherOutcome
dftt$otherOutcome [dftt$otherOutcome == ""] <- "not investigated/not given/not relevant"
dftt$otherOutcome <- factor (dftt$otherOutcome)
#summary (dftt$otherOutcome)

## outcomeNumerical
dftt$outcomeNumerical [dftt$outcomeNumerical == -1] <- NA

## outcomeUnit
dftt$outcomeUnit [dftt$outcomeUnit == ""] <- "not investigated/not given/not relevant"
dftt$outcomeUnit <- factor (dftt$outcomeUnit)

## outcomes_C
dftt$outcomes_C [dftt$outcomes_C == ""] <- "not investigated/not given/not relevant"

## weakenesses
dftt$weakenesses <- NULL
dftt$strengths <- NULL

#dftt %>% select (refID) %>%
 # filter (is.na (dftt$mortalityPerc), is.na (dftt$efficacy), dftt$otherOutcome == "not investigated/not given/not relevant",
  #        is.na(dftt$titres), is.na(dftt$nPositive), is.na(dftt$nNegative))

#colnames (dftt)

references <- read.csv("../DistillerData_cleaned/Treatments_refid.csv")
ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
ref.merge <- unique(ref.merge)
dftt <- merge(dftt,ref.merge,by.x="refID",by.y="Refid")


## To export data as .csv
write.csv(dftt, file = "../DistillerData_cleaned/Treatments_cleaned.csv", row.names = F)
write.csv2(dftt, file = "../DistillerData_cleaned/Treatments_cleaned2.csv", row.names = F)

# ### END OF DATA CLEANING #######################################################################################################


## code to update the "refid" file when downloaded directly from Distiller,
## instead of using the one on the folder

#  references <- read.csv("Treatments_refid_distiller.csv",
#                       header=T, sep=",", stringsAsFactors = F,encoding  = "UTF-8")
#
# colnames(references)<-  c("Refid", "ShortBibliography", "Author","Title","Abstract", "publicationYear","user", "level","agent","targetSpecies","X")
# references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
#
# references$FullBibliography <- as.character(references$FullBibliography)
# for (r in 1:dim(references)[1]){
#   references$FullBibliography[r]<- dftt$FullReference[which(dftt$refID==references$Refid[r])[1]]
# }
#
#  references <- references[,c("Refid", "agent", "targetSpecies", "ShortBibliography", "FullBibliography", "Author","Title","Abstract", "publicationYear")]
#  references <- references[!duplicated(references[,c(1,2,3)]),]
#
#  write.csv(references, file="Treatments_refid.csv", row.names = F)
#  write.csv2(references, file="Treatments_refid2.csv", row.names = F)
#
#



