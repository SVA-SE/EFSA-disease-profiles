### DACRAH3# - B - VECTORS (VC) ###

## This script is for data cleaning of the whole dataset of vectors

## working directory assumed to be the folder where this file lives
## setwd("location")
## Loading necessary packages
library(readr)
library(dplyr)
library(stringr)

#source('VectorControl_definitions.R')

## Loading dataset retrieved from Distiller ----
dfvc <- read.csv("data/DistillerData_raw/VectorControl.csv",
                 header=T, sep=",", stringsAsFactors = F, dec = ".",encoding  = "UTF-8")


## Distiller exports the data with large text or with codes.
## The use of such column names would be impractical.
## Thus, a dataset with column names was created.
columnsVC <- read.csv("SLR_objectives/VectorControl_columnNames.csv",
                      header=F, sep=",", stringsAsFactors = F)
# write.table(columnsVC[1:70,], file="SLR_objectives/VectorControl_columnNames.csv",
#          col.names = F,row.names=F,sep=",")



# Naming columns in our df
if (dim(columnsVC)[1]==dim(dfvc)[2]){
  colnames(dfvc)<- columnsVC[,2]
}else{print("ERROR!")}
rm(columnsVC)

#a row count used to match data in some operations
dfvc$rowID <- 1:dim(dfvc)[1]

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
## This was done for consistency (so that we would always the same dataset out).
## Removing columns with only NA:
dfvc[sapply(dfvc, function(x) all(is.na(x)))] <- NULL

##refID
#any(is.na(dfvc$refid))

dfvc$author <- NULL
dfvc$title <- NULL
dfvc$abstract <- NULL
dfvc$user <- NULL
dfvc$level <- NULL

## "groupID" is a unique identification of groups WITHIN refID
## Please note that for DACRAH1 and DACRAH2 this information is in
## different columns: studyID1 and studyID2
dfvc$groupID <- ifelse(dfvc$refID <= 20000, dfvc$studyID1, dfvc$studyID2)
dfvc$studyID1 <- NULL
dfvc$studyID2 <- NULL

## uniqueID ----
## "uniqueID" is the merge of refID and groupID.
## this is created so that we can detect multiple rows of data referring to the same group, globally
## without having to refer to the refID columns everytime.
dfvc$uniqueID <- paste(dfvc$refID, dfvc$groupID, sep="")

## country
dfvc$country [dfvc$country == ""] <- "not investigated/not given/not relevant"
dfvc$country <- factor (dfvc$country)
#sort(table(dfvc$country), decreasing = TRUE)

## year
dfvc$year [dfvc$year == -1] <- NA
#summary(dfvc$year)

## agent
dfvc$agent <- factor (dfvc$agent)
#sort(table(dfvc$agent), decreasing = T)

## studyTarget_host
dfvc$studyTarget_host <- factor (dfvc$studyTarget_host)
#sort(table(dfvc$studyTarget_host), decreasing = T)

## studyTarget_vector
dfvc$studyTarget_vector <- factor (dfvc$studyTarget_vector)
#sort(table(dfvc$studyTarget_vector), decreasing = T)

## sampUniType
dfvc$sampUnitType[dfvc$sampUnitType == ""] <- "not investigated/not given/not relevant"
dfvc$sampUnitType <- factor (dfvc$sampUnitType)
#dfvc %>% count(sampUnitType, sort =T)
#dfvc %>% select (sampUnitType, refID) %>% filter (sampUnitType == "notGiven")
#sort(table(dfvc$sampUnitType), decreasing = T)

## sampUnitType_C
dfvc$sampUnitType_C [dfvc$sampUnitType_C == ""] <- "not relevant"
dfvc$sampUnitType_C <- factor (dfvc$sampUnitType_C)
#sort(table(dfvc$sampUnitType_C), decreasing = T)


## sampUnitSize
dfvc$sampUnitSize [dfvc$sampUnitSize == -1] <- NA
dfvc$sampUnitSize [dfvc$sampUnitSize == -2] <- NA
dfvc$sampUnitSize <- as.numeric (dfvc$sampUnitSize)
#summary (dfvc$sampUnitSize)
#which (dfvc$sampUnitSize == 0)
#dfvc %>% select (refID, sampUnitSize) %>% filter (sampUnitSize == 0)

## route
dfvc$route <- ifelse(dfvc$route == "" & dfvc$testSubstance1_cat == "CONTROL",
                     "not investigated/not given/not relevant", dfvc$route)
dfvc$route  <- str_replace(dfvc$route,": specify","")
dfvc$route <- factor (dfvc$route)
#sort(table(dfvc$route), decreasing = T)
#dfvc %>% select (refID, route, testSubstance1_cat) %>% filter (route == "")

## route_C
dfvc$route_C [dfvc$route_C == ""] <-"not investigated/not given/not relevant"
dfvc$route_C <- factor (dfvc$route_C)
#sort(table(dfvc$route_C), decreasing = T)

## intervention
dfvc$intervention <- factor (dfvc$intervention)
#sort(table(dfvc$intervention), decreasing = T)

## testSubstance1
dfvc$testSubstance1 <- str_to_lower(dfvc$testSubstance1)
## previous code in case duplicated data were entered with the first letter in upper/lower case.
dfvc$testSubstance1 <- factor (dfvc$testSubstance1)
#dfvc %>% count(testSubstance1, sort = T) %>% print(n=341)

## testSubstance2
dfvc$testSubstance2 <- str_to_lower(dfvc$testSubstance2)
dfvc$testSubstance2 [dfvc$testSubstance2 == ""] <- "notInvestigated"
dfvc$testSubstance2 <- factor (dfvc$testSubstance2)
#dfvc %>% count(testSubstance2, sort = T)

## "testSubstance1_cat"
dfvc$testSubstance1_cat <- factor (dfvc$testSubstance1_cat)
#dfvc %>% count(testSubstance1_cat, sort = T) %>% print (n= 56)
#dfvc %>% select (refID, testSubstance1_cat) %>% filter (testSubstance1_cat == "")

## "testSubstance2_cat"
dfvc$testSubstance2_cat [dfvc$testSubstance2_cat == ""] <- "not investigated/not given/not relevant"
dfvc$testSubstance2_cat <- factor (dfvc$testSubstance2_cat)
#dfvc %>% count(testSubstance2_cat, sort = T)

## "formulation1_Perc"
dfvc$formulation1_Perc <- gsub("%", "", dfvc$formulation1_Perc)
dfvc$formulation1_Perc <- as.numeric(as.character(dfvc$formulation1_Perc))
## dfvc %>% count (formulation1_Perc, sort = T) %>% print (n=36)
## summary (dfvc$formulation1_Perc)

dfvc$dose [dfvc$dose == -1] <- NA

dfvc %>% select (refID, formulation1_Perc, dose, doseUnits1) %>%
  filter (doseUnits1 == "percentage" & formulation1_Perc == dose)

dfvc$dose <- gsub(",", ".", dfvc$dose)
dfvc$dose <- as.numeric(as.character(dfvc$dose))

dfvc %>% select (refID, formulation1_Perc, dose, doseUnits1) %>%
  filter (doseUnits1 == "percentage")

## When formulation is NA and dose is presented as % in dose: dose in % should be moved to
## formulation_perc
dfvc$formulation1_Perc <- ifelse (is.na(dfvc$formulation1_Perc) & dfvc$doseUnits1 == "percentage",
                                  dfvc$dose, dfvc$formulation1_Perc)

#dfvc$dose <- ifelse (dfvc$doseUnits1 == "percentage" & dfvc$formulation1_Perc == dfvc$dose,
##                    NA, dfvc$dose)
## Why above does not work. Check with Nanda.

## When formulation % and dose % are the same: The information will be kept only in formulation
idx <- which(dfvc$doseUnits1 == "percentage" & dfvc$formulation1_Perc == dfvc$dose)
for(i in 1:length(idx))
  dfvc$dose[idx[i]]<- NA

## when dose is NA but there is a dose unit: Dose unit should be NA
dfvc$doseUnits1 <- ifelse (is.na(dfvc$dose) & !is.na(dfvc$doseUnits1), NA, dfvc$doseUnits1)

## When dose is in ppm, it should be moved to formulation in percentage
## with the transforamtion to %
dfvc %>% select (refID, doseUnits1, formulation1_Perc, dose) %>%
  filter (doseUnits1 == "ppm (parts per million)")
dfvc$formulation1_Perc <- ifelse (dfvc$doseUnits1 == "ppm (parts per million)",
                                  dfvc$dose/10000, dfvc$formulation1_Perc)
idx <- which (dfvc$doseUnits1 == "ppm (parts per million)")
for(i in 1:length(idx))
  dfvc$dose[idx[i]]<- NA
dfvc$doseUnits1 [dfvc$doseUnits1 == "ppm (parts per million)" ] <- NA

# Below are the rows with missing formulation/dose
#dfvc %>% select (refID, uniqueID, formulation1_Perc, dose, testSubstance1_cat) %>%
#  filter (is.na(dfvc$formulation1_Perc) & is.na(dfvc$dose) & testSubstance1_cat != "CONTROL")

## "formulation2_Perc"
dfvc$formulation2_Perc <- gsub("%", "", dfvc$formulation2_Perc)
dfvc$formulation2_Perc <- as.numeric (as.character(dfvc$formulation2_Perc))
#summary (dfvc$formulation2_Perc)

dfvc$dose2 [dfvc$dose2 == -1] <- NA

dfvc %>% select (refID, formulation2_Perc, dose2, doseUnits2) %>%
  filter (doseUnits2 == "percentage" | doseUnits2 == "ppm (parts per million)")

dfvc$formulation2_Perc <- ifelse (is.na(dfvc$formulation2_Perc) & dfvc$doseUnits2 == "percentage",
                                  dfvc$dose2, dfvc$formulation2_Perc)

idx <- which(dfvc$doseUnits2 == "percentage" & dfvc$formulation2_Perc == dfvc$dose2)
for(i in 1:length(idx))
  dfvc$dose2[idx[i]]<- NA

dfvc$doseUnits2 <- ifelse (is.na(dfvc$dose2) & !is.na(dfvc$doseUnits2), NA, dfvc$doseUnits2)

dfvc$formulation2_Perc <- ifelse (dfvc$doseUnits2 == "ppm (parts per million)",
                                  dfvc$dose2/10000, dfvc$formulation2_Perc)
idx <- which (dfvc$doseUnits2 == "ppm (parts per million)")
for(i in 1:length(idx))
  dfvc$dose2[idx[i]]<- NA
dfvc$doseUnits2 [dfvc$doseUnits2 == "ppm (parts per million)" ] <- NA

## "dose"
## 3 of the cases that a 0 was entered in dose there was not treatment,
## then it will be NA
idx <- which (dfvc$dose == 0)
for(i in 1:length(idx))
  dfvc$dose[idx[i]]<- NA
#summary (dfvc$dose)
#In some cases a dose was entered for control rows. Check those?
#dfvc %>% select (refID, dose, testSubstance1_cat) %>% filter (dfvc$testSubstance1_cat == "CONTROL")

## "dose2"
#summary (dfvc$dose2)

## "doseUnits1"
dfvc$doseUnits1 [is.na(dfvc$doseUnits1)] <- "not investigated/not given/not relevant"
dfvc$doseUnits1 [dfvc$doseUnits1 == ""] <- "not investigated/not given/not relevant"

#dfvc %>% count(doseUnits1, sort =T) %>% print (n= 26)
#dfvc %>% select (refID, formulation1_Perc, dose, doseUnits1) %>%
#  filter (doseUnits1 == "other"| doseUnits1 == "")
## cases when doseunit is an empty string it was a control.
## What to do with this "other" as dose unit cases?

## "doseUnits2"
dfvc$doseUnits2 [dfvc$doseUnits2 == ""] <- "not investigated/not given/not relevant"
#dfvc %>% count(doseUnits2, sort =T) %>% print (n= 26)
#dfvc %>% select (refID, formulation2_Perc, testSubstance2_cat, doseUnits2) %>% filter (doseUnits2 == "")

## "dosageFreq1"
dfvc$dosageFreq1 <- as.numeric (dfvc$dosageFreq1)

dfvc$dosageFreq1 <- ifelse (dfvc$dosageFreq1 < 0, NA, dfvc$dosageFreq1)
#summary (dfvc$dosageFreq1)
#which(dfvc$dosageFreq1 >= 0 & dfvc$dosageFreq1 < 1)
#Obs: Same row of the missing dose
#dfvc %>% select(refID, dosageFreq1, testSubstance1_cat) %>% filter (is.na(dosageFreq1))

## "dosageFreq2"
dfvc$dosageFreq2 <- as.numeric (dfvc$dosageFreq2)
#dfvc$dosageFreq2 <- ifelse (dfvc$dosageFreq2 <= 0, NA, dfvc$dosageFreq2)
#summary (dfvc$dosageFreq2)

## "dosageInterval1"
dfvc$dosageInterval1 <- ifelse (dfvc$dosageInterval1 <= 0, NA, dfvc$dosageInterval1)
dfvc$dosageInterval1 <- as.numeric(dfvc$dosageInterval1)
#summary(dfvc$dosageInterval1)

## "dosageInterval2"
dfvc$dosageInterval2 <- ifelse (dfvc$dosageInterval2 <= 0, NA, dfvc$dosageInterval2)
dfvc$dosageInterval2 <- as.numeric (dfvc$dosageInterval2)
#summary(dfvc$dosageInterval2)

## exposureTime
#summary(dfvc$exposureTime)
dfvc$exposureTime <- as.numeric (as.character(dfvc$exposureTime))

## exposureUnit
dfvc$exposureUnit [dfvc$exposureUnit == ""] <- "not investigated/not given/not relevant"
#dfvc %>% count (exposureUnit, sort = T)

dfvc <- dfvc %>% mutate(expHours = case_when (dfvc$exposureUnit %in% c("hours") ~ exposureTime,
                                             dfvc$exposureUnit %in% c("minutes") ~ exposureTime/60,
                                             dfvc$exposureUnit %in% c("days") ~ exposureTime*24))

dfvc <- dfvc %>% mutate(exMinutes = case_when (dfvc$exposureUnit %in% c("minutes") ~ exposureTime,
                                              dfvc$exposureUnit %in% c("hours") ~ exposureTime*60,
                                              dfvc$exposureUnit %in% c("days") ~ exposureTime*1440))

dfvc <- dfvc %>% mutate(expDays = case_when (dfvc$exposureUnit %in% c("days") ~ exposureTime,
                                              dfvc$exposureUnit %in% c("hours") ~ exposureTime/24,
                                              dfvc$exposureUnit %in% c("minutes") ~ exposureTime/1440))

#dfvc$exposureTime <- NULL
#dfvc$exposureUnit <- NULL

#summary (dfvc$expHours)
#summary (dfvc$exMinutes)
#summary (dfvc$expDays)

## "substanceDose_C"
dfvc$substanceDose_C [dfvc$substanceDose_C == ""] <- "not investigated/not given/not relevant"
#dfvc %>%  count(substanceDose_C, sort =T)

## "sampledMatrix"
dfvc$sampledMatrix [dfvc$sampledMatrix == ""] <- "not investigated/not given/not relevant"
dfvc$sampledMatrix <- factor (dfvc$sampledMatrix)
#dfvc %>% count (sampledMatrix, sort =T)

## "labTest"
dfvc$labTest [dfvc$labTest == ""] <- "not investigated/not given/not relevant"
dfvc$labTest [dfvc$labTest == "Not applicable"] <- "not investigated/not given/not relevant"
dfvc$labTest <- factor (dfvc$labTest)
#dfvc %>% count(labTest, sort = T)

## "LabMethod_description"
dfvc$LabMethod_description [dfvc$LabMethod_description == ""] <- "not investigated/not given/not relevant"
#dfvc %>% count (LabMethod_description, sort = T)

## "targetLabtest"
dfvc$targetLabtest [dfvc$targetLabtest == ""] <- "not investigated/not given/not relevant"
dfvc$targetLabtest <- factor (dfvc$targetLabtest)
#dfvc %>% count (targetLabtest, sort = T)

## "timePoint"
#summary (dfvc$timePoint)
#dfvc$timePoint <- as.numeric(dfvc$timePoint)

## "mortality"

dfvc$mortalityUnit [dfvc$mortalityUnit == ""] <- "not investigated/not given/not relevant"
#dfvc %>% count (mortalityUnit, sort = T)
#dfvc %>% select (mortalityUnit, mortality, sampUnitSize, mortPerc, mortNr) %>% filter (mortalityUnit == "individuals (animals or vectors)")

dfvc$mortNr <- ifelse (dfvc$mortalityUnit %in% c ("individuals (animals or vectors)"),
                       dfvc$mortality, (dfvc$mortality/100)*dfvc$sampUnitSize)
dfvc$mortNr <- round (dfvc$mortNr)
dfvc$mortPerc <- ifelse(dfvc$mortalityUnit %in% c ("percentage"),
                        dfvc$mortality, (dfvc$mortNr/dfvc$sampUnitSize)*100)
dfvc$mortPerc <- round(dfvc$mortPerc,1)
#dfvc$mortality <- NULL
#dfvc$mortalityUnit <- NULL

## "mortality_LCI"
#summary(dfvc$mortality_LCI)

## "mortality_UCI"
#summary (dfvc$mortality_UCI)

## "mortalityTime"- Time between application of treatment and mortality
#summary (dfvc$mortalityTime)

## "MortalityTimeUnit"
#dfvc %>% count (mortalityTimeUnit, sort = T)

#dfvc$mortalityTimeUnit[dfvc$mortalityTimeUnit == ""] <- "not investigate/not given/not relevant"

dfvc <- dfvc %>% mutate(mortSec = case_when (dfvc$mortalityTimeUnit %in% c("seconds") ~ mortalityTime,
                                             dfvc$mortalityTimeUnit %in% c("minutes") ~ mortalityTime*60,
                                             dfvc$mortalityTimeUnit %in% c("hours") ~ mortalityTime*3600,
                                             dfvc$mortalityTimeUnit %in% c("days") ~ mortalityTime*86400))

dfvc <- dfvc %>% mutate(mortMin = case_when (dfvc$mortalityTimeUnit %in% c("seconds") ~ mortalityTime/60,
                                             dfvc$mortalityTimeUnit %in% c("minutes") ~ mortalityTime,
                                             dfvc$mortalityTimeUnit %in% c("hours") ~ mortalityTime*60,
                                             dfvc$mortalityTimeUnit %in% c("days") ~ mortalityTime*1440))

dfvc <- dfvc %>% mutate(mortHours = case_when (dfvc$mortalityTimeUnit %in% c("seconds") ~ mortalityTime/3600,
                                             dfvc$mortalityTimeUnit %in% c("minutes") ~ mortalityTime/60,
                                             dfvc$mortalityTimeUnit %in% c("hours") ~ mortalityTime,
                                             dfvc$mortalityTimeUnit %in% c("days") ~ mortalityTime*24))

dfvc <- dfvc %>% mutate(mortDays = case_when (dfvc$mortalityTimeUnit %in% c("seconds") ~ mortalityTime/86400,
                                               dfvc$mortalityTimeUnit %in% c("minutes") ~ mortalityTime/1440,
                                               dfvc$mortalityTimeUnit %in% c("hours") ~ mortalityTime/24,
                                               dfvc$mortalityTimeUnit %in% c("days") ~ mortalityTime))

dfvc$mortalityTime <- NULL
dfvc$mortalityTimeUnit <- NULL

## "efficacy"
#summary(dfvc$efficacy)
# If > 1 is in percentage.
dfvc$efficacy <- ifelse (dfvc$efficacy > 1, dfvc$efficacy/100, dfvc$efficacy)
#dfvc %>% select (refID, efficacy) %>% filter (efficacy < 0.02)

## "efficacy_LCI"
#summary(dfvc$efficacy_LCI)
dfvc$efficacy_LCI <- ifelse (dfvc$efficacy_LCI > 1, dfvc$efficacy_LCI/100, dfvc$efficacy_LCI)
dfvc %>% select (refID, efficacy_LCI) %>% filter (efficacy_LCI > 1)

## "efficacy_UCI"
#summary(dfvc$efficacy_UCI)
dfvc$efficacy_UCI <- ifelse (dfvc$efficacy_UCI > 1, dfvc$efficacy_UCI/100, dfvc$efficacy_UCI)
dfvc %>% select (refID, efficacy_UCI) %>% filter (efficacy_UCI > 1)

## "outcomeType"
dfvc$outcomeType [dfvc$outcomeType == ""] <- "not investigated/not given/not relevant"
#dfvc %>% count (outcomeType, sort = T)

## "outcomeNumber"
#summary(dfvc$outcomeNumber)

## "outcomeUnit"
dfvc$outcomeUnit [dfvc$outcomeUnit == ""] <- "not investigated/not given/not relevant"
#dfvc %>% count(outcomeUnit, sort = T)
#dfvc %>% select (outcomeNumber, outcomeUnit) %>% filter (outcomeUnit != "not investigate/not given/not relevant")

## "nTested"
#summary(dfvc$nTested)
#dfvc%>% select (efficacy, nTested, nPositive, nNegative) %>% filter (nPositive ==0)

## "nNegative"
#summary (dfvc$nNegative)

## "nPositive"
#summary (dfvc$nPositive)

## "outcomes_C"
dfvc$outcomes_C [dfvc$outcomes_C == ""] <- "not investigated/not given/not relevant"
#dfvc %>% count(outcomes_C, sort = T)

## "strengths"
dfvc$strengths <- NULL

## "weaknesses"
dfvc$weaknesses <- NULL




## refid ----


references <- read.csv("data/DistillerData_raw/VectorControl_refid_distiller.csv",
                       header=T, sep=",", stringsAsFactors = F,encoding  = "UTF-8")
colnames(references)<-  c("Refid","ShortBibliography","Author","Title","Abstract", "publicationYear", "user", "level","agent","targetSpecies")
references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
references <- references[,c("Refid", "agent", "targetSpecies", "ShortBibliography", "FullBibliography", "Author","Title","Abstract", "publicationYear")]
references <- references[!duplicated(references[,c(1,2,3)]),]
references$FullBibliography <- as.character(references$FullBibliography)
for (r in 1:dim(references)[1]){
  references$FullBibliography[r]<- dfvc$FullReference[which(dfvc$refID==references$Refid[r])[1]]
}
write.csv(references, file="data/FilesDownload/VectorControl_refid.csv", row.names = F)
#write.csv2(references, file="data/FilesDownload/VectorControl_refid2.csv", row.names = F)




ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
ref.merge <- unique(ref.merge)
dfvc <- merge(dfvc,ref.merge,by.x="refID",by.y="Refid")

## To export data as .csv
write.csv(dfvc, file = "data/FilesDownload/VectorControl_cleaned.csv", row.names = F)
#write.csv2(dfvc, file = "data/FilesDownload/VectorControl_cleaned2.csv", row.names = F)

