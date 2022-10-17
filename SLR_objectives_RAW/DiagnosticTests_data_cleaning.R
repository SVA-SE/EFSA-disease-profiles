### DACRAH3# - C - DIAGNOSTIC TESTS EVALUATION (DT) ###

## This script is for data cleaning of the whole dataset of vectors

## working directory assumed to be the folder where this file lives
## setwd("location")
## getwd()
## Loading necessary packages
library(readr)
library(dplyr)
library(stringr)


## Loading dataset retrieved from Distiller ----
dfdt <- read.csv("data/DistillerData_raw/DiagnosticTests.csv",
                 header=T, sep=",", stringsAsFactors = F, dec = ".",encoding  = "UTF-8")


## Distiller exports the data with large text or with codes.
## The use of such column names would be impractical.
## Thus, a dataset with column names was created.
columnsDT <- read.csv("SLR_objectives/DiagnosticTests_columnNames.csv",
                      header=F, sep=",", stringsAsFactors = F)

# write.table(columnsDT[1:73,], file="SLR_objectives/DiagnosticTests_columnNames.csv",
#          col.names = F,row.names=F,sep=",")


# Naming columns in our df
if (dim(columnsDT)[1]==dim(dfdt)[2]){
  colnames(dfdt)<- columnsDT[,2]
}else{print("ERROR!")}
rm(columnsDT)

#a row count used to match data in some operations
dfdt$rowID <- 1:dim(dfdt)[1]

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
dfdt[sapply(dfdt, function(x) all(is.na(x)))] <- NULL

##refID
#any(is.na(dfdt$refid))

dfdt$user <- NULL
dfdt$level <- NULL

## groupID
colnames(dfdt)[which(names(dfdt) == "studyID")] <- "groupID"
#dfdt$groupID

## uniqueID ----
## "uniqueID" is the merge of refID and groupID.
## this is created so that we can detect multiple rows of data referring to the same group, globally
## without having to refer to the refID columns everytime.
dfdt$uniqueID <- paste(dfdt$refID, dfdt$groupID, sep="")

## context
dfdt$context <- factor (dfdt$context)
#dfdt %>% count (context, sort =T)

## year
#summary(dfdt$year)

## agent
dfdt$agent <- factor (dfdt$agent)
#dfdt %>% count (agent, sort =T)

## agentSuptypeDC1. DC1 means question from Dacrah1.Not mandatory field.
dfdt$agentSuptypeDC1 [dfdt$agentSuptypeDC1 == ""] <- "not investigated/not given/not relevant"
#dfdt %>% count (agentSuptypeDC1, sort =T)

## target species
dfdt$targetSpecies <- factor (dfdt$targetSpecies)
#dfdt %>% count (targetSpecies, sort = T)

## ageDC1
dfdt$ageDC1 [dfdt$ageDC1 == ""] <- "not investigated/not given/not relevant"
dfdt$ageDC1 <- factor (dfdt$ageDC1)
#dfdt %>% count (ageDC1, sort =T)

## age months
dfdt$ageMonths [dfdt$ageMonths == -1] <- NA

## infMode (mode of infection of the animals)
dfdt$infMode [dfdt$infMode == ""] <-  "not investigated/not given/not relevant"
dfdt$infMode <- factor (dfdt$infMode)
#dfdt %>% count (infMode, sort =T)

## sampStrategy
dfdt$sampStrategy [dfdt$sampStrategy == "Other: specify"] <- "Other"
dfdt$sampStrategy <- factor (dfdt$sampStrategy)
#dfdt %>% count (sampStrategy, sort = T)

## sampUnit
dfdt$sampUnit <- factor (dfdt$sampUnit)
#dfdt %>% count (sampUnit)

## sampUnitSize
dfdt$sampUnitSize <- as.numeric(dfdt$sampUnitSize)
dfdt$sampUnitSize [dfdt$sampUnitSize == -1] <- NA
#dfdt %>% select (refID, uniqueID, sampUnitSize) %>% filter (sampUnitSize == 0)

## route
dfdt$route [dfdt$route == ""] <- "not investigated/not given/not relevant"
dfdt$route <- factor (dfdt$route)
#dfdt %>% count (route, sort = T)

## dose
dfdt$dose [dfdt$dose == ""] <- "not investigated/not given/not relevant"
#dfdt %>% count (dose, sort = T)
#dfdt %>% select (refID, dose) %>% filter (dose != "")

## intervention
dfdt$intervention <- factor (dfdt$intervention)
#dfdt %>% count(intervention, sort = T)

## matrix
dfMatrix <- dfdt %>% select (starts_with("matrix"))

namesMatrix <- which(dfMatrix!="",arr.ind=TRUE)
dfMatrix[namesMatrix] <- names(dfMatrix)[namesMatrix[,"col"]]

for(c in 1:dim(dfMatrix)[2]){
  dfMatrix[which(dfMatrix[,c]==""),c]<-NA
}

dfMatrix$matrix <- apply((select(dfMatrix,starts_with("matrix"))),1,paste,collapse="//")
dfMatrix$matrix <- (str_remove_all(dfMatrix$matrix,"NA//"))
dfMatrix$matrix <- (str_remove_all(dfMatrix$matrix,"//NA"))
dfdt <- dfdt %>% select (-starts_with("matrix"))
dfdt <- cbind(dfdt, matrix = dfMatrix$matrix)
dfdt$matrix <- as.character(dfdt$matrix)
dfdt$matrix [dfdt$matrix == "NA"] <- "not investigatedd/not given/not relevant"
dfdt$matrix <- str_remove_all(dfdt$matrix,"matrix")
dfdt$matrix <- factor (dfdt$matrix)
rm(dfMatrix)
rm(namesMatrix)
#dfdt %>% count (matrix, sort=T)

## targetLab
dfdt$targetLab <- factor (dfdt$targetLab)
#dfdt %>% count (targetLab, sort = T)
#dfdt %>% select (refID, targetLab, labTest) %>% filter (targetLab == "" )

## labtest
dfdt$labTest <- factor (dfdt$labTest)
#dfdt %>% count (labTest, sort = T)

## labtest_C (plain text)
dfdt$labTest_C [dfdt$labTest_C == ""] <-  "not investigatedd/not given/not relevant"
#dfdt %>% count (labTest_C, sort =T)

## reagents (plain text)
dfdt$reagents [dfdt$reagents == ""] <- "not investigated/not given/not relevant"
dfdt$reagents <- factor (dfdt$reagents)
#dfdt %>% count (reagents, sort =T) %>% print (n=57)

## testLimit (plain text)
dfdt$testLimit [dfdt$testLimit == ""] <- "not investigated/not given/not relevant"
dfdt$testLimit <- factor (dfdt$testLimit)
#dfdt %>% count (testLimit, sort = T) %>% print (n=33)

## timePoint (days)
dfdt$timePoint <- as.numeric(dfdt$timePoint)
dfdt$timePoint [dfdt$timePoint == -1] <- NA
#summary (dfdt$timePoint)

## nTested
dfdt$nTested <- as.numeric (dfdt$nTested)
summary (dfdt$nTested)

## nTruePositive (only NA and -1)
dfdt$nTruePositive [dfdt$nTruePositive == -1] <- NA

## nTrueNegative (only NA and -1)
dfdt$nTrueNegative [dfdt$nTrueNegative == -1] <- NA

## nPositive
dfdt$nPositive <- as.numeric (dfdt$nPositive)
#summary (dfdt$nPositive)

## nNegative
dfdt$nNegative <- as.numeric (dfdt$nNegative)
#summary (dfdt$nNegative)

## sensitivity
#dfdt$sensitivity

## specificity
#dfdt$specificity

## UCI_Sen
#dfdt$UCI_Sen

## LCI_Sen
#dfdt$LCI_Sen

## UCI_Spe
#dfdt$UCI_Spe

## LCI_Spe
#dfdt$LCI_Spe

## crossReactivity
dfdt$crossReactivity [dfdt$crossReactivity == ""] <- "not investigated/not given/not relevant"
#dfdt %>% count (crossReactivity, sort = T)

dfdt$studyType <- NA

for (r in dfdt$rowID){

  interventions <- unique(as.character(dfdt$intervention[dfdt$refID==dfdt$refID[r]]))
  if(length(interventions)>1){
    dfdt$studyType[r] <- "comparison"
  }else{
    dfdt$studyType[r] <- interventions
  }

}

## "strengths"
dfdt$Strengths <- NULL

## "weaknesses"
dfdt$Weaknesses <- NULL



## refid ----


references <- read.csv("data/DistillerData_raw/DiagnosticTests_refid_distiller.csv",
                     header=T, sep=",", stringsAsFactors = F,encoding  = "UTF-8")
colnames(references)<-  c("Refid","ShortBibliography","Author","Title","Abstract", "publicationYear", "user", "level","agent","targetSpecies","labTest","labTestcomment")
references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
references <- references[,c("Refid", "agent", "targetSpecies","labTest", "ShortBibliography", "FullBibliography", "Author","Title","Abstract", "publicationYear")]
references <- references[!duplicated(references[,c(1,2,3,4)]),]
references$FullBibliography <- as.character(references$FullBibliography)
for (r in 1:dim(references)[1]){
  references$FullBibliography[r]<- dfdt$FullReference[which(dfdt$refID==references$Refid[r])[1]]
}
write.csv(references, file="data/FilesDownload/DiagnosticTests_refid.csv", row.names = F)
#write.csv2(references, file="data/FilesDownload/DiagnosticTests_refid2.csv", row.names = F)




ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
ref.merge <- unique(ref.merge)
dfdt <- merge(dfdt,ref.merge,by.x="refID",by.y="Refid")

## To export data as .csv
write.csv(dfdt, file = "data/FilesDownload/DiagnosticTests_cleaned.csv", row.names = F)
#write.csv2(dfdt, file = "data/FilesDownload/DiagnosticTests_cleaned2.csv", row.names = F)

