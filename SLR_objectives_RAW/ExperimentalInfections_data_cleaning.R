### DACRAH3# - A - EXPERIMENTAL INFECTION (EI) ###

## This script is for data cleaning of the whole dataset of experimental infections

## working directory assumed to be the folder where this file lives
## setwd("location")

## Loading necessary packages
library(readr)
library(dplyr)
library(stringr)

source('SLR_objectives/ExperimentalInfections_definitions.R')

## Loading dataset retrieved from Distiller ----
dfei <- read.csv("data/DistillerData_raw/ExperimentalInfections.csv",
                  header=T, sep=",", stringsAsFactors = F, dec = ".",encoding  = "UTF-8")

## Distiller exports the data with large text or with codes.
## The use of such column names would be impractical.
## Thus, a dataset with column names was created.
columnsEI <- read.csv("SLR_objectives/ExperimentalInfections_columnNames.csv",
                      header=F, sep=",", stringsAsFactors = F)
columnsEI$V2 <- str_replace(columnsEI$V2,"Abscence","Absence")
columnsEI$V2 <- str_replace(columnsEI$V2,"Diarrhea","Diarrhoea")

#write.table(columnsEI, file="SLR_objectives/ExperimentalInfections_columnNames.csv",
#          col.names = F,row.names=F,sep=",")



## This dataset with column names should have the same number of rows as the
## main dataset - this CHECK will print "ERROR!" if not
if (dim(columnsEI)[1]==dim(dfei)[2]){
  colnames(dfei)<- columnsEI[,2]
}else{print("ERROR!")}
rm(columnsEI)

#a row count used to match data in some operations
dfei$rowID <- 1:dim(dfei)[1]


## Data seems to be retrieved with an extra column filled with NA,
## check if that is the case and if so, remove that column
if (all(is.na(dfei[,dim(dfei)[2]]))){
  dfei <- dfei[,-dim(dfei)[2]]}

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
dfei[sapply(dfei, function(x) all(is.na(x)))] <- NULL

## refID ----
## "refID" refers to the paper ID number in DACRAH
## this column is needed to identify unique studies
## checking that it is never empty
if (any(is.na(dfei$refID))){
  print("ERROR!")
}

## "user" refers to the distiller account that filled the forms
## not needed to the data analyses
dfei$user <- NULL


## "level" refers to the stage in the systematic review, and only one stage(last)
## involves data collection
## not needed to the data analyses
dfei$level <- NULL


## "groupID" is a unique identification of groups WITHIN refID
##within the experimental infection research objective, specifically, the data collection team
##seems to have tried to make this unique overall sometimes by using a number that also included refID
## the result is a lack of consistency between DACRAH1 and DACRAH2
## this should not matter, AS LONG AS GROUPID CAN BE USED AS A UNIQUE ID OF GROUPS PER REFID
##which was checked again and again that it is true. the actual values have no meaning, except to
## distinguish groups with diferent values within the same refID.

## uniqueID ----
## "uniqueID" is the merge of refID and groupID.
## this is created so that we can detect multiple rows of data referring to the same group, globally
## without having to refer to the refID columns everytime.
dfei$uniqueID <- paste(dfei$refID, dfei$groupID, sep="")

## year ----
## "year" refers to the year of the study
## to differentiate missing values from "not given in the paper", the later were filled with "-1"
## during data collection.
## For data analyses and summarization, we should treat those as truely missing values
dfei$year[dfei$year == -1] <- NA
#anything smaller than 1920 is likely to be a typo
dfei$year[dfei$year < 1920] <- NA
#anything greater than the current year is a typo
dfei$year[dfei$year > as.numeric(substr(as.character(Sys.Date()),1,4))] <- NA

## agent ----
## "agent" refers to causative agent of vector bourne disease
dfei$agent <- factor (dfei$agent)

## AgentSubtypeType
dfei$agentSubtypeType [dfei$agentSubtypeType == ""] <- "notGiven"
dfei$agentSubtypeType <- factor (dfei$agentSubtypeType)

## "agentDetails"
dfei$agentDetails [dfei$agentDetails == ""] <- "notGiven"
##left as character. If factor desireable:
#dfei$agentDetails <- factor (dfei$agentDetails)

## "agentSubtype", information only from DACRAH1
## on DACRAH 2 it was pasted on Details
## usually only well organized for specific groups, for instance serotypes
## as we look more and more through the data, we will be able to clean up the details and
##place them back into sgentSubtype, where they should be well categorized for use wihtin pathogens
dfei$agentSubtype[dfei$agentSubtypeType=="serotype"]<-
  str_remove(dfei$agentDetails[dfei$agentSubtypeType=="serotype"],"-")
dfei$agentSubtype [dfei$agentSubtype == ""] <- "notGiven"
dfei$agentSubtype <- factor (dfei$agentSubtype)



## "TargetSpecies"
dfei$targetSpecies <- factor (dfei$targetSpecies)

## ageMonths, only from DACRAH1

## "minAgeMonths", only from Dacrah2
dfei$minAgeMonths <- as.numeric (as.character(dfei$minAgeMonths))
dfei$minAgeMonths [dfei$minAgeMonths == -1] <- NA

## "maxAgeMonths" only from DACRAH2
dfei$maxAgeMonths <- as.numeric (as.numeric (dfei$maxAgeMonths))
dfei$maxAgeMonths [dfei$maxAgeMonths == -1] <- NA

## "AgeMonths" information only collected in DACRAH1
dfei$ageMonths <- as.numeric (as.character(dfei$ageMonths))

### Categorizing age ----
## This variable will be more relevant according to the species
#dfei %>% count(targetSpecies, sort = T)
## Data was recorded as Age in DACRAH1 and as min and max in DACRAH2.
## Due to this different ways of recording age data and also a considerable
## variation of this data among the target species, we have chosen to categorize it.
## The categorization depends on the target species and it will be young or adult animals.
## It will be performed for target species most commonly appearing in the studies (cattle and small ruminants).
## The cut-offs set to categorize age are defined in the DEFINITIONS file
##        cattle.min.adult
##        smallRuminants.min.adult
## and should be changed there
## For the other species with less observations, the categorical age was left for now as "unspecified"
## other species will be addressed as needed, as we detail the analyses further

#summary(dfei$minAgeMonths)
#summary(dfei$maxAgeMonths)
#summary(dfei$ageMonths)
#dfei %>% select (refID, rowID, minAgeMonths, maxAgeMonths, ageMonths) %>%
# filter (refID > 20000)
ageCattle <- dfei %>% select (refID, rowID, targetSpecies, minAgeMonths, maxAgeMonths, ageMonths) %>%
  filter (targetSpecies == "Cattle (Bos taurus)") %>%
  filter (!is.na (maxAgeMonths)| !is.na (ageMonths)) %>%
  mutate (ageCat = case_when (maxAgeMonths >= cattle.min.adult | ageMonths >=cattle.min.adult ~ "adult",
                              maxAgeMonths < cattle.min.adult | ageMonths < cattle.min.adult ~ "young"))
ageCattle <- ageCattle[which(ageCattle$refID < 20000 | !(ageCattle$minAgeMonths < cattle.min.adult & ageCattle$maxAgeMonths >= cattle.min.adult)),]

ageSR <- dfei %>% select (refID, rowID, targetSpecies, minAgeMonths, maxAgeMonths, ageMonths) %>%
  filter (targetSpecies == "Sheep (Ovis aries)" | targetSpecies == "Goat (Capra aegagrus hircus)") %>%
  filter (!is.na (maxAgeMonths)| !is.na (ageMonths)) %>%
  mutate (ageCat = case_when (maxAgeMonths >= smallRuminants.min.adult | ageMonths >=smallRuminants.min.adult ~ "adult",
                              maxAgeMonths < smallRuminants.min.adult | ageMonths < smallRuminants.min.adult ~ "young"))
ageSR <- ageSR[which(ageSR$refID < 20000 | !(ageSR$minAgeMonths < smallRuminants.min.adult & ageSR$maxAgeMonths >= smallRuminants.min.adult)),]

ageCat <- rbind (ageCattle, ageSR)
rm(ageCattle, ageSR)
dfei$ageCat <- ageCat [match(dfei$rowID, ageCat$rowID), "ageCat"]
rm(ageCat)

dfei$ageCat [is.na(dfei$ageCat)] <- "Unspecified"
dfei$ageCat <- as.factor(dfei$ageCat)


## "sampUnitSize" - Number of animals infected (use -1 when not reported)
dfei$sampUnitSize [dfei$sampUnitSize == -1] <- NA
dfei$sampUnitSize <- as.numeric (dfei$sampUnitSize)


## "route" - route of exposure
## This variable is a result of many columns related to route of exposure merged
## we have MERGED all routes of exposure reported within the same group using "//" as a separator
## this should allow us to have a more manegeable number of columns in the dataset, and
## when producing summaries per pathogen, we can see which ones really appear per pathogen
## and if needed, the "//" marker can be used to reseparate the occurrences.
dfRoute <- dfei %>% select (starts_with("route"))

namesRoute <- which(dfRoute!="",arr.ind=TRUE)
dfRoute[namesRoute] <- names(dfRoute)[namesRoute[,"col"]]

for(c in 1:dim(dfRoute)[2]){
  dfRoute[which(dfRoute[,c]==""),c]<-NA
}

dfRoute <- data.frame(lapply(dfRoute, function(x) {
  gsub("route_", "", x)
       }))

dfRoute$route <- apply((select(dfRoute,starts_with("route"))),1,paste,collapse="//")
dfRoute$route <- (str_remove_all(dfRoute$route,"NA//"))
dfRoute$route <- (str_remove_all(dfRoute$route,"//NA"))
dfei <- dfei %>% select (-starts_with("route"))
dfei <- cbind(dfei, route = dfRoute$route)
dfei$route <- as.character(dfei$route)
## There is "NA" that should be "notGiven"
dfei$route[dfei$route == "NA"] <- "notGiven"
#dfei %>% count(route, sort =T) %>% print (n=42)
##remove dfRoute
rm(dfRoute)
dfei$route <- factor(dfei$route)
rm(namesRoute)

## "transplac": Evidence of transplacental transmission through detection of the virus in the foetus
## or in the neonate, excluding experimental infection in utero and vectorial infection in neonates
## Only in DACRAH2
dfei$transplac [dfei$transplac == ""] <- "not investigated/not given/not relevant"
dfei$transplac <- factor (dfei$transplac)
#dfei %>% count (transplac, sort = T)

## hostHost means: Evidence of direct host-to-host transmission trhough experiments
## including contact animals and excluding vectorial transmission
## Only in DACRAH2
dfei$hostHost [dfei$hostHost == ""] <- "not investigated/not given/not relevant"
dfei$hostHost <- factor (dfei$hostHost)
#dfei %>% count (hostHost, sort = T)
## Note that in This one, "no" was filled in some forms.

##Matrix
## Same approach for cleaning "route" will be used for "matrix"
## that is, merging using "//", which can always be used to break apart back

dfMatrix <- dfei %>% select (starts_with("matrix"))

namesMatrix <- which(dfMatrix!="",arr.ind=TRUE)
dfMatrix[namesMatrix] <- names(dfMatrix)[namesMatrix[,"col"]]

for(c in 1:dim(dfMatrix)[2]){
  dfMatrix[which(dfMatrix[,c]==""),c]<-NA
}

dfMatrix <- data.frame(lapply(dfMatrix, function(x) {
  gsub("matrix_", "", x)
}))

dfMatrix$matrix <- apply((select(dfMatrix,starts_with("matrix"))),1,paste,collapse="//")
dfMatrix$matrix <- (str_remove_all(dfMatrix$matrix,"NA//"))
dfMatrix$matrix <- (str_remove_all(dfMatrix$matrix,"//NA"))
dfei <- dfei %>% select (-starts_with("matrix"))
dfei <- cbind(dfei, matrix = dfMatrix$matrix)
dfei$matrix <- as.character(dfei$matrix)
dfei$matrix [dfei$matrix == "NA"] <- "not investigated/not given/not relevant"
dfei$matrix [dfei$matrix == "NotSampled"] <- "not investigated/not given/not relevant"
dfei$matrix <- str_remove_all(dfei$matrix,"matrix")
dfei$matrix <- str_trunc(dfei$matrix,90,side="right")
dfei$matrix <- factor (dfei$matrix)
#dfei %>% count(matrix, sort = T) %>% print (n=75)
rm(dfMatrix)
rm(namesMatrix)





##"targetLabTest"
## Not reported and "" means the same = "notGiven"
dfei$targetLabTest [dfei$targetLabTest == ""] <- "notGiven"
dfei$targetLabTest [dfei$targetLabTest == "Not reported"] <- "notGiven"
dfei$targetLabTest <- factor(dfei$targetLabTest)
#dfei %>% count(targetLabTest,sort = T)

## "labTestUsed"
## "Unspecified" should be not given for consistency
dfei$labTestUsed [dfei$labTestUsed == "Unspecified "] <- "notGiven"
dfei$labTestUsed <- factor (dfei$labTestUsed)
#dfei %>% count (labTestUsed, sort = T)

## "labTestUsedC"
## most are "" (notgiven)
dfei$labTestUsedC [dfei$labTestUsedC == ""] <- "notGiven"
#dfei %>% count (labTestUsedC, sort = T)

##"durationPI"
##How many days, after inoculation, the animals were kept (if reported)
dfei$durationPI <- as.numeric (as.character(dfei$durationPI))
dfei$durationPI [dfei$durationPI == -1] <- NA
#summary(dfei$durationPI)

##"minIncub"
##Minimum number of days post inoculation to observe clinical signs
dfei$minIncub <- as.numeric (as.character(dfei$minIncub))
dfei$minIncub [dfei$minIncub == -1] <- NA

##"maxIncub"
## Maximum number of days post inoculation to observe clinical signs
dfei$maxIncub <- as.numeric (as.character(dfei$maxIncub))
dfei$maxIncub [dfei$maxIncub == -1] <- NA

## "minDetect"
## Minimum number of days post inoculation to detect pathogen or nucleic acid
dfei$minDetect <- as.numeric(as.character(dfei$minDetect))
dfei$minDetect [dfei$minDetect == -1] <- NA

## "maxDetect"
## Maximum number of days post inoculation to observe pathogen or nucleic acid
dfei$maxDetect <- as.numeric (as.character(dfei$maxDetect))
dfei$maxDetect [dfei$maxDetect == -1] <- NA

## Mortality data could be given as number or percentage. The following codes will create columns
## for such information as number and as percentage regardless of the data entered.

## deadInfection
dfei$deadInfection [dfei$deadInfection == "Dead during experiment because of illnes, caused by the infection"] <- "yes"
dfei$deadInfection [dfei$deadInfection == ""] <- "no"
dfei$deadInfection <- factor (dfei$deadInfection)
#dfei %>% count (deadInfection, sort = T)

## nrDeadInfection
dfei$nrDeadInfection <- as.numeric (dfei$nrDeadInfection)
#summary (dfei$nrDeadInfection)

## "deadOther"
## Dead animals (or percentage) during experiment because of other reason
dfei$deadOther [dfei$deadOther == ""] <- "no"
dfei$deadOther [dfei$deadOther == "Dead during experiment because of other reason"] <- "yes"
dfei$deadOther <- factor (dfei$deadOther)
#dfei %>% count (deadOther)

## nrDeadOther - Number of dead animals (or percentage) during experiment because of other reason
dfei$nrDeadOther <- as.numeric (dfei$nrDeadOther)
#summary(dfei$nrDeadOther)

## eutInfection - Dead animals (or percentage) euthanised during experiment because of illness due to the infection
dfei$eutInfection [dfei$eutInfection == ""] <- "no"
dfei$eutInfection [dfei$eutInfection == "Euthanised during experiment because of illness due to the infection"] <- "yes"
dfei$eutInfection <- factor (dfei$eutInfection)
#dfei %>% count (eutInfection)

## nrEutDisease - Number of dead animals (or percentage) euthanised during experiment because of illness due to the infection
dfei$nrEutInfection <- as.numeric(dfei$nrEutInfection)
#summary (dfei$nrEutInfection)

## eutOther - Dead animals (or percentage), euthanised during experiment because of other reasons/other illnesses
dfei$eutOther [dfei$eutOther == ""] <- "no"
dfei$eutOther [dfei$eutOther == "Euthanised during experiment because of other reasons/other illnesses"] <- "yes"
dfei$eutOther <- factor (dfei$eutOther)
#dfei %>% count (eutOther, sort = T)

## nrEutOther - Number of dead animals (or percentage), euthanised during experiment because of other reasons/other illnesses
dfei$nrEutOther <- as.numeric(dfei$nrEutOther)
#summary (dfei$nrEutOther)

## eutProtocol - Dead animals (or percentage), euthanised during experiment because of protocol (for example collecting samples)
dfei$eutProtocol [dfei$eutProtocol == ""] <- "no"
dfei$eutProtocol [dfei$eutProtocol == "Euthanised during experiment because of protocol (for example collecting samples)"] <- "yes"
dfei$eutProtocol <- factor (dfei$eutProtocol)
#dfei %>% count (eutProtocol)

## nrEutProtocol - Number of dead animals (or percentage), euthanised during experiment because of protocol (for example collecting samples)
dfei$nrEutProtocol <- as.numeric(dfei$nrEutProtocol)
dfei$nrEutProtocol [dfei$nrEutProtocol == -1] <- NA
#summary(dfei$nrEutProtocol)

## eutEnd - Dead animals (or percentage), euthanised at the end of the experiment period
dfei$eutEnd [dfei$eutEnd == ""] <- "no"
dfei$eutEnd [dfei$eutEnd == "Euthanised at the end of the experiment period"] <- "yes"
dfei$eutEnd <- factor (dfei$eutEnd)
#dfei %>% count (eutEnd, sort = T)

## nrEutEnd - Number of dead animals (or percentage), euthanised at the end of the experiment period
dfei$nrEutEnd [dfei$nrEutEnd == -1] <- NA
#summary(dfei$nrEutEnd)

## unitDead - mortality data is reported as number or as proportion.
dfei$unitDead [dfei$unitDead == ""] <- "animals"
dfei$unitDead [dfei$unitDead == "animals (number of dead animals)"] <- "animals"
dfei$unitDead [dfei$unitDead == "percentage of animals in the group (mortality)"] <- "percentage"
dfei$unitDead <- factor (dfei$unitDead)
#dfei %>% count (unitDead, sort =T)

## Codes for giving mortality dead as number and as percentage:
dfei$mortalityNr <- ifelse(dfei$unitDead %in% c ("animals"), dfei$nrDeadInfection,(dfei$nrDeadInfection/100)*dfei$sampUnitSize)
dfei$percDeadInfection <- ifelse(dfei$unitDead %in% c ("percentage"), dfei$nrDeadInfection, (dfei$mortalityNr/dfei$sampUnitSize)*100)
dfei$percDeadInfection <- round(dfei$percDeadInfection, 1)
dfei$nrDeadInfection <- dfei$mortalityNr
dfei$mortalityNr <- NULL
#dfei %>% select (refID, sampUnitSize, deadInfection, nrDeadInfection, percDeadInfection, unitDead) %>%
 #filter (deadInfection == "yes")

dfei$mortOtherNr <- ifelse(dfei$unitDead %in% c ("animals"), dfei$nrDeadOther,(dfei$nrDeadOther/100)*dfei$sampUnitSize)
dfei$percDeadOther <- ifelse(dfei$unitDead %in% c ("percentage"), dfei$nrDeadOther, (dfei$mortOtherNr/dfei$sampUnitSize)*100)
dfei$percDeadOther <- round(dfei$percDeadOther, 1)
dfei$nrDeadOther <- dfei$mortOtherNr
dfei$mortOtherNr <- NULL
#dfei %>% select (refID, sampUnitSize, deadOther, nrDeadOther, percDeadOther, unitDead) %>%
 #filter (deadOther == "yes")

dfei$eutNr <- ifelse (dfei$unitDead %in% c ("animals"), dfei$nrEutInfection, (dfei$nrEutInfection/100)*dfei$sampUnitSize)
dfei$percEutInfection <- ifelse(dfei$unitDead %in% c ("percentage"), dfei$nrEutInfection, (dfei$eutNr/dfei$sampUnitSize)*100)
dfei$percEutInfection <- round(dfei$percEutInfection, 1)
dfei$nrEutInfection <- dfei$eutNr
dfei$eutNr <- NULL
#dfei %>% select (refID, sampUnitSize, eutInfection, nrEutInfection, percEutInfection, unitDead) %>%
 # filter (eutInfection == "yes")

dfei$mortEutOtherNr <- ifelse(dfei$unitDead %in% c ("animals"), dfei$nrEutOther,(dfei$nrEutOther/100)*dfei$sampUnitSize)
dfei$percEutOther <- ifelse(dfei$unitDead %in% c ("percentage"), dfei$nrEutOther, (dfei$mortEutOtherNr/dfei$sampUnitSize)*100)
dfei$percEutOther <- round(dfei$percEutOther, 1)
dfei$nrEutOther <- dfei$mortEutOtherNr
dfei$mortEutOtherNr <- NULL
#dfei %>% select (refID, sampUnitSize, eutOther, nrEutOther, percEutOther, unitDead) %>%
 #filter (eutOther == "yes")

dfei$mortEutProtocolNr <- ifelse(dfei$unitDead %in% c ("animals"), dfei$nrEutProtocol,(dfei$nrEutProtocol/100)*dfei$sampUnitSize)
dfei$percEutProtocol <- ifelse(dfei$unitDead %in% c ("percentage"), dfei$nrEutProtocol, (dfei$mortEutProtocolNr/dfei$sampUnitSize)*100)
dfei$percEutProtocol <- round(dfei$percEutProtocol, 1)
dfei$nrEutProtocol <- dfei$mortEutProtocolNr
dfei$mortEutProtocolNr <- NULL
#dfei %>% select (refID, sampUnitSize, eutProtocol, nrEutProtocol, percEutProtocol, unitDead) %>%
#filter (eutProtocol == "yes")

dfei$mortEutEndNr <- ifelse(dfei$unitDead %in% c ("animals"), dfei$nrEutEnd,(dfei$nrEutEnd/100)*dfei$sampUnitSize)
dfei$percEutEnd <- ifelse(dfei$unitDead %in% c ("percentage"), dfei$nrEutEnd, (dfei$mortEutEndNr/dfei$sampUnitSize)*100)
dfei$percEutEnd <- round(dfei$percEutEnd, 1)
dfei$nrEutEnd <- dfei$mortEutEndNr
dfei$mortEutEndNr <- NULL
#dfei %>% select (refID, sampUnitSize, eutEnd, nrEutEnd, percEutEnd, unitDead) %>%
 #filter (eutEnd == "yes")


## minMortTime means - Report the time (in days) between experimental infection and recorded mortality,
## minimum reported for the animal group
dfei$minMortTime <- as.numeric (as.character (dfei$minMortTime))
dfei$minMortTime [dfei$minMortTime == -1] <- NA

## maxMortTime means: report the time (in days) between experimental infection and recorded mortality,
## maximum reported for the animal group
dfei$maxMortTime <- as.numeric (as.character(dfei$maxMortTime))
dfei$maxMortTime [dfei$maxMortTime == -1] <- NA

## In all cases the mortality time is reported in days (checked during data cleaning, see DACRAH2 report)
dfei$unitMortTime[dfei$unitMortTime==""] <- "days"
dfei$unitMortTime <- as.factor(dfei$unitMortTime)

## Clinical signs columns
## There is a high number of columns related to clinical signs.
## In some studies, no clinical sign is reported. Our suggestion is to create a column
## that will categorize studies as reporting and not reporting clinical signs. Then,
## with such information, it is possible to filter according to
## clinical signs being reported or not.

##note that for every CS, TWO columns exist
##    CS_clinicalSign reports if the clinical sign was observed in the group
##    CSnr_clinicalSign reports the NUMBER of animals with that clinical sign in the group
##All original Distiller columns were left, since we believe only descriptive summaries will be possible (next stage).

dfei$reportingCS <- do.call (paste0, dfei %>% select (starts_with("CS")) %>% select_if(is.character))
# dfei %>% count(reportingCS, sort = T) %>% print (n=75)
dfei$reportingCS [dfei$reportingCS == "Not reported" | dfei$reportingCS == ""] <- "not investigated/not given/not relevant"
dfei$reportingCS [dfei$reportingCS == "No clinical signs observed for the duration of the experiment"] <- "no"
## Obs.: We need to check why in a few cases the option "no clinical signs observed for the duration of experiment" was entered and
## a clinical signs was entered as well
dfei$reportingCS [dfei$reportingCS != "not investigated/not given/not relevant" & dfei$reportingCS != "no"] <- "yes"
#dfei %>% count(reportingCS, sort = T)

dfCS <- dfei %>% select (starts_with("CS")) %>% select_if(is.character)
for(c in 1:dim(dfCS)[2]){
  dfCS[which(dfCS[,c]==""),c]<-"not investigated/not given/not relevant"
}
for(c in 1:dim(dfCS)[2]){
  dfCS[which(dfCS[,c]!="not investigated/not given/not relevant"),c]<-"yes"
}
dfCS <- dfCS %>% mutate_at(vars(starts_with("CS")), funs(factor))
rm(c)

dfCSnr <- dfei %>% select (starts_with("CS")) %>% select_if(is.numeric)
-1 %in% as.matrix (dfCSnr)
for(c in 1:dim(dfCSnr)[2]){
  dfCSnr[which(dfCSnr[,c]== -1),c]<- NA
}
dfei <- dfei %>% select (-starts_with("CS"))
#dfCS <- dfCS %>%
 # select(cs_reporting, everything())
df <- cbind (dfei, dfCS, dfCSnr)
dfCS <- NULL
dfCSnr <- NULL
dfei <- NULL
dfei <- df
df <- NULL
dfei <- dfei %>%
  select (-weakeness, -rowID, weakeness, rowID)
dfei <- dfei %>% mutate_at(vars(starts_with("CSnr")), funs(as.numeric))
dfei$reportingCS <- factor (dfei$reportingCS)
#dfei %>% count(reportingCS, sort = T) %>% print (n=75)
rm(df, dfCS, dfCSnr)


## Weakeness will be deleted
dfei$weakeness <- NULL



references <- read.csv("data/DistillerData_raw/ExperimentalInfections_refid_distiller.csv",
                       header=T, sep=",", stringsAsFactors = F,encoding  = "UTF-8")

colnames(references)<-  c("Refid","ShortBibliography","Author","Title", "Abstract", "publicationYear", "User", "Level","agent","targetSpecies")
references$FullBibliography <- paste(references$Author,references$publicationYear,references$Title,sep=".")
references$FullBibliography <- as.character(references$FullBibliography)
for (r in 1:dim(references)[1]){
  references$FullBibliography[r]<- dfei$FullReference[which(dfei$refID==references$Refid[r])[1]]
}
references <- references[,c("Refid","agent","targetSpecies","ShortBibliography", "FullBibliography","Author", "Title", "Abstract", "publicationYear")]
references <- references[!duplicated(references[,c(1,2,3)]),]

write.csv(references, file="data/FilesDownload/ExperimentalInfections_refid.csv", row.names = F)
#write.csv2(references, file="data/FilesDownload/ExperimentalInfections_refid2.csv", row.names = F)



ref.merge <- references[,c("Refid","ShortBibliography","Author","Title","Abstract","publicationYear")]
ref.merge <- unique(ref.merge)
dfei <- merge(dfei,ref.merge,by.x="refID",by.y="Refid")



## To export data as .csv
write.csv(dfei, file = "data/FilesDownload/ExperimentalInfections_cleaned.csv", row.names = F)
#write.csv2(dfei, file = "data/FilesDownload/ExperimentalInfections_cleaned2.csv", row.names = F)





