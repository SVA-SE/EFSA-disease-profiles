library(plotly)
library(dplyr)
library(stringr)
library(knitr)
library(gdata)
library(kableExtra)
library(DT)

source("SLR_objectives/Functions.r")
source("SLR_objectives/Metaanalysis_functions.r")

dfei <- read.csv("data/FilesDownload/ExperimentalInfections_cleaned.csv")
references <- read.csv("data/FilesDownload/ExperimentalInfections_refid.csv")

agent = agents[a]
agent.folder.name <- agent.folder.names[a]


dfDZ <- dfei[dfei$agent==agent,]


dfDZ$targetSpecies <- gsub("\\s*\\([^\\)]+\\)","",as.character(dfDZ$targetSpecies))

dfDZ <- species.name.cleanup(data=dfDZ)

dfDZ$targetSpecies <- as.factor(dfDZ$targetSpecies)


### Kaplan-Meier curves with parametric Confidence Intervals

min.groups <- 4

data.set <- prepare.data.MA(datainput=dfDZ)

# Incubation ----
result.list.incub.ci <- list()
dd.plot <- prepare.data.INCUB(datainput=data.set, agent=agent)

if(dim(dd.plot)[1]>0){

result.km.incub.ci <- km.incub.ci()

png(filename = paste0("agents/",agent.folder.name,
                      "/AgentAssets/pages/impact_ma1.png"),width = 800, height = 600)
print(km.incub.ci())
dev.off()


#2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
#to be deleted later
png(filename = paste0("SLR_objectives/impact_ma1.png"),width = 800, height = 600)
print(km.incub.ci())
dev.off()

#include_graphics(paste0("../agents/",agent.folder.name,
# "/AgentAssets/images/impact_ma1.png"),
# auto_pdf=FALSE)
}

### Kaplan-Meier curves with Inter-Quartile Intervals ----


result.list.incub.iq <- list()
dd.plot <- prepare.data.INCUB(datainput=data.set, agent=agent)
if(dim(dd.plot)[1]>0){

result.km.incub.iq <- km.incub.iq()

png(filename = paste0("agents/",agent.folder.name,
                      "/AgentAssets/pages/impact_ma2.png"),width = 800, height = 600)
print(km.incub.iq())
dev.off()

png(filename = paste0("SLR_objectives/impact_ma2.png"),width = 800, height = 600)
print(km.incub.ci())
dev.off()
}


# include_graphics(paste0("../agents/",agent.folder.name,
#                         "/AgentAssets/images/impact_ma2.png"),
#                  auto_pdf=FALSE)

