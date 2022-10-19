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


### Kaplan-Meier curves with parametric Confidence Intervals - Virus detection



min.groups <- 4
data.set <- prepare.data.MA(datainput=dfDZ)

# Detection ----

targetLabTest.selection <- "Virus"
if(length(which(data.set$targetLabTest==targetLabTest.selection))>0){

result.list.detect.ci <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)

if(dim(dd.plot)[1]>0){
result.km.detect.ci.v <- km.detect.ci()


png(filename = paste0("agents/",agent.folder.name,
                      "/AgentAssets/pages/transmission_ma1.png"),width = 800, height = 600)
print(km.detect.ci())
dev.off()


#2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
#to be deleted later
png(filename = paste0("SLR_objectives/transmission_ma1.png"),width = 800, height = 600)
print(km.detect.ci())
dev.off()
}

# include_graphics("transmission_ma1.png",
#                  auto_pdf=FALSE)





### Kaplan-Meier curves with Inter-Quantile Ranges - Virus detection -----

result.list.detect.iq <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)


if(dim(dd.plot)[1]>0){

result.km.detect.iq.v <- km.detect.iq()


png(filename = paste0("agents/",agent.folder.name,
                      "/AgentAssets/pages/transmission_ma2.png"),width = 800, height = 600)
print(km.detect.iq())
dev.off()


#2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
#to be deleted later
png(filename = paste0("SLR_objectives/transmission_ma2.png"),width = 800, height = 600)
print(km.detect.iq())
dev.off()

}
}

### Kaplan-Meier curves with parametric Confidence Intervals - DNA/RNA detection

targetLabTest.selection <- "Nucleic acid"
if(length(which(data.set$targetLabTest==targetLabTest.selection))>0){

result.list.detect.ci <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)

if(dim(dd.plot)[1]>0){
result.km.detect.ci.na <- km.detect.ci()


png(filename = paste0("agents/",agent.folder.name,
                      "/AgentAssets/pages/transmission_ma3.png"),width = 800, height = 600)
print(km.detect.ci())
dev.off()



#2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
#to be deleted later
png(filename = paste0("SLR_objectives/transmission_ma3.png"),width = 800, height = 600)
print(km.detect.ci())
dev.off()
}



### Kaplan-Meier curves with Inter-Quantile Ranges - DNA/RNA detection


result.list.detect.iq <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)

if(dim(dd.plot)[1]>0){
result.km.detect.iq.na <- km.detect.iq()



png(filename = paste0("agents/",agent.folder.name,
                      "/AgentAssets/pages/transmission_ma4.png"),width = 800, height = 600)
print(km.detect.iq())
dev.off()


#2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
#to be deleted later
png(filename = paste0("SLR_objectives/transmission_ma4.png"),width = 800, height = 600)
print(km.detect.iq())
dev.off()

}
}


# ANTIGEN ----

targetLabTest.selection <- "Antigen"
if(length(which(data.set$targetLabTest==targetLabTest.selection))>0){

result.list.detect.ci <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)

if(dim(dd.plot)[1]>0){
    result.km.detect.ci.v <- km.detect.ci()


    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/transmission_ma5.png"),width = 800, height = 600)
    print(km.detect.ci())
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/transmission_ma5.png"),width = 800, height = 600)
    print(km.detect.ci())
    dev.off()
}

# include_graphics("transmission_ma1.png",
#                  auto_pdf=FALSE)





### Kaplan-Meier curves with Inter-Quantile Ranges - Virus detection -----

result.list.detect.iq <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)


if(dim(dd.plot)[1]>0){

    result.km.detect.iq.v <- km.detect.iq()


    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/transmission_ma6.png"),width = 800, height = 600)
    print(km.detect.iq())
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/transmission_ma6.png"),width = 800, height = 600)
    print(km.detect.iq())
    dev.off()

}
}




# Bacterium/Rickettsial bodies ----

targetLabTest.selection <- "Bacterium/Rickettsial bodies"

if(length(which(data.set$targetLabTest==targetLabTest.selection))>0){
result.list.detect.ci <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)

if(dim(dd.plot)[1]>0){
    result.km.detect.ci.v <- km.detect.ci()


    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/transmission_ma7.png"),width = 800, height = 600)
    print(km.detect.ci())
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/transmission_ma7.png"),width = 800, height = 600)
    print(km.detect.ci())
    dev.off()
}

# include_graphics("transmission_ma1.png",
#                  auto_pdf=FALSE)





### Kaplan-Meier curves with Inter-Quantile Ranges - Virus detection -----

result.list.detect.iq <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)


if(dim(dd.plot)[1]>0){

    result.km.detect.iq.v <- km.detect.iq()


    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/transmission_ma8.png"),width = 800, height = 600)
    print(km.detect.iq())
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/transmission_ma8.png"),width = 800, height = 600)
    print(km.detect.iq())
    dev.off()

}

}


# Parasite ----

targetLabTest.selection <- "Parasite"
if(length(which(data.set$targetLabTest==targetLabTest.selection))>0){

result.list.detect.ci <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)

if(dim(dd.plot)[1]>0){
    result.km.detect.ci.v <- km.detect.ci()


    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/transmission_ma9.png"),width = 800, height = 600)
    print(km.detect.ci())
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/transmission_ma9.png"),width = 800, height = 600)
    print(km.detect.ci())
    dev.off()
}

# include_graphics("transmission_ma1.png",
#                  auto_pdf=FALSE)





### Kaplan-Meier curves with Inter-Quantile Ranges - Virus detection -----

result.list.detect.iq <- list()
dd.plot <- prepare.data.DETECT(datainput=data.set)


if(dim(dd.plot)[1]>0){

    result.km.detect.iq.v <- km.detect.iq()


    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/transmission_ma10.png"),width = 800, height = 600)
    print(km.detect.iq())
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/transmission_ma10.png"),width = 800, height = 600)
    print(km.detect.iq())
    dev.off()

}
}

