library(plotly)
library(dplyr)
library(stringr)
library(knitr)
library(gdata)
library(kableExtra)
library(DT)
library(grid)
library(lattice)
library(latticeExtra)
library(survival)

source("SLR_objectives/Functions.r")
source("SLR_objectives/Metaanalysis_functions.r")

dfps <- read.csv("data/FilesDownload/PathogenSurvival_cleaned.csv")
references <- read.csv("data/FilesDownload/PathogenSurvival_refid.csv")

agent = agents[a]
agent.folder.name <- agent.folder.names[a]

dfDZ <- dfps[dfps$agent==agent,]


dfDZ$matrix <- gsub("\\s*\\([^\\)]+\\)","",as.character(dfDZ$matrix))
dfDZ$matrix <- gsub("(.*):.*", "\\1", as.character(dfDZ$matrix))
dfDZ$matrix <- gsub("FEED","Feed",as.character(dfDZ$matrix),ignore.case=F)


dfDZ$targetSpecies <- gsub("\\s*\\([^\\)]+\\)","",as.character(dfDZ$targetSpecies))

dfDZ <- species.name.cleanup(data=dfDZ)

dfDZ$targetSpecies <- as.factor(dfDZ$targetSpecies)

#table(dfDZ$targetSpecies)



### VIRUS DETECTION in materials not frozen or heated (0-30 degrees Celsius) (PARAMETRIC intervals) ----

min.groups <- 4
plot.CI <- TRUE

dd.raw <- dfDZ  %>%
    as_tibble
data.set <- dd.raw %>%
    filter(temperature>0, temperature<30, !is.na(maxDetect)) %>%
    mutate(cns=ifelse(truncated=="not investigated/not given/not relevant", "no", as.character(truncated))) %>%
    mutate(cns=ifelse(cns=="yes", T, F))


data.set.vi <- data.set%>%filter(targetLab=="Bacterium/Rickettsial bodies")%>%droplevels()


matrix.table <- sort(table(data.set.vi$matrix),decreasing=T)
keep <- rownames(matrix.table)[matrix.table>=4]

data.set.vi<-data.set.vi[data.set.vi$matrix%in%keep,]
#table(data.set.vi$matrix)


if(dim(data.set.vi)[1]>0){

    loops=1

    if(length(keep)==6){
        n.per.group=6
    }else{
        n.per.group=5}

    if(length(keep)>n.per.group){
        loops <- ceiling(length(keep)/n.per.group)
    }



    l=1
    start=((l-1)*n.per.group)+1
    end=(((l-1)*n.per.group)+n.per.group)
    selected.matrices <- keep[start:end]
    data.set.km <- data.set.vi %>%
        filter(matrix %in% selected.matrices)


    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/survival_ma1.png"),width = 800, height = 600)
    print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/survival_ma1.png"),width = 800, height = 600)
    print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
    dev.off()

}



if(dim(data.set.vi)[1]>0){
    if(loops>1){
        l=2
        start=((l-1)*n.per.group)+1
        end=(((l-1)*n.per.group)+n.per.group)
        selected.matrices <- keep[start:end]
        data.set.km <- data.set.vi %>%
            filter(matrix %in% selected.matrices)



        png(filename = paste0("agents/",agent.folder.name,
                              "/AgentAssets/pages/survival_ma1b.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
        dev.off()


        #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
        #to be deleted later
        png(filename = paste0("SLR_objectives/survival_ma1b.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
        dev.off()

    }
}



if(dim(data.set.vi)[1]>0){

    if(loops>2){
        l=3
        start=((l-1)*n.per.group)+1
        end=(((l-1)*n.per.group)+n.per.group)
        selected.matrices <- keep[start:end]
        data.set.km <- data.set.vi %>%
            filter(matrix %in% selected.matrices)


        png(filename = paste0("agents/",agent.folder.name,
                              "/AgentAssets/pages/survival_ma1c.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
        dev.off()


        #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
        #to be deleted later
        png(filename = paste0("SLR_objectives/survival_ma1c.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
        dev.off()
    }
}



### VIRUS DETECTION in materials not frozen or heated (0-30 degrees Celsius) (IQ range) ----


if(dim(data.set.vi)[1]>0){


    l=1
    start=((l-1)*n.per.group)+1
    end=(((l-1)*n.per.group)+n.per.group)
    selected.matrices <- keep[start:end]
    data.set.km <- data.set.vi %>%
        filter(matrix %in% selected.matrices)


    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/survival_ma2.png"),width = 800, height = 600)
    print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/survival_ma2.png"),width = 800, height = 600)
    print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
    dev.off()

}


if(dim(data.set.vi)[1]>0){

    if(loops>1){
        l=2
        start=((l-1)*n.per.group)+1
        end=(((l-1)*n.per.group)+n.per.group)
        selected.matrices <- keep[start:end]
        data.set.km <- data.set.vi %>%
            filter(matrix %in% selected.matrices)



        png(filename = paste0("agents/",agent.folder.name,
                              "/AgentAssets/pages/survival_ma2b.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
        dev.off()


        #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
        #to be deleted later
        png(filename = paste0("SLR_objectives/survival_ma2b.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
        dev.off()
    }
}


if(dim(data.set.vi)[1]>0){

    if(loops>2){
        l=3
        start=((l-1)*n.per.group)+1
        end=(((l-1)*n.per.group)+n.per.group)
        selected.matrices <- keep[start:end]
        data.set.km <- data.set.vi %>%
            filter(matrix %in% selected.matrices)

        png(filename = paste0("agents/",agent.folder.name,
                              "/AgentAssets/pages/survival_ma2c.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
        dev.off()


        #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
        #to be deleted later
        png(filename = paste0("SLR_objectives/survival_ma2c.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
        dev.off()

        }
}


### DNA/RNA detection in materials not frozen or heated (0-30 degrees Celsius) (PARAMETRIC intervals) ----


data.set.na <- data.set%>%filter(targetLab=="Nucleic acid")%>%droplevels()


matrix.table <- sort(table(data.set.na$matrix),decreasing=T)
keep <- rownames(matrix.table)[matrix.table>=4]

data.set.na<-data.set.na[data.set.na$matrix%in%keep,]
#table(data.set.na$matrix)


if(!dim(data.set.na)[1]>0){

    cat("Not enough evidence for Meta Analyses")

}else{


    loops=1

    if(length(keep)==6){
        n.per.group=6
    }else{
        n.per.group=5}

    if(length(keep)>n.per.group){
        loops <- ceiling(length(keep)/n.per.group)
    }



    l=1
    start=((l-1)*n.per.group)+1
    end=(((l-1)*n.per.group)+n.per.group)
    selected.matrices <- keep[start:end]
    data.set.km <- data.set.na %>%
        filter(matrix %in% selected.matrices)



    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/survival_ma3.png"),width = 800, height = 600)
    print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/survival_ma3.png"),width = 800, height = 600)
    print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
    dev.off()

}


if(dim(data.set.na)[1]>0){

    if(loops>1){
        l=2
        start=((l-1)*n.per.group)+1
        end=(((l-1)*n.per.group)+n.per.group)
        selected.matrices <- keep[start:end]
        data.set.km <- data.set.na %>%
            filter(matrix %in% selected.matrices)


        png(filename = paste0("agents/",agent.folder.name,
                              "/AgentAssets/pages/survival_ma3b.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
        dev.off()


        #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
        #to be deleted later
        png(filename = paste0("SLR_objectives/survival_ma3b.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
        dev.off()
    }
}



if(dim(data.set.na)[1]>0){

    if(loops>2){
        l=3
        start=((l-1)*n.per.group)+1
        end=(((l-1)*n.per.group)+n.per.group)
        selected.matrices <- keep[start:end]
        data.set.km <- data.set.na %>%
            filter(matrix %in% selected.matrices)


        png(filename = paste0("agents/",agent.folder.name,
                              "/AgentAssets/pages/survival_ma3c.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
        dev.off()


        #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
        #to be deleted later
        png(filename = paste0("SLR_objectives/survival_ma3c.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=TRUE, table=FALSE))
        dev.off()
    }
}


### DNA/RNA detection in materials not frozen or heated (0-30 degrees Celsius) (IQ range)---


if(!dim(data.set.na)[1]>0){

    cat("Not enough evidence for Meta Analyses")

}else{


    l=1
    start=((l-1)*n.per.group)+1
    end=(((l-1)*n.per.group)+n.per.group)
    selected.matrices <- keep[start:end]
    data.set.km <- data.set.na %>%
        filter(matrix %in% selected.matrices)



    png(filename = paste0("agents/",agent.folder.name,
                          "/AgentAssets/pages/survival_ma4.png"),width = 800, height = 600)
    print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
    dev.off()


    #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
    #to be deleted later
    png(filename = paste0("SLR_objectives/survival_ma4.png"),width = 800, height = 600)
    print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
    dev.off()

}



if(dim(data.set.na)[1]>0){

    if(loops>1){
        l=2
        start=((l-1)*n.per.group)+1
        end=(((l-1)*n.per.group)+n.per.group)
        selected.matrices <- keep[start:end]
        data.set.km <- data.set.na %>%
            filter(matrix %in% selected.matrices)


        png(filename = paste0("agents/",agent.folder.name,
                              "/AgentAssets/pages/survival_ma4b.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
        dev.off()


        #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
        #to be deleted later
        png(filename = paste0("SLR_objectives/survival_ma4b.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
        dev.off()

    }
}


if(dim(data.set.na)[1]>0){

    if(loops>2){
        l=3
        start=((l-1)*n.per.group)+1
        end=(((l-1)*n.per.group)+n.per.group)
        selected.matrices <- keep[start:end]
        data.set.km <- data.set.na %>%
            filter(matrix %in% selected.matrices)


        png(filename = paste0("agents/",agent.folder.name,
                              "/AgentAssets/pages/survival_ma4c.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
        dev.off()


        #2nd figure is just for the flexdashboard construction to work (relative to file being rendered)
        #to be deleted later
        png(filename = paste0("SLR_objectives/survival_ma4c.png"),width = 800, height = 600)
        print(make.KM.curves(data.set.km, agent, CI=FALSE, table=FALSE))
        dev.off()

    }
}
