# impact on animal health ----

CS.plots <- function(dataset,category,min.groups.plot=min.groups.plot){

  dfIncInf <- dataset[!(is.na(dataset$minIncub)&is.na(dataset$maxIncub)),]
  dfIncInf <- dfIncInf[,c("uniqueID","minIncub","maxIncub","targetSpecies",category)]
  dfIncInf <- unique(dfIncInf)

  b=sort(table(dfIncInf[,category]),decreasing = T)
  if(is.table(b)){
  selected.cat<-rownames(b)[1:length(which(table(dfIncInf[,category])>=min.groups.plot))]
  }else{
    selected.cat<-names(b)
  }
  selected.rows<- dfIncInf[,category]%in%selected.cat

  dfIncInf <- dfIncInf[selected.rows,]
  dfIncInf[,category] <- as.factor(as.character(dfIncInf[,category]))


  df1 <- data.frame(
    Species=dfIncInf[,category],
    Days=dfIncInf$minIncub,
    limit="Start of Clinical Signs"
  )
  df2 <- data.frame(
    Species=dfIncInf[,category],
    Days=dfIncInf$maxIncub,
    limit="End of Clinical Signs"
  )
  dfplot <- rbind(df1,df2)

  plot <- plot_ly(dfplot, y = ~Species, x = ~Days, color = ~limit, type = "box") %>%
    layout(boxmode = "group", legend = list(traceorder="reversed",x = 0.7, y = 0.9))
  plot

}

clinical.signs.plot <- function(dataset,ID="uniqueID",min.groups.plot=min.groups.plot){
  #dataset=dfBT
  #ID="uniqueID"

  # plotting CS per uniqueID: duplicated rows should be removed
  dataset <- dataset[!duplicated(dataset[,ID]),]
  dataset <- dataset %>% select (uniqueID, targetSpecies, reportingCS, starts_with("CS_"))

  #selecting from studies that reported CS
  dataset<-dataset[dataset$reportingCS !="not investigated/not given/not relevant",]

  d1 <- dataset[,c("uniqueID", "targetSpecies", "reportingCS")]
  d2 <- dataset %>% select (starts_with("CS_"))
  d2 <- d2[,sapply(d2, function(x) length(unique(x))) > 0]
  dataset <- cbind(d1,d2)

  #it is the SPECIES that should go into the columns (the selectable part)
  #and the clinical signs in rows
  ss <- table(dataset$targetSpecies)
  if(dim(ss)>1){
  select.species<-rownames(sort(table(dataset$targetSpecies),decreasing = T))[1:length(which(table(dataset$targetSpecies)>=min.groups.plot))]
  }else{
    select.species<-names(ss)
  }

  ClinicalSigns<-colnames(dataset %>% select(starts_with("CS_")))
  ClinicalSigns<-str_remove(ClinicalSigns,"CS_")
  ClinicalSigns<-unique(ClinicalSigns)
  ClinicalSigns[which(ClinicalSigns=="NotReported")]<-NA
  ClinicalSigns <- ClinicalSigns[!is.na(ClinicalSigns)]

  if(length(select.species)>0){

    species.columns <- matrix(NA,nrow=length(ClinicalSigns),ncol=length(select.species))
    colnames(species.columns)<-select.species

    dataCS <- data.frame(ClinicalSigns,species.columns)
    colnames(dataCS)<-c("ClinicalSigns",select.species)

    dataCSd <- dataCS
    dataCSn <- dataCS


    for (r in 1:length(ClinicalSigns)){
      for (c in 1:length(select.species)){

        col <- dataset %>% select(starts_with(paste0("CS_",ClinicalSigns[r])))
        rows <- which(dataset$targetSpecies==select.species[c])
        total <- length(rows)
        numerator <- length(which(col=="yes"&dataset$targetSpecies==select.species[c]))

        dataCS[r,c+1]<-round((numerator/total)*100,0)
        dataCSn[r,c+1]<-numerator
        dataCSd[r,c+1]<-total

      }  }



    dataCS <- dataCS[order(dataCS[,2],decreasing=TRUE),]
    colnames(dataCS)<-str_remove_all(colnames(dataCS)," ")
    colnames(dataCS)<-gsub("\\s*\\([^\\)]+\\)","",as.character(colnames(dataCS)))

    dataCS$ClinicalSigns <- factor(dataCS$ClinicalSigns, levels = dataCS[["ClinicalSigns"]])
    dataCS <- dataCS[order(as.character(dataCS$ClinicalSigns)),]

    if(length(which(dataCS$ClinicalSigns=="Abscence"))>0){
    dataCS$ClinicalSigns <- relevel(dataCS$ClinicalSigns,"Abscence")
    }

      }

  dataCS <- dataCS[apply(dataCS[,-1,drop=F], 1, function(x) !all(x==0)),]
  if(length(which(dataCS$ClinicalSigns=="ScoreTypical"))>0){
  dataCS <- dataCS[-which(dataCS$ClinicalSigns=="ScoreTypical"),]
  }
  dataCS$ClinicalSigns <- drop.levels(dataCS$ClinicalSigns)

  numerators <- dataCSn[match(dataCS$ClinicalSigns,dataCSn$ClinicalSigns),]
  denominators <- dataCSd[match(dataCS$ClinicalSigns,dataCSn$ClinicalSigns),]



  if(length(select.species)>0){
    p <- plot_ly(dataCS, x = ~ClinicalSigns, y = ~dataCS[,2], type = 'bar',
                 name = colnames(dataCS)[2], hoverinfo = 'text',
                 text = paste0(dataCS[,1],"<br>",dataCS[,2],"% (",numerators[,2],"/",denominators[,2],")"))%>%
      layout(yaxis = list(title = '% animal groups reporting sign'),
             xaxis = list(tickangle=-45),
             barmode = 'group')


    if (dim(dataCS)[2]>2){
      for (t in 3:dim(dataCS)[2]){
        p <- p %>% add_trace(y = ~dataCS[,t], name = colnames(dataCS)[t],
                             hoverinfo = 'text',
                             text = paste0(dataCS[,1],"<br>",dataCS[,t],
                                           "% (",numerators[,t],"/",denominators[,t],")"))
      }
    }

    p
  }
}

clinical.signs.reported <- function(dataset,ID="uniqueID",styling=T){
  # plotting CS per uniqueID: duplicated rows should be removed
  dataset <- dataset[!duplicated(dataset[,ID]),]
  dataset <- dataset %>% select (uniqueID, targetSpecies, reportingCS, starts_with("CS_"))

  #selecting from studies that reported CS
  dataset<-dataset[dataset$reportingCS !="not investigated/not given/not relevant",]
  dataset <- dataset[, sapply(dataset, function(x) length(unique(x))) > 1]
  dataset[dataset=="not investigated/not given/not relevant"]<-NA

  #it is the SPECIES that should go into the columns (the selectable part)
  #and the clinical signs in rows
  select.species<-rownames(sort(table(dataset$targetSpecies),decreasing = T))[1:length(which(table(dataset$targetSpecies)>=1))]

  if(length(select.species)>0){

    cs <- list()

    for (r in (1:length(select.species))){

      cs.sp <- dataset[dataset$targetSpecies==select.species[r],] %>% select(starts_with("CS_"))
      cs.sp <- cs.sp[!apply(cs.sp, 1, function(x) all(is.na(x))),]
      cs.sp <- cs.sp[,!apply(cs.sp, 2, function(x) all(is.na(x)))]

      ClinicalSigns<-colnames(cs.sp)
      ClinicalSigns<-str_remove(ClinicalSigns,"CS_")
      ClinicalSigns<-unique(ClinicalSigns)
      ClinicalSigns[which(ClinicalSigns=="NotReported")]<-NA
      ClinicalSigns <- ClinicalSigns[!is.na(ClinicalSigns)]

      if(length(ClinicalSigns)==0){
        cs[[r]]<-"Studies did not investigate clinical signs"
      }else{
        cs[[r]]<-str_c(ClinicalSigns,collapse=", ")
      }
    }

    species.cs<-cbind(select.species,cs)
    colnames(species.cs)<- c("Species","All listed observed Clinical Signs")



    if (styling==T){
      kable(species.cs)%>%
        kable_styling(full_width=F) %>%
        column_spec(column=2,width="20em")
    }else{
      if(styling=="word"){
        datatable(species.cs)
      }else{
        kable(species.cs)
      }
    }




  }
}

mortality.group.plot <- function(dataset,ID="uniqueID",subcategory=NULL){ #dataset=dfDZ

  # mortality per uniqueID: duplicated rows should be removed
  dataset <- dataset[!duplicated(dataset[,ID]),]

  #remove groups in which mortality was never reported
  dataset<-dataset[dataset$reportingCS !="NOT REPORTED",]

  if(is.na(subcategory)){
    dataset <- dataset[,c("uniqueID", "targetSpecies",
                          "deadInfection", "eutInfection")]
  }else{
  dataset <- dataset[,c("uniqueID", "targetSpecies",
                   "deadInfection", "eutInfection", subcategory)]
  }


  #it is the SPECIES that should go into the columns (the selectable part)
  #and the mortality in rows
  select.species<-rownames(sort(table(dataset$targetSpecies),decreasing = T))[1:length(which(table(dataset$targetSpecies)>=1))]

  subcategories <- 1

  if(!is.na(subcategory)){
    subcategories <-sort(unique(dataset[,subcategory]))
  }

  mort.matrix <- matrix(NA,nrow=length(select.species),ncol=length(subcategories))
  colnames(mort.matrix) <- subcategories
  rownames(mort.matrix) <- select.species

    for (c in 1:length(subcategories)){
      for (r in 1:length(select.species)){

        if(subcategories[1]==1){
          rows <- which(dataset$targetSpecies==select.species[r])
        }else{
        rows <- which(dataset$targetSpecies==select.species[r]&
                        dataset[,subcategory]==subcategories[c])
        }
        col <- dataset[rows,c("deadInfection", "eutInfection")]

        total <- length(rows) * dim(col)[2]
        numerator <- length(which(col=="yes"))

        mort.matrix[r,c]<-(numerator/total)*100

      }  }

  mort.matrix[which(is.nan(mort.matrix))]<- NA
  mort.matrix <- as.data.frame(mort.matrix)


  #mort.matrix_long <- gather(mort.matrix, subtype, mortality, subcategories, factor_key=TRUE)
  mort.matrix$Species<- select.species



    p <- plot_ly(mort.matrix, x = ~Species, y = mort.matrix[,1], name=colnames(mort.matrix)[1],type = 'bar',
                 text=paste0(round(mort.matrix[,1],1),"%"), textposition = 'outside')%>%
      #add_text(text=paste0(round(mort.matrix[,1],1),"%"), hoverinfo='none', textposition = 'auto', showlegend = FALSE,
      #         textfont=list(size=20, color="black")) %>%
      layout(yaxis = list(title = '% of animal groups',range=list(0,100)),
             xaxis = list(title = 'Host species'),
             barmode = 'group',
             title =  'Case fatality associated with infection, per animal group')

    if (dim(mort.matrix)[2]>2){
      for (t in 2:(dim(mort.matrix)[2]-1)){
        p <- p %>% add_trace(mort.matrix, x = ~Species, y = mort.matrix[,t], name=colnames(mort.matrix)[t],type = 'bar',
                             text=paste0(round(mort.matrix[,t],1),"%"), textposition = 'outside')
       #add_text(text=paste0(round(mort.matrix[,t],1),"%"), hoverinfo='none', textposition = 'top', showlegend = FALSE,
      #             textfont=list(size=20, color="black"))
      }
    }

    p
  }






# transmission ----

inf.plots <- function(dataset,category,targetLabTest=NULL,matrix=NULL,not.matrix=NULL,min.groups.plot=min.groups.plot,
                      separated.tests=TRUE,agent.type="virus"){

  dfIncInf <- dataset[!(is.na(dataset$minIncub)&is.na(dataset$maxIncub)&is.na(dataset$minDetect)&is.na(dataset$maxDetect)),]
  dfIncInf <- dfIncInf[,c("uniqueID","minDetect","maxDetect",
                          "matrix","targetLabTest","targetSpecies",category)]
  dfIncInf <- unique(dfIncInf)

  #if set to NULL, simply does not select based on that parameter
  if (!is.null(targetLabTest)){
    dfIncInf <- dfIncInf[dfIncInf$targetLabTest==targetLabTest,]
  }

  if (!is.null(matrix)){
    rows.matrix <- str_detect(tolower(dfIncInf$matrix),tolower(matrix))
    dfIncInf <- dfIncInf[rows.matrix,]
  }

  if (!is.null(not.matrix)){
    rows.matrix.not <- str_detect(tolower(dfIncInf$matrix),tolower(not.matrix))
    dfIncInf <- dfIncInf[!rows.matrix.not,]
  }

  b=sort(table(dfIncInf[,category]),decreasing = T)
  if(is.table(b)){
    selected.cat<-rownames(b)[1:length(which(table(dfIncInf[,category])>=min.groups.plot))]
  }else{
    selected.cat<-names(b)
  }
  selected.rows<- dfIncInf[,category]%in%selected.cat

  dfIncInf <- dfIncInf[selected.rows,]
  dfIncInf[,category] <- as.factor(as.character(dfIncInf[,category]))

  if(separated.tests==TRUE){
    #dfIncInf[dfIncInf$targetLabTest==targetLabTest,]


    df0<- data.frame(
      Species=NA,
      Days=NA,
      limit=NA
    )

    if(agent.type=="virus"){
    try({df1 <- data.frame(
      Species=dfIncInf[dfIncInf$targetLabTest=="Virus",category],
      Days=dfIncInf$minDetect[dfIncInf$targetLabTest=="Virus"],
      limit="VIRUS:Earliest_Detection_Day"
    )
    df0 <- rbind(df0,df1)
    },silent=TRUE)

    try({df2 <- data.frame(
      Species=dfIncInf[dfIncInf$targetLabTest=="Virus",category],
      Days=dfIncInf$maxDetect[dfIncInf$targetLabTest=="Virus"],
      limit="VIRUS:Latest_Detection_Day"
    )
    df0 <- rbind(df0,df2)
    },silent=TRUE)
    }

    if(agent.type=="bacteria"){
      try({df1 <- data.frame(
        Species=dfIncInf[dfIncInf$targetLabTest=="Bacterium/Rickettsial bodies",category],
        Days=dfIncInf$minDetect[dfIncInf$targetLabTest=="Bacterium/Rickettsial bodies"],
        limit="AGENT DETECTION:Earliest_Detection_Day"
      )
      df0 <- rbind(df0,df1)
      },silent=TRUE)

      try({df2 <- data.frame(
        Species=dfIncInf[dfIncInf$targetLabTest=="Bacterium/Rickettsial bodies",category],
        Days=dfIncInf$maxDetect[dfIncInf$targetLabTest=="Bacterium/Rickettsial bodies"],
        limit="AGENT DETECTION:Latest_Detection_Day"
      )
      df0 <- rbind(df0,df2)
      },silent=TRUE)
    }

    if(agent.type=="parasite"){
      try({df1 <- data.frame(
        Species=dfIncInf[dfIncInf$targetLabTest=="Parasite",category],
        Days=dfIncInf$minDetect[dfIncInf$targetLabTest=="Parasite"],
        limit="AGENT DETECTION:Earliest_Detection_Day"
      )
      df0 <- rbind(df0,df1)
      },silent=TRUE)

      try({df2 <- data.frame(
        Species=dfIncInf[dfIncInf$targetLabTest=="Parasite",category],
        Days=dfIncInf$maxDetect[dfIncInf$targetLabTest=="Parasite"],
        limit="AGENT DETECTION:Latest_Detection_Day"
      )
      df0 <- rbind(df0,df2)
      },silent=TRUE)
    }


    try({df3 <- data.frame(
      Species=dfIncInf[dfIncInf$targetLabTest=="Nucleic acid",category],
      Days=dfIncInf$minDetect[dfIncInf$targetLabTest=="Nucleic acid"],
      limit="DNA/RNA:Earliest_Detection_Day"
    )
    df0 <- rbind(df0,df3)
    },silent=TRUE)

    try({df4 <- data.frame(
      Species=dfIncInf[dfIncInf$targetLabTest=="Nucleic acid",category],
      Days=dfIncInf$maxDetect[dfIncInf$targetLabTest=="Nucleic acid"],
      limit="DNA/RNA:Latest_Detection_Day"
    )
    df0 <- rbind(df0,df4)
    },silent=TRUE)

    try({df5 <- data.frame(
      Species=dfIncInf[dfIncInf$targetLabTest=="Antigen",category],
      Days=dfIncInf$minDetect[dfIncInf$targetLabTest=="Antigen"],
      limit="ANTIGEN:Earliest_Detection_Day"
    )
    df0 <- rbind(df0,df5)
    },silent=TRUE)

    try({df6 <- data.frame(
      Species=dfIncInf[dfIncInf$targetLabTest=="Antigen",category],
      Days=dfIncInf$maxDetect[dfIncInf$targetLabTest=="Antigen"],
      limit="ANTIGEN:Latest_Detection_Day"
    )
    df0 <- rbind(df0,df6)
    },silent=TRUE)

   # dfplot <- rbind(df1,df2,df3,df4,df5,df6)
    dfplot <- df0

    text.title <- "Detection in BLOOD"
    if(is.null(matrix)){
      text.title <- "Distribution for ALL matrices evaluated"
    }

    dfplot$limit<- as.ordered(dfplot$limit)

    plot <- plot_ly(dfplot, y = ~Species, x = ~Days, color = ~limit, type = "box") %>%
      layout(boxmode = "group",  title=list(text=text.title,y = 0.95, x = 0.5),
             legend = list(traceorder="reversed",x = 0.7, y = 0.9))
    plot




  }else{
  df3 <- data.frame(
    Species=dfIncInf[,category],
    Days=dfIncInf$minDetect,
    limit="Earliest_Detection_Day"
  )
  df4 <- data.frame(
    Species=dfIncInf[,category],
    Days=dfIncInf$maxDetect,
    limit="Latest_Detection_Day"
  )
  dfplot <- rbind(df3,df4)
  dfplot$limit<- as.ordered(dfplot$limit)

text.title <- "Detection in BLOOD"
if(is.null(matrix)){
  text.title <- "Distribution for ALL matrices evaluated"
}

  plot <- plot_ly(dfplot, y = ~Species, x = ~Days, color = ~limit, type = "box") %>%
    layout(boxmode = "group", title=list(text=text.title,y = 0.95, x = 0.5),
           legend = list(traceorder="reversed",x = 0.7, y = 0.9))
  plot
  }

}

multiple.cat.summary.numerical <- function(dataset,columns,outcomes,ID="uniqueID",labels=NULL,FUN="median",
                                           styling=T,outcome.type=NULL){

  if(is.null(labels)){
    labels <- c(columns,outcomes)
  }

  table.data <- dataset[,c(ID,columns,outcomes)]
  table.data <- unique(table.data)
  table.data[,ID]<-1

  table.groups <- aggregate(table.data[,c(ID, outcomes)],by=as.list(table.data[,columns]),FUN=sum,na.rm=TRUE)
colnames(table.groups)<- c(columns,ID,outcomes)

  table.data <- aggregate(table.data[,outcomes],by=as.list(table.data[,columns]),FUN,na.rm=TRUE)
colnames(table.data)<- c(columns,outcomes)

if(!is.null(outcome.type)){
  if(outcome.type=="percentage"){
  table.data[,outcomes] <- paste0(as.character(round(table.data[,outcomes],1)),"%")
  }
}


    merged <- cbind(table.groups[,c(columns,ID)],table.data[,outcomes])


  table.data <- merged[order(merged[,columns[1]],merged[,columns[2]]),]
  colnames(table.data)<- labels

  if (styling==T){
  kable(table.data, align="c",row.names = F)%>%
    kable_styling()
  }else{

    if(styling=="word"){

      datatable(table.data)

    }else{
    kable(table.data, align="c",row.names = F)
    }
  }

}

multiple.cat.summary.numerical.dfOutput <- function(dataset,columns,outcomes,ID="uniqueID",labels=NULL,FUN="median",
                                           styling=T){

  if(is.null(labels)){
    labels <- c(columns,outcomes)
  }

  table.data <- dataset[,c(ID,columns,outcomes)]
  table.data <- unique(table.data)
  table.data[,ID]<-1

  table.groups <- aggregate(table.data[,c(ID, outcomes)],by=as.list(table.data[,columns]),FUN=sum,na.rm=TRUE)
  colnames(table.groups)<- c(columns,ID,outcomes)

  table.data <- aggregate(table.data[,outcomes],by=as.list(table.data[,columns]),FUN,na.rm=TRUE)
  colnames(table.data)<- c(columns,outcomes)


  merged <- cbind(table.groups[,c(columns,ID)],table.data[,outcomes])


  table.data <- merged[order(merged[,columns[1]],merged[,columns[2]]),]
  colnames(table.data)<- labels

  return(table.data)
}

# survival ----
papers.groups.table <- function(dataset,column,styling=TRUE){

  papers <- table(dataset[!duplicated(dataset[,c("refID",column)]),column])
  groups <- table(dataset[!duplicated(dataset[,c("uniqueID",column)]),column])

  table.data <- cbind(papers,groups)
  colnames(table.data)<-c("Articles reviewed","Study groups reported")
  table.data <- table.data[order(table.data[,1], decreasing = T),,drop=F]
  table.data <- table.data[!apply(table.data, 1, function(x) all(x ==0)),,drop=F]
  table.data <- addmargins(table.data,margin=1)


  if (styling==T){
    kable(table.data, align="c")%>%
      kable_styling()
  }else{

    if(styling=="word"){

      datatable(table.data)

    }else{
      kable(table.data, align="c")
    }
  }

}


# vectors ----

prepare.df.for.barplots <- function(df1=df1){

  df1 <- df1 %>% filter (intervention != "Control")
  df1 <- df1 %>% filter (intervention != "No intervention")
  df1 <- df1 %>% filter (intervention != "Placebo")

  df1$efficacy <- df1$efficacy*100

  df1 <- df1 %>% select (refID, groupID, studyTarget_host, route, efficacy, mortPerc,
                         testSubstance1_cat, testSubstance2_cat,formulation1_Perc,formulation2_Perc,
                         dose,	dose2,	doseUnits1,	doseUnits2,	dosageFreq1,	dosageFreq2,
                         dosageInterval1,	dosageInterval2,
                         timePoint, exMinutes, mortHours, rowID, groupIDunique
  )

  df1 <- df1[order(df1$route,
                   df1$testSubstance1_cat,
                   df1$studyTarget_host,
                   df1$testSubstance2_cat,
                   df1$timePoint,
                   df1$refID, df1$groupID),]

  df1[df1=="not investigated/not given/not relevant"]<-NA


  df1$route <- as.factor(as.character(df1$route))
  df1$testSubstance1_cat <-(as.character(df1$testSubstance1_cat))

  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

  df1$color <- col_vector[as.numeric(as.factor(df1$testSubstance1_cat))]

  #df1$mortPerc <- ifelse(is.na(df1$efficacy),df1$mortPerc,0)

  #df1$mortPerc[is.na(df1$mortPerc)]<- 0
  #df1$efficacy[is.na(df1$efficacy)]<- 0

  return(df1)
}



efficacy.route.species.boxplots <- function(df,title=NULL){
  fig <- plot_ly(df, x = ~substanceDisplay, y = ~efficacy, color = ~studyTarget_host, type = "box")%>%
    layout(title = title,
           xaxis = list(title = 'Substance or combination of substances tested',tickangle = -45),
           yaxis = list(title = 'Efficacy %'))
  #fig <- fig %>% layout(barmode = "group")
  if(length(which(is.na(df$efficacy)&!is.na(df$mortPerc)))>0){
    fig <- fig %>% add_boxplot(df, x = ~substanceDisplay, y = ~mortPerc, color = ~studyTarget_host,
                               line = list(color = 'rgb(7,40,89)'))
  }
  fig
}


efficacy.direct.boxplots <- function(df,title=NULL){
  fig <- plot_ly(df, x = ~substanceDisplay, y = ~efficacy, type = "box",name='efficacy')%>%
    layout(title = title,
           xaxis = list(title = 'Substance or combination of substances tested',
                        tickangle = -45),
           yaxis = list(title = '% Mortality or Efficacy'),
           margin = list(b = 160))
  #fig <- fig %>% layout(barmode = "group")
  if(length(which(is.na(df$efficacy)&!is.na(df$mortPerc)))>0){
    fig <- fig %>% add_boxplot(df, x = ~substanceDisplay, y = ~mortPerc, name='mortality')
  }
  fig
}



species.name.cleanup <- function(data,colname="targetSpecies"){

  species.clean <- rbind(
    c("ALPACA - Alpaca","Alpaca"),
    c("ALPACA - Vicugna","Vicugna"),
    c("ASS - Asses","Ass"),
    c("BUFFALO - Buffalo","Buffalo"),
    c("BUFFALO - Water buffalo","Water Buffalo"),
    c("CAMELUS - Camel","Camel"),
    c("CAMELUS - Dromedary","Dromedary"),
    c("CATTLE - Cattle","Cattle"),
    c("CATTLE - Zebu","Cattle"),
    c("DEER - black-tailed deer","Black-tailed deer"),
    c("DEER - Fallow deer","Fallow deer"),
      c("DEER - Red Deer","Red deer"),
    c("DEER - Roe deer","Roe deer"),
        c("DEER - Wapiti elk","Wapiti elk"),
    c("DEER - white-tailed deer","White-tailed deer"),
              c("HORSE - Equines","Equine (Equus sp.)"),
    c("HORSE - Horse","Horses"),
    c("LEPORIDAE - European hare","European hare"),
                  c("LEPORIDAE - Rabbit","Rabbit"),
                      c("PIG - Pig","Pigs"),
    c("PIG - Wild boar","Wild boar")
  )

  for(s in 1:dim(species.clean)[1]){
  data[,colname]<- str_replace(as.character(data[,colname]), species.clean[s,1], species.clean[s,2])
  }



  return(data)
}
