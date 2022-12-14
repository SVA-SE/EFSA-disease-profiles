source("SLR_objectives/RUN-list-header.r")


#local.dir <- "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/"

a=1

# ExperimentalInfections ---- 
    i=1
  for (a in 1:length(agents)){ 


    dfei <- read.csv("data/FilesDownload/ExperimentalInfections_cleaned.csv")
    agent <- agents[a] 
    dfDZ <- dfei[dfei$agent==agent,]
    n.papers <- length(unique(dfDZ$refID))
      
    file.input <- paste0("SLR_objectives/",input.files[i])
      if(n.papers==0)(file.input <- "SLR_objectives/ExperimentalInfections-NOPAPER.Rmd" )
      if(n.papers==1)(file.input <- "SLR_objectives/ExperimentalInfections-onlyREF.Rmd" )
  
  if(n.papers > 1){
    source("SLR_objectives/ExperimentalInfections_MA.r")
  }
      
    rmarkdown::render(file.input,
                    params = list(agent = agents[a],
                                  agent.subtype = agent.subtypes[a],
                                  styling = TRUE,
                                  min.groups.plot = 1),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  # f <- paste0("agents/",agent.folder.names[a],
  #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  # x <- readLines(f)
  # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  # x <- gsub( paste0(local.dir, "SLR_objectives/"),
  #            "", x )
  # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
  #            "src=../../assets/css/images/info.png", x )
  # cat(x, file=f, sep="\n")
  unlink("SLR_objectives/*.png")

  }


    
    # transmission ----
    
  i=2
  for (a in 1:length(agents)){ #a=26
     
   
    dfei <- read.csv("data/FilesDownload/ExperimentalInfections_cleaned.csv")
    agent <- agents[a] 
    dfDZ <- dfei[dfei$agent==agent,]
    n.papers <- length(unique(dfDZ$refID))
    
    file.input <- paste0("SLR_objectives/",input.files[i])
    if(n.papers==0)(file.input <- "Transmission-NOPAPER.Rmd" )
    if(n.papers==1)(file.input <- "Transmission-onlyREF.Rmd" )
    
    if(n.papers > 1){
    
      source("SLR_objectives/Transmission_MA.r")
      file.input <- input.files[i]
        if(agent.types[a]=="b")(file.input <- "Transmission_Bact.Rmd" )
        if(agent.types[a]=="p")(file.input <- "Transmission_Paras.Rmd" )
    }
  
    rmarkdown::render(paste0("SLR_objectives/",file.input),
                    params = list(agent = agents[a],
                                  agent.subtype = agent.subtypes[a],
                                  styling = TRUE,
                                  min.groups.plot = 1,
                                  matrix = matrix.vector[a]),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
    }
  # f <- paste0("agents/",agent.folder.names[a],
  #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  # x <- readLines(f)
  # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  # x <- gsub( paste0(local.dir, "SLR_objectives/"),
  #            "", x )
  # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
  #            "src=../../assets/css/images/info.png", x )
  # cat(x, file=f, sep="\n")
  unlink("SLR_objectives/*.png")




  # PathogenSurvival  ----
  
  #source("SLR_objectives/RUN-list-header.r")
  
  i=3

  for (a in 1:length(agents)){ #a=48
    
  file.input <- input.files[i]
  if(agent.types[a]!="v"){
    (file.input <- "PathogenSurvival_Bact.Rmd" )
    source("SLR_objectives/PathogenSurvival_MA_Bact.r")
  }else{
    source("SLR_objectives/PathogenSurvival_MA.r")
    }

  rmarkdown::render(paste0("SLR_objectives/",file.input),
                    params = list(agent = agents[a],
                                  agent.subtype = agent.subtypes[a],
                                  styling = TRUE,
                                  min.groups.plot = 1),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  # f <- paste0("agents/",agent.folder.names[a],
  #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  # x <- readLines(f)
  # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  # x <- gsub( paste0(local.dir, "SLR_objectives/"),
  #            "", x )
  # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
  #            "src=../../assets/css/images/info.png", x )
  # cat(x, file=f, sep="\n")
  unlink("SLR_objectives/*.png")
}

  
# DiagnosticTests ----


  i=4
  
  for (a in 1:length(agents)){ #a=1
    
  rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                    params = list(agent = agents[a],
                                  agent.subtype = agent.subtypes[a],
                                  styling = TRUE,
                                  min.groups.plot = 1),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  # f <- paste0("agents/",agent.folder.names[a],
  #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  # x <- readLines(f)
  # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  # x <- gsub( paste0(local.dir, "SLR_objectives/"),
  #            "", x )
  # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
  #            "src=../../assets/css/images/info.png", x )
  # cat(x, file=f, sep="\n")
}


  
  # treatments -----
  i=5
  
  for (a in 1:length(agents)){ 
    
  rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                    params = list(agent = agents[a]),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  # f <- paste0("agents/",agent.folder.names[a],
  #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  # x <- readLines(f)
  # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  # x <- gsub( paste0(local.dir, "SLR_objectives/"),
  #            "", x )
  # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
  #            "src=../../assets/css/images/info.png", x )
  # cat(x, file=f, sep="\n")


  }
  
  
  # vaccines ----
  
  i=6
  
  #a=13
  #for (a in 1:length(agents)){ 
  for (a in 1:c(3,13,14)){ #restrict to those with known SLRs for now, as the others are manually set to have pictures.
    
  rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                    params = list(agent = agents[a]),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  # f <- paste0("agents/",agent.folder.names[a],
  #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  # x <- readLines(f)
  # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  # x <- gsub( paste0(local.dir, "SLR_objectives/"),
  #            "", x )
  # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
  #            "src=../../assets/css/images/info.png", x )
  # cat(x, file=f, sep="\n")

}


  
  
#Vector control ----

for(a in ticks.dz){
rmarkdown::render(paste0("SLR_objectives/","VectorControl-Ticks.Rmd"),
                  output_file = paste0("../agents/",agent.folder.names[a],
                                       "/AgentAssets/pages/VectorControl-Ticks.html")
)
# f <- paste0("agents/",agent.folder.names[a],
#             "/AgentAssets/pages/VectorControl-Ticks.html")
# x <- readLines(f)
# x <- gsub( paste0("VectorControl-Ticks_files/"), "../../assets/flexdash_libs/", x )
# x <- gsub( paste0(local.dir, "SLR_objectives/"),
#            "", x )
# x <- gsub( "src=../../../../templates/assets/css/images/info.png",
#            "src=../../assets/css/images/info.png", x )
# cat(x, file=f, sep="\n")
}


for(a in mosquitoes.dz){
  rmarkdown::render(paste0("SLR_objectives/","VectorControl-Mosquitoes.Rmd"),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/VectorControl-Mosquitoes.html")
  )
  # f <- paste0("agents/",agent.folder.names[a],
  #             "/AgentAssets/pages/VectorControl-Mosquitoes.html")
  # x <- readLines(f)
  # x <- gsub( paste0("VectorControl-Mosquitoes_files/"), "../../assets/flexdash_libs/", x )
  # x <- gsub( paste0(local.dir, "SLR_objectives/"),
  #            "", x )
  # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
  #            "src=../../assets/css/images/info.png", x )
  # cat(x, file=f, sep="\n")
}



for(a in midges.dz){
  rmarkdown::render(paste0("SLR_objectives/","VectorControl-Midges.Rmd"),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/VectorControl-Midges.html")
  )
  # f <- paste0("agents/",agent.folder.names[a],
  #             "/AgentAssets/pages/VectorControl-Midges.html")
  # x <- readLines(f)
  # x <- gsub( paste0("VectorControl-Midges_files/"), "../../assets/flexdash_libs/", x )
  # x <- gsub( paste0(local.dir, "SLR_objectives/"),
  #            "", x )
  # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
  #            "src=../../assets/css/images/info.png", x )
  # cat(x, file=f, sep="\n")
}



# Diseases ----
  
  
  i=7
  
 for (a in 1:length(agents)){ #a=3
    
    rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                      params = list(agent = agents[a],
                                    agent.subtype = agent.subtypes[a],
                                    styling = TRUE,
                                    min.groups.plot = 1),
                      output_file = paste0("../agents/",agent.folder.names[a],
                                           "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
    )
    # f <- paste0("agents/",agent.folder.names[a],
    #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
    # x <- readLines(f)
    # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
    # x <- gsub( paste0(local.dir, "SLR_objectives/"),
    #            "", x )
    # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
    #            "src=../../assets/css/images/info.png", x )
    # cat(x, file=f, sep="\n")
  }
  
  

