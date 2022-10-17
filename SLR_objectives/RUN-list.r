source("SLR_objectives/RUN-list-header.r")


for (a in 1:length(agents)){ #a=13

  #for (a in 12:14){

  #for (i in 1:length(input.files)){ #i=1 i=3

  #for(a in c(1:14,17,19,20,22,23,25,26,27,28,29,30,32,38,43,44,46,47,48)){
    i=1
  source("SLR_objectives/1impact_MA.r")
  rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                    params = list(agent = agents[a],
                                  agent.subtype = agent.subtypes[a],
                                  styling = TRUE,
                                  min.groups.plot = 1),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  x <- readLines(f)
  x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")
  unlink("SLR_objectives/*.png")
#}



  #for(a in c(1:14,17,18,19,20,21,22,23,25,26,27,28,29,30,32,33,38,41,43,44,46,47,48)){

  i=2
  source("SLR_objectives/2transmission_MA.r")
  file.input <- input.files[i]
  if(agent.types[a]=="b")(file.input <- "2transmission_Bact.Rmd" )
  if(agent.types[a]=="p")(file.input <- "2transmission_Paras.Rmd" )

  rmarkdown::render(paste0("SLR_objectives/",file.input),
                    params = list(agent = agents[a],
                                  agent.subtype = agent.subtypes[a],
                                  styling = TRUE,
                                  min.groups.plot = 1),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  x <- readLines(f)
  x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")
  unlink("SLR_objectives/*.png")
}


  #for(a in c(19,41,43,44,47)){
#for(a in c(1,2,14,13,11,10,7,6)){

  i=3

  file.input <- input.files[i]
  if(agent.types[a]!="v"){
    (file.input <- "3survival_Bact.Rmd" )
    source("SLR_objectives/3survival_MA_Bact.r")
  }else{
    source("SLR_objectives/3survival_MA.r")
    }

  rmarkdown::render(paste0("SLR_objectives/",file.input),
                    params = list(agent = agents[a],
                                  agent.subtype = agent.subtypes[a],
                                  styling = TRUE,
                                  min.groups.plot = 1),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  x <- readLines(f)
  x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")
  unlink("SLR_objectives/*.png")
#}


#for(a in c(19,44)){ #a=13

  i=4
  rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                    params = list(agent = agents[a],
                                  agent.subtype = agent.subtypes[a],
                                  styling = TRUE,
                                  min.groups.plot = 1),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  x <- readLines(f)
  x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")
#}


  i=5
  rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                    params = list(agent = agents[a]),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  x <- readLines(f)
  x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")


  i=6
  rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                    params = list(agent = agents[a]),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
  x <- readLines(f)
  x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")




 #}
}


#Vector control ----

for(a in ticks.dz){
rmarkdown::render(paste0("SLR_objectives/","VectorControl-Ticks.Rmd"),
                  output_file = paste0("../agents/",agent.folder.names[a],
                                       "/AgentAssets/pages/VectorControl-Ticks.html")
)
f <- paste0("agents/",agent.folder.names[a],
            "/AgentAssets/pages/VectorControl-Ticks.html")
x <- readLines(f)
x <- gsub( paste0("VectorControl-Ticks_files/"), "../../assets/flexdash_libs/", x )
x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
           "", x )
x <- gsub( "src=../../../../templates/assets/css/images/info.png",
           "src=../../assets/css/images/info.png", x )
cat(x, file=f, sep="\n")
}


for(a in mosquitoes.dz){
  rmarkdown::render(paste0("SLR_objectives/","VectorControl-Mosquitoes.Rmd"),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/VectorControl-Mosquitoes.html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/VectorControl-Mosquitoes.html")
  x <- readLines(f)
  x <- gsub( paste0("VectorControl-Mosquitoes_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")
}



for(a in midges.dz){
  rmarkdown::render(paste0("SLR_objectives/","VectorControl-Midges.Rmd"),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/VectorControl-Midges.html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/VectorControl-Midges.html")
  x <- readLines(f)
  x <- gsub( paste0("VectorControl-Midges_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")
}





