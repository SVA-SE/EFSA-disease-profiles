require(openxlsx)

rm(list=ls())
source("SLR_objectives/RUN-list-header.r")



####### manually choose which one ##########
a=19




# create dir ----

#for (a in 15:48){
dir.create(file.path("agents/", paste0(agent.folder.names[a],
                                       "/AgentAssets/images")),
           recursive=TRUE, showWarnings = FALSE)
dir.create(file.path("agents/", paste0(agent.folder.names[a],
                                       "/AgentAssets/pages")),
           showWarnings = FALSE)
#}



# covetlab image ----

#for (a in 15:48){
file.copy("templates/assets/css/images/covetlab.png", paste0("agents/",agent.folder.names[a],
                                                                        "/AgentAssets/images/covetlab.png"))
# cerate a file so that git can recognize the folder,
#and i can committhis single folder before the many flexdash libraries are created
#which then always crashes git
file.copy("templates/assets/css/images/covetlab.png", paste0("agents/",agent.folder.names[a],
                                                             "/AgentAssets/pages/DELETE.png"))

#}



# EXCEL ----


for (a in a){

text_to_look <- 'XXXXX'
text_to_replace <- agents.short[a]

if(oie.list[a]==T){
    df <- openxlsx::read.xlsx("agents/VBD-Data-Contents_example_OIE.xlsx")
}else{
    df <- openxlsx::read.xlsx("agents/VBD-Data-Contents_example_notOIE.xlsx")
}


  df[] <- lapply(df, function(x) {x <- gsub(text_to_look,text_to_replace,x);x})
  openxlsx::write.xlsx(df, file=paste0("agents/",agent.folder.names[a],
                                       "/",agent.folder.names[a],"-Data-Contents.xlsx"))


  refs <- openxlsx::read.xlsx("agents/VBD-Data-Contents_example_OIE.xlsx",sheet=2)
  agent.file <- loadWorkbook(paste0("agents/",agent.folder.names[a],
                                       "/",agent.folder.names[a],"-Data-Contents.xlsx"))
  addWorksheet(agent.file, "References")
  writeData(agent.file, sheet = "References", refs)
  saveWorkbook(agent.file, paste0("agents/",agent.folder.names[a],
                          "/",agent.folder.names[a],"-Data-Contents.xlsx"),
               overwrite = TRUE)
}





# individual files without redirect (open to see) ----


for (a in a){
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
    # i=1
    # rmarkdown::render(paste0("SLR_objectives/","1impact-NOPAPER.Rmd"),
    #                   output_file = paste0("../agents/",agent.folder.names[a],
    #                                        "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
    # )
    # f <- paste0("agents/",agent.folder.names[a],
    #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
    # x <- readLines(f)
    # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
    # x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
    #            "", x )
    # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
    #            "src=../../assets/css/images/info.png", x )
    # cat(x, file=f, sep="\n")
    # unlink("SLR_objectives/*.png")


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

    # i=2
    # rmarkdown::render(paste0("SLR_objectives/","1impact-NOPAPER.Rmd"),
    #                   output_file = paste0("../agents/",agent.folder.names[a],
    #                                        "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
    # )
    # f <- paste0("agents/",agent.folder.names[a],
    #             "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
    # x <- readLines(f)
    # x <- gsub( paste0(output.files[i],agent.folder.names[a],"_files/"), "../../assets/flexdash_libs/", x )
    # x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
    #            "", x )
    # x <- gsub( "src=../../../../templates/assets/css/images/info.png",
    #            "src=../../assets/css/images/info.png", x )
    # cat(x, file=f, sep="\n")
    # unlink("SLR_objectives/*.png")



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



    i=4
    rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                      params = list(agent = agents[a],
                                    agent.subtype = agent.subtypes[a],
                                    styling = TRUE,
                                    min.groups.plot = 1),
                      output_file = paste0("../agents/",agent.folder.names[a],
                                           "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
    )



    i=5
    rmarkdown::render(paste0("SLR_objectives/",input.files[i]),
                      params = list(agent = agents[a],
                                    agent.subtype = agent.subtypes[a],
                                    styling = TRUE,
                                    min.groups.plot = 1),
                      output_file = paste0("../agents/",agent.folder.names[a],
                                           "/AgentAssets/pages/",output.files[i], agent.folder.names[a], ".html")
    )




}




# re-run with address correction ----

for (a in a){
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


           unlink(paste0("agents/",agent.folder.names[a],
                         "/AgentAssets/pages/delete.png"))
}



# VECTORS ----

    if (a%in%ticks.dz){
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


    if(a%in%mosquitoes.dz){
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



    if(a%in%midges.dz){
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

if(a%in%sandflies.dz){
  rmarkdown::render(paste0("SLR_objectives/","VectorControl-Sandflies.Rmd"),
                    output_file = paste0("../agents/",agent.folder.names[a],
                                         "/AgentAssets/pages/VectorControl-Sandflies.html")
  )
  f <- paste0("agents/",agent.folder.names[a],
              "/AgentAssets/pages/VectorControl-Sandflies.html")
  x <- readLines(f)
  x <- gsub( paste0("VectorControl-Sandflies_files/"), "../../assets/flexdash_libs/", x )
  x <- gsub( "C:/Users/fernanda.dorea/Documents/AzureDevOps/storymaps/SLR_objectives/",
             "", x )
  x <- gsub( "src=../../../../templates/assets/css/images/info.png",
             "src=../../assets/css/images/info.png", x )
  cat(x, file=f, sep="\n")
}

