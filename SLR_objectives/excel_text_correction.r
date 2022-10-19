rm(list=ls())
require(openxlsx)
require(stringr)

source("SLR_objectives/RUN-list-header.r")

for (a in c(1:38,40:48)){
#for (a in 1:14){

    #text_to_look <- 'https://svastatichosting.z6.web.core.windows.net/storymaps/'
    #text_to_replace <- 'https://svastatichosting.z6.web.core.windows.net/disease-profiles/'

    #text_to_look <- "Refer to the \\{ref008:OIE Technical disease card\\} for a key summary of the disease in animal hosts."
    #text_to_replace <- "A summary of the disease in animal hosts is given in the \\{ref008:OIE Technical disease card\\}."


    # text_to_look <- "\\(true positive and negative samples\\)"
    # text_to_replace <- "\\(samples from truly infected and non-infected animals\\)"

     text_to_look <- "Impact-"
     text_to_replace <- "ExperimentalInfections-"



    df <- openxlsx::read.xlsx(paste0("agents/",agent.folder.names[a],
                                     "/",agent.folder.names[a],"-Data-Contents.xlsx"))

    refs <- openxlsx::read.xlsx(paste0("agents/",agent.folder.names[a],
                                       "/",agent.folder.names[a],"-Data-Contents.xlsx"),sheet=2)


    df[] <- lapply(df, function(x) {x <- gsub(text_to_look,text_to_replace,x);x})
    refs[] <- lapply(refs, function(x) {x <- gsub(text_to_look,text_to_replace,x);x})



    df$ID <- as.numeric(df$ID)
    df$ParentID  <- as.numeric(df$ParentID)
    df$Level  <- as.numeric(df$Level)

    refs$AddToList <- as.numeric(refs$AddToList)

    openxlsx::write.xlsx(df, file=paste0("agents/",agent.folder.names[a],
                                         "/",agent.folder.names[a],"-Data-Contents.xlsx"),
                         overwrite = T)


    agent.file <- loadWorkbook(paste0("agents/",agent.folder.names[a],
                                      "/",agent.folder.names[a],"-Data-Contents.xlsx"))
    addWorksheet(agent.file, "References")
    writeData(agent.file, sheet = "References", refs)
    saveWorkbook(agent.file, paste0("agents/",agent.folder.names[a],
                                    "/",agent.folder.names[a],"-Data-Contents.xlsx"),
                 overwrite = TRUE)


}






