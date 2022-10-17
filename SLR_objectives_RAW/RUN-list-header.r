rm(list=ls())

agents = c("African swine fever virus",   #a=1
           "African horse sickness virus", #a=2
           "Rift Valley fever virus",      #a=3
           "Peste des petit ruminants",    #a=4
           "Sheep and goat pox",            #a=5
           "Classical Swine Fever",        #a=6
           "Rinderpest",                   #a=7
           "Contagious bovine pleuropneumonia",   #a=8
           "Contagious caprine pleuropneumonia",  #a=9
           "Glanders (Burkholderia mallei)",      #a=10
           "FMD - Foot and Mouth Disease",        #a=11
           "Lumpy Skin Disease",                  #a=12
           "HPAI - Highly Pathogenic Avian Influenza",   #a=13
           "Newcastle disease",                           #a=14
           "Alkhurma haemorrhagic fever virus", #a=15
           "Aino virus", #a=16
           "Akabane virus", #a=17
           "Bhanja virus", #a=18
           "Bluetongue virus", #a=19
           "Bovine ephemeral fever virus", #a=20
           "Bunyamwera virus", #a=21
           "Crimean-Congo haemorrhagic fever virus", #a=22
           "Eastern equine encephalitis virus", #a=23
           "Equine encephalosis virus",#a=24
           "Ehrlichia ruminantium", #a=25
           "Epizootic haemorrhagic disease virus", #a=26
           "Getah virus", #a=27
           "Hepatozoon canis", #a=28
           "Highlands J virus", #a=29
           "Japanese encephalitis virus", #a=30
           "Kotonkon virus",#a=31
           "Leishmania infantum", #a=32
           "Main drain virus", #a=33
           "Middelburg virus", #a=34
           "Nairobi sheep disease virus", #a=35
           "Palyam virus", #a=36
           "Peruvian horse sickness virus", #a=37
           "Schmallenberg virus", #a=38
           "Semliki Forest virus", #a=39
           "Shuni virus", #a=40
           "St. Louis encephalitis virus", #a=41
           "Thogoto virus", #a=42
           "Venezuelan equine encephalitis virus", #a=43
           "Vesicular stomatitis virus", #a=44
           "Yunnan orbivirus", #a=45
           "Wesselsbron virus", #a=46
           "West Nile virus", #a=47
           "Western equine encephalitis virus" #a=48
)

agents.short = c("ASFV","AHSV","RVFV",
                 "PPRV","SPPV",	"CSFV",	"RPV",	"CBPP",	"CCPP",	"Bmallei",
                 "FMDV",	"LSDV",	"HPAI",	"NDV",
                 "AHFV","AINOV","AKAV",
                 "BHAV", "BTV", "BEFV",
                 "CVV", "CCHFV","EEEV","EEV",
                 "Eruminantium","EHDV","GETV",
                 "hepat","HJV", "JEV",
                 "KOTV", "Linfantum", "MDV",
                 "MIDV", "NSDV",
                 "KASV", "PHSV", "SBV",
                 "Semliki", "SHUV", "SLEV",
                 "THOV", "VEED", "VSV",
                 "YUOV", "WSLV","WNFV", "WEEV"
)

agent.types <- c("v","v","v","v","v","v","v","b","b","b","v","v","v","v",
                 "v","v","v","v","v","v","v","v","v","v",
                 "b","v","v","p","v","v","v",
                 "p","v","v","v","v","v","v","v","v","v",
                 "v","v","v","v","v","v","v")

oie.list <- c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,
              F,F,F,F,T,F,F,T,T,T,F,T,F,F,F,T,F,
              T,F,F,T,F,F,T,F,F,F,F,T,T,F,F,T,T)

agent.subtypes=c("agentSubtype",
                 "agentSubtype",NA,
                 NA,# PPR - 1 serotype, 4 lineages
                 NA, #pox
                 NA, #CSF one serotype divided into three major genotypes, NOT ON OUR DATA
                 NA,#rinderpest single serotype
                 NA, #CBPP
                 NA, #CCPP
                 NA, #Glanders
                 NA, #FMD -  A, O, C, SAT1, SAT2, SAT3, and Asia1
                 NA, #LSD --
                 NA, #HPAI - HN
                 NA,  #Newcastle - There are ten
                 # serotypes of avian paramyxoviruses designated APMV-I to APMV-10 and ND virus (NDV) has been
                 # designated APMV-1. NDV has also been categorised into five pathotypes based on clinical signs in
                 # infected chickens, designated: a) viscerotropic velogenic, b) neurotropic velogenic, c) mesogenic,
                 # d) lentogenic or respiratory and e) aymptomatic. Pathotype groupings are rarely clear-cut.
                 NA,NA,NA,NA,
                 "agentSubtype",#"Bluetongue virus" #a=19
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                 NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)

agent.folder.names = agents.short

ticks.dz <- c(1,15,18,19,22,25,28,35,42,44)
midges.dz <- c(2,16,17,20,24,26,31,33,36,38,40,44)
mosquitoes.dz <- c(3,16,17,21,23,27,29,30,34,37,40,41,43,44,45,46,47,48)
sandflies.dz <- c(32)

input.files = c("1impact.Rmd",
                "2transmission_Virus.Rmd",
                "3survival.Rmd",
                "4diagnosis.Rmd",
                "5treatment.Rmd",
                "6vaccines.Rmd")
#input.files=input.files[4]



output.files = c("Impact-",
                 "Transmission-",
                 "Survival-",
                 "Diagnostic-",
                 "Treatment-",
                 "Vaccines-")
#output.files=output.files[4]



