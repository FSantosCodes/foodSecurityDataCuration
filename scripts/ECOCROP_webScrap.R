# ROUTINE FOR WEBSCRAP FUNIBER DATABASE #

# Author: Fabián Santos
# Date: 06/03/2023
# Version: 1.0
# Require: ECOCROP data links (https://ecocrop.review.fao.org)
# Licence: GPL (>= 3)
# URL: https://github.com/FSantosCodes/foodSecurityDataCuration

#### R ENVIRONMENT ####

library(RSelenium)
library(netstat)
library(xml2)
library(xlsx)
library(tidyr)
library(reshape2)
library(XML)

#output folder to save scrapped data
output_folder <- "E:/DATA/webScrap/IEEE/data/ECOCROP"
#curated crop list from ESPAC data
#crop.base <- read.xlsx("F:/seguridadAlimentaria/cropsBase/crop_dictionary_9_consolidacion.xlsx",2)
crop.base <- read.xlsx("E:/DATA/webScrap/IEEE/data/ESPAC/cropsList/cropsList.xlsx",1)
#ECOCROP links
link.search <- paste0('https://ecocrop.review.fao.org/ecocrop/srv/en/cropList?name=',letters,'&relation=beginsWith')

#### INITIATE PROCESSING ####

#prepare server
#chromever: (105.0.5195.19),(105.0.5195.52),(106.0.5249.21),(106.0.5249.61),(107.0.5304.18),(110.0.5481.77),(111.0.5563.19),(111.0.5563.41)
rs.object <- rsDriver(browser = "chrome",
               chromever = "105.0.5195.19",
               port=free_port())
#if do not work above with chrome, test with firefox
#rs.object <- rsDriver(browser = "firefox",
#                      chromever= "105.0.5195.19", 
#                      port=free_port())
#proceed
rs.cli <- rs.object$client
#manipulate
rs.cli$maxWindowSize()
#get codes
table.ls <- list()
for(i in 1:length(link.search)){
  rs.cli$navigate(link.search[i])
  Sys.sleep(1)
  table.i <- htmlParse(rs.cli$getPageSource()[[1]])
  table.i <- readHTMLTable(table.i)
  table.ls[[i]] <- table.i
  print(i)
}
table.ls <- lapply(table.ls,function(x){
  x <- x[[1]]
})
table.ls <- do.call("rbind.data.frame",table.ls)
names(table.ls) <- table.ls[1,]
table.ls <- table.ls[2:3461,]
#save
saveRDS(table.ls,paste0(output_folder,"/tableCodes.rds"))

#### ECOCROP WEBSCRAP ####

#filter crops dictionary
crop.base <- crop.base[,c("ESPAC_code","class","name_spa","name_eng","name_cie")]
#match crops
code.ls <- list()
for(i in 1:nrow(crop.base)){
  pos.i <- grep(crop.base[i,5],table.ls$Name)
  crop.i <- table.ls[pos.i,]
  if(nrow(crop.i)>=2){
    crop.i <- crop.i[1,]
  }else if(nrow(crop.i)==0){
    crop.i <- data.frame(Name="",Code="",Operation="")
  }
  crop.i <- cbind(crop.base[i,],crop.i)
  code.ls[[i]] <- crop.i
}
code.ls <- do.call("rbind.data.frame",code.ls)
code.ls$Operation <- NULL
#prepare links
code.links <- code.ls[!is.na(code.ls$Code),]
dataSheet.ls <- list()
for(i in 1:nrow(code.links)){
  code.links.i <- code.links[i,]
  rs.cli$navigate(paste0("https://ecocrop.review.fao.org/ecocrop/srv/en/dataSheet?id=",code.links.i$Code))
  Sys.sleep(1)
  table.i <- htmlParse(rs.cli$getPageSource()[[1]])
  table.i <- readHTMLTable(table.i)
  table.i[[7]] <- code.links.i
  dataSheet.ls[[i]] <- table.i
  print(i)
}
#save
saveRDS(dataSheet.ls,paste0(output_folder,"/ECOCROP_dataSheet.rds"))

#### POST-PROCESSING ####

#process datasheets
crops.database <- list()
for(i in 1:length(dataSheet.ls)){
  data.i <- dataSheet.ls[[i]]
  #1
  form.i <- data.i[[1]][2,2]
  physiology.i <- data.i[[1]][2,4]
  habit.i <- data.i[[1]][3,2]
  category.i <- data.i[[1]][3,4]
  lifeSpan.i <- data.i[[1]][4,2]
  attributes.i <- data.i[[1]][4,4]
  #2
  tempOptMin.i <- data.i[[2]][4,2]
  tempOptMax.i <- data.i[[2]][4,3]
  rainOptMin.i <- data.i[[2]][5,2]
  rainOptMax.i <- data.i[[2]][5,3]
  soilPHOptMin.i <- data.i[[2]][8,2]
  soilPHOptMax.i <- data.i[[2]][8,3]
  lightOptMin.i <- data.i[[2]][9,2]
  lightOptMax.i <- data.i[[2]][9,3]
  soilDephtOpt.i <- data.i[[2]][3,7]
  soilTextOpt.i <- data.i[[2]][4,7]
  soilFertOpt.i <- data.i[[2]][5,7]
  soilSalOpt.i <- data.i[[2]][7,7]
  soilDrainOpt.i <- data.i[[2]][8,7]
  #3
  climate.i <- data.i[[3]][1,2]
  photoperiod.i <- data.i[[3]][1,4]
  killTempEarly.i <- data.i[[3]][2,4]
  #4
  cropcycleMin.i <- data.i[[4]][3,4]
  cropcycleMax.i <- data.i[[4]][3,5]
  #6
  uses.i <- paste(unique(data.i[[6]][3:nrow(data.i[[6]]),1]),collapse=", ")
  #create data frame
  data.i <- data.frame(ESPAC_code=data.i[[7]][,"ESPAC_code"],
                       ECOCROP_code=data.i[[7]][,"Code"],
                       class=data.i[[7]][,"class"],
                       name_spa=data.i[[7]][,"name_spa"],
                       name_eng=data.i[[7]][,"name_eng"],
                       name_cie=data.i[[7]][,"name_cie"],
                       lifeForm=form.i,
                       physiology=physiology.i,
                       habit=habit.i,
                       category=category.i,
                       lifeSpan=lifeSpan.i,
                       attributes=attributes.i,
                       uses=uses.i,
                       tempMin=tempOptMin.i,
                       tempMax=tempOptMax.i,
                       rainMin=rainOptMin.i,
                       rainMax=rainOptMax.i,
                       soilPHMin=soilPHOptMin.i,
                       soilPHMax=soilPHOptMax.i,
                       lightMin=lightOptMin.i,
                       lightMax=lightOptMax.i,
                       soilDepht=soilDephtOpt.i,
                       soilTex=soilTextOpt.i,
                       soilFert=soilFertOpt.i,
                       soilSalin=soilSalOpt.i,
                       soilDrain=soilDrainOpt.i,
                       climate=climate.i,
                       photoperiod=photoperiod.i,
                       killTempEarly=killTempEarly.i,
                       cropcycleMin=cropcycleMin.i,
                       cropcycleMax=cropcycleMax.i
  )
  crops.database[[i]] <- data.i
}
crops.database <- do.call("rbind.data.frame",crops.database)
#save
saveRDS(crops.database,paste0(output_folder,"/ECOCROP_cropDatabase.rds"))
#end
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)