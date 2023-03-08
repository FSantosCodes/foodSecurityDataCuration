# ROUTINE FOR WEBSCRAP FUNIBER DATABASE #

# Author: Fabián Santos
# Date: 06/03/2023
# Version: 1.0
# Require: FUNIBER data from Ecuador (https://www.composicionnutricional.com). Other countries should check their web links. 
# Licence: GPL (>= 3)
# URL: https://github.com/FSantosCodes/foodSecurityDataCuration

#### R ENVIRONMENT ####

library(RSelenium)
library(netstat)
library(xml2)
library(xlsx)
library(tidyr)
library(reshape2)

#output folder to save scrapped data
output_folder <- "E:/DATA/webScrap/IEEE/data/FUNIBER"
#links from FUNIBER-Ecuador
links <- paste0('https://www.composicionnutricional.com/foods/view/EC-',633837:634346)

#### INITIATE PROCESSING ####

#prepare server
#chromever: (105.0.5195.19),(105.0.5195.52),(106.0.5249.21),(106.0.5249.61),(107.0.5304.18),(110.0.5481.77),(111.0.5563.19),(111.0.5563.41)
#rs.object <- rsDriver(browser = "chrome",
#                      chromever = "105.0.5195.19",
#                      port=free_port())
#if do not work above with chrome, test with firefox
rs.object <- rsDriver(browser = "firefox",
                      chromever= "105.0.5195.19", 
                      port=free_port())
#proceed
rs.cli <- rs.object$client
#manipulate and open 
rs.cli$maxWindowSize()

#### FUNIBER WEBSCRAP ####

#prepare loop
data.ls <- list()
#start web scrapping. If something fails, please identify the i iteration number and replace 1 with it (e.g. failed 315, then 315:length(links))
for(i in 1:length(links)){
  rs.cli$navigate(links[i])
  Sys.sleep(3)
  #get table
  table.i <- rs.cli$findElement(using = "css selector", '#body > div.body-content > div.dual-table')$getElementText()
  table.i <- read.table(text=table.i[[1]], sep="\n")
  table.i <- table.i[!(table.i$V1=="NUTRIENTS QUANTITY"|table.i$V1=="NUTRIENTES CANTIDADES"),]
  nutrientes <- sapply(strsplit(table.i," "),"[[",1)
  nutrientes[16] <- "Vit.A(retinol)"
  cantidad <- as.numeric(sapply(strsplit(table.i," "),tail,1))
  table.i <- data.frame(nutrientes=nutrientes,cantidad=cantidad)
  #get title
  title.i <- rs.cli$findElement(using = "css selector", '#body > div.body-content > div.table-options > h1')$getElementText()
  table.i$alimento <- tolower(title.i[[1]])
  #get link
  table.i$link <- links[i]
  data.ls[[i]] <- table.i
  print(i)
}
#reshape
data.df <- lapply(data.ls,function(x){
  x.m <- as.data.frame(t(x[,c("nutrientes","cantidad")]))
  names(x.m) <- x.m[1,]
  x.m <- x.m[2,]
  rownames(x.m) <- NULL
  x.df <- data.frame(alimento=unique(x$alimento))
  x.df <- cbind(x.df,x.m)
  return(x.df)
})
data.df <- do.call("rbind.data.frame",data.df)
#save
saveRDS(data.df,paste0(output_folder,"/FUNIBER_foodsEcuador.rds"))
#close java otherwise will affect performance
gc()
system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
