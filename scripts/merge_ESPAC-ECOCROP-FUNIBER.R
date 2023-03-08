# MERGE FUNIBER AND ECOCROP DATABASES FOR ESPAC CROPS LIST #

# Author: Fabián Santos
# Date: 06/03/2023
# Version: 1.0
# Require: proccesed FUNIBER and ECOCROP crops list derived from ESPAC
# Licence: GPL (>= 3)
# URL: https://github.com/FSantosCodes/foodSecurityDataCuration

#### R ENVIRONMENT ####

library(xlsx)
library(ggplot2)
library(reshape2)
library(caret)
library(stringi)

#input databases
ECOCROP_file <- "E:/DATA/webScrap/IEEE/data/ECOCROP/ECOCROP_cropDatabase.rds"
FUNIBER_file <- "E:/DATA/webScrap/IEEE/data/FUNIBER/FUNIBER_foodsEcuador.rds"
#output folder
output_folder <- "E:/DATA/webScrap/IEEE/data/MERGED_DATABASES"

#### INITATE PROCESSING ####

#read databases
eco.data <- readRDS(ECOCROP_file)
funiber.data <- readRDS(FUNIBER_file)
#start join
match.ls <- list()
for(i in 1:nrow(eco.data)){
  #identify crop name in spanish in ECOCROP
  crop.name.i <- eco.data$name_spa[i]
  crop.name.i <- unlist(strsplit(tolower(crop.name.i),"[(]"))[1]
  crop.name.i <- stri_trans_general(crop.name.i, "Latin-ASCII")
  crop.name.i <- gsub("^ ","",unlist(strsplit(crop.name.i,",")))
  #find it in FUNIBER
  funi.names <- tolower(stri_trans_general(funiber.data$alimento,"Latin-ASCII"))
  funi.names <- gsub("[().]","",funi.names)
  match.i <- lapply(crop.name.i,function(x){
    x <- funiber.data[grep(x,funi.names,ignore.case=T),]
  })
  match.i <- match.i[sapply(match.i,nrow)!=0]
  match.i <- match.i[!duplicated(match.i)]
  match.i <- do.call("rbind.data.frame",match.i)
  #proceed if match or not
  if(nrow(match.i)!=0){
    match.i$name_spa <- eco.data$name_spa[i]
    match.i$ESPAC_code <- eco.data$ESPAC_code[i]
    match.i <- match.i[,c(29,28,1:27)]
    match.ls[[i]] <- match.i
  }else{
    match.i <- data.frame(ESPAC_code=eco.data$ESPAC_code[i],
                          name_spa=eco.data$name_spa[i])
    fill.data <- as.data.frame(matrix(rep(NA,27),1,27))
    names(fill.data) <- c("alimento","Carbohidratos","Monoinsaturados","Poliinsaturados","Saturado","Fibra",
                          "Energía","Proteína","Grasas","Manganeso","Calcio","Magnesio",
                          "Hierro","Potasio","Fosforo","Tiamina","Vit.A(retinol)","VitaminaB2",
                          "AcidoPantotenico","Piridoxina","AcidoAscorbico","Tocoferol","Biotina",
                          "AcidoMalico","AcidoFolico","AcidoOxalico","AcidoNicotinico")
    match.i <- cbind(match.i,fill.data)
    match.ls[[i]] <- match.i
  }
}

match.df <- match.ls[!sapply(match.ls,is.null)]
match.df <- do.call("rbind.data.frame",match.df)
#derive medians from nutrients
crops.means <- split(match.df,match.df$name_spa)
crops.means <- lapply(crops.means,function(x){
  if(all(!is.na(x$alimento))){
    x[,4:29] <- lapply(x[,4:29],as.numeric)
    nfoods <- nrow(x)
    if(nrow(x)==1){
      x.val <- x[,4:29]
    }else{
      x.val <- round(apply(x[,4:29],2,median,na.rm=T),6)
      x.val <- as.data.frame(t(x.val))
    }
    x <- cbind(x[1,1:2],nfoods=nfoods,x.val)
  }else{
    x$nfoods <- 0
    x <- x[,c(1,2,30,4:29)]
  }
  return(x)
})
crops.means <- do.call("rbind.data.frame",crops.means)
rownames(crops.means) <- NULL
#clean
zero.var <- nearZeroVar(crops.means,saveMetrics=T)
crops.means <- crops.means[,!zero.var$zeroVar]
names(crops.means)
names(crops.means) <- c("ESPAC_code","name_spa","nfoods",
                        "CarboHydrates","MonounSaturated","PolyUnsaturated","Saturated","Fiber",
                        "Energy","Protein","Fat","Calcium","Iron","Phosphorous",
                        "Thiamine","VitaminA","VitaminB2","Pyridoxine","AscorbicAcid","Tocopherol","NicotinicAcid")
head(crops.means)
crops.means$name_spa <- NULL
#add ECOCROP DATA
baseMerged <- merge(eco.data,crops.means,by="ESPAC_code")
#prepare base
baseMerged <- lapply(baseMerged,function(x){
  x[x==""] <- NA
  x[x=="no input"] <- NA
  return(x)
})
baseMerged <- do.call("cbind.data.frame",baseMerged)
#save
out.file <- paste0(output_folder,"/ECOCROP-FUNIBER_merged.xlsx")
write.xlsx(baseMerged,out.file,row.names=F)
