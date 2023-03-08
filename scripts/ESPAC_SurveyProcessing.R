# ESPAC SURVEY DATA PROCESSING #

# Author: Fabián Santos
# Date: 06/03/2023
# Version: 1.0
# Require: ESPAC data obtained from INEC (see: https://www.ecuadorencifras.gob.ec/estadisticas-agropecuarias-2/)
# Licence: GPL (>= 3)
# URL: https://github.com/FSantosCodes/foodSecurityDataCuration

#### R ENVIRONMENT ####

library(sf)
library(survey)
library(powerjoin)
library(xlsx)

#land use file (SUNAC file as: INEC_sunac_BD_2021.csv)
sunac_file <- "E:/DATA/webScrap/IEEE/data/ESPAC/raw/INEC_sunac_BD_2021.csv"
#prepared dictionary from ESPAC manuals. It is a list with three databases: 1) crops names and codes (required for this analysis);
#2) variables definition and their types; and 3) ESPEC databases prefixes explanation
dictionary_file <- "E:/DATA/webScrap/IEEE/data/ESPAC/dictionary/prepared_dictionary.rds"
#shapefile with administative limits (provinces)
provinces_shp <- "E:/DATA/webScrap/IEEE/data/PROVINCES/nxprovincias.shp"
#output_folder
output_folder <- "E:/DATA/webScrap/IEEE/data/ESPAC/processed"

#### PREPARE INPUT DATA ####

#read map + project
prov.shp <- st_read(provinces_shp)
prov.shp <- st_transform(prov.shp,4326)
#remove zero variance columns
prov.shp$DPA_VALOR <- NULL
#reorder
prov.shp <- prov.shp[,c("DPA_DESPRO","DPA_ANIO","DPA_PROVIN","geometry")]
#all to lower + rename for join
names(prov.shp) <- tolower(names(prov.shp))
var.names <- c("provincia","anio","ual_prov")
names(prov.shp) <- c(var.names,"geometry")
#read dictionary and open variables
dic.vars <- readRDS(dictionary_file)$vars
dic.crops <- readRDS(dictionary_file)$crop

#### PROCESS sunac ####

#read & prepare names
sunac.base <- read.csv2(sunac_file,colClasses = "character",fileEncoding="UTF-8-BOM")
names(sunac.base) <- tolower(names(sunac.base))
#prepare territory codes
sunac.base$ual_parr <- paste0(sunac.base$ual_prov,sunac.base$ual_cant,sunac.base$ual_parr)
sunac.base$ual_cant <- paste0(sunac.base$ual_prov,sunac.base$ual_cant)
#filter variables to operate
vars.numeric <- c("supertotal","fact_exp_fin")
sunac.base <- sunac.base[,c("rc_clacul","ual_prov","ual_cant","ual_parr",vars.numeric)]
#prepare numeric variables and remove weightening 
for(i in 1:length(vars.numeric)){
  pos.i <- grep(vars.numeric[i],names(sunac.base),fixed=T)
  sunac.base[,pos.i] <- as.numeric(gsub(",",".",sunac.base[,pos.i]))
}
#split base by crops codes
sunac.base <- split(sunac.base,sunac.base$rc_clacul)
#function for get estimates
derive.svytotal <- function(x="supertotal",y="ual_prov",z=sunac.survey){
  x.form <- c(as.formula(paste0("~",x)),as.formula(paste0("~",y)))
  x.by <- svyby(x.form[[1]],x.form[[2]],svytotal,na.rm=T,design=z)
  x.by <- as.data.frame(x.by)
  rownames(x.by) <- NULL
  names(x.by)[3] <- paste0("SE_",names(x.by)[2])
  x.data <- x.by[,c(1,2)]
  x.se <- x.by[,c(1,3)]
  x <- list(x.data,x.se)
  return(x)
}
#start processing of crops
sunac.ls <- list()
#update estimation variables
vars.estimate <- vars.numeric[1:length(vars.numeric)-1]
for(i in 1:length(sunac.base)){
  if(nrow(sunac.base[[i]])>=2){
    #process with no fpc (population known size), defining strata
    survey.i <- svydesign(id=~1,weights=~fact_exp_fin,data=sunac.base[[i]])
    #apply to variables
    sunac.i <- list()
    for(j in 1:length(vars.estimate)){
      sunac.i[[j]] <- derive.svytotal(vars.numeric[j],"ual_prov",survey.i)
    }
    #prepare name
    crop.name <- dic.crops[grep(names(sunac.base)[i],dic.crops$code),"name"]
    if(length(crop.name)==0){
      crop.name <- ""
    }
    #merge data
    sunac.estimates <- lapply(sunac.i,"[[",1)
    sunac.estimates <- power_full_join(sunac.estimates, by="ual_prov")
    sunac.estimates$rc_clacul <- names(sunac.base)[i]
    sunac.estimates$crop_name <- crop.name
    sunac.estimates$province <- prov.shp$provincia[prov.shp$ual_prov %in% sunac.estimates$ual_prov]
    #compute statndard errors
    sunac.se <- lapply(sunac.i,"[[",2)
    sunac.se <- lapply(sunac.se,function(x){
      data.frame(mean=mean(x[,2]),SD=sd(x[,2]))
    })
    sunac.se <- do.call("rbind.data.frame",sunac.se)
    sunac.se$rc_clacul <- names(sunac.base)[i]
    sunac.se$crop_name <- crop.name
    sunac.se$variable <- gsub("SE_","",sapply(lapply(lapply(sunac.i,"[[",2),names),"[[",2))
    #finish
    sunac.ls[[i]] <- list(sunac.estimates,sunac.se)
  }else{
    sunac.ls[[i]] <- NA
  }
}
#clean
sunac.ls <- sunac.ls[sapply(sunac.ls,length)==2]
#consolidate estimates
sunac.estimates <- lapply(sunac.ls,"[[",1)
sunac.estimates <- do.call("rbind.data.frame",sunac.estimates)
sunac.estimates[,2] <- round(sunac.estimates[,2],2)
sunac.estimates$class <- as.character(sapply(sunac.estimates$rc_clacul,function(x){
  x <- dic.crops[dic.crops$code %in% x,2]
}))
sunac.estimates$state <- as.character(sapply(sunac.estimates$rc_clacul,function(x){
  x <- dic.crops[dic.crops$code %in% x,6]
}))
sunac.estimates <- sunac.estimates[,c("class","crop_name","state","rc_clacul","province","ual_prov","supertotal")]
sunac.estimates <- sunac.estimates[sunac.estimates$crop_name != "",]
#consolidate se
sunac.se <- lapply(sunac.ls,"[[",2)
sunac.se <- do.call("rbind.data.frame",sunac.se)
sunac.se[,1:2] <- lapply(sunac.se[,1:2],round,2)
sunac.se$class <- as.character(sapply(sunac.se$rc_clacul,function(x){
  x <- dic.crops[dic.crops$code %in% x,2]
}))
sunac.se$state <- as.character(sapply(sunac.se$rc_clacul,function(x){
  x <- dic.crops[dic.crops$code %in% x,2]
}))
sunac.se <- sunac.se[,c("class","crop_name","state","rc_clacul","variable","mean","SD")]
sunac.se <- sunac.se[sunac.se$crop_name != "",]
#save
out.file <- paste0(output_folder,"/sunac_estimates.xlsx")
write.xlsx(sunac.estimates,out.file,"estimates",row.names=F)
write.xlsx(sunac.se,out.file,"standard_error",append=T,row.names=F)

