# MIDAS IMPLEMENTATION TO FILL DATABASES #

# Author: Fabián Santos
# Date: 06/03/2023
# Version: 1.0
# Require: database of numeric, character or logical data types (here defined as merged.data)
# Licence: GPL (>= 3)
# URL: https://github.com/FSantosCodes/foodSecurityDataCuration

#### R ENVIRONMENT ####

library(rMIDAS)
set_python_env(x = "rmidas", type = "conda") #if python comes from a CONDA instalation (recommendable)
library(xlsx)
library(tibble)
library(ggcorrplot)
library(Metrics)

#input database to fill (in this case the one previously merge with ECOCROP and FUNIBAR data)
merged.data <- read.xlsx("E:/DATA/webScrap/IEEE/data/MERGED_DATABASES/ECOCROP-FUNIBER_merged.xlsx",1)
#name identifiers columns in merged.base
id.vars <- c("ESPAC_code","ECOCROP_code","class","name_spa","name_eng","name_cie","nfoods")
#identify numeric columns in merged.base
num.vars <- c("tempMin","tempMax","rainMin","rainMax","soilPHMin","soilPHMax","killTempEarly","cropcycleMin","cropcycleMax",
              "CarboHydrates","MonounSaturated","PolyUnsaturated","Saturated","Fiber","Energy","Protein","Fat","Calcium",
              "Iron","Phosphorous","Thiamine","VitaminA","VitaminB2","Pyridoxine","AscorbicAcid","Tocopherol","NicotinicAcid")
#identify a column to use as test (should be numeric)
test.var <- "CarboHydrates"
#output folder
ouput_folder <- "E:/DATA/webScrap/IEEE/data/FILLED_DATABASES"

#### INITATE PROCESSING ####

#extract id vars
id.data <- merged.data[,id.vars,drop=F]
merged.data[,id.vars] <- NULL
#format numeric
merged.data[,num.vars] <- lapply(merged.data[,num.vars],as.numeric)

#this step not required but is useful to ignore some categorical variables as they increase prediction error
#merged.vars$lifeForm <- NULL
#merged.vars$physiology <- NULL
#merged.vars$habit <- NULL
#merged.vars$lifeSpan <- NULL
#merged.vars$soilFert <- NULL
#merged.vars$soilSalin <- NULL
#merged.vars$soilDrain <- NULL
#merged.vars$soilDepht <- NULL
#merged.vars$soilTex <- NULL
#merged.vars$climate <- NULL
#merged.vars$lightMin <- NULL
#merged.vars$lightMax <- NULL
#merged.vars$photoperiod <- NULL
#merged.vars$tempMin <- NULL
#merged.vars$tempMax <- NULL
#merged.vars$rainMin <- NULL
#merged.vars$cropcycleMin <- NULL
#merged.vars$killTempEarly <- NULL

#get categorical and logical variables
cat.vars <- names(merged.data)[sapply(merged.data,is.character)]
#identify categorical variables which has less than 1 class
cat.vars.classes <- sapply(merged.data[cat.vars],function(x)length(unique(x)))
cat.vars.classes <- names(cat.vars.classes[cat.vars.classes<=1])
#remove categorical classes which has less than 1 class
if(length(cat.vars.classes)!=0){
  merged.data[,cat.vars.classes] <- NULL
}
#correlation matrix
corr.plot <- merged.data[,names(merged.data) %in% num.vars]
ggcorrplot(cor(corr.plot[complete.cases(corr.plot),]),lab=T)
#derive test variable
test.data <- merged.data[,test.var]
test.data[sample(1:length(test.data),length(test.data)/2)] <- NA
merged.data$test <- test.data
test.data <- merged.data[,test.var]

#### RUN MIDAS ####

#Apply rMIDAS preprocessing steps
merged.conv <- convert(merged.data, 
                      cat_cols = cat.vars,
                      minmax_scale = TRUE)
#train with the best architecture experimented in the paper but less complex to reduce processing time
merged.train <- train(merged.conv,
                      training_epochs = 100,
                      layer_structure = c(256,256,256),
                      input_drop = 0.9,
                      seed = 668)
#derive 10 complete datasets
merged.complete <- complete(merged.train, m = 10)
#identify the one with minimum RMSE
merged.best <- sapply(merged.complete,function(x){
  x.test <- data.frame(test=x$test,validation=test.data)
  x.test <- x.test[complete.cases(x.test),]
  x.test <- Metrics::rmse(x.test$validation,x.test$test)
  return(x.test)
})
merged.complete <- merged.complete[[which.min(merged.best)]]
#add identifiers
merged.complete <- cbind(id.data,merged.complete)
#save data
out.file <- paste0(ouput_folder,"/filled_databases.xlsx")
write.xlsx(merged.complete,out.file,row.names=F)
