library(dplyr)
library(terra)
library(ggplot2)
library(lubridate)

dirs <- list.dirs("F:/Thesis/Landsat5")[-1]
dirs
files <- list()
files
fileB6 <- character()
fileQA <- character()
for(i in 1:length(dirs)){
  files[i] <- list.files(dirs[i])
  fileB6[i] <- list.files(dirs[i], pattern = "B6")
  fileQA[i] <- list.files(dirs[i], pattern= "QA_PIXEL")
}
fileB6
fileQA

#every directory is one set set of tiles, found the B6 and the QA pixel and stacked them 
ST_Files <- list()
for(i in 1:length(dirs)){
  ST_Files[[i]] <- c(rast(paste0(dirs[i],"/", fileB6[i]))) 
                    
}
plot(ST_Files[[1]])

ST_mask <- list()
for(i in 1:length(dirs)){
  ST_mask[[i]] <- ifel(rast(paste0(dirs[i], "/", fileQA[i])) == 5440, 
                       1, 
                       NA)
}
plot(ST_mask[[1]])
#https://www.usgs.gov/faqs/how-do-i-use-a-scale-factor-landsat-level-2-science-products

ST_5 <- list()
for(i in 1:length(dirs)){
  ST_5[[i]] <- mask(ST_Files[[i]], ST_mask[[i]])
}
plot(ST_5[[4]])

date_5 <- character()
for(i in 1:length(dirs)){
  date_5[i] <- strsplit(fileB6[i],"_")[[1]][4]
}

date_L5 <- ymd(date_5)

#next thing, multiply the scale factor and additive offset 
L5 <- c(ST_5)


##landsat 8/9
dirs8 <- list.dirs("F:/Thesis/Landsat 8")[-1]
dirs8
files8 <- list()
files8
fileB10_8 <- character()
fileQA_8 <- character()
for(i in 1:length(dirs8)){
  files8[[i]] <- list.files(dirs8[i])
  fileB10_8[i] <- list.files(dirs8[i], pattern = "B10")
  fileQA_8[i] <- list.files(dirs8[i], pattern = "QA_PIXEL")
}
fileB10_8
fileQA_8

ST_Files8 <- list()
for(i in 1:length(dirs8)){
  ST_Files8[[i]] <- c(rast(paste0(dirs8[i],"/", fileB10_8[i]))) 
  
}
plot(ST_Files8[[1]])


ST_mask8 <- list()
for(i in 1:length(dirs8)){
  ST_mask8[[i]] <- ifel(rast(paste0(dirs8[i], "/", fileQA_8[i])) == 2720, 
                       1, 
                       NA)
}
plot(ST_mask[[1]])

ST_8 <- list()
for(i in 1:length(dirs8)){
  ST_8[[i]] <- mask(ST_Files8[[i]], ST_mask8[[i]])
}
#this isn't plotting! 
plot(ST_8[[1]])

##mask = 2720
date_8 <- character()
for(i in 1:length(dirs8)){
  date_8[i] <- strsplit(fileB10_8[i],"_")[[1]][4]
}

date_L5 <- ymd(date_5)

##read in NLCD data 
NLCD2001 <- rast("F:/Thesis/NLCD/NLCD_2001_Land_Cover_L48_20210604_srXvgfE7CJBW6GEcHH5B.tiff")
NLCD2006 <- rast("F:/Thesis/NLCD/NLCD_2006_Land_Cover_L48_20210604_srXvgfE7CJBW6GEcHH5B.tiff")
NLCD2011 <- rast("F:/Thesis/NLCD/NLCD_2011_Land_Cover_L48_20210604_srXvgfE7CJBW6GEcHH5B.tiff")
NLCD2016 <- rast("F:/Thesis/NLCD/NLCD_2016_Land_Cover_L48_20210604_srXvgfE7CJBW6GEcHH5B.tiff")
NLCD2021 <- rast("F:/Thesis/NLCD/NLCD_2021_Land_Cover_L48_20230630_srXvgfE7CJBW6GEcHH5B.tiff")

Landsat_average <- character()
for(i in 1:length(dirs)){
  date_[i] <- strsplit(fileB10_8[i],"_")[[1]][4]
}



