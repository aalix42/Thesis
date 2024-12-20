
library(dplyr)
library(terra)
library(ggplot2)
library(lubridate)
library(sf)
library(sp)
library(ras)

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

date_5 <- character()
for(i in 1:length(dirs)){
  date_5[i] <- strsplit(fileB6[i],"_")[[1]][4]
}

plot(ST_5[[4]])

date_L5 <- ymd(date_5)

ST_5[[1]]
#next thing, multiply the scale factor and additive offset. Apply function to list of rasters. Can't just do single calc

LandsatFunction <- function(x){
  ((x)*0.00341802)+149
}

L5Calc <- list()
for(i in 1:length(dirs)){
  L5Calc[[i]] <- (((ST_5[[i]])*0.00341802)+149) - 273.15
}

plot(L5Calc[[1]])



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

L8Calc <- list()
for(i in 1: length (dirs8)){
  L8Calc[[i]] <- (LandsatFunction(ST_Files8[[i]])) - 273.15
}
plot(L8Calc[[1]])



ST_mask8 <- list()
for(i in 1:length(dirs8)){
  ST_mask8[[i]] <- ifel(rast(paste0(dirs8[i], "/", fileQA_8[i])) == 21824, 
                       1, 
                       NA)
}


plot(ST_mask8[[2]])

ST_8 <- list()
for(i in 1:length(dirs8)){
  ST_8[[i]] <- mask(L8Calc[[i]], ST_mask8[[i]])
}

ST_5 <- list()
for(i in 1: length(dirs)) {
  ST_5[[i]] <- mask(L5Calc[[i]], ST_mask[[i]])
}

##mask = 2720
date_8 <- character()
for(i in 1:length(dirs8)){
  date_8[i] <- strsplit(fileB10_8[i],"_")[[1]][4]
}

date_L5 <- as.numeric(ymd(date_5))

##read in NLCD data 
NLCD2001 <- rast("F:/Thesis/NLCD/NLCD_2001_Land_Cover_L48_20210604_srXvgfE7CJBW6GEcHH5B.tiff")
NLCD2006 <- rast("F:/Thesis/NLCD/NLCD_2006_Land_Cover_L48_20210604_srXvgfE7CJBW6GEcHH5B.tiff")
NLCD2011 <- rast("F:/Thesis/NLCD/NLCD_2011_Land_Cover_L48_20210604_srXvgfE7CJBW6GEcHH5B.tiff")
NLCD2016 <- rast("F:/Thesis/NLCD/NLCD_2016_Land_Cover_L48_20210604_srXvgfE7CJBW6GEcHH5B.tiff")
NLCD2021 <- rast("F:/Thesis/NLCD/NLCD_2021_Land_Cover_L48_20230630_srXvgfE7CJBW6GEcHH5B.tiff")

Landsat_average <- numeric()

#the goal here is to access the date from each file! 
#for(i in 1:length(dirs)){
#  date_L5[i] <- strsplit(fileB10_8[i],"_")[[1]][4]
#}

#print(date_L5[i])

NationalUrbanArea <- st_read("F:/Thesis/tl_2020_us_uac20/tl_2020_us_uac20.shp")

UticaBoundary <- NationalUrbanArea[NationalUrbanArea$NAME20 == "Utica, NY",]
##plot(UticaBoundary)
##data model for SF doesn't work well with terra. Conversion is necessary. 

usp <- as(UticaBoundary, "Spatial")

#converted the sf format into terra 
##apply mask with for loop format!! 


UticaBoundaryT <- vect(usp)
plot(UticaBoundaryT)

crs(ST_5[[1]])
##reprojecting 
UticaWGS84 <- project(UticaBoundaryT,
                      ST_5[[i]])


##BoundedST5 <- list()
#for(i in 1: length(dirs)) {
#  BoundedST5[[i]] <- crop(ST_5[[i]], UticaWGS84)
#}


##crs(ST_5[[1]])
##reprojecting 
UticaWGS84 <- project(UticaBoundaryT,
                              ST_5[[i]])

(ST_5[[2]])
crs(UticaWGS84)
crs(ST_5[[1]])

UticaWGS84
ST_5[[1]]
#####################################
plot(ST_5[[1]])



plot(UticaWGS84)
plot(L5Calc[[1]])

plot(ST_5[[1]])

ST_5Cropped <- list()
for(i in 1:length(dirs)){
  ST_5Cropped[[i]] <- mask(L5Calc[[i]], UticaWGS84)
}

plot(ST_5Cropped[[3]])

##note to self: crop then mask 
ST_8Cropped1 <- list()
for(i in 1:length(dirs8)){
  ST_8Cropped1[[i]] <- crop(ST_8[[i]], UticaWGS84)
}

ST_8Cropped <- list()
for(i in 1:length(dirs8)){
  ST_8Cropped[[i]] <- mask(ST_8Cropped1[[i]], UticaWGS84)
}


print()

#weird extent! 
plot(ST_8Cropped[[1]])


#plot(ST_5Cropped[[2]])
#plot(ST_5Cropped[[3]])
anamoly <- function(x,y){
  mean(masked)
}

plot(ST_8Cropped[[1]])

##averaging the correlating years: 
Landsat2016 <- (ST_8Cropped[[1]] + ST_8Cropped[[2]])/2

ST_8Cropped[[1]]
crs(UticaWGS84)
ST_8Cropped[[2]]

##weird extent

##creating a reference grid so I can get rid of the "extents don't match" error. 
reference_grid <- raster(extent(ST_8), 
                         resolution = c(30, 30), 
                         crs = crs(ST_8[[1]]))  

extent_bounds <- ext(344000, 578000, 661985,4899915)
reference_raster <- rast(ext=extent_bounds, resolution=30, crs="EPSG:4326")

alignedST8 <- list()
for(i in 1: length(dirs8)){
  alignedST8[[i]] <- resample(ST_8Cropped[[i]], reference_raster, method="bilinear")
}

#averaging the years together 
Landsat2016 <- (ST_8Cropped[[1]] + ST_8Cropped[[2]])/2
Landsat2021 <- (ST_8Cropped[[3]] + ST_8Cropped[[4]])/2
plot(Landsat2016) #error, won't plot! 

alignedST5 <- list()
for(i in 1: length(dirs)){
  alignedST5[[i]] <- resample(ST_5Cropped[[i]], reference_raster, method="bilinear")
}

Landsat2011 <- alignedST5[[6]]
Landsat2006 <- (alignedST5[[4]] + alignedST5[[5]])/2
Landsat2001 <- (alignedST5[[2]] + alignedST5[[3]])/2



print(alignedST5)
print(ST_5Cropped[1])
 
##with land cover match up, match to match land cover (the second input), change method to near
##descrete data = near. 

#next step, do the same thing with landsat 5!! 
print(ST_5Cropped)

