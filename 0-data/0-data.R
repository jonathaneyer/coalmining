library(tidyverse)
library(readr)
library(dplyr)
library(magrittr)


coalreceipts <- readRDS("0-data/coalreceipts.rds")

# aggregate shipments
coalreceipts_county_year <- coalreceipts %>% 
  group_by(YEAR, coalminefips,Plant.Id) %>%
  summarize( x = sum(QUANTITY))

coalreceipts_mine_year <- coalreceipts %>% 
  filter(!is.na(Coalmine.Msha.Id)) %>%
  group_by(YEAR, Coalmine.Msha.Id,Plant.Id) %>%
  summarize( x = sum(QUANTITY))

# load msha locations
coaldata <- read_delim("C:/Users/Jonathan/Downloads/Mines/Mines.txt",delim = "|") %>%
  mutate(MINE_ID = as.numeric(as.character(MINE_ID))) %>%
  select(MINE_ID, LATITUDE, LONGITUDE, CURRENT_MINE_TYPE,NO_EMPLOYEES,AVG_MINE_HEIGHT)


# load plant locations
plant2012 <- read_csv("C:/Users/Jonathan/Google Drive/coalmining/data/plant2012.csv") %>% 
  filter(!is.na(Latitude), Latitude != 0) %>%
  select(`Utility ID`,`Plant Code`,Latitude,Longitude) 
plant2012 <- setNames(plant2012,make.names(names(plant2012), unique = T))

# create matrix of plant-mining counties
opts_minecounty <- expand.grid(unique(coalreceipts_county_year$YEAR), unique(coalreceipts_county_year$coalminefips),
                               unique(coalreceipts_county_year$Plant.Id)) %>% 
  inner_join(plant2012,by = c("Var3" = "Plant.Code")) %>%
  left_join( read.delim("C:/Users/Jonathan/Google Drive/coalmining/data/Gaz_counties_national.txt") %>%
               select(GEOID,INTPTLAT,INTPTLONG), by = c("Var2"="GEOID"))

opts_mine <- expand.grid(unique(coalreceipts_mine_year$YEAR), unique(coalreceipts_mine_year$Coalmine.Msha.Id),
                         unique(coalreceipts_mine_year$Plant.Id)) %>% 
  inner_join(plant2012,by = c("Var3" = "Plant.Code")) %>%
  left_join(coaldata, by = c("Var2" = "MINE_ID"))


# recode international locations to lat/long
opts_minecounty$INTPTLAT[opts_minecounty$Var2 == 1000000] <- -25.274398 
opts_minecounty$INTPTLONG[opts_minecounty$Var2 == 1000000] <- 133.775136 
opts_minecounty$INTPTLAT[opts_minecounty$Var2 == 2000000] <- 4.570868 
opts_minecounty$INTPTLONG[opts_minecounty$Var2 == 2000000] <- 	-74.297333 
opts_minecounty$INTPTLAT[opts_minecounty$Var2 == 3000000] <- 56.130366 	
opts_minecounty$INTPTLONG[opts_minecounty$Var2 == 3000000] <- 	-106.346771 
opts_minecounty$INTPTLAT[opts_minecounty$Var2 == 4000000] <-  	-0.789275 	
opts_minecounty$INTPTLONG[opts_minecounty$Var2 == 4000000] <- 	113.921327
opts_minecounty$INTPTLAT[opts_minecounty$Var2 == 5000000] <- -0.789275  
opts_minecounty$INTPTLONG[opts_minecounty$Var2 == 5000000] <- 	113.921327
opts_minecounty$INTPTLAT[opts_minecounty$Var2 == 6000000] <- 61.52401 	
opts_minecounty$INTPTLONG[opts_minecounty$Var2 == 6000000] <-  105.318756 
opts_minecounty$INTPTLAT[opts_minecounty$Var2 == 7000000] <- 55.378051 	
opts_minecounty$INTPTLONG[opts_minecounty$Var2 == 7000000] <- 	-3.435973
opts_minecounty$INTPTLAT[opts_minecounty$Var2 == 8000000] <- 6.42375 	
opts_minecounty$INTPTLONG[opts_minecounty$Var2 == 8000000] <- -66.58973

opts_mine <- opts_mine %>% 
  filter(!is.na(LATITUDE))

opts_minecounty <- opts_minecounty %>% 
  filter(!is.na(INTPTLAT))

source('C:/Users/Jonathan/Google Drive/coalmining/coalmining_git/0-data/0-functions.R')

opts_minecounty$minecongress <- latlong2congress(data.frame(x= opts_minecounty$INTPTLONG, y =opts_minecounty$INTPTLAT))
opts_minecounty$plantcongress <- latlong2congress(data.frame(x= opts_minecounty$Longitude, y =opts_minecounty$Latitude))
opts_minecounty$minestate <- latlong2state(data.frame(x= opts_minecounty$INTPTLONG, y =opts_minecounty$INTPTLAT))
opts_minecounty$plantstate <- latlong2state(data.frame(x= opts_minecounty$Longitude, y =opts_minecounty$Latitude))
opts_minecounty$minecounty <- latlong2county(data.frame(x= opts_minecounty$INTPTLONG, y =opts_minecounty$INTPTLAT))
opts_minecounty$plantcounty <- latlong2county(data.frame(x= opts_minecounty$Longitude, y =opts_minecounty$Latitude))

### fix some missing intersections
opts_minecounty$minestate[opts_minecounty$Var2 >57000] <- "Foreign"
opts_minecounty$minecounty[opts_minecounty$Var2 >57000] <- "Foreign"
opts_minecounty$minecongress[opts_minecounty$Var2 >57000] <- "Foreign"
opts_minecounty$plantstate[opts_minecounty$Var3 == 10675] <- "connecticut"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 10675] <- "new london,connecticut"
opts_minecounty$plantcongress[opts_minecounty$Var3 == 10675] <- "0902"
opts_minecounty$plantstate[opts_minecounty$Var3 == 643] <- "florida"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 643] <- "bay,florida"
opts_minecounty$plantstate[opts_minecounty$Var3 == 646] <- "florida"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 646] <- "hillsborough,florida"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1554] <- "maryland"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1554] <- "anne arundel,maryland"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1626] <- "massachussets"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1626] <- "essex,massachussets"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1702] <- "michigan"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1702] <- "bay,michigan"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1702] <- "michigan"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1702] <- "bay,michigan"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1732] <- "michigan"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1732] <- "bay,michigan"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1733] <- "michigan"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1733] <- "monroe,michigan"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1740] <- "michigan"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1740] <- "wayne,michigan"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1743] <- "michigan"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1743] <- "st. clair,michigan"
opts_minecounty$plantstate[opts_minecounty$Var3 == 6034] <- "michigan"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 6034] <- "st. clair,michigan"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1825] <- "michigan"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1825] <- "ottawa,michigan"
opts_minecounty$plantcongress[opts_minecounty$Var3 == 1843] <- "2601"
opts_minecounty$plantstate[opts_minecounty$Var3 == 1897] <- "minnesota"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 1897] <- "st. louis,michigan"
opts_minecounty$plantstate[opts_minecounty$Var3 == 2554] <- "new york"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 2554] <- "chautauqua,new york"
opts_minecounty$plantcongress[opts_minecounty$Var3 == 2554] <- "3623"
opts_minecounty$plantstate[opts_minecounty$Var3 == 2642] <- "new york"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 2642] <- "monroe,new york"
opts_minecounty$plantstate[opts_minecounty$Var3 == 6082] <- "new york"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 6082] <- "niagara,new york"
opts_minecounty$plantstate[opts_minecounty$Var3 == 2837] <- "ohio"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 2837] <- "lake,ohio"
opts_minecounty$plantstate[opts_minecounty$Var3 == 2857] <- "ohio"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 2857] <- "lorain,ohio"
opts_minecounty$plantstate[opts_minecounty$Var3 == 3809] <- "virginia"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 3809] <- "york,virginia"
opts_minecounty$plantstate[opts_minecounty$Var3 == 3920] <- "washiington"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 3920] <- "pierce,washington"
opts_minecounty$plantcongress[opts_minecounty$Var3 == 3982] <- '5507'
opts_minecounty$plantcongress[opts_minecounty$Var3 == 3983] <- '5507'
opts_minecounty$plantcongress[opts_minecounty$Var3 == 4040] <- '5506'
opts_minecounty$plantstate[opts_minecounty$Var3 == 4072] <- "wisconsin"
opts_minecounty$plantcounty[opts_minecounty$Var3 == 4072] <- "brown,wisconsin"

opts_mine$minecongress <- latlong2congress(data.frame(x= opts_mine$LONGITUDE, y =opts_mine$LATITUDE))
opts_mine$plantcongress <- latlong2congress(data.frame(x= opts_mine$Longitude, y =opts_mine$Latitude))
opts_mine$minestate <- latlong2state(data.frame(x= opts_mine$LONGITUDE, y =opts_mine$LATITUDE))
opts_mine$plantstate <- latlong2state(data.frame(x= opts_mine$Longitude, y =opts_mine$Latitude))
opts_mine$minecounty <- latlong2county(data.frame(x= opts_mine$LONGITUDE, y =opts_mine$LATITUDE))
opts_mine$plantcounty <- latlong2county(data.frame(x= opts_mine$Longitude, y =opts_mine$Latitude))

### fix some missing intersections
opts_mine$minestate[opts_mine$Var2 >57000] <- "Foreign"
opts_mine$minecounty[opts_mine$Var2 >57000] <- "Foreign"
opts_mine$minecongress[opts_mine$Var2 >57000] <- "Foreign"
opts_mine$plantstate[opts_mine$Var3 == 10675] <- "connecticut"
opts_mine$plantcounty[opts_mine$Var3 == 10675] <- "new london,connecticut"
opts_mine$plantcongress[opts_mine$Var3 == 10675] <- "0902"
opts_mine$plantstate[opts_mine$Var3 == 643] <- "florida"
opts_mine$plantcounty[opts_mine$Var3 == 643] <- "bay,florida"
opts_mine$plantstate[opts_mine$Var3 == 646] <- "florida"
opts_mine$plantcounty[opts_mine$Var3 == 646] <- "hillsborough,florida"
opts_mine$plantstate[opts_mine$Var3 == 1554] <- "maryland"
opts_mine$plantcounty[opts_mine$Var3 == 1554] <- "anne arundel,maryland"
opts_mine$plantstate[opts_mine$Var3 == 1626] <- "massachussets"
opts_mine$plantcounty[opts_mine$Var3 == 1626] <- "essex,massachussets"
opts_mine$plantstate[opts_mine$Var3 == 1702] <- "michigan"
opts_mine$plantcounty[opts_mine$Var3 == 1702] <- "bay,michigan"
opts_mine$plantstate[opts_mine$Var3 == 1702] <- "michigan"
opts_mine$plantcounty[opts_mine$Var3 == 1702] <- "bay,michigan"
opts_mine$plantstate[opts_mine$Var3 == 1732] <- "michigan"
opts_mine$plantcounty[opts_mine$Var3 == 1732] <- "bay,michigan"
opts_mine$plantstate[opts_mine$Var3 == 1733] <- "michigan"
opts_mine$plantcounty[opts_mine$Var3 == 1733] <- "monroe,michigan"
opts_mine$plantstate[opts_mine$Var3 == 1740] <- "michigan"
opts_mine$plantcounty[opts_mine$Var3 == 1740] <- "wayne,michigan"
opts_mine$plantstate[opts_mine$Var3 == 1743] <- "michigan"
opts_mine$plantcounty[opts_mine$Var3 == 1743] <- "st. clair,michigan"
opts_mine$plantstate[opts_mine$Var3 == 6034] <- "michigan"
opts_mine$plantcounty[opts_mine$Var3 == 6034] <- "st. clair,michigan"
opts_mine$plantstate[opts_mine$Var3 == 1825] <- "michigan"
opts_mine$plantcounty[opts_mine$Var3 == 1825] <- "ottawa,michigan"
opts_mine$plantcongress[opts_mine$Var3 == 1843] <- "2601"
opts_mine$plantstate[opts_mine$Var3 == 1897] <- "minnesota"
opts_mine$plantcounty[opts_mine$Var3 == 1897] <- "st. louis,michigan"
opts_mine$plantstate[opts_mine$Var3 == 2554] <- "new york"
opts_mine$plantcounty[opts_mine$Var3 == 2554] <- "chautauqua,new york"
opts_mine$plantcongress[opts_mine$Var3 == 2554] <- "3623"
opts_mine$plantstate[opts_mine$Var3 == 2642] <- "new york"
opts_mine$plantcounty[opts_mine$Var3 == 2642] <- "monroe,new york"
opts_mine$plantstate[opts_mine$Var3 == 6082] <- "new york"
opts_mine$plantcounty[opts_mine$Var3 == 6082] <- "niagara,new york"
opts_mine$plantstate[opts_mine$Var3 == 2837] <- "ohio"
opts_mine$plantcounty[opts_mine$Var3 == 2837] <- "lake,ohio"
opts_mine$plantstate[opts_mine$Var3 == 2857] <- "ohio"
opts_mine$plantcounty[opts_mine$Var3 == 2857] <- "lorain,ohio"
opts_mine$plantstate[opts_mine$Var3 == 3809] <- "virginia"
opts_mine$plantcounty[opts_mine$Var3 == 3809] <- "york,virginia"
opts_mine$plantstate[opts_mine$Var3 == 3920] <- "washiington"
opts_mine$plantcounty[opts_mine$Var3 == 3920] <- "pierce,washington"
opts_mine$plantcongress[opts_mine$Var3 == 3982] <- '5507'
opts_mine$plantcongress[opts_mine$Var3 == 3983] <- '5507'
opts_mine$plantcongress[opts_mine$Var3 == 4040] <- '5506'
opts_mine$plantstate[opts_mine$Var3 == 4072] <- "wisconsin"
opts_mine$plantcounty[opts_mine$Var3 == 4072] <- "brown,wisconsin"

library(fields)
opts_minecounty$distances <- rdist.earth.vec(as.matrix(
  cbind(opts_minecounty$INTPTLONG, opts_minecounty$INTPTLAT))
  ,as.matrix(cbind(opts_minecounty$Longitude, opts_minecounty$Latitude)), miles = T)

opts_mine$distances <- rdist.earth.vec(as.matrix(
  cbind(opts_mine$LONGITUDE, opts_mine$LATITUDE))
  ,as.matrix(cbind(opts_mine$Longitude, opts_mine$Latitude)), miles = T)

opts_minecounty <- opts_minecounty %>%
  left_join(coalreceipts_county_year, by = c("Var1"="YEAR",
                                             "Var2" = "coalminefips", "Var3" ="Plant.Id") ) %>%
  replace_na(list(x = 0)) %>%
  mutate(samestate = minestate == plantstate, samecounty = minecounty == plantcounty,
         samecongress = minecongress == plantcongress) 


opts_mine <- opts_mine %>%
  left_join(coalreceipts_mine_year, by = c("Var1"="YEAR",
                                           "Var2" = "Coalmine.Msha.Id", "Var3" ="Plant.Id") ) %>%
  replace_na(list(x = 0)) %>%
  mutate(samestate = minestate == plantstate, samecounty = minecounty == plantcounty,
         samecongress = minecongress == plantcongress) 

coalmine <- opts_mine %>%  group_by(Var1, Var2) %>% 
  summarize( mineprod = sum(x))

coalcounty <- opts_minecounty %>%  group_by(Var1, Var2) %>% 
  summarize( countyprod = sum(x))

plantmine <- opts_mine %>% group_by(Var1,Var3) %>% 
  summarize(plantpurch = sum(x))

plantcounty <- opts_minecounty %>% group_by(Var1,Var3) %>% 
  summarize(plantpurch_county = sum(x))

opts_mine <- opts_mine %>% 
  left_join(coalmine, by = c("Var1" = "Var1", "Var2"= "Var2")) %>%
  left_join(plantmine, by = c("Var1" = "Var1","Var3"="Var3")) %>% 
  filter(mineprod >0, plantpurch > 0) %>%
  mutate(anytrade = x > 0, mineprod = mineprod/1000000, plantpurch = plantpurch/1000000)


opts_minecounty <- opts_minecounty %>% 
  left_join(coalcounty, by = c("Var1" = "Var1", "Var2" = "Var2")) %>%
  left_join(plantcounty, by = c("Var1"= "Var1","Var3" = "Var3")) %>%
  filter(countyprod > 0, plantpurch_county > 0) %>%
  mutate(anytrade = x > 0, countyprod = countyprod/1000000, plantpurch_county = plantpurch_county/1000000)


write_rds(opts_mine,path =  "C:/Users/Jonathan/Google Drive/coalmining/coalmining_git/0-data/opts_mine.rds")
write_rds(opts_minecounty,path =  "C:/Users/Jonathan/Google Drive/coalmining/coalmining_git/0-data/opts_minecounty.rds")
