require(raster)
library(sp)
library(maps)
library(maptools)
require(data.table)
require(fields)
require(readstata13)
require(gdata)



# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

latlong2congress <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  congr <- shapefile("C:/Users/Jonathan/Google Drive/coalmining/data/spatial/cb_2014_us_cd114_5m/cb_2014_us_cd114_5m.shp")
  IDs <- sapply(strsplit(congr$GEOID, ":"), function(x) x[1])
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, congr)
  # Return the county names of the Polygons object containing each point
  return(indices$GEOID)
  #congrNames <- sapply(congr@polygons, function(x) x@ID)
  #congrNames[indices]
}

latlong2shale <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  shale <- shapefile("C:/Users/Jonathan/Google Drive/coalmining/data/spatial/TightOil_ShaleGas_Plays_Lower48_EIA/TightOil_ShaleGas_US_EIA_Aug2015_v2.shp")
  IDs <- sapply(strsplit(shale$Basin, ":"), function(x) x[1])
  # Convert pointsDF to a SpatialPoints object
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  # Use 'over' to get _indices_ of the Polygons object containing each point
  indices <- over(pointsSP, shale)
  # Return the county names of the Polygons object containing each point
  return(indices$Basin)
  #congrNames <- sapply(congr@polygons, function(x) x@ID)
  #congrNames[indices]
}

fuelreceipts_2014 <- read.csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2014/fuelreceipts_2014.csv")
fuelreceipts_2014$Average.Mercury.Content <- NULL # not in older years of data
fuelreceipts_2013 <- read.csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2013/fuelreceipts_2013.csv")
names(fuelreceipts_2013) <- names(fuelreceipts_2014)
fuelreceipts_2012 <- read.csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2012/fuelreceipts_2012.csv")
names(fuelreceipts_2012) <- names(fuelreceipts_2014)
fuelreceipts_2011 <- read.csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2011/fuelreceipts_2011.csv")
names(fuelreceipts_2011) <- names(fuelreceipts_2014)
fuelreceipts_2010 <- read.csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2010/fuelreceipts_2010.csv")
names(fuelreceipts_2010) <- names(fuelreceipts_2014)
fuelreceipts_2009 <- read.csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2009/fuelreceipts_2009.csv")
fuelreceipts_2009 <- fuelreceipts_2009[,1:27] # get rid of a bunch of NA fields
names(fuelreceipts_2009) <- names(fuelreceipts_2014)
fuelreceipts_2008 <- read.csv("C:/Users/Jonathan/Google Drive/coalmining/data/f923_2008/fuelreceipts_2008.csv")
names(fuelreceipts_2008) <- names(fuelreceipts_2014)


fercreceipts2007 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232007.xls")
fercreceipts2006 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232006.xls")
fercreceipts2005 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232005.xls")
fercreceipts2004 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232004.xls")
fercreceipts2003 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232003.xls")
fercreceipts2002 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232002.xls")
eiareceipts2007 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232007/f4232007.xls")
eiareceipts2006 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232006/f4232006.xls")
eiareceipts2005 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232005/f4232005.xls")
eiareceipts2004 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232004/f4232004.xls")
eiareceipts2003 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232003/e4232003.xls")
eiareceipts2002 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/e4232002/f4232002.xls")
names(eiareceipts2003) <- names(eiareceipts2002)

receipts2001 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232001.xls")
receipts2000 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4232000.xls")
receipts1999 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231999.xls")
receipts1998 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231998.xls")
receipts1997 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231997.xls")
receipts1996 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231996.xls")
receipts1995 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231995.xls")
receipts1994 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231994.xls")
receipts1993 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231993.xls")
receipts1992 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231992.xls")
receipts1991 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231991.xls")
receipts1990 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231990.xls")
# receipts1989 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231989.xls")
# receipts1988 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231988.xls")
# receipts1987 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231987.xls")
# receipts1986 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231986.xls")
# receipts1985 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231986.xls")
# receipts1984 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231984.xls")
# receipts1983 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231983.xls")
# receipts1982 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231982.xls")
# receipts1981 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231981.xls")
# receipts1980 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231980.xls")
# receipts1979 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231979.xls")
# receipts1978 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231978.xls")
# receipts1977 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231977.xls")
# receipts1976 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231976.xls")
# receipts1975 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231975.xls")
# receipts1974 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231974.xls")
# receipts1973 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231973.xls")
# receipts1972 <- read.xls("C:/Users/Jonathan/Google Drive/coalmining/data/old423/f4231972.xls")

fuelreceipts72_89<- rbind(receipts1972,receipts1973,receipts1974,receipts1975,receipts1976,receipts1977,receipts1978,receipts1979,receipts1980,receipts1981,receipts1982,receipts1983, receipts1984, receipts1985, receipts1986, receipts1987,receipts1988,receipts1989 )
fuelreceipts90_98 <- rbind(receipts1990,receipts1991,receipts1992,receipts1993,receipts1994,receipts1995,receipts1996,receipts1997,receipts1998)
fuelreceipts99_01 <- rbind(receipts1999,receipts2000,receipts2001)
fercreceipts02_07 <- rbind(fercreceipts2002,fercreceipts2003,fercreceipts2004,fercreceipts2005, fercreceipts2006, fercreceipts2007)
eiareceipts02_07 <- rbind(eiareceipts2002,eiareceipts2003,eiareceipts2004,eiareceipts2005, eiareceipts2006, eiareceipts2007)
fuelreceipts08_14 <- rbind(fuelreceipts_2008,fuelreceipts_2009,fuelreceipts_2010,fuelreceipts_2011,fuelreceipts_2012,fuelreceipts_2013,fuelreceipts_2014)
eiareceipts02_07$COST <- NA
rm(receipts1972,receipts1973,receipts1974,receipts1975,receipts1976,receipts1977,receipts1978,receipts1979,receipts1980,receipts1981,receipts1982,receipts1983, receipts1984, receipts1985, receipts1986, receipts1987,receipts1988,receipts1989 )
rm(receipts1990,receipts1991,receipts1992,receipts1993,receipts1994,receipts1995,receipts1996,receipts1997,receipts1998)
rm(receipts1999,receipts2000,receipts2001)
rm(fercreceipts2002,fercreceipts2003,fercreceipts2004,fercreceipts2005, fercreceipts2006, fercreceipts2007)
rm(eiareceipts2002,eiareceipts2003,eiareceipts2004,eiareceipts2005, eiareceipts2006, eiareceipts2007)
rm(fuelreceipts_2008,fuelreceipts_2009,fuelreceipts_2010,fuelreceipts_2011,fuelreceipts_2012,fuelreceipts_2013,fuelreceipts_2014)


eiareceipts02_07$FACILITY_STATE <- as.character(eiareceipts02_07$FACILITY_STATE)
eiareceipts02_07$FUEL_TYPE_CODE <- as.character(eiareceipts02_07$FUEL_TYPE_CODE)
eiareceipts02_07$COALMINE_TYPE   <- as.character(eiareceipts02_07$COALMINE_TYPE)
eiareceipts02_07$COALMINE_STATE   <- as.character(eiareceipts02_07$COALMINE_STATE)
eiareceipts02_07$COALMINE_COUNTY   <- as.character(eiareceipts02_07$COALMINE_COUNTY)
fercreceipts02_07$PLT_CODE <- as.character(fercreceipts02_07$PLT_CODE)
fercreceipts02_07$PLST.ST <- as.character(fercreceipts02_07$PLST.ST)
fercreceipts02_07$GENERFUEL <- as.character(fercreceipts02_07$GENERFUEL )
fercreceipts02_07$SPECF_FUEL <- as.character(fercreceipts02_07$SPECF_FUEL )
fercreceipts02_07$ORIG.ST <- as.character(fercreceipts02_07$ORIG.ST )
fercreceipts02_07$MINE_TYPE <- as.character(fercreceipts02_07$MINE_TYPE )
fercreceipts02_07$COUNTY <- as.character(fercreceipts02_07$COUNTY )
fercreceipts02_07$QUANTITY <- as.character(fercreceipts02_07$QUANTITY )
fuelreceipts08_14$Plant.State <- as.character(fuelreceipts08_14$Plant.State)
fuelreceipts08_14$ENERGY_SOURCE <- as.character(fuelreceipts08_14$ENERGY_SOURCE)
fuelreceipts08_14$FUEL_GROUP <- as.character(fuelreceipts08_14$FUEL_GROUP)
fuelreceipts08_14$FUEL_COST <- as.character(fuelreceipts08_14$FUEL_COST)
fuelreceipts08_14$Coalmine.Type <- as.character(fuelreceipts08_14$Coalmine.Type)
fuelreceipts08_14$Coalmine.State <- as.character(fuelreceipts08_14$Coalmine.State)
fuelreceipts08_14$Coalmine.County <- as.character(fuelreceipts08_14$Coalmine.County)
fuelreceipts99_01$GENER_FUEL <- as.character(fuelreceipts99_01$GENER_FUEL)
fuelreceipts99_01$MINE_TYPE <- as.character(fuelreceipts99_01$MINE_TYPE)
fuelreceipts99_01$SPECF_FUEL <- as.character(fuelreceipts99_01$SPECF_FUEL)
fuelreceipts90_98$GENER_FUEL <- as.character(fuelreceipts90_98$GENER_FUEL)
fuelreceipts90_98$SPECF_FUEL <- as.character(fuelreceipts90_98$SPECF_FUEL)
eiareceipts02_07$ENERGY_SOURCE <- 0
eiareceipts02_07$ENERGY_SOURCE[eiareceipts02_07$FUEL_TYPE_CODE %in% c("BIT","SUB","ANT","LIG")] <- 0
fercreceipts02_07$ORIG.ST[fercreceipts02_07$BOM_DIST == 25] <- "PL"
fercreceipts02_07$ORIG.ST[fercreceipts02_07$BOM_DIST == 30] <- "SA"
fercreceipts02_07$ORIG.ST[fercreceipts02_07$BOM_DIST == 35] <- "AU"
fercreceipts02_07$ORIG.ST[fercreceipts02_07$BOM_DIST == 40] <- "CN"
fercreceipts02_07$ORIG.ST[fercreceipts02_07$BOM_DIST == 45] <- "CL"
fercreceipts02_07$ORIG.ST[fercreceipts02_07$BOM_DIST == 50] <- "VZ"
fercreceipts02_07$ORIG.ST[fercreceipts02_07$BOM_DIST == 55] <- "IS"


fuelreceipts08_14 <- fuelreceipts08_14[,c("YEAR","MONTH","Plant.Id","Plant.State","FUEL_GROUP","ENERGY_SOURCE","Coalmine.Type","Coalmine.State","Coalmine.County","QUANTITY","Average.Heat.Content","Average.Sulfur.Content","Average.Ash.Content","FUEL_COST")]
fuelreceipts99_01 <- fuelreceipts99_01[,c("YEAR","MONTH","PLT_CODE","PLT_ST","GENER_FUEL","SPECF_FUEL","MINE_TYPE","ORIG_ST","COUNTY","QUANTITY","BTU","SULFUR","ASH","COST")]
fuelreceipts90_98 <- fuelreceipts90_98[,c("YEAR","MONTH","PLT_CODE","PLT_ST","GENER_FUEL","SPECF_FUEL","MINE_TYPE","ORIG_ST","COUNTY","QUANTITY","BTU","SULFUR","ASH","COST")]
fercreceipts02_07 <- fercreceipts02_07[c("YEAR","MONTH","PLT_CODE","PLST.ST","GENERFUEL","SPECF_FUEL","MINE_TYPE","ORIG.ST","COUNTY","QUANTITY","BTUCONTENT","SULFUR","ASH","COST")]
eiareceipts02_07 <- eiareceipts02_07[c("YEAR","MONTH","FACILITY_CODE","FACILITY_STATE","ENERGY_SOURCE","FUEL_TYPE_CODE","COALMINE_TYPE","COALMINE_STATE","COALMINE_COUNTY","QUANTITY_RECEIVED","BTU_CONTENT","SULFUR_CONTENT","ASH_CONTENT","COST")]



fercreceipts02_07$QUANTITY <- as.numeric(as.character(gsub(",","",x = fercreceipts02_07$QUANTITY)))
eiareceipts02_07$QUANTITY_RECEIVED <- as.numeric(as.character(gsub(",","",x = eiareceipts02_07$QUANTITY_RECEIVED)))
fuelreceipts99_01$QUANTITY <- as.numeric(as.character(gsub(",","",x = fuelreceipts99_01$QUANTITY)))
fuelreceipts90_98$QUANTITY <- as.numeric(as.character(gsub(",","",x = fuelreceipts90_98$QUANTITY)))
fuelreceipts08_14$QUANTITY  <- as.numeric(as.character(gsub(",","",x = fuelreceipts08_14$QUANTITY)))

names(fercreceipts02_07) <- names(fuelreceipts08_14)
names(eiareceipts02_07)  <- names(fuelreceipts08_14)
names(fuelreceipts99_01)  <- names(fuelreceipts08_14)
names(fuelreceipts90_98)  <- names(fuelreceipts08_14)

fuelreceipts <- rbind(fuelreceipts08_14,fercreceipts02_07,eiareceipts02_07,fuelreceipts99_01,fuelreceipts90_98)
rm(fuelreceipts08_14,fercreceipts02_07,eiareceipts02_07,fuelreceipts99_01,fuelreceipts90_98)
coalreceipts <- subset(fuelreceipts,fuelreceipts$ENERGY_SOURCE %in% c("BIT","SUB","LIG","ANT"))
coalreceipts <- subset(coalreceipts,!is.na(coalreceipts$Coalmine.County))
coalreceipts <- subset(coalreceipts,coalreceipts$Coalmine.County != "")
coalreceipts <- subset(coalreceipts, !is.na(coalreceipts$Coalmine.State))

coalreceipts$QUANTITY <- as.numeric(as.character(gsub(pattern = ",","",x = coalreceipts$QUANTITY)))
coalreceipts$coalminefips <- 0
coalreceipts$Coalmine.County <- as.numeric(coalreceipts$Coalmine.County)
coalreceipts <- subset(coalreceipts, !is.na(coalreceipts$Coalmine.County))
coalreceipts$YEAR[coalreceipts$YEAR < 100] <- coalreceipts$YEAR[coalreceipts$YEAR < 100] + 1900
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AL"] <- 1000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "AL"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AK"] <- 2000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "AK"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AZ"] <- 4000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "AZ"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AR"] <- 5000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "AR"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "CO"] <- 8000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "CO"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "IL"] <- 17000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "IL"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "IN"] <- 18000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "IN"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "KS"] <- 20000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "KS"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "KY"] <- 21000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "KY"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "LA"] <- 22000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "LA"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "MD"] <- 24000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "MD"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "MO"] <- 29000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "MO"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "MS"] <- 28000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "MS"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "MT"] <- 30000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "MT"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "ND"] <- 38000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "ND"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "NM"] <- 35000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "NM"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "OH"] <- 39000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "OH"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "OK"] <- 40000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "OK"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "PA"] <- 42000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "PA"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "TN"] <- 47000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "TN"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "TX"] <- 48000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "TX"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "UT"] <- 49000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "UT"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "VA"] <- 51000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "VA"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "WV"] <- 54000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "WV"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "WY"] <- 56000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "WY"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "WA"] <- 53000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "WA"]))

coalreceipts$coalminefips[coalreceipts$Coalmine.State == "1"] <- 1000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "1"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "17"] <- 17000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "17"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "18"] <- 18000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "18"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "19"] <- 19000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "19"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "20"] <- 20000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "20"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "21"] <- 21000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "21"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "22"] <- 22000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "22"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "24"] <- 24000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "24"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "29"] <- 29000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "29"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "3"] <- 3000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "3"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "30"] <- 30000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "30"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "35"] <- 35000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "35"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "38"] <- 38000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "38"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "39"] <- 39000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "39"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "4"] <- 4000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "4"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "40"] <- 40000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "40"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "42"] <- 42000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "42"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "47"] <- 47000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "47"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "48"] <- 48000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "48"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "49"] <- 49000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "49"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "51"] <- 51000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "51"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "53"] <- 53000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "53"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "54"] <- 54000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "54"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "56"] <- 56000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "56"]))
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "8"] <- 8000+as.numeric(as.character(coalreceipts$Coalmine.County[coalreceipts$Coalmine.State == "8"]))

coalreceipts$coalminefips[coalreceipts$Coalmine.State == "AU"] <- 1000000 ## Australia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "CL"] <- 2000000 ## Columbia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "CN"] <- 3000000 ## Canada
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "IS"] <- 4000000 ## Indonesia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "ID"] <- 4000000 ## Indonesia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "PL"] <- 5000000 ## Poland
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "RS"] <- 6000000 ## Russia
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "UK"] <- 7000000 ## UK
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "VZ"] <- 8000000 ## Venezuela
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "OT"] <- 9000000 ## Other (to be dropped)
coalreceipts$coalminefips[coalreceipts$Coalmine.State == "IM"] <- 9000000 ## Other (to be dropped)


coalreceipts_county_year <- aggregate(coalreceipts$QUANTITY, by = list(coalreceipts$YEAR, coalreceipts$coalminefips, coalreceipts$Plant.Id), FUN = sum)

plant2012 <- read.csv("C:/Users/Jonathan/Google Drive/coalmining/data/plant2012.csv")
#plantloc <- plant2012[,c("Plant.Code","State","Latitude","Longitude")]
plantloc <- plant2012
plantloc <- subset(plantloc, !is.na(plantloc$Latitude))
plantloc <- subset(plantloc, plantloc$Latitude != 0)
plantlist <- plantloc[,c("Utility.ID","Plant.Code","Latitude","Longitude")]

opts <- expand.grid(unique(coalreceipts_county_year$Group.1), unique(coalreceipts_county_year$Group.2), unique(coalreceipts_county_year$Group.3))
opts <- merge(opts ,plantlist,by.x = "Var3",by. = "Plant.Code")

Gaz_counties_national <- read.delim("C:/Users/Jonathan/Google Drive/coalmining/data/Gaz_counties_national.txt")
Gaz_counties_national <- Gaz_counties_national[,c("GEOID","INTPTLAT","INTPTLONG")]

opts <- merge(opts, Gaz_counties_national,by.x = "Var2",by.y = "GEOID", all.x = T)
opts$INTPTLAT[opts$Var2 == 1000000] <- -25.274398 
opts$INTPTLONG[opts$Var2 == 1000000] <- 133.775136 
opts$INTPTLAT[opts$Var2 == 2000000] <- 4.570868 
opts$INTPTLONG[opts$Var2 == 2000000] <- 	-74.297333 
opts$INTPTLAT[opts$Var2 == 3000000] <- 56.130366 	
opts$INTPTLONG[opts$Var2 == 3000000] <- 	-106.346771 
opts$INTPTLAT[opts$Var2 == 4000000] <-  	-0.789275 	
opts$INTPTLONG[opts$Var2 == 4000000] <- 	113.921327
opts$INTPTLAT[opts$Var2 == 5000000] <- -0.789275  
opts$INTPTLONG[opts$Var2 == 5000000] <- 	113.921327
opts$INTPTLAT[opts$Var2 == 6000000] <- 61.52401 	
opts$INTPTLONG[opts$Var2 == 6000000] <-  105.318756 
opts$INTPTLAT[opts$Var2 == 7000000] <- 55.378051 	
opts$INTPTLONG[opts$Var2 == 7000000] <- 	-3.435973
opts$INTPTLAT[opts$Var2 == 8000000] <- 6.42375 	
opts$INTPTLONG[opts$Var2 == 8000000] <- -66.58973

opts <- subset(opts, !is.na(opts$INTPTLAT))
opts <- subset(opts, !is.na(opts$Latitude))

just2008 <- subset(opts, opts$Var1 == 2008)
just2008$minecongress <- latlong2congress(data.frame(x= just2008$INTPTLONG, y =just2008$INTPTLAT))
just2008$plantcongress <- latlong2congress(data.frame(x= just2008$Longitude, y =just2008$Latitude))
just2008$minestate <- latlong2state(data.frame(x= just2008$INTPTLONG, y =just2008$INTPTLAT))
just2008$plantstate <- latlong2state(data.frame(x= just2008$Longitude, y =just2008$Latitude))
just2008$minecounty <- latlong2county(data.frame(x= just2008$INTPTLONG, y =just2008$INTPTLAT))
just2008$plantcounty <- latlong2county(data.frame(x= just2008$Longitude, y =just2008$Latitude))

### fix some missing intersections
just2008$minestate[just2008$Var2 >57000] <- "Foreign"
just2008$minecounty[just2008$Var2 >57000] <- "Foreign"
just2008$minecongress[just2008$Var2 >57000] <- "Foreign"
just2008$plantstate[just2008$Var3 == 10675] <- "connecticut"
just2008$plantcounty[just2008$Var3 == 10675] <- "new london,connecticut"
just2008$plantcongress[just2008$Var3 == 10675] <- "0902"
just2008$plantstate[just2008$Var3 == 643] <- "florida"
just2008$plantcounty[just2008$Var3 == 643] <- "bay,florida"
just2008$plantstate[just2008$Var3 == 646] <- "florida"
just2008$plantcounty[just2008$Var3 == 646] <- "hillsborough,florida"
just2008$plantstate[just2008$Var3 == 1554] <- "maryland"
just2008$plantcounty[just2008$Var3 == 1554] <- "anne arundel,maryland"
just2008$plantstate[just2008$Var3 == 1626] <- "massachussets"
just2008$plantcounty[just2008$Var3 == 1626] <- "essex,massachussets"
just2008$plantstate[just2008$Var3 == 1702] <- "michigan"
just2008$plantcounty[just2008$Var3 == 1702] <- "bay,michigan"
just2008$plantstate[just2008$Var3 == 1702] <- "michigan"
just2008$plantcounty[just2008$Var3 == 1702] <- "bay,michigan"
just2008$plantstate[just2008$Var3 == 1732] <- "michigan"
just2008$plantcounty[just2008$Var3 == 1732] <- "bay,michigan"
just2008$plantstate[just2008$Var3 == 1733] <- "michigan"
just2008$plantcounty[just2008$Var3 == 1733] <- "monroe,michigan"
just2008$plantstate[just2008$Var3 == 1740] <- "michigan"
just2008$plantcounty[just2008$Var3 == 1740] <- "wayne,michigan"
just2008$plantstate[just2008$Var3 == 1743] <- "michigan"
just2008$plantcounty[just2008$Var3 == 1743] <- "st. clair,michigan"
just2008$plantstate[just2008$Var3 == 6034] <- "michigan"
just2008$plantcounty[just2008$Var3 == 6034] <- "st. clair,michigan"
just2008$plantstate[just2008$Var3 == 1825] <- "michigan"
just2008$plantcounty[just2008$Var3 == 1825] <- "ottawa,michigan"
just2008$plantcongress[just2008$Var3 == 1843] <- "2601"
just2008$plantstate[just2008$Var3 == 1897] <- "minnesota"
just2008$plantcounty[just2008$Var3 == 1897] <- "st. louis,michigan"
just2008$plantstate[just2008$Var3 == 2554] <- "new york"
just2008$plantcounty[just2008$Var3 == 2554] <- "chautauqua,new york"
just2008$plantcongress[just2008$Var3 == 2554] <- "3623"
just2008$plantstate[just2008$Var3 == 2642] <- "new york"
just2008$plantcounty[just2008$Var3 == 2642] <- "monroe,new york"
just2008$plantstate[just2008$Var3 == 6082] <- "new york"
just2008$plantcounty[just2008$Var3 == 6082] <- "niagara,new york"
just2008$plantstate[just2008$Var3 == 2837] <- "ohio"
just2008$plantcounty[just2008$Var3 == 2837] <- "lake,ohio"
just2008$plantstate[just2008$Var3 == 2857] <- "ohio"
just2008$plantcounty[just2008$Var3 == 2857] <- "lorain,ohio"
just2008$plantstate[just2008$Var3 == 3809] <- "virginia"
just2008$plantcounty[just2008$Var3 == 3809] <- "york,virginia"
just2008$plantstate[just2008$Var3 == 3920] <- "washiington"
just2008$plantcounty[just2008$Var3 == 3920] <- "pierce,washington"
just2008$plantcongress[just2008$Var3 == 3982] <- '5507'
just2008$plantcongress[just2008$Var3 == 3983] <- '5507'
just2008$plantcongress[just2008$Var3 == 4040] <- '5506'
just2008$plantstate[just2008$Var3 == 4072] <- "wisconsin"
just2008$plantcounty[just2008$Var3 == 4072] <- "brown,wisconsin"

just2008 <- just2008[,c("Var2","Var3","Latitude","Longitude","INTPTLAT","INTPTLONG", "minecongress","plantcongress","minestate","plantstate","minecounty","plantcounty")]

just2008$distances <- 0
for(j in 1:nrow(just2008)){
  just2008$distances[j] <- rdist.earth(matrix(c(just2008$INTPTLONG[j],just2008$INTPTLAT[j]), ncol = 2), matrix(c(just2008$Longitude[j], just2008$Latitude[j]), ncol = 2))
  if(j%%10000 == 0){print(j)}
}

just2008 <- just2008[,c("Var2","Var3","distances", "minecongress","plantcongress","minestate","plantstate","minecounty","plantcounty")]
opts2 <- merge(opts, just2008, by = c("Var2","Var3"))

opts2 <- merge(opts2, coalreceipts_county_year,by.x = c("Var1","Var2","Var3"),by.y = c("Group.1","Group.2","Group.3"),all.x = T)
#opts2$plantcounty <- latlong2county(data.frame(x= opts2$Longitude, y = opts2$Latitude))

load("C:/Users/Jonathan/Downloads/utilityfipslink.R")
countyfips <- county.fips
opts2 <- merge(opts2, countyfips,by.x = "plantcounty",by.y = "polyname")

#opts2$minestatefips <- floor(opts2$Var2/1000)
#opts2$plantstatefips <- floor(opts2$fips/1000)
opts2$differentstate <- opts2$plantstate != opts2$minestate
opts2$differentcounty <- opts2$plantcounty != opts2$minecounty
opts2$differentcongress <- opts2$plantcongress != opts2$minecongress
opts2$x[is.na(opts2$x)] <- 0

totalplant <- aggregate(opts2$x,by =list(opts2$Var3, opts2$Var1), FUN  = sum)
totalmine  <- aggregate(opts2$x,by =list(opts2$Var2, opts2$Var1), FUN  = sum)

opts2 <- merge(opts2, totalplant,by.x = c("Var3","Var1"),by.y = c("Group.1","Group.2"))
opts2 <- merge(opts2, totalmine,by.x = c("Var2","Var1"),by.y = c("Group.1","Group.2"))

opts2$anytrade <- opts2$x.x > 0

#plantdata <- plant2012[,c]

distancepoly <- poly(opts2$distances,4)
opts2$distancepoly1 <- distancepoly[,1]
opts2$distancepoly2 <- distancepoly[,2]
opts2$distancepoly3 <- distancepoly[,3]
opts2$distancepoly4 <- distancepoly[,4]

plantlatpoly <- poly(opts2$Latitude,4)
plantlonpoly <- poly(opts2$Longitude,4)
opts2$latpol1 <- plantlatpoly[,1]
opts2$latpol2 <- plantlatpoly[,2]
opts2$latpol3 <- plantlatpoly[,3]
opts2$latpol4 <- plantlatpoly[,4]
opts2$lonpol1 <- plantlonpoly[,1]
opts2$lonpol2 <- plantlonpoly[,2]
opts2$lonpol3 <- plantlonpoly[,3]
opts2$lonpol4 <- plantlonpoly[,4]
rm(distancepoly, plantlatpoly, plantlonpoly)


opts2 <- opts2[order(opts2$Var2,opts2$Var3, opts2$Var1),]


mkdata <- read.dta13("C:/Users/Jonathan/Downloads/je.dta")
mkdata <- subset(mkdata, mkdata$year == 2008)
mkdata$state <-  c(state.abb[1:8],"DC",state.abb[9:50])
opts2$plantstatefips <- floor(opts2$fips/1000)
opts2 <- merge(opts2, mkdata,by.x = "plantstatefips", by.y ="fips")
require('Hmisc')

require('mfx')
goodobs <- subset(opts2, opts2$x.y > 0 & opts2$x > 0)
spline <- rcspline.eval(goodobs$distances, nk =5, inclx = T)
goodobs$spline1 <- spline[,1]
goodobs$spline2 <- spline[,2]
goodobs$spline3 <- spline[,3]
goodobs$spline4 <- spline[,4]
rm(spline)
gc()
goodobs$Year <- as.factor(goodobs$Var1)

goodobs$x <- goodobs$x/1000000
goodobs$x.x <- goodobs$x.x/1000000
goodobs$x.y <- goodobs$x.y/1000000

require('XLConnect')
print("Reg with 4 order distance polynomial")
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4")
gc()
print("Reg with 3 order distance polynomial")
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3")
gc()

print("Reg with spline")
out<- capture.output(logitmfx(formula = anytrade ~  spline1 + spline2  + spline3 + spline4  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3" ))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline")
gc()

print("Reg with 4 order distance polynomial - TIme FE")
out<- capture.output(logitmfx(formula = anytrade ~ Year + distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4_YearFE")
gc()
print("Reg with 3 order distance polynomial  - TIme FE")
out<- capture.output(logitmfx(formula = anytrade ~ Year+  distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3_YearFE")
gc()

print("Reg with spline  - TIme FE")
out<- capture.output(logitmfx(formula = anytrade ~ Year+  spline1 + spline2  + spline3 + spline4  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3" ))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline_YearFE")
gc()



print("Reg with 4 order distance polynomial")
l1 <- lm(formula = x.x ~  Year+ distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs)
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = capture.output(super.cluster.fun(l1, goodobs$Var3)), sheet = "quantity_poly4")
rm(l1)
gc()
print("Reg with 3 order distance polynomial")
l2 <- lm(formula = x.x ~ Year+ distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs)
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = capture.output(super.cluster.fun(l2, goodobs$Var3)), sheet = "quantity_poly3")
rm(l2)
gc()
print("Reg with spline")
l3 <- lm(formula = x.x ~  Year +spline1 + spline2  + spline3 + spline4  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs)
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = capture.output(super.cluster.fun(l3, goodobs$Var3)), sheet = "quantity_spline")
rm(l3)
gc()
positiveobs <- subset(goodobs, goodobs$x.x > 0)
print("Reg with 4 order distance polynomial")
posobs_4 <- lm(formula = x.x ~Year+  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = positiveobs)
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = capture.output(super.cluster.fun(posobs_4, positiveobs$Var3)), sheet = "quantitypositive_poly4")
gc()
print("Reg with 3 order distance polynomial")
posobs_3 <- lm(formula = x.x ~Year+  distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = positiveobs)
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = capture.output(super.cluster.fun(posobs_3, positiveobs$Var3)), sheet = "quantitypositive_poly3")
print("Reg with spline")
posobs_poly <- lm(formula = x.x ~ Year+ spline1 + spline2  + spline3 + spline4  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = positiveobs)
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = capture.output(super.cluster.fun(posobs_poly, positiveobs$Var3)), sheet = "quantitypositive_spline")


utility <- read.csv("C:/Users/Jonathan/Downloads/eia8602014/utility.csv")
utilitymerge <- utility[,c("Utility.ID","Entity.Type")]

goodobs <- merge(goodobs, utilitymerge, by = "Utility.ID")
goodobs$Entity.C <- goodobs$Entity.Type == "C"
goodobs$Entity.COM <- goodobs$Entity.Type == "COM"
goodobs$Entity.F <- goodobs$Entity.Type == "F"
goodobs$Entity.I <- goodobs$Entity.Type == "I"
goodobs$Entity.IND <- goodobs$Entity.Type == "IND"
goodobs$Entity.M <- goodobs$Entity.Type == "M"
goodobs$Entity.P <- goodobs$Entity.Type == "P"
goodobs$Entity.Q <- goodobs$Entity.Type == "Q"
goodobs$Entity.S <- goodobs$Entity.Type == "S"

goodobs$Entity.C.diffstate <- goodobs$Entity.C*goodobs$differentstate
goodobs$Entity.COM.diffstate <- goodobs$Entity.COM*goodobs$differentstate
goodobs$Entity.F.diffstate <- goodobs$Entity.F*goodobs$differentstate
goodobs$Entity.I.diffstate <- goodobs$Entity.I*goodobs$differentstate
goodobs$Entity.IND.diffstate <- goodobs$Entity.IND*goodobs$differentstate
goodobs$Entity.M.diffstate <- goodobs$Entity.M*goodobs$differentstate
goodobs$Entity.P.diffstate <- goodobs$Entity.P*goodobs$differentstate
goodobs$Entity.Q.diffstate <- goodobs$Entity.Q*goodobs$differentstate
goodobs$Entity.S.diffstate <- goodobs$Entity.S*goodobs$differentstate

goodobs$Entity.C.diffcounty <- goodobs$Entity.C*goodobs$differentcounty
goodobs$Entity.COM.diffcounty <- goodobs$Entity.COM*goodobs$differentcounty
goodobs$Entity.F.diffcounty <- goodobs$Entity.F*goodobs$differentcounty
goodobs$Entity.I.diffcounty <- goodobs$Entity.I*goodobs$differentcounty
goodobs$Entity.IND.diffcounty <- goodobs$Entity.IND*goodobs$differentcounty
goodobs$Entity.M.diffcounty <- goodobs$Entity.M*goodobs$differentcounty
goodobs$Entity.P.diffcounty <- goodobs$Entity.P*goodobs$differentcounty
goodobs$Entity.Q.diffcounty <- goodobs$Entity.Q*goodobs$differentcounty
goodobs$Entity.S.diffcounty <- goodobs$Entity.S*goodobs$differentcounty

goodobs$Entity.C.diffcong <- goodobs$Entity.C*goodobs$differentcong
goodobs$Entity.COM.diffcong <- goodobs$Entity.COM*goodobs$differentcong
goodobs$Entity.F.diffcong <- goodobs$Entity.F*goodobs$differentcong
goodobs$Entity.I.diffcong <- goodobs$Entity.I*goodobs$differentcong
goodobs$Entity.IND.diffcong <- goodobs$Entity.IND*goodobs$differentcong
goodobs$Entity.M.diffcong <- goodobs$Entity.M*goodobs$differentcong
goodobs$Entity.P.diffcong <- goodobs$Entity.P*goodobs$differentcong
goodobs$Entity.Q.diffcong <- goodobs$Entity.Q*goodobs$differentcong
goodobs$Entity.S.diffcong <- goodobs$Entity.S*goodobs$differentcong

#### Entity Regs
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + Entity.C + Entity.COM + Entity.F + Entity.IND + Entity.M + Entity.P + Entity.Q + Entity.S + Entity.C.diffstate + Entity.COM.diffstate + Entity.F.diffstate + Entity.IND.diffstate + Entity.M.diffstate + Entity.P.diffstate + Entity.Q.diffcounty + Entity.S.diffstate +  Entity.C.diffcounty + Entity.COM.diffcounty + Entity.F.diffcounty + Entity.IND.diffcounty + Entity.M.diffcounty + Entity.P.diffcounty + Entity.Q.diffcounty + Entity.S.diffcounty + Entity.C.diffcong + Entity.COM.diffcong + Entity.F.diffcong + Entity.IND.diffcong + Entity.M.diffcong + Entity.P.diffcong + Entity.Q.diffcong + Entity.S.diffcong + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4_entity")
gc()
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + Entity.C + Entity.COM + Entity.F + Entity.IND + Entity.M + Entity.P + Entity.Q + Entity.S + Entity.C.diffstate + Entity.COM.diffstate + Entity.F.diffstate + Entity.IND.diffstate + Entity.M.diffstate + Entity.P.diffstate + Entity.Q.diffcounty + Entity.S.diffstate +  Entity.C.diffcounty + Entity.COM.diffcounty + Entity.F.diffcounty + Entity.IND.diffcounty + Entity.M.diffcounty + Entity.P.diffcounty + Entity.Q.diffcounty + Entity.S.diffcounty + Entity.C.diffcong + Entity.COM.diffcong + Entity.F.diffcong + Entity.IND.diffcong + Entity.M.diffcong + Entity.P.diffcong + Entity.Q.diffcong + Entity.S.diffcong + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3_entity")
gc()
out<- capture.output(logitmfx(formula = anytrade ~ spline1 + spline2 + spline3 + spline4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + Entity.C + Entity.COM + Entity.F + Entity.IND + Entity.M + Entity.P + Entity.Q + Entity.S + Entity.C.diffstate + Entity.COM.diffstate + Entity.F.diffstate + Entity.IND.diffstate + Entity.M.diffstate + Entity.P.diffstate + Entity.Q.diffcounty + Entity.S.diffstate +  Entity.C.diffcounty + Entity.COM.diffcounty + Entity.F.diffcounty + Entity.IND.diffcounty + Entity.M.diffcounty + Entity.P.diffcounty + Entity.Q.diffcounty + Entity.S.diffcounty + Entity.C.diffcong + Entity.COM.diffcong + Entity.F.diffcong + Entity.IND.diffcong + Entity.M.diffcong + Entity.P.diffcong + Entity.Q.diffcong + Entity.S.diffcong + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline_entity")
gc()

#### Other measure of entity type
goodobs$ownership <- "government" 
goodobs$ownership[goodobs$Entity.Type %in% c("IND","COM","Q")] <- "IPP"
goodobs$ownership[goodobs$Entity.Type == "I"] <- "Investor"
goodobs$ownership <- as.factor(goodobs$ownership)
goodobs$invest.diffstate <- (goodobs$ownership == "Investor")*goodobs$differentstate
goodobs$invest.diffcounty <- (goodobs$ownership == "Investor")*goodobs$differentcounty
goodobs$invest.diffcongr <- (goodobs$ownership == "Investor")*goodobs$differentcongress
goodobs$government <- goodobs$ownership == "government"
goodobs$government.diffstate <- goodobs$government*goodobs$differentstate 
goodobs$government.diffcounty <- goodobs$government*goodobs$differentcounty 
goodobs$government.diffcongr <- goodobs$government*goodobs$differentcongress 
goodobs$ipp.diffstate <- (goodobs$ownership == "IPP")*goodobs$differentstate
goodobs$ipp.diffcounty <- (goodobs$ownership == "IPP")*goodobs$differentcounty
goodobs$ipp.diffcongr <- (goodobs$ownership == "IPP")*goodobs$differentcongress


out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + ownership+ differentstate + differentcounty + differentcongress + invest.diffstate+invest.diffcounty+ invest.diffcongr + ipp.diffstate+ ipp.diffcounty+ ipp.diffcongr +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4_government")
rm(out)
gc()
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + ownership+ differentstate + differentcounty + differentcongress + invest.diffstate+invest.diffcounty+ invest.diffcongr + ipp.diffstate+ ipp.diffcounty+ ipp.diffcongr +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3_government")
rm(out)
gc()
out<- capture.output(logitmfx(formula = anytrade ~   spline1 + spline2 + spline3 + spline4 + latpol1 + latpol2 + latpol3+ latpol4 + ownership+ differentstate + differentcounty + differentcongress + invest.diffstate+invest.diffcounty+ invest.diffcongr + ipp.diffstate+ ipp.diffcounty+ ipp.diffcongr +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline_government")
rm(out)
gc()
out<- capture.output(logitmfx(formula = anytrade ~  Year+ distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4  + ownership+ differentstate + differentcounty + differentcongress + invest.diffstate+invest.diffcounty+ invest.diffcongr + ipp.diffstate+ ipp.diffcounty+ ipp.diffcongr +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4_gov_yFE")
rm(out)
gc()
out<- capture.output(logitmfx(formula = anytrade ~Year+  distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4  + ownership+ differentstate + differentcounty + differentcongress + invest.diffstate+invest.diffcounty+ invest.diffcongr + ipp.diffstate+ ipp.diffcounty+ ipp.diffcongr +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3_gov_yFE")
rm(out)
gc()
out<- capture.output(logitmfx(formula = anytrade ~  Year+ spline1 + spline2 + spline3 + spline4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4  + ownership+ differentstate + differentcounty + differentcongress + invest.diffstate+invest.diffcounty+ invest.diffcongr + ipp.diffstate+ ipp.diffcounty+ ipp.diffcongr +x + x.y,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline_gov_yFE")
rm(out)
gc()

##### Shale Regs
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y + isshale,data = goodobs, clustervar1  = "Var3"))
rm(out)
gc()
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4_shale")
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y + isshale,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3_shale")
rm(out)
gc()
out<- capture.output(logitmfx(formula = anytrade ~  spline1 + spline2 + spline3 + spline4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y + isshale,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline_shale")
rm(out)
gc()

##### Political Regs
goodobs$diffstate_Dem <- goodobs$differentstate*goodobs$Dem 
goodobs$diffcounty_Dem <- goodobs$differentcounty*goodobs$Dem
goodobs$diffcongr_Dem <- goodobs$differentcongress*goodobs$Dem
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress + Dem + diffstate_Dem + diffcounty_Dem + diffcongr_Dem + x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4_dem")
out<- capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress  + Dem + diffstate_Dem + diffcounty_Dem + diffcongr_Dem + +x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3_dem")
out<- capture.output(logitmfx(formula = anytrade ~  spline1 + spline2 + spline3 + spline4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress  + Dem + diffstate_Dem + diffcounty_Dem + diffcongr_Dem + +x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline_dem")

out<- capture.output(logitmfx(formula = anytrade ~  Year + distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress + Dem + diffstate_Dem + diffcounty_Dem + diffcongr_Dem + x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4_dem_YFE")
out<- capture.output(logitmfx(formula = anytrade ~  Year + distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress  + Dem + diffstate_Dem + diffcounty_Dem + diffcongr_Dem + +x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3_dem_YFE")
out<- capture.output(logitmfx(formula = anytrade ~ Year +  spline1 + spline2 + spline3 + spline4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress  + Dem + diffstate_Dem + diffcounty_Dem + diffcongr_Dem + +x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline_dem_YFE")

##### Unemployment Regs

load("C:/Users/Jonathan/Google Drive/coalmining/data/unemployment/unemploy90_14.R")
goodobs <- merge(goodobs, unemploy, by.x = c("fips","Var1"),by.y = c("fips","X.3"))
goodobs$differentstate_unemploy <- goodobs$differentstate*goodobs$X.8
goodobs$differentcounty_unemploy <- goodobs$differentcounty*goodobs$X.8
goodobs$differentcongress_unemploy <- goodobs$differentcongress*goodobs$X.8

out<- capture.output(logitmfx(formula = anytrade ~  Year + distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress + X.8 + differentstate_unemploy + differentcounty_unemploy + differentcongress_unemploy + x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly4_unemploy_YFE")
rm(out)
gc()
out<- capture.output(logitmfx(formula = anytrade ~  Year + distancepoly1 + distancepoly2 + distancepoly3  + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress  +  X.8 + differentstate_unemploy + differentcounty_unemploy + differentcongress_unemploy + +x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_poly3_unemploy_YFE")
rm(out)
gc()
out<- capture.output(logitmfx(formula = anytrade ~ Year +  spline1 + spline2 + spline3 + spline4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress  + + X.8 + differentstate_unemploy + differentcounty_unemploy + differentcongress_unemploy+ +x + x.y ,data = goodobs, clustervar1  = "Var3"))
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_logit.xlsx",data = out, sheet = "anytrade_spline_unemploy_YFE")
rm(out)
gc()
#### Annual regs
t6_reg <- logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress +x + x.y,data = goodobs, clustervar1  = "Var3" )
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress+ x + x.y,data = subset(goodobs, goodobs$Var1 == 1990), clustervar1  = "Var3" )), sheet = "logit1990")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4+ latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4  + differentstate + differentcounty + differentcongress+ x + x.y,data = subset(goodobs, goodobs$Var1 == 1991), clustervar1  = "Var3" )), sheet = "logit1991")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data =capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4+ latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4  + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 1992), clustervar1  = "Var3" )), sheet = "logit1992")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data =capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 1993), clustervar1  = "Var3" )), sheet = "logit1993")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data =capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 1994), clustervar1  = "Var3" )), sheet = "logit1994")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 1995), clustervar1  = "Var3" )), sheet = "logit1995")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 1996), clustervar1  = "Var3" )), sheet = "logit1996")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4+ latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4  + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 1997), clustervar1  = "Var3" )), sheet = "logit1997")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 1998), clustervar1  = "Var3" )), sheet = "logit1998")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 1999), clustervar1  = "Var3" )), sheet = "logit1999")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2000), clustervar1  = "Var3" )), sheet = "logit2000")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4+ latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4  + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2001), clustervar1  = "Var3" )), sheet = "logit2001")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2002), clustervar1  = "Var3" )), sheet = "logit2002")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress+ x + x.y,data = subset(goodobs, goodobs$Var1 == 2003), clustervar1  = "Var3" )), sheet = "logit2003")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress+ x + x.y,data = subset(goodobs, goodobs$Var1 == 2004), clustervar1  = "Var3" )), sheet = "logit2004")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2005), clustervar1  = "Var3" )), sheet = "logit2005")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2006), clustervar1  = "Var3" )), sheet = "logit2006")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2007), clustervar1  = "Var3" )), sheet = "logit2007")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2008), clustervar1  = "Var3" )), sheet = "logit2008")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2009), clustervar1  = "Var3" )), sheet = "logit2009")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2010), clustervar1  = "Var3" )), sheet = "logit2010")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty+ differentcongress + x + x.y,data = subset(goodobs, goodobs$Var1 == 2011), clustervar1  = "Var3" )), sheet = "logit2011")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress+ x + x.y,data = subset(goodobs, goodobs$Var1 == 2012), clustervar1  = "Var3" )), sheet = "logit2012")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty + differentcongress+ x + x.y,data = subset(goodobs, goodobs$Var1 == 2013), clustervar1  = "Var3" )), sheet = "logit2013")
writeWorksheetToFile(file = "C:/Users/Jonathan/Google Drive/coalmining/output_annual.xlsx",data = capture.output(logitmfx(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + latpol1 + latpol2 + latpol3+ latpol4 + lonpol1 + lonpol2 + lonpol3 + lonpol4 + differentstate + differentcounty ++ differentcongress+ x + x.y,data = subset(goodobs, goodobs$Var1 == 2014), clustervar1  = "Var3" )), sheet = "logit2014")

t7_reg <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = goodobs )
t7_reg_1990 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1990) )
t7_reg_1991 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1991) )
t7_reg_1992 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1992) )
t7_reg_1993 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1993) )
t7_reg_1994 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1994) )
t7_reg_1995 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1995) )
t7_reg_1996 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1996) )
t7_reg_1997 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1997) )
t7_reg_1998 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1998) )
t7_reg_1999 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 1999) )
t7_reg_2000 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2000) )
t7_reg_2001 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2001) )
t7_reg_2002 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2002) )
t7_reg_2003 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2003) )
t7_reg_2004 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2004) )
t7_reg_2005 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2005) )
t7_reg_2006 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2006) )
t7_reg_2007 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2007) )
t7_reg_2008 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2008) )
t7_reg_2009 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2009) )
t7_reg_2010 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2010) )
t7_reg_2011 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2011) )
t7_reg_2012 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2012) )
t7_reg_2013 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2013) )
t7_reg_2014 <- lm(formula = x.x ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + differentcounty + x + x.y,data = subset(goodobs, goodobs$Var1 == 2014) )


summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x ,data = subset(goodobs, goodobs$Var1 == 2014) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2013) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2012) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2011) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2010) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2009) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2008) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2007) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2006) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2005) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2004) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2003) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2002) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate+ x.y + x   ,data = subset(goodobs, goodobs$Var1 == 2001) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 2000) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1999) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1998) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1997) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate+ x.y + x   ,data = subset(goodobs, goodobs$Var1 == 1996) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1995) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1994) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1993) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1993) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1992) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1991) ))
summary(lm(formula = anytrade ~  distancepoly1 + distancepoly2 + distancepoly3 + distancepoly4 + differentstate + x.y + x  ,data = subset(goodobs, goodobs$Var1 == 1990) ))
