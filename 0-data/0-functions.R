# Declare functions that will be re-used

# ---- start --------------------------------------------------------------

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