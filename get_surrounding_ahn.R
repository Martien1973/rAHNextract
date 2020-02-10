get_surrounding_ahn <- function(X, Y, .LOLAT = FALSE, AHN = my_ahn, dem = dem, resolution = resolution, radius = 0, redownload = FALSE){
  
  #create point
  spatialpoint <- data.frame("X"=X,"Y"=Y)
  coordinates(spatialpoint) <- ~X+Y
  
  #convert lat lon
  if(LONLAT == TRUE){
    crs(spatialpoint) <- CRS("+init=epsg:4326")
  } else {
    crs(spatialpoint) <- CRS(epsg_rd)
  }
  
  if(LONLAT == TRUE){
    spatialpoint <- spTransform(spatialpoint, CRS = CRS(epsg_rd))
  }
  
  point.sf <- st_as_sf(spatialpoint)
  surroundingBuffer <- st_buffer(point.sf,dist=radius) #since RDcoords are in meters width is also in m
  
  #get BBOX extent of buffer area
  surroundingExtent <- extent(surroundingBuffer)
  my_bbox <- paste(toString(surroundingExtent@my_xmin), toString(surroundingExtent@my_ymin), toString(surroundingExtent@my_xmax), toString(surroundingExtent@my_ymax), sep=",")

  return(my_bbox)
}
