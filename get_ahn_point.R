get_ahn_point <- function(name = "", X, Y, LONLAT = FALSE, resolution){
  #RD new coordinaten systeem
  epsg_rd <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
  
  if(name != ""){
    #name_trim <- getName_trim(name = name, addition = addition)
  }
  
  #create point
  spatialpoint <- data.frame("X"=X,"Y"=Y)
  coordinates(spatialpoint) <- ~X+Y
  
  #convert LATLON
  if(LONLAT == TRUE){
    crs(spatialpoint) <- CRS("+init=epsg:4326")
  } else {
    crs(spatialpoint) <- CRS(epsg_rd)
  }
  if(LONLAT == TRUE){
    spatialpoint <- spTransform(spatialpoint, CRS = CRS(epsg_rd))
  }
  
  coords <- coordinates(spatialpoint)
  if(coords[1,"X"] < 12628.0541 || coords[1,"X"] > 283594.4779){
    stop("X coordinate out of range.")
  }
  if(coords[1,"Y"] < 308179.0423 || coords[1,"Y"] > 611063.1429){
    stop("Y coordinate out of range.")
  }
  
  #create 9 pixels bbox
  xround <- round(X)
  yround <- round(Y)
  
  if(X - xround > 0){
    my_xmin <- xround - (1 * resolution)
    my_xmax <- xround + (2 * resolution)
  } else if(X - xround < 0){
    my_xmin <- xround - (2 * resolution)
    my_xmax <- xround + (1 * resolution)
  } else {
    my_xmin <- X - (1 * resolution)
    my_xmax <- X + (2 * resolution)
  }
  
  if(Y - yround > 0){
    my_ymin <- yround - (1 * resolution)
    my_ymax <- yround + (2 * resolution)
  } else if(Y - yround < 0){
    my_ymin <- yround - (2 * resolution)
    my_ymax <- yround + (1 * resolution)
  } else {
    my_ymin <- Y - (1 * resolution)
    my_ymax <- Y + (2 * resolution)
  }
  
  bbox <- list("xmin"= my_xmin, "xmax"= my_xmax, "ymin" = my_ymin, "ymax" = my_ymax)
  return(list("name" = name, "point" = point, "bbox" = bbox))
}
