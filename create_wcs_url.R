create_wcs_url <- function(wcs_baseUrl = "https://geodata.nationaalgeoregister.nl/ahn3/wcs?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage", bbox, AHN = "AHN3", resolution = 0.5, dem = "dsm", interpolated = TRUE){
  if(AHN == "AHN1"){
    wcs_baseUrl = "https://geodata.nationaalgeoregister.nl/ahn1/wcs?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage"
  } else if(AHN == "AHN2"){
    wcs_baseUrl = "https://geodata.nationaalgeoregister.nl/ahn2/wcs?SERVICE=WCS&VERSION=1.0.0&REQUEST=GetCoverage"
  }
  #get resolution
  my_resolution <- get_resolution(AHN= AHN, resolution = resolution)
  
  #get dem type
  dem <- get_dem(AHN = AHN, resolution = my_resolution$res, dem = dem)
  
  #get BBOX extent of buffer area
  my_bbox <- paste(bbox$xmin, bbox$ymin, bbox$xmax, bbox$ymax, sep=",")
  bbox_url <- paste0("BBOX=", my_bbox)

  #create image pixel dimensions
  my_width <- 3
  my_height <- 3
  
  dimensions_url <- paste0("WIDTH=", my_width, "&HEIGHT=", my_height)
  #name of layer
  if(dem == ""){
    underscore <-  ""
  } else {
    underscore <- "_"
  }
  name_layer_url <- paste0("COVERAGE=", tolower(AHN), "_" , resolution$res_name , underscore, dem)
  
  #wcs image format
  imgFormat_url <- "FORMAT=GEOTIFF_FLOAT32"
  
  #coordinate system
  crs_url <- "CRS=EPSG:28992&RESPONSE_CRS=EPSG:28992"
  
  #generate URL
  wcsUrl <- paste(wcs_baseUrl, name_layer_url, bbox_url, crs_url, imgFormat_url, dimensions_url, sep="&")
  
  print(wcsUrl)
  
  return (wcsUrl)
}