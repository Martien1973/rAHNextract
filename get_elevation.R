get_elevation_points <- function(name, X, Y, LONLAT = FALSE, AHN = "AHN3", dem = "dsm", resolution, rm = FALSE, type = "points"){
  
  #selected AHN layer
  ahn_lower <- tolower(AHN)
  if(ahn_lower != "ahn1" && ahn_lower != "ahn2" && ahn_lower != "ahn3"){
    stop("No correct AHN is provided. Please select 'AHN3' or 'AHN2'")
  } else {
    my_ahn <- toupper(AHN)
  }
  
  if(type == "points"){
    my_point <- get_ahn_point(name = name, X = X, Y = Y, LONLAT = LONLAT, resolution = resolution)
    my_url <- create_wcs_url(bbox = my_point$bbox, AHN = my_ahn, dem = dem, resolution = resolution)
    my_raster <- download_point_raster(my_url, name)
  } else {
    get_surrounding_ahn(AHN = my_ahn, dem = dem, resolution = resolution, radius = 0, redownload = FALSE)
    my_url <- create_wcs_url(bbox = my_point$bbox, AHN = my_ahn, dem = dem, resolution = resolution)
    #my_raster <- 
  }
  
  
  my_elevation <- intersect_raster(my_raster$raster, my_point$point)
  
  if(rm == TRUE){
    file.remove(my_raster$file)
  }
  my_elevation <- format(round(my_elevation, 5), nsmall = 5)
  print(paste("Hoogte:", my_elevation, "m.", sep=" "))
  return (my_elevation)
}
points <- fread(file = "coordinates.csv", data.table = FALSE)

indiv_ID <- 47
indiv_elevation <- get_elevation_points(name = points[indiv_ID, "ID"], X = points[indiv_ID, "X"], Y = points[indiv_ID, "Y"], resolution = 0.5, rm = TRUE)

#indiv_elevation <- get_elevation_points(name = "test", X = 103538, Y = 481494, resolution = 0.5, rm = TRUE)
indiv_result <- data.frame(points[indiv_ID,], indiv_elevation)
View(indiv_result)

elevation <- mapply(get_elevation_points, name = points$ID, X = points$X, Y = points$Y, resolution = 0.5, rm = TRUE)
results <- data.frame(points, elevation)
colnames(results) <- c("ID", "X", "Y", "elevation")

View(results)
fwrite(results, "coordinates_elevations.csv")