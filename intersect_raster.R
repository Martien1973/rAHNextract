intersect_raster <- function(ras, point){
  print("Intersecting raster. Getting elevation...")
  my_elevation <- raster::extract(ras, point, method = "bilinear")
  return (my_elevation)
}
 