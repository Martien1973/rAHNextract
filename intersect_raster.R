intersect_raster <- function(ras, point){
  print("Intersecting raster. Getting elevation...")
  my_elevation <- raster::extract(ras, point.sp)
  return (my_elevation)
}
 