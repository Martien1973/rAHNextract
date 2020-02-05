download_point_raster <- function(wcsUrl, name = "elevation"){
        image_name <- paste0(name, ".tif")
        image <- download.file(wcsUrl, image_name)
        print("Download raster image succeeded.")
        my_raster <- raster(image_name)
        NAvalue(my_raster) <- --32768.0
        plot(my_raster, xlab="RD X", ylab="RD Y", main="Elevation (m)")
        ras <- list("raster" = my_raster, "file" = image_name)
        return(ras)
}