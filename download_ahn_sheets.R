ahn_sheets <- function(name, surroundingBuffer, ahn = "AHN3"){  
  ###BladIndex method ###
  #get AHN bladIndex
  my_EPSG <- "EPSG:28992"
  
  directory <- paste("data", AHN, sep="/")
  if (!dir.exists(directory)){
    dir.create(directory)
  }
  bladIndex_shape_filepath <- paste(directory , sep="/")
  bladIndex_shape_file <- paste(bladIndex_shape_filepath, "/", AHN, "_bladIndex", ".shp", sep="")
  print(bladIndex_shape_file)
  if(!file.exists(bladIndex_shape_file)){
    print("Download ahn wfs blad Index")
    ahn_WFS_baseUrl <- paste0("https://geodata.nationaalgeoregister.nl/", tolower(AHN), "/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=", tolower(AHN), ":", tolower(AHN), "_bladindex")
    print(ahn_WFS_baseUrl)
    ahn_wfs <- paste("WFS:", ahn_WFS_baseUrl, "&SRSNAME=", my_EPSG, sep="")
    ogr2ogr(src_datasource_name = ahn_wfs , dst_datasource_name = bladIndex_shape_file, layer = paste0(tolower(AHN),":", tolower(AHN), "_bladindex"), overwrite = TRUE)
  }
  #load intersected blad indexes
  bladIndex.shp <- readOGR(dsn = directory, layer = paste0(AHN, "_bladIndex"), stringsAsFactors=FALSE)
  bladIndex.shp <- spTransform(bladIndex.shp, epsg_rd)
  bladIndex.sf <- st_as_sf(bladIndex.shp)
  
  st_agr(bladIndex.sf) <- "constant"
  st_agr(surroundingBuffer) <- "constant"
  bladIndex_buffer_intsct.sf <- st_intersection(bladIndex.sf, surroundingBuffer)
  
  bladnrs <- bladIndex_buffer_intsct.sf$bladnr
  
  dir.create(paste(directory, name_trim, sep="/"), showWarnings = FALSE)
  aws_working_directory <- paste(directory, name_trim, sep="/")
  
  ahn_atomFeed_BaseUrl <- paste("https://geodata.nationaalgeoregister.nl/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")
  #download raw ahn of corresponding resolultion
  indiv_raw_rasters <- list()
  if(raw_ahn == TRUE){  
    ahn_raw_directory <- paste(aws_working_directory, "raw", sep="/")
    ahn_raw_raster_filename <- paste(ahn_raw_directory, "/", name_trim, "_", AHN, "_raw_ahn", '.tif', sep="")
    if(!file.exists(ahn_raw_raster_filename)){
      dir.create(paste(aws_working_directory, "raw", sep="/"), showWarnings = FALSE)
      print(ahn_raw_directory)
      # rawXList <<- c()
      # rawYList <<- c()
      print(paste("Amount of sheets found:", length(bladnrs), sep=" "))
      ahn_raw_file_paths <- c()
      for(r in 1:length(bladnrs)){
        #ahn2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_ruw/r32cn1.tif.zip
        #ahn3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dsm/R_32CN1.zip
        if(AHN == "AHN3"){
          tifZip = ".ZIP"
          ahn_raw_letter <- "R"
          ahn_raw_naming <- paste0("_dsm/", ahn_raw_letter,"_")
          ahn_raw_downloadLink <- paste(ahn_atomFeed_BaseUrl, resolution_name, ahn_raw_naming,  toupper(bladnrs[[r]]), tifZip, sep="")
          ahn_raw_file_path <- paste(ahn_raw_directory, "/r_", tolower(bladnrs[[r]]), ".tif",sep="")
        } else {
          tifZip = ".tif.zip"
          ahn_raw_letter <- "r"
          ahn_raw_naming <- paste0("_ruw/", ahn_raw_letter)
          ahn_raw_downloadLink <- paste(ahn_atomFeed_BaseUrl, resolution_name, ahn_raw_naming,  tolower(bladnrs[[r]]), tifZip, sep="")
          ahn_raw_file_path <- paste(ahn_raw_directory, "/", ahn_raw_letter, tolower(bladnrs[[r]]), ".tif",sep="")
        }
        
        print(ahn_raw_downloadLink)
        ahn_rawZip_file_path <- paste(ahn_raw_directory, "/", ahn_raw_letter, tolower(bladnrs[[r]]), ".zip", sep="")
        
        if(!file.exists(ahn_raw_file_path)){
          download.file(ahn_raw_downloadLink, destfile = ahn_rawZip_file_path)
          unzip(ahn_rawZip_file_path, overwrite = TRUE, exdir = ahn_raw_directory)
          file.remove(ahn_rawZip_file_path)
        }
        ahn_sheet_raw <-stack(ahn_raw_file_path)
        ahn_raw_file_paths <- cbind(ahn_raw_file_paths, ahn_raw_file_path)
        proj4string(ahn_sheet_raw)<-CRS("+init=epsg:28992") 
        ahn_raw_crop <- raster::crop(ahn_sheet_raw,surroundingBuffer)
        #plot(ahn_raw_crop)
        # rXs <- c(xmin(ahn_raw_crop), xmax(ahn_raw_crop))
        # rYs <- c(ymin(ahn_raw_crop), ymax(ahn_raw_crop))
        # rawXList <- rbind(rawXList, rXs)
        # rawYList <- rbind(rawYList, rYs)
        indiv_raw_rasters[[r]] <- ahn_raw_crop
      }
      
      print(ahn_raw_raster_filename)
      indiv_raw_rasters$filename <- paste(name_trim, "_", AHN ,"_raw_ahn", '.tif', sep="")
      if(length(bladnrs) > 1){
        indiv_raw_rasters$overwrite <- TRUE
        print("Merging all raw rasters...")
        print(ahn_raw_raster_filename)
        ahn_raw_raster <- do.call(merge, indiv_raw_rasters)
        writeRaster(ahn_raw_raster, filename = ahn_raw_raster_filename, overwrite = TRUE)
        file.remove(paste(name_trim, "_", AHN , "_raw_ahn.tif", sep=""))
        message("Download and merge of raw rasters complete.")
      } else if(length(bladnrs) == 1){
        ahn_raw_raster <- indiv_raw_rasters[[1]]
        writeRaster(ahn_raw_raster, ahn_raw_raster_filename, overwrite = TRUE)
        message("Download of raw rasters complete.")
      }
      if(delete_sheets == TRUE){
        for(fr in 1:length(ahn_raw_file_paths)){
          file.remove(ahn_raw_file_paths[fr])
        }
      }
    } else {
      if(redownload == TRUE){
        redownload_files <- TRUE
        file.remove(ahn_raw_raster_filename)
      } else {
        warning(paste("Cropped raw raster for", name, "already exists and was returned. Please remove it if you want to download it again or set redownload to TRUE.",sep =" "))
        ahn_raw_raster <- raster(paste(ahn_raw_directory, "/", name_trim, "_", AHN ,"_raw_ahn", '.tif', sep=""))
      }
    }
  }
  
  #download terrain ahn of corresponding resolution
  indiv_terrain_rasters <- list()
  if(terrain_ahn == TRUE){
    ahn_terrain_directory <- paste(aws_working_directory, "terrain", sep="/")
    ahn_terrain_raster_filename <- paste(ahn_terrain_directory, "/", name_trim, "_", AHN ,"_terrain_ahn", '.tif', sep="")
    # terrainXList <<- c()
    # terrainYList <<- c()
    if(!file.exists(ahn_terrain_raster_filename)){
      dir.create(paste(aws_working_directory, "terrain", sep="/"), showWarnings = FALSE)
      print(ahn_terrain_directory)
      print(paste("Amount of sheets found:", length(bladnrs), sep=" "))
      ahn_terrain_file_paths <- c()
      for(t in 1:length(bladnrs)){
        #AHN2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_int/i32cn1.tif.zip
        #AHN3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dtm/M_32CN1.ZIP
        if(AHN == "AHN3"){
          tifZip = ".ZIP"
          ahn_terrain_letter <- "M"
          ahn_terrain_naming <- paste0("_dtm/", ahn_terrain_letter,"_")
          ahn_terrain_downloadLink <- paste(ahn_atomFeed_BaseUrl, resolution_name, ahn_terrain_naming,  toupper(bladnrs[[t]]), tifZip, sep="")
          ahn_terrain_file_path <- paste(ahn_terrain_directory, "/m_", tolower(bladnrs[[t]]), ".tif",sep="")
        } else {
          tifZip = ".tif.zip"
          ahn_terrain_letter <- "i"
          ahn_terrain_naming <- paste0("_int/", ahn_terrain_letter)
          ahn_terrain_downloadLink <- paste(ahn_atomFeed_BaseUrl, resolution_name, ahn_terrain_naming,  tolower(bladnrs[[t]]), tifZip, sep="")
          ahn_terrain_file_path <- paste(ahn_terrain_directory, "/", ahn_terrain_letter, tolower(bladnrs[[t]]), ".tif",sep="")
        }
        
        print(ahn_terrain_downloadLink)
        ahn_terrainZip_file_path <- paste(ahn_terrain_directory, "/", ahn_terrain_letter, bladnrs[[t]], ".zip", sep="")
        if(!file.exists(ahn_terrain_file_path)){
          download.file(ahn_terrain_downloadLink, destfile = ahn_terrainZip_file_path)
          unzip(ahn_terrainZip_file_path, overwrite = TRUE, exdir = ahn_terrain_directory)
          file.remove(ahn_terrainZip_file_path)
        }
        ahn_terrain_file_paths <- cbind(ahn_terrain_file_paths, ahn_terrain_file_path)
        
        ahn_sheet_terrain <-stack(ahn_terrain_file_path)
        proj4string(ahn_sheet_terrain)<-CRS("+init=epsg:28992") 
        ahn_terrain_crop<-raster::crop(ahn_sheet_terrain,surroundingBuffer)
        # tXs <- c(xmin(ahn_terrain_crop), xmax(ahn_terrain_crop))
        # tYs <- c(ymin(ahn_terrain_crop), ymax(ahn_terrain_crop))
        # terrainXList <- rbind(terrainXList, tXs)
        # terrainYList <- rbind(terrainYList, tYs)
        indiv_terrain_rasters[[t]] <- ahn_terrain_crop
      }
      
      indiv_terrain_rasters$filename <- paste(name_trim, "_", AHN , "_terrain_ahn", '.tif', sep="")
      if(length(bladnrs) > 1){
        indiv_terrain_rasters$overwrite <- TRUE
        print("Merging all terrain rasters...")
        print(ahn_terrain_raster_filename)
        ahn_terrain_raster <- do.call(merge, indiv_terrain_rasters, envir = )
        writeRaster(ahn_terrain_raster, filename = ahn_terrain_raster_filename, overwrite = TRUE)
        file.remove(paste(name_trim, "_", AHN , "_terrain_ahn.tif", sep=""))
        message("Download and merge of terrain rasters complete.")
      } else if(length(bladnrs) == 1){
        ahn_terrain_raster <- indiv_terrain_rasters[[1]]
        writeRaster(ahn_terrain_raster, ahn_terrain_raster_filename, overwrite = TRUE)
        message("Download of terrain rasters complete.")
      }
      
      if(delete_sheets == TRUE){
        for(ft in 1:length(ahn_terrain_file_paths)){
          file.remove(ahn_terrain_file_paths[ft])
        }
      }
    } else {
      if(redownload == TRUE){
        redownload_files <- TRUE
        file.remove(ahn_terrain_raster_filename)
      } else {
        warning(paste("Cropped terrain raster for", name, "already exists and was returned. Please remove it if you want to download it again or set redownload to TRUE.",sep =" "))
        ahn_terrain_raster <- raster(paste(ahn_terrain_directory, "/", name_trim, "_", AHN ,"_terrain_ahn", '.tif', sep=""))
      }
    }
  }
  
  if(redownload_files == TRUE){
    warning("AHN file(s) already existed and were redownloaded")
    import_single_ahn(df = df, name = name, addition = addition, station_coords = station_coords, X = X, Y = Y, LONLAT = LONLAT, resolution = resolution, radius = radius, raw_ahn = raw_ahn, terrain_ahn = terrain_ahn, AHN3 = AHN3, delete_sheets = delete_sheets, redownload = TRUE)
  } else {
    if(raw_ahn == TRUE & terrain_ahn == TRUE){
      ahn_rasters <- stack(ahn_raw_raster, ahn_terrain_raster)
      names(ahn_rasters) <- c("raw", "terrain")
    } else if(raw_ahn == TRUE & terrain_ahn == FALSE){
      ahn_rasters <- stack(ahn_raw_raster)
      names(ahn_rasters) <- c("raw")
    } else if(raw_ahn == FALSE & terrain_ahn == TRUE){
      ahn_rasters <- stack(ahn_terrain_raster)
      names(ahn_rasters) <- c("terrain")
    } else {
      ahn_rasters <- NULL
    }
    return (ahn_rasters)
  }
  # if(delete_sheets == TRUE){
  #   file.remove(bladIndex_shape_file)
  #   file.remove(paste( bladIndex_shape_filepath, "/", AHN, "_bladIndex", ".shx", sep=""))
  #   file.remove(paste( bladIndex_shape_filepath, "/", AHN, "_bladIndex", ".dbf", sep=""))
  #   file.remove(paste( bladIndex_shape_filepath, "/", AHN, "_bladIndex", ".prj", sep=""))
  # }
}
