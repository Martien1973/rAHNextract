get_dem <- function(AHN, dem, resolution, interpolated = TRUE){
  if(AHN == "AHN1"){
    if(tolower(dem) == "dsm"){
      warning(paste("There is no dsm available for this dataset. Dtm (maaiveld)", resolution, "m was used.", sep = " "))
    }
    info <- paste(AHN, resolution, "m resolution dtm (maaiveld) selected.", sep = " ")
    dem <- ""
  } else if(AHN == "AHN2"){
    if(resolution == 5){
      dem <- ""
      info <- "AHN2 5 m resolution dtm (maaiveld) selected."
    } else if(resolution == 0.5){
      if(tolower(dem) == "dtm"){
        if(interpolated == TRUE){
          dem <- "int"
          info <- "AHN2 0.5 m resolution dtm (maaiveld) interpolated (opgevuld) selected."
        } else if(interpolated == FALSE){
          dem <- "non"
          info <- "AHN2 0.5 m resolution dtm (maaiveld) (niet opgevuld) selected."
        } else {
          stop("No correct interpolated parameter is provided. Please set it to 'TRUE' or 'FALSE'.")
        }
      } else if (tolower(dem) == "dsm"){
        dem <- "ruw"
        info <- "AHN2 0.5 m resolution dsm (ruw) selected."
      } else {
        stop("No correct dem is provided. Please select 'dtm' or 'dsm'.")
      }
    }
  } else if(AHN == "AHN3"){
    if(tolower(dem) != "dtm" && tolower(dem) != "dsm"){
      stop("Provided wrong dem, Please select 'dsm' or 'dtm'.")
    } else {
      dem <- tolower(dem)
      if(dem == "dtm"){
        specs <- "(ruw)"
      } else if(dem == "dsm"){
        specs <- "(maaiveld)"
      }
      info <- paste("AHN3", resolution, "resolution", dem, specs, "selected.", sep = " ")
    }
  }
  message(info)
  return(dem)
}
