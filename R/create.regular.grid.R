create.regular.grid <- function(resolution = 0.25,
                                coord.list = continent2coord("Tropics"),
                                format = "df"){

  min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

  if (length(resolution) == 1){
    resolution <- rep(resolution,2)
  }

  regular.grid <- rasterFromXYZ(as.data.frame(xy.grid(x = seq(min.lon.analysis,max.lon.analysis,resolution[1]),
                                                      y = seq(min.lat.analysis,max.lat.analysis,resolution[2]))))

  if (format == "df"){
    return(as.data.frame(rasterToPoints(regular.grid)) %>% rename(lon = x,
                                                                  lat = y))

  } else if (format == "raster"){
    return(regular.grid)
  } else{
    stop("Error of format for regular grid")
  }

}
