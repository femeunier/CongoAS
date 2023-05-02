resample.all.columns <- function(df,
                                 cols2keep = c("value"),
                                 method = "ngb",
                                 resolution = 0.25,
                                 coord.list = continent2coord("Tropics")){

  regular.grid <- create.regular.grid(resolution = resolution,
                                      coord.list = coord.list,
                                      format = "raster")


  lat <- df[,"lat"]
  lon <- df[,"lon"]

  if (length(unique(diff(sort(unique(lat))))) > 1){
    res <- max(c(diff(sort(unique(lat))),diff(sort(unique(lon)))))
    cdf.rst <- raster(SpatialPixelsDataFrame(points = cdf[c("lon","lat")],
                                          data = cdf[cols2keep],
                                          tolerance = res/10))
  } else {
    cdf.rst <- rasterFromXYZ(df[,c("lon","lat",cols2keep)])
  }



  cdf.rst.rspld <- resample(cdf.rst,regular.grid,method = method)

  cdf.df.rspld <- as.data.frame(rasterToPoints(cdf.rst.rspld)) %>% rename(lon = x,
                                                                          lat = y)

  return(cdf.df.rspld)
}
