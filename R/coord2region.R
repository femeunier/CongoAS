
coord2region <- function(lat,lon,regions = CongoAS::all.basins){

  Nregions <- length(regions)
  Names <- as.character(regions$Name)

  temp.df <- data.frame(lon,lat,value = 1)

  dfr <- rasterFromXYZ(temp.df,
                       crs = "+proj=longlat +ellps=WGS84 +no_defs")

  cdf.all.masked <- data.frame()
  for (iregion in seq(1,Nregions)){
    cRegion <- regions[iregion,]
    cdfr.masked <- raster::mask(raster::crop(dfr, extent(cRegion)),cRegion)

    cdf.masked <- as.data.frame(rasterToPoints(cdfr.masked,xy = TRUE)) %>%
      filter(!is.na(value)) %>% mutate(region = Names[iregion])

    cdf.all.masked <- bind_rows(list(cdf.all.masked,
                                     cdf.masked))
  }

  region2return <- temp.df %>% ungroup() %>% dplyr::select(-value) %>%
    mutate(lat = round(lat*10000)/10000,
           lon = round(lon*10000)/10000) %>%
    left_join(cdf.all.masked %>% ungroup() %>%
                rename(lon = x,
                       lat = y) %>%
                mutate(lat = round(lat*10000)/10000,
                       lon = round(lon*10000)/10000) %>%
                dplyr::select(-value),
              by = c("lon","lat")) %>% pull(region)

  return(region2return)

}
