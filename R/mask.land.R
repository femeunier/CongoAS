mask.land <- function(lon,lat){

  data("wrld_simpl")

  cdf <- data.frame(lon,lat) %>% mutate(ID = 1:length(lon))


  if (length(unique(diff(unique(lat)))) > 1){
    res <- max(c(diff(unique(lat)),diff(unique(lon))))
    temp <- raster(SpatialPixelsDataFrame(points = cdf[c("lon","lat")],
                                          data = cdf["ID"],
                                          tolerance = res/10))
  } else {
    temp <- rasterFromXYZ(cdf)
  }

  masked <- as.data.frame(rasterToPoints(raster::mask(temp,
                                                      wrld_simpl))) %>%
    rename(lon = x,
           lat = y) %>% mutate(is.land = 1)

  cdf.masked <- cdf %>%
    left_join(masked,
              by = c("ID")) %>%
    mutate(is.land = case_when(is.land == 1 ~ 1,
                               TRUE ~ 0))


  # ggplot(data = world) +
  #   geom_raster(data = cdf.masked,
  #               aes(x = lon, y = lat,fill = is.land),na.rm = TRUE) +        # kgC/m2
  #   geom_sf(fill = NA) +
  #   # coord_sf(xlim = c(-15, 50),
  #   #          ylim = c(-15, 20),
  #   #          expand = FALSE) +
  #   labs(x = "",y = "") +
  #   theme_bw()

    return(as.vector(cdf.masked[["is.land"]]))
}
