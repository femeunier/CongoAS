read.grid <- function(ncfile,
                      coord.analysis,
                      var = "cVeg",
                      lat.names = c("latitude","lat","lat_FULL"),
                      lon.names = c("longitude","lon","lon_FULL")){


  nc <- open.nc(ncfile)

  lats <- NULL ; i = 1
  while(is.null(lats) & i <= length(lat.names)){
    lats <- tryCatch(suppressMessages(var.get.nc(nc,lat.names[i])),
                     error = function(e) NULL)
    i = i +1
  }

  lats.pos <- (lats >= coord.analysis[[1]][3]) & (lats <= coord.analysis[[1]][4])


  lons <- NULL ; i = 1
  while(is.null(lons) & i <= length(lon.names)){
    lons <- tryCatch(suppressMessages(var.get.nc(nc,lon.names[i])),
                     error = function(e) NULL)
    i = i +1
  }
  lons[lons > 180] <- lons[lons > 180] - 360
  lons.pos <- (lons >= coord.analysis[[1]][1]) & (lons <= coord.analysis[[1]][2])

  cVar <- var.get.nc(nc,var)

  if (length(dim(cVar)) == 4){

    cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,1,1]
  } else {

    cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,1]
  }

  close.nc(nc)

  df <- melt(cVar) %>%
    mutate(lon = (lons[lons.pos])[Var1],
           lat = (lats[lats.pos])[Var2],
           cVar = value) %>%
    dplyr::select(lat,lon,time,cVar) %>%
    mutate(lon = case_when(lon > 180 ~ (lon - 360),
                           TRUE ~ lon)) %>%
    filter(lon  >= coord.analysis[[1]][1], lon <= coord.analysis[[1]][2],
           lat >= coord.analysis[[1]][3], lat <= coord.analysis[[1]][4]) %>%
    group_by(lon,lat)

  return(df)

}
