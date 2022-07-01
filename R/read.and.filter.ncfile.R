read.and.filter.ncfile <- function(ncfile,
                                   coord.analysis,
                                   var = "cVeg",
                                   aggr = TRUE,
                                   yr.rel = NULL){

  nc <- open.nc(ncfile)
  times <- var.get.nc(nc,"time")
  tunits <- att.get.nc(nc, 'time','units')
  tmp.date <- str_split(tunits," ")[[1]]
  origin <- as.Date(paste(tmp.date[3],tmp.date[4]))

  if (tmp.date[1] == "seconds"){
    fac = 365*86400
    fac2 = 1
  } else if (tmp.date[1] == "days"){
    fac = 365
    fac2 = 86400
  } else{
    print(tmp.date)
    error()
  }


  yr.origin <- year(as.POSIXct(times*fac2,origin = origin))

  lats <- var.get.nc(nc,"lat")
  lats.pos <- (lats >= coord.analysis[[1]][3]) & (lats <= coord.analysis[[1]][4])

  lons <- var.get.nc(nc,"lon")
  lons[lons > 180] <- lons[lons > 180] - 360
  lons.pos <- (lons >= coord.analysis[[1]][1]) & (lons <= coord.analysis[[1]][2])

  cVar <- var.get.nc(nc,var)

  if (length(dim(cVar)) == 4){
    if (!is.null(yr.rel)){
      cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,1,yr.rel]
    } else{
      cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,1,]
    }
  } else {
    if (!is.null(yr.rel)){
      cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,yr.rel]
    } else{
      cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,]
    }
  }

  close.nc(nc)

  df <- melt(cVar) %>%
    mutate(lon = (lons[lons.pos])[Var1],
           lat = (lats[lats.pos])[Var2],
           time = times[Var3],
           cVar = value) %>%
    dplyr::select(lat,lon,time,cVar) %>%
    mutate(lon = case_when(lon > 180 ~ (lon -360),
                           TRUE ~ lon)) %>%
    filter(lon  >= coord.analysis[1], lon <= coord.analysis[2],
           lat >= coord.analysis[3], lat <= coord.analysis[4]) %>%
    mutate(time0 = time - min(time),
           yr = floor(time0/fac)) %>%
    group_by(lat, lon, yr)

  if (aggr){
    df <- df %>% summarise(cVar = mean(cVar),
                           .groups = "keep")
  }
  return(df %>% ungroup() %>%
           rename(!!var := "cVar") %>%
           mutate(yr.origin = yr.origin)
  )
}
