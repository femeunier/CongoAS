read.and.filter.ncfile <- function(ncfile,
                                   coord.analysis,
                                   var = "cVeg",
                                   aggr = TRUE,
                                   years = NULL,
                                   lat.names = c("latitude","lat","lat_FULL"),
                                   lon.names = c("longitude","lon","lon_FULL"),
                                   debug = FALSE){


  # ncfile <- "/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/pr/pr_Amon_IITM-ESM_historical_r1i1p1f1_gn_185001-185912.nc"
  # coord.analysis = continent2coord("Africa")
  # var = "pr"
  # aggr = FALSE
  # lat.names = c("latitude","lat","lat_FULL")
  # lon.names = c("longitude","lon","lon_FULL")
  # debug = FALSE

  nc <- open.nc(ncfile)
  times <- var.get.nc(nc,"time")
  tunits <- att.get.nc(nc, 'time','units')

  if (debug){
    print(ncfile)
    print(paste0("- ",tunits))
  }

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
  date.origin <- (as.POSIXct(times[1]*fac2,origin = origin))

  ncfilin <- nc_open(ncfile)
  dates <- nc.get.time.series(f = ncfilin,
                              time.dim.name = "time")
  nc_close(ncfilin)


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
    if (!is.null(years)){
      year.pos = which(year(dates) %in% years)
      cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,1,year.pos]
    } else{
      cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,1,]
    }
  } else {
    if (!is.null(years)){
      year.pos = which(year(dates) %in% years)
      cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,years]
    } else{
      cVar <- var.get.nc(nc,var)[lons.pos,lats.pos,]
    }
  }

  close.nc(nc)

  df <- melt(cVar) %>%
    mutate(lon = (lons[lons.pos])[Var1],
           lat = (lats[lats.pos])[Var2],
           time = dates[Var3],
           # time = times[Var3],
           cVar = value) %>%
    dplyr::select(lat,lon,time,cVar) %>%
    mutate(lon = case_when(lon > 180 ~ (lon - 360),
                           TRUE ~ lon)) %>%
    filter(lon  >= coord.analysis[[1]][1], lon <= coord.analysis[[1]][2],
           lat >= coord.analysis[[1]][3], lat <= coord.analysis[[1]][4]) %>%
    mutate(time0 = time - min(time),
           year = year(time),
           month = month(time),
           day = day(time)
           # yr = floor(time0/fac)
           ) %>%
    dplyr::select(-time) %>%
    group_by(lat, lon, year)

  if (aggr){
    df <- df %>% summarise(cVar = mean(cVar),
                           .groups = "keep")
  }
  return(df %>% ungroup() %>%
           rename(!!var := "cVar"))
         # %>% mutate(date.origin = date.origin)
}
