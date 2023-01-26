read.and.filter.ncfiles <- function(ncfiles,
                                    coord.analysis = continent2coord("World"),
                                    var = "cVeg",
                                    aggr = TRUE,
                                    mask.ocean = NULL,
                                    start.year = NULL,
                                    progressbar = FALSE,
                                    yr.rel = NULL,
                                    lat.names = c("latitude","lat","lat_FULL"),
                                    lon.names = c("longitude","lon","lon_FULL"),
                                    debug = FALSE){


  df.data.all <- data.frame()
  yr.init <- 0

  # Special case, single file

  if (length(ncfiles) == 1){
    df.data <- read.and.filter.ncfile(ncfile = ncfiles,
                                      coord.analysis = coord.analysis,
                                      var,
                                      aggr,
                                      yr.rel)

    if (!is.null(mask.ocean)){
      df.data.mask <- mask(df.data,mask.ocean)
      cdf <-  df.data.mask
    } else {
      cdf <-  df.data %>% mutate(yr = yr + yr.init)
    }

    if (!is.null(start.year)){
      cdf <- cdf %>% mutate(yr = yr + start.year)
    }

    df.data.all <- bind_rows(list(df.data.all,
                                  cdf))

    return(df.data.all)

  }

  if (progressbar){
    pb = txtProgressBar(min = 0, max = length(ncfiles), initial = 0)
  }


  for (ifile in seq(1,length(ncfiles))){

    if (progressbar){setTxtProgressBar(pb,ifile)}

    df.data <- read.and.filter.ncfile(ncfile = ncfiles[ifile],
                                      coord.analysis = coord.analysis,
                                      var,
                                      aggr)

    if (!is.null(mask.ocean)){
      df.data.mask <- mask(df.data,mask.ocean)
      cdf <-  df.data.mask
    } else {
      cdf <-  df.data
    }

    if (!is.null(start.year)){
      cdf <- cdf %>% mutate(yr = yr + start.year[ifile])
    }


    df.data.all <- bind_rows(list(df.data.all,
                                  cdf))

    yr.init <- (max(cdf$yr) + 1)
  }

  if (progressbar){close(pb)}

  return(df.data.all %>%
           ungroup() %>%
           mutate(yr.origin.all = min(yr.origin)))
}
