read.and.filter.ncfiles <- function(ncfiles,
                                    coord.analysis = continent2coord("World"),
                                    var = "cVeg",
                                    aggr = TRUE,
                                    mask.ocean = NULL,
                                    start.year = NULL,
                                    years = NULL,
                                    progressbar = FALSE,
                                    lat.names = c("latitude","lat","lat_FULL"),
                                    lon.names = c("longitude","lon","lon_FULL"),
                                    debug = FALSE){




  df.data.all <- data.frame()

  # Special case, single file

  if (length(ncfiles) == 1){
    df.data <- read.and.filter.ncfile(ncfile = ncfiles,
                                      coord.analysis = coord.analysis,
                                      years = years,
                                      var,
                                      aggr,
                                      debug = debug)

    if (!is.null(mask.ocean)){
      df.data.mask <- mask(df.data,mask.ocean)
      cdf <-  df.data.mask
    } else {
      cdf <-  df.data
    }

    if (!is.null(start.year)){
      cdf <- cdf %>% mutate(year = year + start.year)
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


    if (ifile %in% c(1,2)){
      df.data <- read.and.filter.ncfile(ncfile = ncfiles[ifile],
                                        coord.analysis = coord.analysis,
                                        var,
                                        years = years,
                                        aggr,
                                        debug = debug)
    } else {
      df.data <- read.and.filter.ncfile(ncfile = ncfiles[ifile],
                                        coord.analysis = coord.analysis,
                                        var,
                                        years = years,
                                        aggr,
                                        debug = FALSE)
    }


    if (!is.null(mask.ocean)){
      df.data.mask <- CongoAS::mask(df.data,mask.ocean)
      cdf <-  df.data.mask
    } else {
      cdf <-  df.data
    }

    if (!is.null(start.year)){
      cdf <- cdf %>% mutate(year = year + start.year)
    }


    df.data.all <- bind_rows(list(df.data.all,
                                  cdf))

  }

  if (progressbar){close(pb)}

  return(df.data.all %>%
           ungroup())
}
