read.and.filter.ncfiles <- function(ncfiles,
                                   coord.analysis,
                                   var = "cVeg",
                                   aggr = TRUE,
                                   mask.ocean = NULL,
                                   progressbar = FALSE){


  df.data.all <- data.frame()
  yr.init <- 0

  # Special case, single file

  if (length(ncfiles) == 1){
    df.data <- read.and.filter.ncfile(ncfile = ncfiles,
                                      coord.analysis = coord.analysis[[1]],
                                      var,
                                      aggr)

    if (!is.null(mask.ocean)){
      df.data.mask <- mask(df.data,mask.ocean)
      cdf <-  df.data.mask %>% mutate(yr = yr + yr.init)
    } else {
      cdf <-  df.data %>% mutate(yr = yr + yr.init)
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
                                      coord.analysis = coord.analysis[[1]],
                                      var,
                                      aggr)

    if (!is.null(mask.ocean)){
      df.data.mask <- mask(df.data,mask.ocean)
      cdf <-  df.data.mask %>% mutate(yr = yr + yr.init)
    } else {
      cdf <-  df.data %>% mutate(yr = yr + yr.init)

    }

    df.data.all <- bind_rows(list(df.data.all,
                                  cdf))

    yr.init <- yr.init + (max(cdf$yr) + 1)
  }

  if (progressbar){close(pb)}

  return(df.data.all)
}
