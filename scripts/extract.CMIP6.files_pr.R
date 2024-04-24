rm(list = ls())

library(epwshiftr)
library(dplyr)
library(CongoAS)
library(RNetCDF)
library(reshape2)
library(stringr)
library(lubridate)
library(stringr)

models.with.vegetation <- init_cmip6_index(activity = "CMIP",
                                           variable = 'cVeg',
                                           frequency = 'mon',
                                           experiment = c("historical"),
                                           source = NULL,
                                           variant = NULL,
                                           replica = FALSE,
                                           latest = TRUE,
                                           resolution = NULL,
                                           limit = 10000L,
                                           data_node = NULL)

files2download <- models.with.vegetation %>% filter(member_id == "r1i1p1f1")
models <- unique(files2download$source_id)

df.pr <- data.frame()

for (imodel in seq(1,length(models))){

  print("=========================")
  print(paste("==",models[imodel],"=="))

  cfiles <- files2download %>% filter(source_id == models[imodel])

  if (sum(year(cfiles[["datetime_start"]]) == 1850) > 1){
    cfiles <- cfiles[1:(which(year(cfiles[["datetime_start"]]) == 1850)[2] -1),]
  }

  if (!any(year(cfiles[["datetime_start"]]) == 1850)){
    next()
  }

  cVeg.files <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/cVeg",basename(cfiles$file_url))
  pr.files <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/pr",str_replace(basename(cVeg.files),"cVeg_L","pr_A"))

  if (all(file.exists(pr.files))){

    cdf.pr <- read.and.filter.ncfiles(pr.files,
                                      coord.analysis = continent2coord("Africa"),
                                      var = "pr",
                                      aggr = FALSE,
                                      start.year = year(cfiles[["datetime_start"]])) %>%
      # filter(yr <= 1860) %>%
      filter(yr <= 2014,yr > 2004) %>%
      dplyr::select(lat,lon,time,pr,time0,yr)

    df.pr <- bind_rows(list(df.pr,
                            cdf.pr %>% mutate(model = models[imodel])))
  } else {
    print(paste("Files",
                pr.files[!file.exists(pr.files)],"do not exist"))
  }

}

saveRDS(df.pr,"./outputs/pr.CMIP6.historical_end.RDS")

# scp /home/femeunier/Documents/projects/CongoAS/scripts/extract.CMIP6.files_pr.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R



