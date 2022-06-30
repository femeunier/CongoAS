rm(list = ls())

library(epwshiftr)
library(dplyr)
library(CongoAS)
library(RNetCDF)
library(reshape2)

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

for (imodel in seq(1,length(models))){

  cfiles <- files2download %>% filter(source_id == models[imodel])
  files.path <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/cVeg",basename(cfiles$file_url))

  if (all(file.exists(files.path))){
    df <- read.and.filter.ncfiles(files.path,
                                  continent2coord("Tropics")[[1]],
                                  var = "cVeg",
                                  aggr = TRUE)

    stop()
  }
}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/extract.CMIP6.files.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R



