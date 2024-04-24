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

df.cVeg <- df.vegFrac <- df.pr <- data.frame()

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

  if (all(file.exists(cVeg.files))){

    cdf.cVeg <- read.and.filter.ncfiles(cVeg.files,
                                  coord.analysis = continent2coord("Tropics"),
                                  var = "cVeg",
                                  aggr = TRUE,
                                  start.year = year(cfiles[["datetime_start"]]))




    if (max(cdf.cVeg$yr) > 2020) stop()

    df.cVeg <- bind_rows(list(df.cVeg,
                              cdf.cVeg %>% mutate(model = models[imodel])))
  } else {
    print(paste("Files",
                cVeg.files[!file.exists(cVeg.files)],"do not exist"))
  }

  pr.files <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/pr",str_replace(basename(cVeg.files),"cVeg_L","pr_A"))

  if (all(file.exists(pr.files))){

    cdf.pr <- read.and.filter.ncfiles(pr.files,
                                        coord.analysis = continent2coord("Tropics"),
                                        var = "pr",
                                        aggr = TRUE,
                                        start.year = year(cfiles[["datetime_start"]]))

    df.pr <- bind_rows(list(df.pr,
                             cdf.pr %>% mutate(model = models[imodel])))
  } else {
    print(paste("Files",
                pr.files[!file.exists(pr.files)],"do not exist"))
  }

  vegFrac.file <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/vegFrac",str_replace(basename(cVeg.files[1]),"cVeg_L","vegFrac_E"))

  if (file.exists(vegFrac.file)){

    cdf.vegFrac <- read.and.filter.ncfiles(vegFrac.file,
                                           coord.analysis = continent2coord("Tropics"),
                                           var = "vegFrac",
                                           aggr = TRUE,
                                           start.year = year(cfiles[["datetime_start"]])[1])




    df.vegFrac <- bind_rows(list(df.vegFrac,
                                 cdf.vegFrac %>% mutate(model = models[imodel])))
  } else {
    print(paste("Files",
                vegFrac.file,"do not exist"))
  }

}

saveRDS(df.cVeg,"./outputs/cVeg.CMIP6.historical.RDS")
# saveRDS(df.pr,"./outputs/pr.CMIP6.historical.RDS")
# saveRDS(df.vegFrac,"./outputs/vegFrac.CMIP6.historical.RDS")

# scp /home/femeunier/Documents/projects/CongoAS/scripts/extract.CMIP6.files.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R



