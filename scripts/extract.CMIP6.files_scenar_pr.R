rm(list = ls())

library(epwshiftr)
library(dplyr)
library(CongoAS)
library(RNetCDF)
library(reshape2)
library(stringr)
library(lubridate)
library(stringr)
library(ncdf4)
library(ncdf4.helpers)
library(lubridate)

# activities = "CMIP"
# experiments = "1pctCO2"
# years.min = 1900
# years.max = 2020
# var.names <- c("cVeg")

# activities = c("CMIP",rep("ScenarioMIP",4))
# experiments = c("historical","ssp126","ssp245","ssp370","ssp585")

activities = c("CMIP",rep("ScenarioMIP",4))
experiments = c("historical","ssp126","ssp245","ssp370","ssp585")
years.min = c(1984,rep(2070,4))
years.max = c(2020,rep(2100,4))

# activities = c(rep("ScenarioMIP",4))
# experiments = c("ssp245","ssp245","ssp370","ssp585")
# years.min = c(rep(2070,4))
# years.max = c(rep(2100,4))

var.names <- c("tasmin","pr","tasmax")

aggr = FALSE

for (i in seq(1,length(var.names))){

  var.name <- var.names[i]
  print(paste("-",var.name))

  for (j in seq(1,length(experiments))){

    c.years <- years.min[j]:years.max[j]

    experiment <- experiments[j]
    activity <- activities[j]

    year.min <- years.min[j]
    year.max <- years.max[j]

    print(paste("--",experiment))

    all.files <- init_cmip6_index(activity = activity,
                                  variable = var.name,
                                  frequency = 'mon',
                                  experiment = c(experiment),
                                  source = NULL,
                                  variant = NULL,
                                  replica = FALSE,
                                  latest = TRUE,
                                  resolution = NULL,
                                  limit = 10000L,
                                  data_node = NULL)

    files2download <- all.files %>% filter(member_id == "r1i1p1f1")

    models <- unique(files2download$source_id)

    df.pr <- data.frame()

    for (imodel in seq(1,length(models))){

      print("=========================")
      print(paste("==",models[imodel],"==",sprintf("%.2f",100*imodel/length(models)),"% =="))

      cfiles <- files2download %>%
        filter(source_id == models[imodel]) %>%
        rowwise() %>%
        filter(any(year(datetime_start):year(datetime_end) %in% c.years))

      init.file <- cfiles[1]

      # stop()
      # if (sum(year(cfiles[["datetime_start"]]) == year(init.file[["datetime_start"]])) > 1){
      #   cfiles <- cfiles[1:(which(year(cfiles[["datetime_start"]]) == year(init.file[["datetime_start"]]))[2] -1),]
      # }
      #
      # if (!any(year(cfiles[["datetime_start"]]) == year(init.file[["datetime_start"]]))){
      #   next()
      # }

      # cVeg.files <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/",experiment,"cVeg",basename(cfiles$file_url))
      pr.files <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/",experiment,var.name,basename(cfiles$file_url))

      if (all(file.exists(pr.files))){

        tmp <- tryCatch(read.and.filter.ncfiles(pr.files,
                                                coord.analysis = continent2coord("Tropics"),
                                                var = var.name,
                                                years = c.years,
                                                aggr = aggr),
                 error = function(e) NULL)

        if(is.null(tmp)){
          print(paste("Skipping",models[imodel]))
          next()
        }


        tmp <- tmp %>% dplyr::filter(lat >= -20, lat <= 15,
                                     lon >= -85, lon <= 45)

        if (aggr){
          cdf.pr <- tmp %>%
            filter(year <= year.max,year > year.min) %>%
            dplyr::select(lat,lon,year,month,!!var.name)
        } else {
          cdf.pr <- tmp %>%
            filter(year <= year.max,year > year.min) %>%
            dplyr::select(lat,lon,year,month,!!var.name)
        }


        df.pr <- bind_rows(list(df.pr,
                                cdf.pr %>%
                                  # group_by(lon,lat,month) %>%
                                  # summarise(!!var.name := mean(get(var.name)),
                                  #           .groups = "keep") %>%
                                  mutate(model = models[imodel])))
      } else {
        print(paste("Files",
                    pr.files[!file.exists(pr.files)],"do not exist"))
      }
    }

    saveRDS(df.pr,
            file.path("./outputs/",paste0(var.name,".CMIP6.scenar.",experiment,"_yr.min_",year.min,"_yr.max_",year.max,".RDS")))

  }
}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/extract.CMIP6.files_scenar_pr.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R



