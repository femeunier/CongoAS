rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

scenarios <- c("rcp26","rcp45","rcp60","rcp85")
# scenarios <- c("ssp245")

Gridarea <- readRDS("./outputs/Gridarea.RDS") %>%
  ungroup() %>%
  rename(land = value)

dir <- "./outputs/"

models <- unique(sapply(strsplit(list.files(dir,
                                            pattern = "CMIP5.monthly.nbp.global.*_rspld.RDS$"),split = "\\."),
                        "[",6))

# models <- "GISS-E2-2-G"

timeseries.CMIP5 <-
  timeseries.CMIP5.tropics <-
  timeseries.CMIP5.all <-
  all.maps <-
  data.frame()

for (cmodel in models){

  print(cmodel)

  history.cfile <- paste0(dir,"CMIP5.monthly.nbp.global.historical",".",cmodel,".r1i1p1f1_rspld.RDS")

  if (!file.exists(history.cfile)){
    print(paste(history.cfile, "doesn't exist"))

    A.all <- data.frame()

  } else{

    A.history <- readRDS(history.cfile)

    A.all <- A.history %>%
      rename(nbp = value) %>%
      ungroup() %>%
      filter(!is.na(nbp)) %>%
      mutate(scenario = "historical",
             variant = "r1i1p1f1",
             model = cmodel) %>%
      left_join(Gridarea,
                by = c("lon","lat"))

  }



  for (cscenario in scenarios){

    print(paste0(cmodel," - ",cscenario))

    cfile <- paste0(dir,"CMIP5.monthly.nbp.global.",cscenario,".",cmodel,".r1i1p1f1_rspld.RDS")

    if (!file.exists(cfile)){
      print(paste(cfile, "doesn't exist"))
      next()
    }

    A <- readRDS(cfile)

    A.all <- bind_rows(A.all,
                       bind_rows(A.history,
                                 A) %>%
                         left_join(Gridarea,
                                   by = c("lon","lat")) %>%
                         rename(nbp = value) %>%
                         ungroup() %>%
                         filter(!is.na(nbp)) %>%
                         mutate(scenario = cscenario,
                                variant = "r1i1p1f1",
                                model = cmodel))


  }

  all.maps <- bind_rows(all.maps,
                        A.all %>%
                          filter((year %in% c(1976:2005) & scenario == "historical") |
                                   (year %in% c(2006:2100))) %>%
                          mutate(timing = case_when(year %in% c(1976:2005) ~ "historical",
                                                    year %in% c(2071:2100) ~ "long_future",
                                                    year %in% c(2041:2070) ~ "mid_future",
                                                    year %in% c(2011:2040) ~ "near_future")) %>%
                          group_by(timing,scenario,model,variant,lon,lat) %>%
                          summarise(nbp.m = mean(nbp),
                                    area = mean(area),
                                    .groups = "keep"))

  timeseries.CMIP5 <- bind_rows(timeseries.CMIP5,
                                A.all %>%
                                  group_by(year,scenario,variant,month,model) %>%
                                  summarise(nbp.m = mean(nbp),
                                            nbp.w.m = weighted.mean(nbp,area),
                                            .groups = "keep"))

  timeseries.CMIP5.tropics <- bind_rows(timeseries.CMIP5.tropics,
                                        A.all %>%
                                          filter(abs(lat) <= 23.5) %>%
                                          group_by(year,scenario,variant,month,model) %>%
                                          summarise(nbp.m = mean(nbp),
                                                    nbp.w.m = weighted.mean(nbp,area),
                                                    .groups = "keep"))

  timeseries.CMIP5.all <- bind_rows(timeseries.CMIP5.all,
                                    A.all %>%
                                      mutate(region = case_when(abs(lat) <= 23.5 ~ "Tropics",
                                                                abs(lat) <= 50 ~ "Temperate",
                                                                TRUE ~ "Boreal")) %>%
                                      group_by(region,year,scenario,variant,month,model) %>%
                                      summarise(nbp.m = mean(nbp),
                                                nbp.w.m = weighted.mean(nbp,area),
                                                .groups = "keep"))

}


saveRDS(timeseries.CMIP5,
        "./outputs/timeseries.CMIP5.World.rspld.RDS")

saveRDS(timeseries.CMIP5.tropics,
        "./outputs/timeseries.CMIP5.tropics.rspld.RDS")

saveRDS(timeseries.CMIP5.all,
        "./outputs/timeseries.CMIP5.all.rspld.RDS")

saveRDS(all.maps,
        "./outputs/all.maps.rspld.CMIP5.RDS")


# scp /home/femeunier/Documents/projects/CongoAS/scripts/compute.time.series.CMIP5.nbp.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

