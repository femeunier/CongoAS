rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

scenarios <- c("ssp126","ssp245","ssp370","ssp585")
# scenarios <- c("ssp245")

Gridarea <- readRDS("./outputs/Gridarea.RDS") %>%
  ungroup() %>%
  rename(land = value)

dir <- "./outputs/"

models <- unique(sapply(strsplit(list.files(dir,
                                            pattern = "CMIP6.monthly.nbp.global.*_rspld.RDS$"),split = "\\."),
                        "[",6))

# models <- "GISS-E2-2-G"

timeseries.CMIP6 <-
  timeseries.CMIP6.tropics <-
  timeseries.CMIP6.all <-
  all.maps <-
  data.frame()

for (cmodel in models){

  print(cmodel)

  history.cfile <- paste0(dir,"CMIP6.monthly.nbp.global.historical",".",cmodel,".r1i1p1f1_rspld.RDS")

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
             variant = case_when(is.na(variant) ~ "r1i1p1f1",
                                 TRUE ~ variant),
             model = cmodel) %>%
      left_join(Gridarea,
                by = c("lon","lat"))

  }



  for (cscenario in scenarios){

    print(paste0(cmodel," - ",cscenario))

    cfile <- paste0(dir,"CMIP6.monthly.nbp.global.",cscenario,".",cmodel,".r1i1p1f1_rspld.RDS")

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
                                variant = case_when(is.na(variant) ~ "r1i1p1f1",
                                                    TRUE ~ variant),
                                model = cmodel))


  }

  all.maps <- bind_rows(all.maps,
                        A.all %>%
    filter((year %in% c(1985:2014) & scenario == "historical") |
             (year %in% c(2015:2100))) %>%
    mutate(timing = case_when(year %in% c(1985:2014) ~ "historical",
                              year %in% c(2071:2100) ~ "long_future",
                              year %in% c(2041:2070) ~ "mid_future",
                              year %in% c(2015:2040) ~ "near_future")) %>%
    group_by(timing,scenario,model,variant,lon,lat) %>%
    summarise(nbp.m = mean(nbp),
              area = mean(area),
              .groups = "keep"))

  timeseries.CMIP6 <- bind_rows(timeseries.CMIP6,
                                A.all %>%
                                  group_by(year,scenario,variant,month,model) %>%
                                  summarise(nbp.m = mean(nbp),
                                            nbp.w.m = weighted.mean(nbp,area),
                                            .groups = "keep"))

  timeseries.CMIP6.tropics <- bind_rows(timeseries.CMIP6.tropics,
                                        A.all %>%
                                          filter(abs(lat) <= 23.5) %>%
                                          group_by(year,scenario,variant,month,model) %>%
                                          summarise(nbp.m = mean(nbp),
                                                    nbp.w.m = weighted.mean(nbp,area),
                                                    .groups = "keep"))

  timeseries.CMIP6.all <- bind_rows(timeseries.CMIP6.all,
                                        A.all %>%
                                      mutate(region = case_when(abs(lat) <= 23.5 ~ "Tropics",
                                                                abs(lat) <= 50 ~ "Temperate",
                                                                TRUE ~ "Boreal")) %>%
                                          group_by(region,year,scenario,variant,month,model) %>%
                                          summarise(nbp.m = mean(nbp),
                                                    nbp.w.m = weighted.mean(nbp,area),
                                                    .groups = "keep"))

}


saveRDS(timeseries.CMIP6,
        "./outputs/timeseries.CMIP6.World.rspld.RDS")

saveRDS(timeseries.CMIP6.tropics,
        "./outputs/timeseries.CMIP6.tropics.rspld.RDS")

saveRDS(timeseries.CMIP6.all,
        "./outputs/timeseries.CMIP6.all.rspld.RDS")

saveRDS(all.maps,
        "./outputs/all.maps.rspld.RDS")


# scp /home/femeunier/Documents/projects/CongoAS/scripts/compute.time.series.CMIP6.nbp.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

