rm(list = ls())

library(CongoAS)
library(purrr)
library(dplyr)
library(tidyr)
library(RNetCDF)
library(stringr)
library(lubridate)
library(reshape2)

maindir <- "/data/gent/vo/000/gvo00074/felicien/CMIP6"
vars <- c("tas","pr","tasmin","tasmax")
scenarios <- c("historical","ssp126","ssp245","ssp370","ssp585")

all.df <- data.frame()
all.files <- data.frame()

for (scenario in scenarios){
  for (var in vars){
    cdir <- file.path(maindir,scenario,var)
    files <- list.files(cdir)
    cfiles <- CMIP6name2attributes(CMIP6name = files)

    all.files <- bind_rows(list(all.files,
                                cfiles))

    cdf <- cfiles %>% dplyr::select(var,model,scenario) %>%
      distinct()

    all.df <- bind_rows(list(all.df,
                             cdf))

  }
}

all.df.wide <- all.df %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = var,
              values_from = present,
              values_fill = FALSE)


models2analyze <- all.df.wide %>%
  mutate(all.present = tas & pr & tasmin & tasmax) %>%
  dplyr::filter(all.present)

models.scenarios <- models2analyze %>%
  dplyr::select(model,scenario,all.present) %>%
  pivot_wider(names_from = scenario,
              values_from = all.present,
              values_fill = FALSE)

models.scenarios.2anal <- models.scenarios %>%
  mutate(all.scenarios = historical & ssp126 & ssp245 & ssp370 & ssp585) %>%
  dplyr::filter(all.scenarios)

models <- unique(models.scenarios.2anal[["model"]])

df.OP <- data.frame()

for (cscenario in scenarios){

  print(cscenario)

  for (cvar in vars){

    print(paste0("- ",cvar))

    for (cmodel in models){

      print(paste0("-- ",cmodel))

      csef.of.files <- all.files %>%
        dplyr::filter(model == cmodel,
                      var == cvar,
                      scenario == cscenario) %>%
        dplyr::filter(timestep == "Amon")

      file.names <- attributes2CMIP6name(csef.of.files)

      files2read <- file.path(maindir,cscenario,cvar,file.names)

      if (all(file.exists(files2read))){
        tmp <- read.and.filter.ncfiles(ncfiles = files2read,
                                       coord.analysis = continent2coord("Africa"),
                                       var = cvar,
                                       aggr = FALSE) %>%
          mutate(yr = yr + yr.origin) %>%
          distinct() %>%
          filter(time > 0)


        tmp.filtered <- tmp %>%
          ungroup() %>%
          dplyr::filter(yr %in% c(min(yr):(min(yr)+29),(max(yr)-30):(max(yr) - 1))) %>%
          mutate(timing = case_when(yr %in% c(min(yr):(min(yr)+29)) ~ "beginning",
                                    TRUE ~ "end"))

        if (nrow(tmp.filtered %>%
                 group_by(lat,lon,yr) %>%
                 summarise(N = length(yr),
                           .groups = "keep") %>%
                 dplyr::filter(N !=12)) > 0){
          warning(paste("Wrong month numbers:", files2read))
          next()
        }

        tmp.filtered.month <- tmp.filtered %>%
          rename(value := !!cvar) %>%
          group_by(lat,lon,yr) %>%
          mutate(month = 1:12) %>%
          group_by(lat,lon,month,timing) %>%
          summarise(value.m = mean(value),
                    .groups = "keep")

        df.OP <- bind_rows(list(df.OP,
                                tmp.filtered.month %>% mutate(scenario = cscenario,
                                                              var = cvar,
                                                              model = cmodel)))

      }
    }
  }
}


saveRDS(df.OP,
        "./outputs/Climate.change.CMIP6.RDS")

# scp /home/femeunier/Documents/projects/CongoAS/scripts/extract.CMIP6.climate.files.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


