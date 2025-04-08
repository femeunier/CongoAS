rm(list = ls())

library(CongoAS)
library(purrr)
library(dplyr)
library(tidyr)
library(RNetCDF)
library(stringr)
library(lubridate)
library(reshape2)
library(ncdf4)
library(ncdf4.helpers)

maindir <- "/data/gent/vo/000/gvo00074/felicien/CMIP6"
# vars <- c("tas","pr","tasmin","tasmax")
vars <- c("tas","pr")
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

    cdf <- cfiles %>%
      dplyr::select(var,model,scenario) %>%
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
  mutate(all.present = tas & pr) %>%
  dplyr::filter(all.present)

models.scenarios <- models2analyze %>%
  dplyr::select(model,scenario,all.present) %>%
  pivot_wider(names_from = scenario,
              values_from = all.present,
              values_fill = FALSE)

models.scenarios.2anal <- models.scenarios %>%
  mutate(all.scenarios = historical & ssp126 & ssp245 & ssp370 & ssp585) #%>%
# dplyr::filter(all.scenarios)

models <- rev(unique(models.scenarios.2anal[["model"]]))

overwrite = FALSE

for (cmodel in models){

  print(cmodel)


  for (cscenario in scenarios){

    print(paste0("- ",cscenario))

    OP.file <- file.path("./outputs/",paste0("Times.series.",cmodel,
                                             ".",cscenario,".RDS"))

    df.OP <- data.frame()

    if (file.exists(OP.file) & !overwrite) next()

    for (cvar in vars){

      print(paste0("-- ",cvar))

      csef.of.files <- all.files %>%
        dplyr::filter(model == cmodel,
                      var == cvar,
                      scenario == cscenario) %>%
        dplyr::filter(timestep == "Amon")

      file.names <- attributes2CMIP6name(csef.of.files)

      files2read <- file.path(maindir,cscenario,cvar,file.names)

      if (all(file.exists(files2read))){
        tmp <- tryCatch(read.and.filter.ncfiles(ncfiles = files2read,
                                                coord.analysis = continent2coord("World"),
                                                var = cvar,
                                                years = 1985:2100,
                                                aggr = FALSE) %>%
                          ungroup() %>%
                          distinct(),
                        error = function(err){NULL})

        if (is.null(tmp)) next()


        # stop()
        # tmp.filtered <- tmp %>%
        #   ungroup() %>%
        #   dplyr::filter(yr %in% c(min(yr):(min(yr)+29),(max(yr)-30):(max(yr) - 1))) %>%
        #   mutate(timing = case_when(yr %in% c(min(yr):(min(yr)+29)) ~ "beginning",
        #                             TRUE ~ "end"))

        # if (nrow(tmp %>%
        #          group_by(lat,lon,year) %>%
        #          summarise(N = length(year),
        #                    .groups = "keep") %>%
        #          dplyr::filter(N !=12)) > 0){
        #   warning(paste("Wrong month numbers:", files2read))
        #   next()
        # }

        tmp <- tmp %>%
          rename(value := !!cvar) %>%
          # group_by(lat,lon,year) %>%
          # mutate(month = 1:12) %>%
          group_by(lat,lon,month,year) %>%
          summarise(value = mean(value,na.rm = TRUE),
                    .groups = "keep")

        df.OP <- bind_rows(df.OP,
                           tmp %>%
                             mutate(scenario = cscenario,
                                    var = cvar,
                                    model = cmodel))

      }
    }

    if (nrow(df.OP) > 0){
      saveRDS(df.OP,
              OP.file)
    }
  }
}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/extract.CMIP6.climate_Quan.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


