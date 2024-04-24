rm(list = ls())

library(CongoAS)
library(purrr)
library(dplyr)
library(tidyr)
library(RNetCDF)
library(ncdf4)
library(stringr)
library(lubridate)
library(reshape2)
library(ncdf4.helpers)

maindir <- "/data/gent/vo/000/gvo00074/felicien/CMIP6"
vars <- c("pr","tas","tasmin","tasmax")
scenarios = c("historical","ssp126","ssp245","ssp370","ssp585")

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
models <- models[!(models %in% c("ICON-ESM-LR","IITM-ESM"))]
# models <- "ICON-ESM-LR"


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
                                       aggr = FALSE,
                                       debug = FALSE) %>%
          mutate(date = as.Date(paste0(year,"/",sprintf("%02d",month),"/",day)))

        # times <- tmp.filtered$yr.origin + tmp.filtered$time/365
        #
        # if (length(unique(times))%%12 != 0){
        #   warning(paste("Wrong month numbers:", files2read))
        #   next()
        # } else{
        #   Nyears = length(unique(times))%/%12
        # }


        tmp.filtered <- tmp %>%
          dplyr::filter(lat >= -20, lat <= 15,
                        lon >= -15, lon <= 50) %>%
          dplyr::filter(year %in% c(min(year):(min(year)+29),
                                    1961:1990,
                                    (max(year)-30):(max(year) - 1))) %>%
          mutate(timing = case_when(year %in% c(min(year):(min(year)+29)) ~ "beginning",
                                    year %in% c(1961:1990) ~ "reference",
                                    TRUE ~ "end"))

        df.OP <- bind_rows(list(df.OP,
                                tmp.filtered %>%
                                  rename(value := !!cvar) %>%
                                  mutate(scenario = cscenario,
                                         var = cvar,
                                         model = cmodel)  %>%
                                  group_by(scenario,var,model,lat,lon,month,timing) %>%
                                  summarise(value.m = mean(value, na.rm = TRUE),
                                            .groups = "keep")))

        print(paste("From", min(tmp$date),"to",max(tmp$date)))

      }
    }
  }
}

saveRDS(df.OP,
        paste0("./outputs/Climate.Change.climate.Africa.RDS"))

# scp /home/femeunier/Documents/projects/CongoAS/scripts/extract.CMIP6.climate.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
