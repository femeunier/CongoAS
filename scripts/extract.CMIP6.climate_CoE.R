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
scenarios <-c("land-cClim","land-cCO2","land-hist")
vars = c("npp","evspsbl","tas","pr")

all.df <- data.frame()
all.files <- data.frame()

years2keep <- c(1850:1879,
                1961:1990,
                1991:2020)

for (scenario in scenarios){
  for (var in vars){
    cdir <- file.path(maindir,scenario,var)
    files <- list.files(cdir)

    if (!dir.exists(cdir) | length(files) == 0) {
      print(cdir)
      next()
    }

    cfiles <- CMIP6name2attributes(CMIP6name = files)

    all.files <- bind_rows(list(all.files,
                                cfiles))

    cdf <- cfiles %>% dplyr::select(var,model,scenario,variant) %>%
      distinct()

    all.df <- bind_rows(list(all.df,
                             cdf))

  }
}

all.df.mv <- all.df %>%
  mutate(model_variant = paste(model,variant,sep = "_")) %>%
  dplyr::select(-c(model,variant))

all.df.wide <- all.df.mv %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = var,
              values_from = present,
              values_fill = FALSE)


# ###############################################################################
# # Climate 0
#
# all.clim <- all.df.wide %>%
#   dplyr::select(-c(npp,evspsbl)) %>%
#   filter(tas | pr) %>%
#   mutate(model = sub("\\_.*", "", model_variant),
#          variant = sub(".*\\_", "", model_variant))
#
# models <- unique(all.clim[["model"]])
# variants <- unique(all.clim[["variant"]])
#
# df.Climate <- data.frame()
#
# for (cscenario in scenarios){
#
#   print(cscenario)
#
#   for (cvariant in variants){
#
#     for (cvar in c("tas","pr")){
#       for (cmodel in models){
#
#         print(paste0("- ",cmodel))
#
#         csef.of.files <- all.files %>%
#           dplyr::filter(model == cmodel,
#                         var == cvar,
#                         scenario == cscenario,
#                         variant == cvariant)
#
#         if (nrow(csef.of.files) == 0){
#           next()
#         }
#
#         file.names <- attributes2CMIP6name(csef.of.files)
#
#         files2read <- file.path(maindir,cscenario,cvar,file.names)
#         files2read.filtered <- c() ; compt <- 1
#
#
#         for (ifile in seq(1,length(files2read))){
#           cyears <- csef.of.files$init.year[ifile] : csef.of.files$end.year[ifile]
#
#           if (any(cyears %in% years2keep)){
#             files2read.filtered[compt] <- files2read[ifile]
#             compt = compt + 1
#           }
#         }
#
#         if (all(file.exists(files2read.filtered)) & length(files2read.filtered) > 0){
#           tmp <- read.and.filter.ncfiles(ncfiles = files2read.filtered,
#                                          coord.analysis = continent2coord("World"),
#                                          var = cvar,
#                                          aggr = FALSE,
#                                          debug = FALSE) %>%
#             mutate(date = as.Date(paste0(year,"/",sprintf("%02d",month),"/",day)))
#
#
#           tmp.filtered <- tmp %>%
#             mutate(timing = case_when(year %in% c(1850:1879) ~ "beginning",
#                                       year %in% c(1961:1990) ~ "reference",
#                                       year %in% c(1991:2020) ~ "end"))
#
#           df.Climate <- bind_rows(list(df.Climate,
#                                   tmp.filtered %>%
#                                     rename(value := !!cvar) %>%
#                                     mutate(scenario = cscenario,
#                                            var = cvar,
#                                            model = cmodel)  %>%
#                                     group_by(scenario,var,model,lat,lon,month,timing) %>%
#                                     summarise(value.m = mean(value, na.rm = TRUE),
#                                               .groups = "keep")))
#
#           print(paste("From", min(tmp$date),"to",max(tmp$date)))
#
#         }
#       }
#     }
#   }
#
#
# }
#
# saveRDS(df.Climate,
#         paste0("./outputs/CMIP6.Climate.RDS"))
###############################################################################
# NPP first
models.scenarios <- all.df.wide %>%
  dplyr::select(model_variant,scenario,npp) %>%
  pivot_wider(names_from = scenario,
              values_from = c(npp),
              values_fill = FALSE)

models.scenarios.types <- models.scenarios %>%
  mutate(is.cClim =
           `land-cClim` & `land-hist`,
         is.cCO2 =
           `land-cCO2` & `land-hist`) %>%
  dplyr::filter(is.cClim | is.cCO2) %>%
  dplyr::select(-c(`land-cClim`,`land-hist`,`land-cCO2`)) %>%
  mutate(model = sub("\\_.*", "", model_variant),
         variant = sub(".*\\_", "", model_variant))

models <- unique(models.scenarios.types[["model"]])
variants <- unique(models.scenarios.types[["variant"]])

df.OP <- data.frame()
cvar = "npp"

for (cscenario in scenarios){

  print(cscenario)

  for (cvariant in variants){
    for (cmodel in models){

      print(paste0("- ",cmodel))

      csef.of.files <- all.files %>%
        dplyr::filter(model == cmodel,
                      var == cvar,
                      scenario == cscenario,
                      variant == cvariant)

      if (nrow(csef.of.files) == 0){
        next()
      }

      file.names <- attributes2CMIP6name(csef.of.files)

      files2read <- file.path(maindir,cscenario,cvar,file.names)
      files2read.filtered <- c() ; compt <- 1

      for (ifile in seq(1,length(files2read))){
        cyears <- csef.of.files$init.year[ifile] : csef.of.files$end.year[ifile]

        if (any(cyears %in% years2keep)){
          files2read.filtered[compt] <- files2read[ifile]
          compt = compt + 1
        }
      }

      if (all(file.exists(files2read.filtered)) & length(files2read.filtered) > 0){
        tmp <- read.and.filter.ncfiles(ncfiles = files2read.filtered,
                                       coord.analysis = continent2coord("World"),
                                       var = cvar,
                                       aggr = FALSE,
                                       debug = FALSE) %>%
          mutate(date = as.Date(paste0(year,"/",sprintf("%02d",month),"/",day)))


        tmp.filtered <- tmp %>%
          mutate(timing = case_when(year %in% c(1850:1879) ~ "beginning",
                                    year %in% c(1961:1990) ~ "reference",
                                    year %in% c(1991:2020) ~ "end"))

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
        paste0("./outputs/CMIP6.NPP.RDS"))


###############################################################################
# ET second

models.scenarios <- all.df.wide %>%
  dplyr::select(model_variant,scenario,evspsbl) %>%
  pivot_wider(names_from = scenario,
              values_from = c(evspsbl),
              values_fill = FALSE)

models.scenarios.types <- models.scenarios %>%
  mutate(is.cClim =
           `land-cClim` & `land-hist`,
         is.cCO2 =
           `land-cCO2` & `land-hist`) %>%
  dplyr::filter(is.cClim | is.cCO2) %>%
  dplyr::select(-c(`land-cClim`,`land-hist`,`land-cCO2`)) %>%
  mutate(model = sub("\\_.*", "", model_variant),
         variant = sub(".*\\_", "", model_variant))

models <- unique(models.scenarios.types[["model"]])
variants <- unique(models.scenarios.types[["variant"]])

df.OP <- data.frame()
cvar = "evspsbl"

for (cscenario in scenarios){

  print(cscenario)

  for (cvariant in variants){
    for (cmodel in models){

      print(paste0("- ",cmodel))

      csef.of.files <- all.files %>%
        dplyr::filter(model == cmodel,
                      var == cvar,
                      scenario == cscenario,
                      variant == cvariant)

      if (nrow(csef.of.files) == 0){
        next()
      }

      file.names <- attributes2CMIP6name(csef.of.files)

      files2read <- file.path(maindir,cscenario,cvar,file.names)
      files2read.filtered <- c() ; compt <- 1

      for (ifile in seq(1,length(files2read))){
        cyears <- csef.of.files$init.year[ifile] : csef.of.files$end.year[ifile]

        if (any(cyears %in% years2keep)){
          files2read.filtered[compt] <- files2read[ifile]
          compt = compt + 1
        }
      }

      if (all(file.exists(files2read.filtered))  & length(files2read.filtered) > 0){
        tmp <- read.and.filter.ncfiles(ncfiles = files2read.filtered,
                                       coord.analysis = continent2coord("World"),
                                       var = cvar,
                                       aggr = FALSE,
                                       debug = FALSE) %>%
          mutate(date = as.Date(paste0(year,"/",sprintf("%02d",month),"/",day)))


        tmp.filtered <- tmp %>%
          # dplyr::filter(lat >= -20, lat <= 15,
          #               lon >= -15, lon <= 50) %>%
          mutate(timing = case_when(year %in% c(1850:1879) ~ "beginning",
                                    year %in% c(1961:1990) ~ "reference",
                                    year %in% c(1991:2020) ~ "end"))

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
        paste0("./outputs/CMIP6.ET.RDS"))

# scp /home/femeunier/Documents/projects/CongoAS/scripts/extract.CMIP6.climate_CoE.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
