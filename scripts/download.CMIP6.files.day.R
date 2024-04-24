rm(list = ls())

library(epwshiftr)
library(dplyr)
library(tidyr)

scenarios = c("historical","ssp245","ssp370","ssp585")
variables = c("tas")
variants = "r1i1p1f1"

# scenarios <-c("land-cClim","land-cCO2","land-hist")
# variables = c("et","gpp","nep","lai","ra","rh","tas","tran","pr")
# variables = c("evspsbl","tran")
# variants = "r1i1p1f1"

overwrite = FALSE

# scenarios = c("historical")
# variables = "tas"
# variants = "r1i1p1f1"

df.all <- data.frame()

########################################################################################################

activities <- rep("ScenarioMIP",length(scenarios))
activities[scenarios %in% c("historical","1pctCO2")] <- "CMIP"
activities[grepl("land",scenarios)] <- "LUMIP"

########################################################################################################
# Functions

download_size <- function(url) {
  as.numeric(httr::HEAD(url)$headers$`content-length`)
}

retry.func <- function (expr, isError = function(x) inherits(x, "try-error"),
                        maxErrors = 5, sleep = 0) {
  attempts = 0
  retval = try(eval(expr))
  while (isError(retval)) {
    attempts = attempts + 1
    if (attempts >= maxErrors) {
      msg = sprintf("retry: too many retries [[%s]]",
                    utils::capture.output(utils::str(retval)))
      # warning(msg)
      return(0)
    }
    else {
      msg = sprintf("retry: error in attempt %i/%i [[%s]]",
                    attempts, maxErrors, utils::capture.output(utils::str(retval)))
      # warning(msg)
    }
    if (sleep > 0)
      Sys.sleep(sleep)
    retval = try(eval(expr))
  }
  return(1)
}

options(timeout=6000)

data.nodes.status <- get_data_node()

######################################################################################################################


for (iscenario in seq(1,length(scenarios))){
  for (ivar in seq(1,length(variables))){

    dest.dir.scenar <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6",
                                 scenarios[iscenario])
    dir.create(dest.dir.scenar,showWarnings = FALSE)

    dest.dir <- file.path(dest.dir.scenar,variables[ivar])
    dir.create(dest.dir,showWarnings = FALSE)


    pr <- init_cmip6_index(activity = activities[iscenario],
                           variable = variables[ivar],
                           frequency = 'day',
                           experiment = scenarios[iscenario],
                           source = NULL,
                           variant = NULL,
                           replica = FALSE,
                           latest = TRUE,
                           resolution = NULL,
                           limit = 10000L,
                           data_node = NULL)

    if (nrow(pr) == 0) next()


    files2download.prc <- pr %>%
      filter(member_id %in% variants,
             data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node))) %>%
      filter(source_id %in% c("EC-Earth3-Veg-LR","CanESM5","NorESM2-LM","FGOALS-g3","CESM2-WACCM",
                              "ACCESS-CM2","GFDL-ESM4","MPI-ESM1-2-LR","TaiESM1"))

    df.all <- bind_rows(list(
      df.all,
      data.frame(model = unique(files2download.prc$source_id),
                 scenario = scenarios[iscenario])
    ))

    print(paste("--- Downloading files for scenario ",scenarios[iscenario], " and variable ",variables[ivar]))
    print(unique(files2download.prc$source_id))

    for (i in seq(1,nrow(files2download.prc))){

      dest.file <- file.path(dest.dir,
                             basename(files2download.prc$file_url[i]))

      if (overwrite | !file.exists(dest.file) | file.info(dest.file)[["size"]] == 0){

        dumb <- retry.func(utils::download.file(url = files2download.prc$file_url[i],
                                                destfile = dest.file),
                           maxErrors = 5,
                           sleep = 0)

      }
    }
  }
}

df.all.wide <- df.all %>%
  distinct() %>%
  mutate(present = TRUE) %>%
  pivot_wider(names_from = scenario,
              values_from = present,
              values_fill = FALSE) %>%
  filter(historical & ssp126 & ssp245 & ssp370 & ssp585)

# c("EC-Earth3-Veg-LR","CanESM5","NorESM2-LM","FGOALS-g3","CESM2-WACCM")
# All models with all scenarios
# [1] "CanESM5"          "FGOALS-g3"        "TaiESM1"          "IITM-ESM"
# [5] "CMCC-CM2-SR5"     "CMCC-ESM2"        "EC-Earth3-Veg"    "EC-Earth3-Veg-LR"
# [9] "ACCESS-CM2"       "ACCESS-ESM1-5"    "MIROC6"           "INM-CM5-0"
# [13] "INM-CM4-8"        "IPSL-CM6A-LR"     "CESM2-WACCM"      "MRI-ESM2-0"
# [17] "MPI-ESM1-2-LR"    "MPI-ESM1-2-HR"    "NorESM2-MM"       "NorESM2-LM"
# [21] "GFDL-ESM4"

# scp /home/femeunier/Documents/projects/CongoAS/scripts/download.CMIP6.files.day.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
