rm(list = ls())

library(epwshiftr)
library(dplyr)

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

options(timeout=600)

# test = init_cmip6_index(activity = "ScenarioMIP",
#                         variable = 'cVeg',
#                         frequency = 'mon',
#                         experiment = c("ssp126"),
#                         source = NULL,
#                         variant = NULL,
#                         replica = FALSE,
#                         latest = TRUE,
#                         resolution = NULL,
#                         limit = 10000L,
#                         data_node = NULL)
#
# esgf_query(variable = "cVeg", experiment = "ssp126", resolution = NULL, limit = 10000L)
# esgf_query(variable = "rss", experiment = "ssp126", type = "File", limit = 1)

data.nodes.status <- get_data_node()

######################################################################################################################
# First download Vegetation

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

models <- unique(models.with.vegetation$source_id)

files2download <- models.with.vegetation %>% filter(member_id == "r1i1p1f1",
                                                    data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node)))

print("--- Downloading cVeg files")

for (i in seq(1,nrow(files2download))){

  dest.file <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/cVeg",basename(files2download$file_url[i]))

  if (!file.exists(dest.file) | file.info(dest.file)[["size"]] == 0 |
      download_size(files2download$file_url[i]) <  file.info(dest.file)[["size"]]){

    dumb <- retry.func(download.file(url = files2download$file_url[i],
                                     destfile = dest.file),
                       maxErrors = 5,
                       sleep = 0)

  }
}



######################################################################################################################
# Then download Precip

prc <- init_cmip6_index(activity = "CMIP",
                        variable = 'prc',
                        frequency = 'mon',
                        experiment = c("historical"),
                        source = NULL,
                        variant = NULL,
                        replica = FALSE,
                        latest = TRUE,
                        resolution = NULL,
                        limit = 10000L,
                        data_node = NULL)

files2download.prc <- prc %>% filter(source_id %in% models,
                                     member_id == "r1i1p1f1",
                                     data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node)))

print("--- Downloading prc files")

for (i in seq(1,nrow(files2download))){

  dest.file <- file.path("/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/prc",basename(files2download.prc$file_url[i]))

  if (!file.exists(dest.file) | file.info(dest.file)[["size"]] == 0){

    dumb <- retry.func(download.file(url = files2download.prc$file_url[i],
                                     destfile = dest.file),
               maxErrors = 5,
               sleep = 0)

  }
}


# unique(test$source_id)
# test %>% filter(source_id == "CAS-ESM2-0")
#
# # "/data/gent/vo/000/gvo00074/felicien/CMIP6"
#
# test = init_cmip6_index(activity = "CMIP",
#                         variable = 'cVeg',
#                         frequency = 'mon',
#                         experiment = c("1pctCO2"),
#                         source = NULL,
#                         variant = NULL,
#                         replica = FALSE,
#                         latest = TRUE,
#                         resolution = NULL,
#                         limit = 10000L,
#                         data_node = NULL)

# scp /home/femeunier/Documents/projects/CongoAS/scripts/example.download.file.CMIP6.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
