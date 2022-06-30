rm(list = ls())

library(dplyr)
library(reshape2)
library(ncdf4)
library(epwshiftr)
library(lubridate)

dir <- "/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/cVeg"

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


data.nodes.status <- get_data_node()
cVegfiles <- models.with.vegetation %>% filter(member_id == "r1i1p1f1",
                                               data_node %in% (data.nodes.status %>% filter(status == "UP") %>% pull(data_node)))

actual.models <- unique(cVegfiles$source_id)

options(warn = 2)

df.allmodels <- data.frame()
for (cmodel in actual.models){

  print("-----------")
  print(cmodel)

  cdf <- cVegfiles %>% filter(source_id == cmodel) %>% arrange(desc(datetime_end))
  ncfile <- file.path(dir <- "/data/gent/vo/000/gvo00074/felicien/CMIP6/historical/cVeg",basename(cdf[["file_url"]]))[1]

  init.year <- year(cdf[["datetime_start"]])[1]

  if (file.exists(ncfile)){
    nc <- nc_open(ncfile)

    times <- ncvar_get(nc,"time")
    years <- init.year + floor(times/ 365)
    year.select <- which(years == 2014)

    lats <- ncvar_get(nc,"lat")
    lons <- ncvar_get(nc,"lon")
    cVar <- (ncvar_get(nc,"cVeg"))[,,year.select]

    nc_close(nc)
  }

  df <- melt(cVar) %>%
    group_by(Var1,Var2,Var3) %>%
    summarise(cVeg = mean(value),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(lon = (lons)[Var1],
           lat = (lats)[Var2],
           year = 2014) %>%
    dplyr::select(lon,lat,year,cVeg) %>%
    mutate(lon = case_when(lon > 180 ~ (lon -360),
                           TRUE ~ lon))

  df.allmodels <- bind_rows(list(df.allmodels,
                                 df %>% mutate(model = cmodel)))

}

saveRDS(df.allmodels,"./outputs/AGB.all.CMIP6.models.RDS")
# scp /home/femeunier/Documents/projects/CongoAS/scripts/analyze.cVeg.CMIP6.files.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R
