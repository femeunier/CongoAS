rm(list = ls())

library(dplyr)
library(tidyr)
library(rnaturalearth)
library(ggplot2)
library(stringr)

# system2('rsync',
#         c("-avz",
#           "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Times.series.ACCESS-CM2.*",
#           "./outputs/"))

files <- list.files("./outputs/",
                    "Times.series.*",
                    full.names = TRUE)

df.files <- data.frame(file = files,
                       model = sapply(str_split(basename(files),"\\."),"[",3),
                       scenario = sapply(str_split(basename(files),"\\."),"[",4))

models <- (unique(df.files$model))

overwrite = TRUE

for(cmodel in models){

  df.all <- bind_rows()

  OP.file <- paste0("./outputs/Scenarios.Quan.",cmodel,".RDS")

  if (!overwrite & file.exists(OP.file)){
    next()
  }

  cdf <- df.files %>%
    filter(model == cmodel)
  scenarios <- cdf %>% pull(scenario)

  for (cscenario in scenarios){

    ccdf <- df.files %>%
      filter(model == cmodel,
             scenario == cscenario)
    cfile <- ccdf$file

    print(cfile)

    A <- readRDS(cfile) %>%
      ungroup() #%>%
      # mutate(value = case_when(is.na(pr) ~ tas,
      #                          is.na(tas) ~ pr)) %>%
      # dplyr::select(-c(time0,day))

    if (nrow(A) == 0){
      next()
    }

    A.wide <- A %>%
      pivot_wider(names_from = var,
                  values_from = value) %>%
      mutate(period = case_when(year %in% c(1985:2014) ~ "historical",
                                year %in% c(2015:2040) ~ "near_future",
                                year %in% c(2041:2070) ~"mid_future",
                                year %in% c(2071:2100) ~ "long_future"))

    if (!("tas" %in% colnames(A.wide))){
      A.wide[["tas"]] <- NA
    }

    if (!("pr" %in% colnames(A.wide))){
      A.wide[["pr"]] <- NA
    }

    A.wide.m <- A.wide %>%
      group_by(model,lon,lat,scenario,period) %>%
      summarise(MAT = mean(tas,na.rm = TRUE) - 273.15,
                MAP = mean(pr,na.rm = TRUE)*86400*365,
                .groups = "keep")

    df.all <- bind_rows(df.all,
                        A.wide.m %>%
                          filter(!is.na(period)) %>%
                          distinct())

  }


  if (nrow(df.all) == 0) next()


  df.all.ref <- df.all %>%
    distinct() %>%
    mutate(reference = case_when(scenario == "historical" ~ TRUE,
                                 TRUE ~ FALSE))

  if ((nrow(df.all.ref %>%
           filter(reference)) == 0) | (nrow(df.all.ref %>%
                                            filter(!reference)) == 0)){
    next()
  }

  df.all.vs <- df.all.ref %>%
    filter(!reference) %>%
    distinct() %>%
    dplyr::select(-reference) %>%
    left_join(df.all.ref %>%
                filter(reference) %>%
                dplyr::select(-c(scenario,period,reference)) %>%
                distinct() %>%
                rename(MAT_reference = MAT,
                       MAP_reference = MAP),
              by = c("model","lon","lat"))


  saveRDS(df.all.vs,
          OP.file)

}

# scp /home/femeunier/Documents/projects/CongoAS/scripts/Compile.CMIP6.Quan.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R


