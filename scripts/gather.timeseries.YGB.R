rm(list = ls())

library(TrENDY.analyses)
library(Congo.ED2)
library(dplyr)
library(ggplot2)
library(lubridate)
library(YGB)
library(tidyr)

# system2("scp",
#         paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/df.CAS-ESM2-0timeseries.RDS",
#               "./outputs/"))

system2("scp",
        paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/all.timeseries.YGB.RDS",
              "./outputs/"))

all.timeseries.YGB <- readRDS("./outputs/all.timeseries.YGB.RDS")

cf <- list.files("./outputs/",
                 pattern = "df.*timeseries.RDS",full.names = TRUE)

all.models <- data.frame()

for (i in seq(1,length(cf))){
  cmodel <- sub("timeseries.RDS","",
                sub("df\\.","",basename(cf[i])))

  print(cmodel)

  cdata <- readRDS(cf[i]) %>%
    mutate(model.lat.lon = paste(model,lat,lon, sep = "."))

  df.search <- cdata %>%
    ungroup() %>%
    dplyr::select(lat,lon,model,model.lat.lon) %>%
    distinct()

  model.lat.lons <- find.coord.Trendy(Trendy.grid = df.search,
                                      target = c(24.45,0.86),
                                      Ngridcells = 1) %>%
    unique()

  cdata.selected <- cdata %>%
    ungroup() %>%
    filter(model.lat.lon %in% model.lat.lons) %>%
    filter(year %in% c(seq(1900,2100))) %>%
    group_by(model,scenario,year,month) %>%
    summarise(
      N = days_in_month(as.Date(
        paste0(year,"/",sprintf('%02d',month),"/01"))),
      pr = mean(value.m[var == "pr"])*86400*days_in_month(as.Date(
        paste0(year,"/",sprintf('%02d',month),"/01"))),
      tas = mean(value.m[var == "tas"]) - 273.15,
      .groups = "keep") %>%
    distinct() %>%
    na.omit()

  cdf.year <- cdata.selected %>%

    group_by(model,scenario,year) %>%

    mutate(diff = pr - 3.33*N) %>%
    mutate(wettest.month = which.max(diff)) %>%
    mutate(month.ord = 1 + (month - wettest.month)) %>%
    mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                                 TRUE ~ month.ord)) %>%
    arrange(year,month.ord) %>%
    mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                           TRUE ~ NA_real_)) %>%
    mutate(CWD = calc.CWD(diff,CWD[1])) %>%
    arrange(year,month) %>%
    mutate(MCWD = min(CWD)) %>%
    dplyr::select(-c(wettest.month,month.ord,CWD)) %>%

    group_by(model,scenario,year) %>%
    summarise(MAT = weighted.mean(tas,N),
              MAP = sum(pr),
              MCWD = min(MCWD),
              .groups = "keep")

  all.models <- bind_rows(all.models,
                          cdf.year %>%
                            mutate(model = cmodel))

}

saveRDS(all.models,
        "./outputs/all.timeseries.YGB.RDS")

# scp /home/femeunier/Documents/projects/CongoAS/scripts/gather.timeseries.YGB.R hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R

###############################################################################
data <- readRDS("/home/femeunier/Documents/projects/Precip.Africa/data/YGB/climate.YGB.RDS") %>%
  mutate(tmean = (tmin + tmax)/2) %>%
  mutate( N = days_in_month(as.Date(
    paste0(year,"/",sprintf('%02d',month),"/01")))) %>%
  mutate(diff = Pmm - 3.33*N) %>%
  group_by(year) %>%
  mutate(wettest.month = which.max(diff)) %>%
  mutate(month.ord = 1 + (month - wettest.month)) %>%
  mutate(month.ord = case_when(month.ord <= 0 ~ month.ord + 12,
                               TRUE ~ month.ord)) %>%
  arrange(year,month.ord) %>%
  mutate(CWD = case_when(month.ord == 1 ~ pmin(0,diff),
                         TRUE ~ NA_real_)) %>%
  mutate(CWD = calc.CWD(diff,CWD[1])) %>%
  arrange(year,month) %>%
  mutate(MCWD = min(CWD)) %>%


  group_by(year) %>%
    summarise(MAT = mean(tmean,na.rm = TRUE),
              MAP = sum(Pmm),
              MCWD = unique(MCWD)) %>%
    pivot_longer(cols = c(MAT,MAP,MCWD),
                 names_to = "variable",
                 values_to = "value.m")

all.timeseries.YGB.long <- all.timeseries.YGB %>%
  group_by(model) %>%
  filter(length(unique(scenario)) == 5) %>%
  pivot_longer(cols = c(MAP,MAT,MCWD),
               names_to = "variable",
               values_to = "value")

all.timeseries.YGB.sum <- all.timeseries.YGB.long  %>%
  group_by(scenario,year,variable) %>%
  summarise(value.m = mean(value),
            value.se = sd(value)/sqrt(length(unique(model))),
            .groups = "keep")



ggplot(data = all.timeseries.YGB.sum %>%
         filter(year >= 1960),
       aes(x = year, y = value.m)) +
  geom_line(aes(color = scenario, fill = scenario)) +
  geom_line(data = all.timeseries.YGB.long %>%
              filter(year >= 1960,
                     scenario == "historical"),
            aes(y = value, group = model), color = "grey",linewidth = 0.1) +
  geom_point(data = data, color = "black") +
  geom_ribbon(aes(ymin = value.m - value.se,
                  ymax = value.m + value.se,
                  color = scenario, fill = scenario), color = NA, alpha = 0.5) +
  stat_smooth(aes(color = scenario),
              method = "lm",
              formula = "y ~ (x)",
              se = FALSE) +
  stat_smooth(data = data,
              method = "lm",
              formula = "y ~ (x)",
              se = FALSE, color = "black") +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()

ggplot(data = all.timeseries.YGB,
       aes(x = year, y = MAT, color = scenario)) +
  geom_line() +
  stat_smooth(method = "lm",
              formula = "y ~ (x)",
              se = FALSE) +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = all.timeseries.YGB,
       aes(x = year, y = MCWD, color = scenario)) +
  geom_line() +
  stat_smooth(method = "lm",
              formula = "y ~ (x)",
              se = FALSE) +
  facet_wrap(~ model) +
  theme_bw()
