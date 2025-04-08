rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

scenarios <- c("historical","ssp126","ssps245","ssp370","ssp585")

Gridarea <- readRDS("./outputs/Gridarea.RDS") %>%
  ungroup()

A.all <- readRDS("./outputs/Emergent.World.rspld.RDS") %>%
  left_join(Gridarea,
            by = c("lon","lat"))

timeseries.CMIP6 <- A.all %>%
  group_by(year,scenario,variant,month,model) %>%
  summarise(nbp.m = weighted.mean(nbp,area),
            .groups = "keep")

saveRDS(timeseries.CMIP6,
        "./outputs/timeseries.CMIP6.World.rspld.RDS")

timeseries.CMIP6 <- A.all %>%
  filter(abs(lat) <= 23.5) %>%
  group_by(year,scenario,variant,month,model) %>%
  summarise(nbp.m = weighted.mean(nbp,area),
            .groups = "keep")

saveRDS(timeseries.CMIP6,
        "./outputs/timeseries.CMIP6.Tropics.rspld.RDS")

maps.CMIP6 <- A.all %>%
  filter(year %in% c(1985:2014,
                     2071:2100)) %>%
  mutate(period = case_when(year < 2020 ~ "historical",
                            TRUE ~ "scenario")) %>%
  group_by(period,lon,lat,model) %>%
  summarise(nbp.m = mean(nbp,na.rm = TRUE),
            .groups = "keep")

saveRDS(maps.CMIP6,
        "./outputs/maps.CMIP6.World.rspld.RDS")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

data2plot <- A.all %>%
  ungroup() %>%
  # filter(lat >= - 15,lat <= 10,
  #        lon >= -15, lon <= 45) %>%         # Congo Basin
  filter(year %in% c(1985:2014,
                     2071:2100)) %>%
  mutate(period = case_when(year < 2020 ~ "historical",
                            TRUE ~ "scenario")) %>%
  group_by(model,variant,lon,lat,period,scenario) %>%
  summarise(nbp.m = mean(nbp,na.rm = TRUE),
            .groups = "keep")

data2plot.wide <-
  data2plot %>%
  ungroup() %>%
  filter(period != "historical") %>%
  dplyr::select(-period) %>%
  left_join(data2plot %>%
              ungroup() %>%
              filter(period == "historical") %>%
              dplyr::select(-c(scenario,period)) %>%
              rename(historical = nbp.m),
            by = c("model","variant","lon","lat"))

ggplot() +
  geom_raster(data = data2plot.wide %>%
                ungroup() %>%
                filter(model == model[1],
                       variant == variant[1],
                       scenario == "ssp245") %>%
                dplyr::select(-scenario) %>%
                rename(ssp245 = nbp.m) %>%
                pivot_longer(cols = c("ssp245","historical"),
                             names_to = "scenario"),
              aes(x = lon, y = lat,
                  fill = value)) +
  geom_sf(data = world,fill = NA, color = "grey") +
  coord_sf(xlim = c(-120, 160), ylim = c(-1, 1)*23.25, expand = FALSE) +
  scale_fill_gradient2(low = "darkgreen",high = "darkred",mid = "white",
                       na.value = NA) +
  theme_bw() +
  facet_wrap(~ scenario, ncol = 1) +
  labs(x = "",y = "",) +
  theme(legend.position = "none",
        text = element_text(size = 20))


A.all.sum <- A.all %>%
  ungroup() %>%
  # filter(lat >= - 15,lat <= 10,
  #        lon >= -15, lon <= 45) %>%         # Congo Basin
  group_by(model,scenario,variant) %>%
  summarise(nbp.m = weighted.mean(nbp,area),
            .groups = "keep") %>%
  mutate(reference = case_when(scenario == "historical" ~ "yes",
                               TRUE ~ "no")) %>%
  ungroup()

A.all.sum.wide <- A.all.sum %>%
  filter(reference != "yes") %>%
  dplyr::select(-reference) %>%
  left_join(A.all.sum %>%
              ungroup() %>%
              filter(reference == "yes") %>%
              dplyr::select(-c(scenario,reference)) %>%
              rename(historical = nbp.m),
            by = c("model","variant"))

saveRDS(A.all.sum.wide,"./outputs/Emergent.constraints.computed.World.rspld.RDS")

stop()

system2("rsync",
        c("-avz",
          "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Emergent.constraints.computed.RDS",
          "./outputs/"))

system2("rsync",
        c("-avz",
          "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/timeseries.CMIP6.RDS",
          "./outputs/"))

system2("rsync",
        c("-avz",
          "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/maps.CMIP6.RDS",
          "./outputs/"))

A.all.sum.wide <- readRDS("./outputs/Emergent.constraints.computed.RDS") %>%
  mutate(historical = case_when(model == "E3SM-1-1-ECA" ~ historical/1000,
                                TRUE ~ historical))


# data.obs <- data.frame(m = historical.obs.m,
#                        se = historical.obs.se,
#                        source = c("bottom-up","top-down"))

fac = 365*86400*10 # MgC/ha/yr

ggplot() +
  geom_point(data = A.all.sum.wide ,
             aes(x = historical*fac,
                 y = nbp.m*fac,
                 color = scenario)) +
  stat_smooth(data = A.all.sum.wide ,
              aes(x = historical*fac,
                  y = nbp.m*fac,
                  color = scenario,
                  fill = scenario),
              method = lm,fullrange = TRUE) +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_grid( ~ scenario) +
  theme_bw() + theme(legend.position = "none")

A.all.sum.wide %>%
  group_by(scenario) %>%
  summarise(R2 = summary(lm(nbp.m ~ historical))[["r.squared"]],
            pval = summary(lm(nbp.m ~ historical))[["coefficients"]][2,4])


# scp /home/femeunier/Documents/projects/CongoAS/scripts/Compute.Emergent.constraints.R hpc:/data/gent/vo/000/gvo00074/felicien/R/
