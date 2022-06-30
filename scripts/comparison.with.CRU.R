rm(list = ls())

library(RNetCDF)
library(reshape2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rhdf5)
library(dplyr)
library(ggplot2)
library(tidyr)
library(CongoAS)
library(RCMIP5)
library(zoo)
library(viridis)
library(raster)

world <- ne_countries(scale = "medium", returnclass = "sf")
continent <- "Tropics"
coord.list <- continent2coord(continent)
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

coord.list.NSA <- continent2coord("America")
min.lon.plot.NSA <- coord.list.NSA[[2]][1]; max.lon.plot.NSA <- coord.list.NSA[[2]][2]; min.lat.plot.NSA <- coord.list.NSA[[2]][3]; max.lat.plot.NSA <- coord.list.NSA[[2]][4]

##################################################################################################
# CRU
crufile <- "./data/grid_10min_pre.dat"
cru <- read.table(crufile) %>%
  dplyr::select(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14) %>%
  rename(lat = V1,
         lon = V2,
         Jan = V3,Feb = V4,Mar = V5,Apr = V6,May = V7,Jun = V8,
         Jul = V9,Aug = V10,Sep = V11,Oct = V12,Nov = V13,Dec = V14)

cru.lon <- cru %>% pivot_longer(cols = -c(lon,lat),
                                names_to = "month",
                                values_to = "prec")
cru.tot <- cru.lon %>% group_by(lat,lon) %>%
  summarise(MAP = sum(prec),
            .groups = "keep")

ggplot(data = world) +
  geom_tile(data = cru.tot,
            aes(x = lon, y = lat,fill = MAP),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "", fill = "MAP (mm)") +
  theme_bw() +
  theme(text = element_text(size = 24))


ggplot(data = world) +
  geom_tile(data = cru.tot %>% filter.continent(continent = "America") %>%
              mutate(MAP.bnd = case_when(MAP > 3000 ~ 3000,
                                         TRUE ~ MAP)),
            aes(x = lon, y = lat,fill = MAP.bnd),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(xlim = c(min.lon.plot.NSA, max.lon.plot.NSA),
           ylim = c(-23,23),
           expand = FALSE) +
  labs(x = "",y = "", fill = "MAP (mm)") +
  theme_bw() +
  theme(text = element_text(size = 24))

cru.cont <- cru.tot %>%
  filter.continent(continent = "Equator") %>%
  mutate(cont = coord2continent(lon))

ggplot(data = cru.cont) +
  geom_boxplot(aes(x = cont, y = MAP, fill = cont)) +
  theme_bw()


##################################################################################################
# Precipitation

prc.file <- c("/home/femeunier/Downloads/prc_Amon_TaiESM1_1pctCO2_r1i1p1f1_gn_000101-015012.nc")
ncfiles.cfile <- c("/home/femeunier/Downloads/cVeg_Lmon_TaiESM1_piControl_r1i1p1f1_gn_060101-070012.nc")

cID = c(2146,1754)

df.control <- read.and.filter.ncfile(ncfiles.cfile,
                                     continent2coord("World")[[1]],
                                     var = "cVeg",
                                     aggr = TRUE,
                                     yr.rel = 1:2)


mask.ocean <- df.control %>% group_by(lat,lon) %>%
  summarise(is.land = !all(cVeg == 0),
            .groups = "keep")

prc <- read.and.filter.ncfiles(ncfiles = prc.file,
                               coord.analysis = continent2coord(continent),
                               var = "prc",
                               aggr = TRUE,
                               progressbar = TRUE)


prc.mask <- CongoAS::mask(prc %>% mutate(lon = round(100*lon)/100,
                                lat = round(100*lat)/100),
                 mask.ocean %>%  mutate(lon = round(100*lon)/100,
                                        lat = round(100*lat)/100)) %>%
  group_by(lat, lon) %>%
  mutate(ID = cur_group_id())

prc.mask.bnd <- prc.mask %>%
  mutate(MAP = prc*86400*365) %>%
  mutate(prc.bnd = case_when(MAP > 3000 ~ 3000,
                             TRUE ~ MAP)) %>%
  mutate(MAP.bnd = prc.bnd) %>%
  mutate(prc.cat = case_when(MAP < 100 ~ 1,
                             MAP < 200 ~ 2,
                             MAP < 300 ~ 3,
                             MAP < 400 ~ 4,
                             MAP < 500 ~ 5,
                             MAP < 600 ~ 6,
                             MAP < 800 ~ 7,
                             MAP < 1000 ~ 8,
                             MAP < 1500 ~ 9,
                             MAP < 2000 ~ 10,
                             TRUE ~ 11))

ggplot(data = world) +
  geom_tile(data = prc.mask.bnd %>% filter(yr == 0),
            aes(x = lon, y = lat,fill = MAP.bnd),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "", fill = "MAP (mm)") +
  theme_bw() +
  theme(text = element_text(size = 24))

ggplot(data = world) +
  geom_tile(data = prc.mask.bnd %>% filter(yr == 0) %>% filter.continent(continent = "America"),
            aes(x = lon, y = lat,fill = MAP.bnd),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(xlim = c(min.lon.plot.NSA, max.lon.plot.NSA),
           ylim = c(-23, 23),
           expand = FALSE) +
  labs(x = "",y = "", fill = "MAP (mm)") +
  theme_bw() +
  theme(text = element_text(size = 24))


ggplot(data = world) +
  geom_tile(data = prc.mask.bnd %>% filter(yr == 0),
            aes(x = lon, y = lat,fill = as.factor(prc.cat)),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_d(direction = -1) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "", fill = "MAP (mm)") +
  theme_bw() +
  theme(text = element_text(size = 24))


prc.diff <- prc.mask %>%
  dplyr::filter(yr %in% c(min(yr),max(yr))) %>%
  mutate(timing = case_when(yr == 0 ~ "Init",
                            TRUE ~ "End")) %>%
  dplyr::select(-yr) %>%
  pivot_wider(names_from = timing,
              values_from = prc) %>%
  mutate(diff = End - Init,
         diff.rel = (End - Init)/Init) %>%
  mutate(diff.rel.bnd = case_when(diff.rel > 1 ~ 1,
                                  diff.rel < -1 ~ -1,
                                  TRUE ~ diff.rel))
ggplot(data = world) +
  geom_tile(data = prc.diff,
            aes(x = lon, y = lat,fill = 100*diff.rel.bnd),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkblue",na.value = NA, limits = 100*c(-1,1)) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "",
       fill = "relative change \r\n in precipitation (%)") +
  theme_bw() +
  theme(text = element_text(size = 24))


ggplot(data = world) +
  geom_tile(data = prc.diff,
            aes(x = lon, y = lat,fill = diff*86400*365),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkblue",na.value = NA) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "",
       fill = "Change \r\n in precipitation (mm)") +
  theme_bw() +
  theme(text = element_text(size = 24))

prc.mask.cont <- prc.mask %>%
  filter.continent(continent = "Equator") %>%
  mutate(cont = coord2continent(lon)) %>%
  group_by(cont,yr)

prc.mask.cont.sum <- prc.mask.cont %>%
  summarise(prc.m = mean(prc)*86400*365,
            prc.sd = sd(prc)*86400*365,
            .groups = "keep")

ggplot(data = prc.mask.cont.sum,
       aes(x = yr, y = prc.m, ymin = prc.m - prc.sd, ymax = prc.m + prc.sd,
           fill = cont, color = cont)) +
  geom_ribbon(color = NA, alpha = 0.4) +
  geom_line() +
  facet_wrap(~ cont) +
  stat_smooth(se = FALSE, method = "lm") +
  theme_bw()

ggplot(data = prc.mask.cont %>% filter(yr == 0)) +
  geom_boxplot(aes(x = cont, y = prc*86400*365, fill = cont)) +
  theme_bw()


ggplot(data = prc.mask.bnd %>% filter(ID %in% cID),
       aes(x = yr, y = MAP, color = as.factor(ID))) +
  geom_line() +
  stat_smooth(se = FALSE, method = "lm") +
  theme_bw()
