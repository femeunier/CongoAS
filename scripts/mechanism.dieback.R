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

##################################################################################################

AS.TS <- readRDS("./outputs/AS.TaiESM1.RDS")
AS.ID <- AS.TS %>% filter(AS) %>% pull(ID) %>% unique()

##################################################################################################
ncfiles.cfile <- c("/home/femeunier/Downloads/cVeg_Lmon_TaiESM1_piControl_r1i1p1f1_gn_060101-070012.nc")
df.control <- read.and.filter.ncfile(ncfiles.cfile,
                                     continent2coord("World")[[1]],
                                     var = "cVeg",
                                     aggr = TRUE,
                                     yr.rel = 1:2)


mask.ocean <- df.control %>% group_by(lat,lon) %>%
  summarise(is.land = !all(cVeg == 0),
            .groups = "keep")

##################################################################################################
# Soil moisture

sm.file <- c("/home/femeunier/Downloads/mrsos_Lmon_TaiESM1_1pctCO2_r1i1p1f1_gn_000102-015012.nc")

continent <- "Tropics"

mrsos.raw <- read.and.filter.ncfiles(ncfiles = sm.file,
                               coord.analysis = continent2coord(continent),
                               var = "mrsos",
                               aggr = FALSE,
                               progressbar = TRUE)

mrsos.mask <- CongoAS::mask(mrsos.raw %>% mutate(lon = round(100*lon)/100,
                                             lat = round(100*lat)/100),
                            mask.ocean %>%  mutate(lon = round(100*lon)/100,
                                                   lat = round(100*lat)/100)) %>%
  group_by(lat, lon) %>%
  mutate(ID = cur_group_id())

mrsos <- mrsos.mask %>% group_by(lat,lon,yr) %>%
  summarise(mrsos = mean(mrsos),
            .groups = "keep")

mrsos.min <- mrsos.mask %>% group_by(lat,lon,yr) %>%
  summarise(mrsos = min(mrsos),
            .groups = "keep")

mrsos.mask.cont <- mrsos %>%
  ungroup() %>% filter.continent(continent = "Equator") %>%
  mutate(cont = coord2continent(lon)) %>%
  group_by(yr,cont) %>%
  summarise(mrsos.m = mean(mrsos/100),
            mrsos.sd = sd(mrsos/100),
            .groups = "keep")

ggplot(data = mrsos.mask.cont,
       aes(x = yr, y = mrsos.m, ymin = mrsos.m - mrsos.sd, ymax = mrsos.m + mrsos.sd,
           color = cont, fill = cont)) +
  geom_ribbon(color = NA, alpha = 0.4) +
  facet_wrap(~ cont) +
  geom_line() +
  theme_bw()


world <- ne_countries(scale = "medium", returnclass = "sf")

coord.list <- continent2coord(continent)
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

ggplot(data = world) +
  geom_tile(data = mrsos %>% filter(yr == 0),
            aes(x = lon, y = lat,fill = mrsos/100),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 24))

mrsos.diff <- mrsos %>%
  group_by(lat,lon) %>%
  dplyr::filter(yr %in% c(min(yr),max(yr))) %>%
  mutate(timing = case_when(yr == 0 ~ "Init",
                            TRUE ~ "End")) %>%
  dplyr::select(-yr) %>%
  pivot_wider(names_from = timing,
              values_from = mrsos) %>%
  mutate(diff = End - Init,
         diff.rel = (End - Init)/Init) %>%
  mutate(diff.rel.bnd = case_when(diff.rel > 1 ~ 1,
                                  diff.rel < -1 ~ -1,
                                  TRUE ~ diff.rel))

ggplot(data = world) +
  geom_tile(data = mrsos.diff %>% filter(diff !=0),
            aes(x = lon, y = lat,fill = 100*diff.rel.bnd),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkblue",na.value = NA, limits = 100*c(-1,1)) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "", fill = "relative change \r\n in SWC (%)") +
  theme_bw() +
  theme(text = element_text(size = 24))

# clat = 5 ; clon = -55
clat = 0 ; clon = 25

cID <- mrsos.mask %>%
  group_by(lat,lon) %>%
  mutate(dist = sqrt((lat - clat)**2 + (lon - clon)**2)) %>%
  arrange((dist)) %>%
  ungroup() %>%
  slice_head(n = 1) %>% pull(ID)

cID = c(2146,1754)

ggplot(data = mrsos.mask %>% dplyr::filter(ID %in% cID)) +
  geom_line(aes(x = yr,y = mrsos/100, color = as.factor(ID))) +
  theme_bw()

ggplot(data = mrsos.mask %>% dplyr::filter(ID %in% AS.ID)) +
  geom_line(aes(x = yr,y = mrsos/100, group = as.factor(ID)),size = 0.1) +
  theme_bw()

##################################################################################################
# Repeat with min. water content

mrsos.mask.cont.min <- mrsos.min %>%
  ungroup() %>% filter.continent(continent = "Equator") %>%
  mutate(cont = coord2continent(lon)) %>%
  group_by(yr,cont) %>%
  summarise(mrsos.m = mean(mrsos/100),
            mrsos.sd = sd(mrsos/100),
            .groups = "keep")

ggplot(data = mrsos.mask.cont.min,
       aes(x = yr, y = mrsos.m, ymin = mrsos.m - mrsos.sd, ymax = mrsos.m + mrsos.sd,
           color = cont, fill = cont)) +
  geom_ribbon(color = NA, alpha = 0.4) +
  facet_wrap(~ cont) +
  geom_line() +
  theme_bw()


ggplot(data = world) +
  geom_tile(data = mrsos.min %>% filter(yr == 0),
            aes(x = lon, y = lat,fill = mrsos/100),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 24))

mrsos.diff.min <- mrsos.min %>%
  ungroup() %>%
  dplyr::filter(yr %in% c(min(yr),max(yr))) %>%
  mutate(timing = case_when(yr == 0 ~ "Init",
                            TRUE ~ "End")) %>%
  dplyr::select(-yr) %>%
  pivot_wider(names_from = timing,
              values_from = mrsos) %>%
  mutate(diff = End - Init,
         diff.rel = (End - Init)/Init) %>%
  mutate(diff.rel.bnd = case_when(diff.rel > 1 ~ 1,
                                  diff.rel < -1 ~ -1,
                                  TRUE ~ diff.rel))

ggplot(data = world) +
  geom_tile(data = mrsos.diff.min,
            aes(x = lon, y = lat,fill = diff),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_gradient2(low = "darkred",mid = "white",high = "darkblue",na.value = NA) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "", fill = "Change \r\n in min SWC (%)") +
  theme_bw() +
  theme(text = element_text(size = 24))



##################################################################################################
# Precipitation

prc.file <- c("/home/femeunier/Downloads/prc_Amon_TaiESM1_1pctCO2_r1i1p1f1_gn_000101-015012.nc")

prc <- read.and.filter.ncfiles(ncfiles = prc.file,
                                 coord.analysis = continent2coord(continent),
                                 var = "prc",
                                 aggr = TRUE,
                                 progressbar = TRUE)


prc.mask <- mask(prc %>% mutate(lon = round(100*lon)/100,
                                lat = round(100*lat)/100),
                 mask.ocean %>%  mutate(lon = round(100*lon)/100,
                                        lat = round(100*lat)/100)) %>%
  group_by(lat, lon) %>%
  mutate(ID = cur_group_id())

coord.list <- continent2coord(continent)
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]


prc.mask.bnd <- prc.mask %>%
  mutate(MAP = prc*86400*365) %>%
  mutate(prc.bnd = case_when(MAP > 2000 ~ 2000,
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
  group_by(cont,yr) %>%
  summarise(prc.m = mean(prc)*86400*365,
            prc.sd = sd(prc)*86400*365,
            .groups = "keep")

ggplot(data = prc.mask.cont,
       aes(x = yr, y = prc.m, ymin = prc.m - prc.sd, ymax = prc.m + prc.sd,
           fill = cont, color = cont)) +
  geom_ribbon(color = NA, alpha = 0.4) +
  geom_line() +
  facet_wrap(~ cont) +
  stat_smooth(se = FALSE, method = "lm") +
  theme_bw()


ggplot(data = prc.mask.bnd %>% filter(ID %in% cID),
       aes(x = yr, y = MAP, color = as.factor(ID))) +
  geom_line() +
  stat_smooth(se = FALSE, method = "lm") +
  theme_bw()

##################################################################################################
# Fire

Fire.file <- c("/home/femeunier/Downloads/fFire_Lmon_TaiESM1_1pctCO2_r1i1p1f1_gn_000102-015012.nc")

continent <- "Tropics"

fFire <- read.and.filter.ncfiles(ncfiles = Fire.file,
                               coord.analysis = continent2coord(continent),
                               var = "fFire",
                               aggr = TRUE,
                               progressbar = TRUE)
fFire.mask <- mask(fFire %>% mutate(lon = round(100*lon)/100,
                                lat = round(100*lat)/100),
                 mask.ocean %>%  mutate(lon = round(100*lon)/100,
                                        lat = round(100*lat)/100)) %>%
  group_by(lat, lon) %>%
  mutate(ID = cur_group_id()) %>%
  group_by(ID) %>%
  mutate(cumfFire = cumsum(fFire))

ggplot(data = world) +
  geom_tile(data = fFire.mask %>% filter(yr == 140),
            aes(x = lon, y = lat,fill = cumfFire),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "", fill = "Fire") +
  theme_bw() +
  theme(text = element_text(size = 24))


ggplot(data = fFire.mask %>% dplyr::filter(ID %in% cID)) +
  geom_line(aes(x = yr,y = cumfFire, color = as.factor(ID))) +
  scale_y_log10() +
  theme_bw()

water.vs.fire <- fFire.mask %>% ungroup()  %>% dplyr::select(ID,lat,lon,yr,fFire,cumfFire) %>%
  left_join(mrsos.mask %>% dplyr::select(lat,lon,yr,mrsos) %>% ungroup(),
            by = c("lat","lon","yr"))

ggplot(data = water.vs.fire %>% filter(ID %in% cID)) +
  geom_point(aes(x = mrsos/100,y = cumfFire,color = as.factor(ID))) +
  theme_bw()

ggplot(data = water.vs.fire %>% filter(ID %in% AS.ID)) +
  geom_point(aes(x = mrsos/100,y = cumfFire,color = as.factor(ID)),size = 0.1) +
  theme_bw() +
  guides(color = "none")

##################################################################################################
# GPP

GPP.file <- c("/home/femeunier/Downloads/gpp_Lmon_TaiESM1_1pctCO2_r1i1p1f1_gn_000102-015012.nc")

gpp <- read.and.filter.ncfiles(ncfiles = GPP.file,
                                 coord.analysis = continent2coord(continent),
                                 var = "gpp",
                                 aggr = TRUE,
                                 progressbar = TRUE)
gpp.mask <- mask(gpp %>% mutate(lon = round(100*lon)/100,
                                    lat = round(100*lat)/100),
                   mask.ocean %>%  mutate(lon = round(100*lon)/100,
                                          lat = round(100*lat)/100)) %>%
  group_by(lat, lon) %>%
  mutate(ID = cur_group_id()) %>%
  group_by(ID)

ggplot(data = world) +
  geom_tile(data = gpp.mask %>% filter(yr == 140),
            aes(x = lon, y = lat,fill = gpp*86400*365),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "", fill = "GPP") +
  theme_bw() +
  theme(text = element_text(size = 24))


ggplot(data = gpp.mask %>% dplyr::filter(ID %in% cID)) +
  geom_line(aes(x = yr,y = gpp*86400*365, color = as.factor(ID))) +
  theme_bw()
