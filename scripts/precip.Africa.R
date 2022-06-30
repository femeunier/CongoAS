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
# Precip

prc.file <- c("./data/prc/prc_Amon_BCC-ESM1_historical_r1i1p1f1_gn_185001-201412.nc")

continent <- "Tropics"

prc <- read.and.filter.ncfiles(ncfiles = prc.file,
                               coord.analysis = continent2coord(continent),
                               var = "prc",
                               aggr = FALSE,
                               progressbar = TRUE)

# YGB

lon.site <- 24 + 31/60
lat.site<- 0 + 53/60
# lon.site <- -60
# lat.site<- 0

prc.ID <- prc %>%
  group_by(lat, lon) %>%
  mutate(ID = cur_group_id())

site.ID <- prc.ID %>% filter(time == min(time)) %>%
  mutate(dist = sqrt( (lat - lat.site)**2 + (lon - lon.site)**2)) %>%
  ungroup() %>%
  filter(dist == min(dist)) %>%
  pull(ID)

YGB.prec <- prc.ID %>% filter(ID %in% site.ID) %>%
  filter(yr < 10) %>%
  mutate(doy = time - yr*365) %>%
  group_by(doy) %>%
  summarise(prc = mean(prc),
            .groups = "keep")

YGB.prec.end <- prc.ID %>% filter(ID %in% site.ID) %>%
  filter(yr > 130) %>%
  mutate(doy = time - yr*365) %>%
  group_by(doy) %>%
  summarise(prc = mean(prc),
            .groups = "keep")

ggplot(data = YGB.prec) +
  geom_line(aes(x = doy, y = 86400*30*prc)) +
  geom_line(data = YGB.prec.end,
            aes(x = doy, y = 86400*30*prc),linetype = 2) +
  labs(x = "DOY", y = "Precip (mm)") +
  theme_bw()

sum((YGB.prec %>% pull(prc))*86400*30)


###################################################################################################################

prc.ID.tropics <- prc.ID %>%
  filter(yr < 10) %>%
  group_by(yr,lat,lon,ID) %>%
  summarise(MAP = sum(prc)*86400*30,
            .groups = "keep")

world <- ne_countries(scale = "medium", returnclass = "sf")

coord.list <- continent2coord(continent)
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]


prc.ID.tropics <- prc.ID.tropics %>%
  mutate(MAP.bnd = case_when(MAP > 3000 ~ 3000,
                             TRUE ~ MAP))

ggplot(data = world) +
  geom_tile(data = prc.ID.tropics ,
            aes(x = lon, y = lat, fill = MAP.bnd),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c() +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 24))
