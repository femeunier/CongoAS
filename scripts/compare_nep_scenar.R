# rm(list = ls())
#
# library(ncdf4)
# library(dplyr)
# library(ggplot2)
# library(reshape2)
# library(zoo)
# library(plotbiomes)
#
# ncfile1 <- "/home/femeunier/Desktop/land-hist.nep.nc"
# nc <- nc_open(ncfile1)
#
# all.lats <- ncvar_get(nc,"lat")
# all.lons <- ncvar_get(nc,"lon")
# all.times <- ncvar_get(nc,"time")
# nep1 <- ncvar_get(nc,"nep")
#
# nc_close(nc)
#
# polys <- geometry(plotbiomes::Whittaker_biomes_poly)
#
# ncfile2 <- "/home/femeunier/Desktop/land-cCO2.nep.nc"
#
# nc <- nc_open(ncfile2)
#
# all.lats <- ncvar_get(nc,"lat")
# all.lons <- ncvar_get(nc,"lon")
# all.times <- ncvar_get(nc,"time")
# nep2 <- ncvar_get(nc,"nep")
#
# nc_close(nc)
#
# df <- data.frame(time = all.times,
#                  nep1 = as.vector(nep1[1,76,]),
#                  nep2 = as.vector(nep2[1,76,]))
#
# plot(df$time/365,
#      df$nep1 - df$nep2,type = "l")
# lines(df$time/365,rollapply(df$nep1 - df$nep2,
#                             width = 12, FUN = mean, align = "center", partial = TRUE, na.rm = TRUE),
#       col = "red",lty = 1)
# abline(h = 0, col = "red",lty = 2)
#
# plotbiomes
#
# library(raster)
#
# x<-c(0,10,25)
# y<-c(100,150,400)
# coords = cbind(x, y)
# sp = SpatialPoints(coords)
# e <- as.data.frame(raster::extract(polys,sp)) %>%
#   rename(point.id = id.y,
#          id = id.x) %>% left_join(biome.id %>% dplyr::select(-biome_id),
#                                   by = "id")

rm(list = ls())

library(tidyr)
library(ncdf4)
library(dplyr)
library(reshape2)
library(raster)
library(ggplot2)
library(plotbiomes)

pr.file <- "/home/femeunier/Documents/projects/CongoAS/data/CoE/land-hist.pr.nc"

nc <- nc_open(pr.file)

all.lats <- ncvar_get(nc,"lat")
all.lons <- ncvar_get(nc,"lon")
all.times <- (ncvar_get(nc,"time")/365 + 1850)
pr <- ncvar_get(nc,"pr",verbose = FALSE)

nc_close(nc)

tas.file <- "/home/femeunier/Documents/projects/CongoAS/data/CoE/land-hist.tas.nc"
nc2 <- nc_open(tas.file)

all.lats2 <- ncvar_get(nc2,"lat")
all.lons2 <- ncvar_get(nc2,"lon")
all.times2 <- (ncvar_get(nc2,"time")/365 + 1850)
tas <- ncvar_get(nc2,"tas")

nc_close(nc2)

df.climate <- bind_rows(list(melt(pr) %>%
                 rename(lon = Var1,
                        lat = Var2,
                        time = Var3) %>%
                 mutate(lat = all.lats[lat],
                        lon = all.lons[lon],
                        time = all.times[time]) %>%
                 filter(time > 1984) %>%
                 mutate(var = "pr"),
               melt(tas) %>%
                      rename(lon = Var1,
                             lat = Var2,
                             time = Var3) %>%
                      mutate(lat = all.lats[lat],
                             lon = all.lons[lon],
                             time = all.times[time]) %>%
                      filter(time > 1984) %>%
                      mutate(var = "tas")
               ))

df.climate.wide <- df.climate %>%
  pivot_wider(names_from = var,
             values_from = value) %>%
  mutate(year = floor(time)) %>%
  group_by(year,lat,lon) %>%
  mutate(MAP = mean(pr)*86400*365,
         MAT = tas - 273.15)

polys <- geometry(plotbiomes::Whittaker_biomes_poly)

df.climate.wide.av <- df.climate.wide %>%
  group_by(lon,lat) %>%
  summarise(MAP.m = mean(MAP),
            MAT.m = mean(MAT),
            .groups = "keep")

df.climate.wide.av.na <- df.climate.wide.av %>%
  filter(!is.na(MAT.m),!is.na(MAP.m))

coords <- cbind(id = 1:nrow(df.climate.wide.av.na),
                df.climate.wide.av.na %>% pull(MAT.m),
                df.climate.wide.av.na %>% pull(MAP.m)/10)

biome.id <- (plotbiomes::Whittaker_biomes) %>% dplyr::select(biome_id,biome) %>% distinct() %>% mutate(id = 1:9)

sp = SpatialPoints(coords[,c(2,3)])
e <- as.data.frame(raster::extract(polys,sp)) %>%
  rename(point.id = id.y,
         id = id.x) %>% left_join(biome.id %>% dplyr::select(-biome_id),
                                  by = "id")


df.climate.wide.av.na["biome"] <- e$biome

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

df.climate.wide.av.na.lon <- df.climate.wide.av.na %>%
  mutate(lon = case_when(lon > 180 ~ lon-360,
                         TRUE ~ lon))

ggplot(data = world) +
  geom_tile(data = df.climate.wide.av.na.lon,
            aes(x = lon, y = lat,fill = MAT.m),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  scale_fill_gradient(low = "white",high = "red",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = world) +
  geom_tile(data = df.climate.wide.av.na.lon,
            aes(x = lon, y = lat,fill = MAP.m),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  scale_fill_gradient(low = "white",high = "darkblue",na.value = "transparent",
                      limits = c(0,2000)) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = world) +
  geom_tile(data = df.climate.wide.av.na.lon %>% filter(!is.na(biome)),
            aes(x = lon, y = lat,fill = as.factor(biome)),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "") +
  theme_bw()

whittaker_base_plot()


ncfile1 <- "/home/femeunier/Documents/projects/CongoAS/data/CoE/land-hist.nep.nc"
nc <- nc_open(ncfile1)

all.lats <- ncvar_get(nc,"lat")
all.lons <- ncvar_get(nc,"lon")
all.times <- (ncvar_get(nc,"time")/365 + 1850)
nep1 <- ncvar_get(nc,"nep")

nc_close(nc)


ncfile2 <- "/home/femeunier/Documents/projects/CongoAS/data/CoE/land-cCO2.nep.nc"

nc <- nc_open(ncfile2)

all.lats <- ncvar_get(nc,"lat")
all.lons <- ncvar_get(nc,"lon")
all.times <- (ncvar_get(nc,"time")/365 + 1850)
nep2 <- ncvar_get(nc,"nep")

nc_close(nc)

df.nep <- bind_rows(list(melt(nep1) %>%
                           rename(lon = Var1,
                                  lat = Var2,
                                  time = Var3) %>%
                           mutate(lat = all.lats[lat],
                                  lon = all.lons[lon],
                                  time = all.times[time]) %>%
                           filter(time > 1984) %>%
                           mutate(var = "nep",
                                  scenar = "land-hist"),
                         melt(nep2) %>%
                           rename(lon = Var1,
                                  lat = Var2,
                                  time = Var3) %>%
                           mutate(lat = all.lats[lat],
                                  lon = all.lons[lon],
                                  time = all.times[time]) %>%
                           filter(time > 1984) %>%
                           mutate(var = "nep",
                                  scenar = "land-cCO2"))) %>%
  filter(!is.na(value))


df.nep.biome <- df.nep %>%
  mutate(lon = case_when(lon > 180 ~ lon-360,
                         TRUE ~ lon)) %>%
  left_join(df.climate.wide.av.na.lon,
            by = c("lon","lat"))


df.nep.biome.wide <- df.nep.biome %>%
  pivot_wider(names_from = scenar,
              values_from = value) %>%
  mutate(diff = `land-cCO2` - `land-hist`)

df.nep.biome.wide.sum <- df.nep.biome.wide %>%
  mutate(year = floor(time)) %>%
  group_by(biome,year) %>%
  summarise(land_hist = mean(`land-hist`),
            land_cCO2 = mean(`land-cCO2`),
            diff.m = mean(diff),
            diff.rel = 100*mean(diff)/max(abs((`land-cCO2`))),
            .groups = "keep")

ggplot(data = df.nep.biome.wide,
       aes(x = MAT.m, y = MAP.m)) +
  geom_raster(aes(fill = diff)) +
  theme_bw()

ggplot(data = df.nep.biome.wide.sum %>% filter(!is.na(biome),
                                               year <= 2013)) +
  geom_line(aes(x = year, y = diff.m, color = as.factor(biome))) +
  theme_bw()

df2plot.long <- df.nep.biome.wide.sum %>% filter(!is.na(biome),
                                                 year <= 2013) %>%
  pivot_longer(cols = c(land_hist,land_cCO2),
               names_to = "experiment",
               values_to = "nep")
ggplot(data = df2plot.long) +
  geom_line(aes(x = year, y = nep,color = as.factor(biome))) +
  facet_wrap(~ experiment) +
  theme_bw()
