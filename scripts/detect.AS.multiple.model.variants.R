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
library(stringr)
library(ncdf4)
library(ncdf4.helpers)
library(lubridate)
library(raster)

sf_use_s2(FALSE)

world <- ne_countries(scale = "medium", returnclass = "sf")
continent = "Tropics" # Africa or America or Tropics

# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/all.criteria.RDS",
#               "./outputs/"))
#
# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/all.ts.RDS",
#               "./outputs/"))
#
# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/all.cVeg.RDS",
#               "./outputs/"))
#
# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/all.grid.RDS",
#               "./outputs/"))
#
# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/global.warming.RDS",
#               "./outputs/"))


coord.list <- continent2coord(continent)
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

################################################################################################
# Plots

Nrows2plot <- ifelse(continent == "Tropics",4,1)

all.criteria <- readRDS("./outputs/all.criteria.RDS") %>%
  mutate(model.variant = paste(model,variant,sep = "."))

ggplot(data = world) +
  geom_raster(data = all.criteria,
            aes(x = lon, y = lat,fill = value),na.rm = TRUE) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(min.lon.analysis, max.lon.analysis),
           ylim = c(min.lat.analysis, max.lat.analysis),
           expand = FALSE) +
  facet_grid(model.variant ~ var) +
  labs(x = "",y = "") +
  theme_bw()


all.ts <- readRDS("./outputs/all.ts.RDS") %>%
  mutate(model.variant = paste(model,variant,sep = "."),
         continent = coord2continent(lon))


ggplot(mapping = aes(x = year,y = cVeg,group = interaction(lat,lon))) +
  geom_line(data = all.ts,
            size = 0.1, color = "darkgrey")+
  geom_line(data = all.ts %>% filter(AS),
            aes(group = as.factor(interaction(lat,lon)),
                color = Trend)) +
  geom_point(data = all.ts %>% filter(all.crit),
             aes(colour = as.factor(Trend)), size = 2) +
  facet_grid(model.variant ~ continent) +
  theme_bw()


all.cVeg.change <- readRDS("./outputs/all.cVeg.RDS") %>%
  mutate(model.variant = paste(model,variant,sep = "."))

ggplot(data = world) +
  geom_raster(data = all.cVeg.change,
            aes(x = lon, y = lat,fill = cVeg),na.rm = TRUE) +
  geom_sf(fill = NA) +
  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "white") +
  coord_sf(xlim = c(min.lon.analysis, max.lon.analysis),
           ylim = c(min.lat.analysis, max.lat.analysis),
           expand = FALSE) +
  facet_grid(model.variant ~ timing) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = world) +
  geom_raster(data = all.ts %>%
              filter(AS) %>% group_by(lat,lon) %>% slice_head(n = 1),
            aes(x = lon, y = lat,fill = Trend),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  scale_fill_manual(values = c("red","darkgreen"), breaks = c("-","+")) +
  labs(x = "",y = "") +
  facet_wrap(~ model.variant) +
  theme_bw() +
  guides(fill = "none") +
  theme(text = element_text(size = 24))


cumAS <- all.ts %>%
  filter.continent(continent = "Tropics") %>%
  group_by(lat, lon, continent, model.variant) %>%
  mutate(CO2 = yr2CO2(year)) %>%
  mutate(cumAS = cummax(all.crit)) %>%
  group_by(year,continent,model.variant) %>%
  summarise(frac.AS = sum(cumAS)/length(AS),
            total.cVeg = sum(cVeg,na.rm = TRUE),
            CO2 = mean(CO2),
            .groups = "keep")

ggplot(data = cumAS) +
  geom_line(aes(x = year, y = frac.AS, color = continent)) +
  facet_wrap(~ model.variant) +
  theme_bw()

all.grid <- readRDS("./outputs/all.grid.RDS") %>%
  mutate(model.variant = paste(model,variant,sep = "."))

all.coord <- data.frame()

for (cmodel.variant in unique(all.grid$model.variant)){

  print(cmodel.variant)

  coord <- all.grid %>%
    dplyr::filter(model.variant == cmodel.variant) %>%
    dplyr::select(model,variant,model.variant,lat,lon) %>%
    distinct() %>%
    mutate(layer = 1)


  # wbb.rspld <- raster::resample(CongoAS::water.bodies,
  #                       raster::rasterFrom(coord %>%
  #                                       dplyr::select(lon,lat)))

  wbb.rspld <- raster::resample(CongoAS::water.bodies,
                                dummy <- raster(SpatialPixelsDataFrame(points = coord[c("lon","lat")],
                                                                       data = coord["layer"],
                                                                       tolerance = 0.5)))

  crs(wbb.rspld) <- "+proj=longlat +datum=WGS84 +no_defs"
  r.Gridarea <- area(wbb.rspld)

  wbb.rspld.rspld <- raster::resample(wbb.rspld,
                                      r.Gridarea)


  all.coord <- bind_rows(list(all.coord,
                              as.data.frame(r.Gridarea,
                                            xy = TRUE) %>%
                                rename(lon = x,
                                       lat = y,
                                       Gridarea = layer) %>%
                                mutate(model.variant = cmodel.variant,
                                       model = coord$model[1],
                                       variant = coord$variant[1]) %>%
                                left_join(as.data.frame(wbb.rspld.rspld,
                                                        xy = TRUE) %>%
                                            rename(lon = x,
                                                   lat = y,
                                                   ocean.frac = Land_Cover_Type_1_Percent_1),
                                          by = c("lon","lat"))
                              ))

}

Veg.dyn <- all.ts  %>%
  mutate(lat = round(lat*10)/10,
         lon = round(lon*10)/10) %>%
  filter.continent(continent = "Tropics") %>%
  left_join(all.coord %>%
              rename(area = Gridarea) %>%
              mutate(lat = round(lat*10)/10,
                     lon = round(lon*10)/10),
            by = c("lat","lon","model","variant","model.variant")) %>%
  group_by(lat,lon) %>%
  mutate(tot.cVeg = cVeg*area)

ggplot(data = world) +
  geom_raster(data = Veg.dyn %>%
              group_by(model.variant) %>%
              filter(year == min(year)),
            aes(x = lon, y = lat,fill = log10(area)),
            na.rm = TRUE, alpha = 0.8) +
  geom_sf(fill = NA, color = "black") +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  scale_fill_viridis_c() +
  facet_wrap(~ model.variant) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 24))

ggplot(data =  Veg.dyn %>%
         mutate(cont = coord2continent(lon)) %>%
         group_by(year,cont,model.variant) %>%
         summarise(cVeg.m = mean(cVeg,na.rm = TRUE),
                   .groups = "keep") %>%
         group_by(cont,model.variant) %>%
         mutate(cVeg.m.anomaly = compute.rolling.means(cVeg.m,year,Nyears = 10),
                cVeg.m = rollmean(cVeg.m,10,align = "center",fill = NA))) +
  geom_line(aes(x = year, y = cVeg.m, color = model.variant)) +
  facet_wrap(~ cont) +
  theme_bw() +
  guides(color = "none")

###########################################################################################################
# vs Global warming

global.warming <- readRDS("./outputs/global.warming.RDS") %>%
  mutate(model.variant = paste(model,variant,sep = ".")) %>%
  group_by(model.variant) %>%
  mutate(year.rel = year - min(year)) %>%
  mutate(tas.anomaly = compute.rolling.means(tas.m,year,Nyears = 10))

ggplot(data = global.warming %>%
         filter(tas.m < 1e2),
       aes(x = year.rel,
           y = tas.anomaly,
           color = model.variant, fill = model.variant)) +

  # geom_ribbon(aes(ymin = tas.anomaly - tas.sd,
  #                 ymax = tas.anomaly + tas.sd),
  #             alpha = 0.2, color = NA) +

  geom_line() +
  theme_bw() +
  guides(color = "none")

cumAS.GW <- cumAS %>% left_join(global.warming,
                                by = c("year","model.variant"))

ggplot(data = cumAS.GW) +
  geom_line(aes(x = tas.anomaly , y = frac.AS,
                color = model.variant)) +
  facet_wrap(~continent) +
  theme_bw() +
  guides(color = "none")






