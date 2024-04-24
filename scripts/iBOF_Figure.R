rm(list = ls())

library(ggthemes)
library(raster)

d <- readRDS("~/Documents/projects/TrENDY.analyses/outputs/Trendy.CLM5.0.S1.evapotrans.global.v11.RDS")
d.sum <- d %>%
  filter(year >= 2000) %>%
  filter(lat <= 15,lat >= -20,
         lon >= -15, lon <= 45) %>%
  group_by(lat,lon) %>%
  summarise(ET = mean(value),
            .groups = "keep")

r <- raster(SpatialPixelsDataFrame(points = d.sum[c("lon","lat")],
                            data = d.sum["ET"],
                            tolerance = 2e-6))
A <- st_read("/home/femeunier/Desktop/FWO/CongoBasin.shp")
r2 <- crop(r, extent(A))
r3 <- raster::mask(r2, A)
temp2 <- as.data.frame(r3, xy = T)

sites <- data.frame(lat = c(0+17/60,0.8071,((1+51.5/60)+ (0+26/60))/2,0.4862, 7.135716,0.74621),
                    lon = c(25+18/60,24.4530,((19+41/60) + (23+32/60))/2,30.3897,-1.887314,26.22101),
                    names = c("Yoko","Yangambi","Djolu","Kibale","Abafour","Bafwamogo"),
                    type = c("Chronosequence","Fluxtower","Chronosequence","Chronosequence","Chronosequence","Chronosequence")) %>%
  filter(names != "Kibale")

ggplot(data = world) +
  geom_tile(data = temp2 %>%
              mutate(ET = pmin(4,pmax(0,ET)*86400)) %>%
              filter(!is.na(ET)),
            aes(x = x, y = y,
                fill = ET),
            color = "black",linewidth = 0.2,
            na.rm = TRUE, alpha = 0.5) +
  geom_sf(data = A,size = 0.5,color = "black",fill = NA) +
  # geom_sf(fill = NA) +
  geom_point(data = sites %>% dplyr::filter(names != "Abafour"),
             aes(x = lon, y = lat), size = 2,
             color = "black", fill = "black") +
  labs(x = "",y = "", fill = "Biome") +
  scale_x_continuous(limits = c(11,34.1),expand = c(0,0)) +
  scale_y_continuous(limits = c(-13.5,9.5),expand = c(0,0)) +
  scale_fill_distiller(palette = "Blues",direction = 1) +
  theme_map() +
  guides(fill = "none")
