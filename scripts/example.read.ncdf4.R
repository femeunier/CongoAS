rm(list = ls())

library(ncdf4)
library(dplyr)
library(ggplot2)
library(reshape2)

ncfile <- "/home/femeunier/Desktop/nep_Emon_IPSL-CM6A-LR_land-hist_r1i1p1f1_gr_185001-201412.nc"

nc <- nc_open(ncfile)

all.lats <- ncvar_get(nc,"lat")
all.lons <- ncvar_get(nc,"lon")
all.times <- ncvar_get(nc,"time")
nep <- ncvar_get(nc,"nep")

names(nc$var)

nc_close(nc)

# Example Yoko
clat = 0.25 ; clon = 25.25
dist.df <- expand.grid(all.lats,all.lons) %>% rename(lat = Var1,
                                                     lon = Var2) %>%
  mutate(dist = sqrt((lat - clat)**2 + (lon - clon)**2)) %>% arrange(dist) %>% slice_head(n = 1)

df <- data.frame(time = all.times,
                 nep = as.vector(nep[which.min(abs(as.vector(all.lons) - (dist.df %>% pull(lon)))),
                                     which.min(abs(as.vector(all.lats) - (dist.df %>% pull(lat)))),
                                     ]))

ggplot(data = df) +
  geom_line(aes(x = time, y = nep)) +
  theme_bw()


# Example regional

df.nep <- melt(apply(nep[,,],c(1,2),mean,na.rm = TRUE)) %>%
  rename(lon = Var1,
         lat = Var2,
         nep = value) %>%
  mutate(lat = all.lats[lat],
         lon = all.lons[lon])

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = df.nep,
            aes(x = lon, y = lat,fill = nep),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-20, 15),
           expand = FALSE) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  labs(x = "",y = "") +
  theme_bw()

