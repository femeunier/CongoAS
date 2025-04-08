rm(list = ls())

library(rnaturalearth)
library(ggplot2)
library(dplyr)

system2("rsync",
        c("-avz",
          "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Scenarios.Quan*",
          "./outputs/"))

files <- list.files("./outputs/",
                    "Scenarios.Quan*",full.names = TRUE)

df.all.vs <- data.frame()

for (cfile in files){
  df.all.vs <- bind_rows(df.all.vs,
                         readRDS(cfile) %>%
                           mutate(Delta_MAP = MAP - MAP_reference,
                                  Delta_MAT = MAT - MAT_reference))
}

ggplot(data = df.all.vs) +
  geom_density(aes(x = Delta_MAT, fill = model),
               color = NA, alpha = 0.5) +
  facet_grid(scenario ~ period) +
  guides(fill = "none") +
  theme_bw()

# nrow(df.all.vs)
df.all.vs.na <- df.all.vs %>%
  na.omit()

sort(unique(df.all.vs.na$model))

df.all.vs.na %>%
  group_by(model) %>%
  summarise(N = n(),
            Nscenario = length(unique(scenario)),
            Ntiming = length(unique(period)))

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_raster(data = df.all.vs.na %>%
                filter(period == "long_future",
                       !is.na(Delta_MAP)),
              aes(x = lon, y = lat, fill = Delta_MAP),
              na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_gradient2(limits = c(-500,500),
                       oob = scales::squish,
                       low = "darkred",
                       high = "darkblue") +
  facet_grid(scenario ~ model) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = world) +
  geom_raster(data = df.all.vs.na %>%
                filter(period == "long_future",
                       !is.na(Delta_MAT)),
              aes(x = lon, y = lat, fill = Delta_MAT),
              na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_gradient(oob = scales::squish, low = "white",
                      limits = c(0,5),high = "darkred") +
  # scale_fill_viridis_c(limits = c(0,2),
  #                      oob = scales::squish) +
  facet_grid(scenario ~ model) +
  labs(x = "",y = "") +
  theme_bw()

saveRDS(df.all.vs.na,
        "./outputs/All.future.scenarios.RDS")
