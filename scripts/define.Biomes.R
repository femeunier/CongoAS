rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(raster)

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.Climate.RDS",
                      "./outputs/"))


Climate <- readRDS("./outputs/CMIP6.Climate.RDS") %>%
  filter(!is.na(timing))

Climate.m <- Climate %>%
  group_by(scenario,var,model,lat,lon,timing) %>%
  summarise(value.m = mean(value.m),
            .groups = "keep")

models <- unique(Climate.m$model)
vars <- unique(Climate.m$var)
timings <- unique(Climate.m$timing)
scenarios <- unique(Climate.m$scenario)

Climate.m %>%
  group_by(model) %>%
  filter(lat == lat[1],
         lon == lon[1]) %>%
  summarise(N = n(),
            .groups = "keep")

df2fill <- data.frame()
df.filled <- data.frame()

for (imodel in seq(1,length(models))){

  cmodel <- models[imodel]

  print(cmodel)

  cdataset <- Climate.m %>%
    filter(model == cmodel)

  for (itiming in seq(1,length(timings))){

    ctiming <- timings[itiming]
    ccdataset <- cdataset %>%
      filter(timing == ctiming)

    for (iscenario in seq(1,length(scenarios))){

      cscenario <- scenarios[iscenario]
      cccdataset <- ccdataset %>%
        filter(scenario == cscenario)

      for (ivar in seq(1,length(vars))){

        cvar <- vars[ivar]
        ccccdataset <- cccdataset %>%
          filter(var == cvar)

         if (nrow(ccccdataset) == 0){
           is.empty = TRUE

           df2fill <- bind_rows(list(df2fill,
                                     data.frame(model = cmodel,
                                                timing = ctiming,
                                                scenario = cscenario,
                                                var = cvar)))

           df2complete <- Climate.m %>%
             filter(model == "MPI-ESM1-2-LR",    # Only model "full"
                    timing == ctiming,
                    scenario == cscenario,
                    var == cvar)

           lat <- (df2complete[["lat"]])
           lon <- (df2complete[["lon"]])

           if (length(unique(diff(sort(unique(lat))))) > 1){
             res <- max(c(diff(sort(unique(lat))),diff(sort(unique(lon)))))
             raster.in <- raster(SpatialPixelsDataFrame(points = df2complete[c("lon","lat")],
                                                        data = df2complete["value.m"],
                                                        tolerance = res/10))
           } else {
             raster.in <- rasterFromXYZ(df2complete[,c("lon","lat","value.m")])
           }

           df.map <- cdataset %>%
             ungroup() %>%
             dplyr::select(lat,lon) %>%
             distinct() %>%
             mutate(value.m = 1)

           lat <- (df.map[["lat"]])
           lon <- (df.map[["lon"]])

           if (length(unique(diff(sort(unique(lat))))) > 1){
             res <- max(c(diff(sort(unique(lat))),diff(sort(unique(lon)))))
             raster.out <- raster(SpatialPixelsDataFrame(points = df.map[c("lon","lat")],
                                                        data = df.map["value.m"],
                                                        tolerance = res/25))
           } else {
             raster.out <- rasterFromXYZ(df.map[,c("lon","lat","value.m")] )
           }

           cdf.rst.rspld <- resample(raster.in,raster.out, method = "bilinear")
           cdf.df.rspld <- as.data.frame(rasterToPoints(cdf.rst.rspld)) %>% rename(lon = x,
                                                                                   lat = y)

           df.filled <- bind_rows(list(df.filled,
                                       cdf.df.rspld %>%
                                         mutate(model = cmodel,
                                                timing = ctiming,
                                                scenario = cscenario,
                                                var = cvar)
                                       ))


         } else {
           is.empty = FALSE

           df.filled <- bind_rows(list(df.filled,
                                       ccccdataset
           ))

         }
      }
    }
  }
}


df.filled.mod <- df.filled %>%
  mutate(lat = round(100*lat)/100,
         lon = round(100*lon)/100)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = df.filled.mod %>%
                filter(var == "pr",
                       timing == "reference"),
              aes(x = lon, y = lat,
                  fill = value.m),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  scale_fill_gradient(low = "white",high = "darkblue",na.value = "transparent") +
  facet_grid(scenario ~ model) +
  labs(x = "",y = "") +
  theme_bw()

ggplot() +
  geom_raster(data = df.filled.mod %>%
                filter(var == "tas",
                       timing == "reference"),
              aes(x = lon, y = lat,
                  fill = value.m - 273.15),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  scale_fill_gradient(low = "white",high = "darkred",na.value = "transparent") +
  facet_grid(scenario ~ model) +
  labs(x = "",y = "") +
  theme_bw()

Climate.m.wide <- df.filled.mod %>%
  pivot_wider(names_from = var,
              values_from = value.m)

ggplot(data = Climate.m.wide %>%
         filter(timing == "end"),
       aes(x = tas, y = pr)) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  facet_grid(scenario ~ model) +
  theme_bw()


###########################################################################################

df.all.units <- Climate.m.wide %>%
  mutate(pre = pr*86400*365/10,        # cm
         tmp = tas-273.15)             # °C

df.all.units.sum <- df.all.units %>%
  group_by(model,timing, scenario,lat,lon) %>%
  summarise(tmp = mean(tmp,na.rm = TRUE),
            pre = mean(pre,na.rm = TRUE),
            .groups = "keep") %>%
  filter(!is.na(tmp) & !is.na(pre))


coords <- cbind(id = 1:nrow(df.all.units.sum),
                df.all.units.sum$tmp,
                df.all.units.sum$pre)

biome.id <- (plotbiomes::Whittaker_biomes) %>% dplyr::select(biome_id,biome) %>% distinct() %>% mutate(id = 1:9)
polys <- geometry(plotbiomes::Whittaker_biomes_poly)

sp <- SpatialPoints(coords[,c(2,3)])
e <- as.data.frame(raster::extract(polys,sp)) %>%
  rename(point.id = id.y,
         id = id.x) %>% left_join(biome.id %>% dplyr::select(-biome_id),
                                  by = "id")


# transform to sf objects
psf   <- sf::st_as_sf(sp) %>%
  dplyr::mutate(ID_point = 1:dim(.)[1])
polsf <- sf::st_as_sf(plotbiomes::Whittaker_biomes_poly)

# remove points inside polygons
in_points  <- lengths(sf::st_within(psf,polsf))
out_points <- psf[in_points == 0, ]

# find nearest poly
nearest <- polsf[sf::st_nearest_feature(out_points, polsf) ,]  %>%
  dplyr::mutate(id_point = out_points$ID)

df.all.units.sum["biome"] <- e$biome
df.all.units.sum["type"] <- "Interpolation"

df.all.units.sum.extrap <- df.all.units.sum
df.all.units.sum.extrap[["biome"]][nearest$id_point] <- nearest$biome
df.all.units.sum.extrap[["type"]][nearest$id_point] <- "Extrapolation"

ggplot(data = df.all.units.sum.extrap %>% filter(model == "CESM2")) +
  geom_point(aes(x = tmp, y = pre, color = biome, shape = type)) +
  scale_shape_manual(values=c(1,16)) +
  facet_grid(timing ~ scenario) +
  theme_bw()

df.all.units.sum.extrap %>%
  group_by(type,biome) %>%
  summarise(N = length(pre))

saveRDS(df.all.units.sum,
        "./outputs/LUMIP.Climate.RDS")

delta_pr <- 5
delta_t <- 0.5

Whittaker_biomes.text <- plotbiomes::Whittaker_biomes %>% group_by(biome) %>%
  summarise(temp_c = mean(temp_c),
            precp_cm = mean(precp_cm),
            .groups = "keep") %>%
  mutate(temp_c = case_when(biome == "Woodland/shrubland" ~ 13,
                            biome == "Temperate seasonal forest" ~ 10,
                            biome == "Temperate rain forest" ~ 15,
                            biome == "Subtropical desert" ~ 25,
                            biome == "Tropical rain forest" ~ 25,
                            biome == "Temperate grassland/desert" ~ 8,
                            biome == "Tropical seasonal forest/savanna" ~ 25,
                            TRUE ~ temp_c),
         precp_cm = case_when(biome == "Tundra" ~ 20,
                              biome == "Woodland/shrubland" ~ 70,
                              biome == "Temperate seasonal forest" ~ 150,
                              biome == "Temperate rain forest" ~ 250,
                              biome == "Subtropical desert" ~ 35,
                              biome == "Temperate grassland/desert" ~ 15,
                              biome == "Tropical rain forest" ~ 325,
                              biome == "Tropical seasonal forest/savanna" ~ 150,
                              TRUE ~ precp_cm),
         biome = case_when(biome == "Tropical seasonal forest/savanna" ~ "Tropical seasonal \r\n forest/savanna",
                           TRUE ~ biome))


df2plot <- df.all.units.sum %>%
  # filter(type == "Interpolation") %>%
  mutate(tas.cat = round((1/delta_t)*tmp)/(1/delta_t),
         pr.cat = round(pre/delta_pr)*delta_pr)


ggplot(df2plot %>% filter(model == "CESM2"),
       aes(x=tmp, y=pre) ) +
  geom_bin2d(bins = 100,
             alpha = 0.7) +
  # scale_fill_continuous(type = "viridis") +
  geom_polygon(data = plotbiomes::Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, linewidth = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)") +
  facet_grid(scenario ~ timing) +
  theme_bw() +
  theme(legend.position = c(0.08,0.75))


world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_tile(data = df.all.units.sum %>%
              filter(!is.na(biome)),
            aes(x = lon, y = lat,
                fill = as.factor(biome)),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Biome") +
  facet_grid(scenario ~ timing) +
  theme_bw()


ggplot(df.all.units.sum %>% filter(!is.na(biome))) +
  geom_bar(aes(x = biome, fill = biome), show.legend = TRUE) +
  labs(x = "", y = "Count (-)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 20)) +
  facet_grid(scenario ~ timing) +
  guides(fill = "none")




ggplot(data = world) +
  geom_tile(data = df.all.units.sum %>%
              filter(!is.na(biome),
                     timing == "reference",
                     scenario == "land-cClim",
                     model == "CESM2"),
            aes(x = lon, y = lat,
                fill = as.factor(biome)),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Biome") +
  theme_bw()


ggplot(df.all.units.sum %>%
         filter(!is.na(biome),
                timing == "reference",
                scenario == "land-cClim",
                model == "CESM2")) +
  geom_bar(aes(x = biome, fill = biome), show.legend = TRUE) +
  labs(x = "", y = "Count (-)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        text = element_text(size = 20)) +
  # facet_grid(scenario ~ timing) +
  guides(fill = "none")

