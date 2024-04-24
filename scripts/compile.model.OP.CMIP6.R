rm(list = ls())

library(dplyr)
library(ggplot2)
library(zoo)
library(reshape2)
library(CongoAS)
library(ggthemes)

system2("rsync",
        paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/model.OP.*",
              "./outputs/"))

cf <- list.files("./outputs/",
                 pattern = "model.OP.*",full.names = TRUE)

cf.op.files <- cf[
  (!(grepl("*\\.Tmp\\.*",basename(cf))))]
cf.tmp.files <- cf[
  grepl("*\\.Tmp\\.*",basename(cf))]

df.op <- data.frame(file = cf.op.files,
             name = tools::file_path_sans_ext(basename(cf.op.files)),
             type = "output",
             model = unlist(lapply(strsplit(basename(cf.op.files),
                                            split = "\\."),
                                   "[[",3)),
             variant = unlist(lapply(strsplit(basename(cf.op.files),
                                              split = "\\."),
                                     "[[",4)))

all.criteria <- all.ts <- all.cVeg <- all.grid <-
  data.frame()

for (i in seq(1,nrow(df.op))){

  model.OP.file <- df.op$file[i]
  cmodel <- df.op$model[i]
  cvariant <- df.op$variant[i]

  print(paste0("- ",cmodel," ",cvariant))

  cdata <- readRDS(model.OP.file)

  all.criteria <- bind_rows(list(
    all.criteria,
    cdata[["criteria"]] %>%
      mutate(model = cmodel,
             variant = cvariant)
  ))

  all.ts <- bind_rows(list(
    all.ts,
    cdata[["TS.AS"]] %>%
      ungroup() %>%
      mutate(year = year - min(year)) %>%
      mutate(model = cmodel,
             variant = cvariant)
  ))

  all.cVeg <- bind_rows(list(
    all.cVeg,
    cdata[["cVeg.change"]] %>%
      mutate(model = cmodel,
             variant = cvariant)
  ))

  all.grid <- bind_rows(list(
    all.grid,
    cdata[["df.grid"]] %>%
      mutate(model = cmodel,
             variant = cvariant)
  ))

}

saveRDS(all.criteria,
        "./outputs/all.criteria.RDS")
saveRDS(all.ts,
        "./outputs/all.ts.RDS")
saveRDS(all.cVeg,
        "./outputs/all.cVeg.RDS")
saveRDS(all.grid,
        "./outputs/all.grid.RDS")

################################################################################
# Now temperature

df.Tmp <- data.frame(file = cf.tmp.files,
                     name = tools::file_path_sans_ext(basename(cf.tmp.files)),
                     type = "Tmp",
                     model = unlist(lapply(strsplit(basename(cf.tmp.files),
                                                    split = "\\."),
                                           "[[",4)),
                     variant = unlist(lapply(strsplit(basename(cf.tmp.files),
                                                      split = "\\."),
                                             "[[",5)))

all.temp <-
  data.frame()

for (i in seq(1,nrow(df.Tmp))){

  tmp.file <- df.Tmp$file[i]
  cmodel <- df.Tmp$model[i]
  cvariant <- df.Tmp$variant[i]

  print(paste0("- ",cmodel," ",cvariant))

  Tmp <- readRDS(tmp.file)
  Tmp.init <- Tmp %>%
    group_by(lat,lon) %>%
    mutate(tas.mov =
             compute.rolling.means(tas,year,Nyears = 10))

  coord <- Tmp.init %>%
    filter(year == year[1]) %>%
    dplyr::select(lat,lon) %>%
    ungroup() %>%
    distinct()

  Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                    lat = as.vector(unique(coord$lat))) %>%
    melt() %>% mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
                      Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
    rename(lon = Var1,
           lat = Var2)

  Tmp.GW <- Tmp.init %>%
    left_join(Gridarea %>%
                rename(area = value),
              by = c("lat","lon")) %>%
    group_by(year) %>%
    summarise(tas = sum(tas*area,na.rm = TRUE)/sum(area),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(tas.anomaly =
             compute.rolling.means(tas,year,Nyears = 10))

  all.temp <- bind_rows(all.temp,
                        Tmp.GW %>%
                          mutate(model = cmodel,
                                 variant = cvariant))

}

ggplot(data = all.temp %>%
         group_by(model,variant) %>%
         mutate(year = year - year[1]),
       aes(x = year, y = tas.anomaly,
           color = interaction(model,variant))) +
  geom_line() +
  scale_x_continuous(limits = c(0,150),
                     expand = c(0,0)) +
  theme_bw() +
  guides(color = "none")

cumAS <- all.ts %>%
  filter(Trend == "-") %>%
  mutate(continent = coord2continent(lon)) %>%
  filter.continent(continent = "Tropics") %>%
  group_by(model, variant, lat, lon, continent) %>%
  mutate(CO2 = yr2CO2(year)) %>%
  mutate(cumAS = cummax(all.crit)) %>%
  group_by(model, variant, year,continent) %>%
  summarise(frac.AS = sum(cumAS)/length(AS),
            total.cVeg = sum(cVeg,na.rm = TRUE),
            CO2 = mean(CO2),
            .groups = "keep")

ggplot(data = cumAS) +
  geom_line(aes(x = year, y = frac.AS,
                color = interaction(model,variant))) +
  facet_wrap(~ continent) +
  scale_x_continuous(limits = c(0,150),
                     expand = c(0,0)) +
  theme_bw() +
  guides(color = "none")

cumASvsTemp <- cumAS %>%
  left_join(all.temp,
            by = c("model","variant","year"))

# Vs regional temperature?
ggplot(data = cumASvsTemp) +
  geom_line(aes(x = tas.anomaly, y = frac.AS,
                color = interaction(model,variant))) +
  facet_wrap(~ continent) +
  theme_bw() +
  guides(color = "none")


coord.list <- continent2coord("Tropics")
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


ggplot(data = world) +
  geom_tile(data = all.cVeg %>%
              filter(model == model[1]),
            aes(x = lon, y = lat,fill = cVeg),na.rm = TRUE) +
  geom_sf(fill = NA) +
  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "white") +
  coord_sf(xlim = c(min.lon.analysis, max.lon.analysis),
           ylim = c(min.lat.analysis, max.lat.analysis),
           expand = FALSE) +
  facet_wrap(~ timing) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = world) +
  geom_raster(data = all.ts %>%
              mutate(model.variant = paste0(model,".",variant)) %>%
              filter(AS) %>%
              filter(model %in% c("EC-Earth3-Veg",
                                  "SAM0-UNICON",
                                  "TaiESM1",
                                  "GFDL-ESM4",
                                  "MPI-ESM1-2-LR",
                                  "UKESM1-0-LL",
                                  "NorCPM1")) %>%
              group_by(model.variant,lat,lon) %>%
              slice_head(n = 1) %>%
              ungroup() ,
            aes(x = lon, y = lat,fill = Trend),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  coord_sf(xlim = c(-85,50),
           ylim = c(-25,20),
           expand = FALSE) +
  scale_fill_manual(values = c("red","darkgreen"),
                    breaks = c("-","+")) +
  labs(x = "",y = "") +
  facet_wrap(~ model.variant) +
  theme_map() +
  guides(fill = "none")


ggplot(data = all.ts %>%
         mutate(model.variant = paste0(model,".",variant)) %>%
         filter(AS)) +
  geom_line(aes(x = year, y = cVeg,
                color = Trend,
                group = interaction(lon,lat))) +
  facet_wrap(~ model.variant) +
  scale_x_continuous(limits = c(0,150),
                     expand = c(0,0)) +
  theme_bw()



ggplot(data = world) +
  geom_raster(data = all.cVeg %>%
              mutate(model.variant = paste0(model,".",variant)) %>%
              filter(timing == "Change"),
            aes(x = lon, y = lat,fill = cVeg),na.rm = TRUE) +
  geom_sf(fill = NA) +
  scale_fill_gradient2(low = "red",high = "darkgreen",mid = "white",
                      limits = c(-5,5),oob = scales::squish,
                       na.value = "white") +
  coord_sf(xlim = c(min.lon.analysis, max.lon.analysis),
           ylim = c(min.lat.analysis, max.lat.analysis),
           expand = FALSE) +
  facet_wrap(~ model.variant) +
  labs(x = "",y = "") +
  theme_bw()


