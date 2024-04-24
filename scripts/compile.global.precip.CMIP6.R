rm(list = ls())

library(dplyr)

system2("rsync",
        paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/global.precip.*",
              "./outputs/"))

cf <- list.files("./outputs/",
                 pattern = "global.precip.*",full.names = TRUE)

global.precip <- data.frame()

model.selection <-
  c("EC-Earth3-Veg","SAM0-UNICON","TaiESM1","GFDL-ESM4","MPI-ESM1-2-LR","UKESM1-0-LL","NorCPM1")


for (i in seq(1,length(cf))){

  print(i/length(cf))

  if (!grepl(pattern = "1pctCO2",basename(cf[i]))){
    next()
  } else if (!grepl(pattern = paste0(model.selection,
                                     collapse = "|"),basename(cf[i]))){
    next()
  }

  cdata <- readRDS(cf[i])
  if (! all(c("model","variant","experiment") %in% colnames(cdata))){
    next()
  }


  global.precip <- bind_rows(list(
    global.precip,
    cdata %>%
      ungroup() %>%
      filter(abs(lat) <= 25) %>%
      mutate(year = year - min(year))
  ))

}

saveRDS(global.precip,
        "./outputs/global.precip.RDS")

MAP <- global.precip


coord.list <- continent2coord("Tropics")
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

MAP.diff <- MAP %>%
  ungroup() %>%
  group_by(model,variant) %>%
  mutate(timing = case_when(year <= (min(year) + 10) ~ "start",
                            year >= (max(year) - 10) ~ "end",
                            TRUE ~ NA_character_)) %>%
  filter(timing %in% c("start","end")) %>%
  group_by(model,timing,variant,lat,lon) %>%
  summarise(MAP.m = mean(MAP.year),
            MCWD.m = mean(MCWD.year),
            .groups = "keep") %>%
  pivot_wider(names_from = "timing",
              values_from = c("MAP.m","MCWD.m")) %>%
  mutate(diff_MAP = MAP.m_end - MAP.m_start,
         diff_MCWD = MCWD.m_end - MCWD.m_start)

ggplot(data = world) +
  geom_raster(data = MAP.diff %>%
                mutate(model.variant = paste0(model,".",variant)) ,
              aes(x = lon, y = lat,
                  fill = diff_MCWD),na.rm = TRUE) +
  geom_sf(fill = NA) +
  scale_fill_gradient2(low = "red",high = "darkgreen",mid = "white",
                       na.value = "white",
                       limits = c(-100,100),
                       oob = scales::squish) +
  coord_sf(xlim = c(min.lon.analysis, max.lon.analysis),
           ylim = c(min.lat.analysis, max.lat.analysis),
           expand = FALSE) +
  facet_wrap(~ model.variant) +
  labs(x = "",y = "") +
  theme_bw()

