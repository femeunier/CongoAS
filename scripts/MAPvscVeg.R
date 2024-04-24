rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

model.selection <-
  c("EC-Earth3-Veg","SAM0-UNICON","TaiESM1","GFDL-ESM4","MPI-ESM1-2-LR","UKESM1-0-LL","NorCPM1")

global.precip <- readRDS("./outputs/global.precip.RDS") %>%
  filter(model %in% model.selection) %>%
  mutate(model.variant = paste0(model,".",variant)) %>%
    mutate(lat = round(lat,digits = 3),
           lon = round(lon,digits = 3))

global.temp <- readRDS("./outputs/global.temp.RDS") %>%
  filter(model %in% model.selection) %>%
  mutate(model.variant = paste0(model,".",variant)) %>%
  mutate(lat = round(lat,digits = 3),
         lon = round(lon,digits = 3))

allTS <- readRDS("./outputs/all.ts.RDS") %>%
  filter(model %in% model.selection)

allTS.selected <- allTS %>%
  mutate(model.variant = paste0(model,".",variant)) %>%
  mutate(lat = round(lat,digits = 3),
         lon = round(lon,digits = 3))

AbruptShifts <- allTS.selected %>%
  filter(AS,
         Trend == "-") %>%
  ungroup() %>%
  mutate(model.variant.lat.lon = paste0(model.variant,".",lat,".",lon)) %>%
  dplyr::select(model.variant.lat.lon) %>%
  distinct()

global.precip.selected.AS <- global.precip  %>%
  ungroup() %>%
  mutate(model.variant.lat.lon = paste0(model.variant,".",lat,".",lon)) %>%
  left_join(allTS.selected %>%
              dplyr::select(-c(model.variant,
                               .groups)),
            by = c("model","variant","lat","lon","year")) %>%
  left_join(global.temp %>%
              dplyr::select(-c(model.variant)),
            by = c("model","variant","lat","lon","year","experiment")) %>%
  rename(MAP = MAP.min,
         MCWD = MCWD.year)

ggplot(data = global.precip.selected.AS %>%
         dplyr::select(lat,lon,model,variant,model.variant,model.variant.lat.lon,
                       cVeg,MAP,MCWD) %>%
         na.omit(),
       aes(x = MAP, y = cVeg)) +
  # geom_line(group = interaction(model.variant,lat,lon),
  #              color = model.variant)) +
  geom_hex() +
  facet_wrap(~ model.variant) +
  scale_x_continuous(limits = c(0,1500)) +
  scale_y_continuous(limits = c(0,20)) +
  scale_fill_viridis_c(trans = "log10") +
  theme_bw() +
  guides(color = "none")


ggplot(data = global.precip.selected.AS %>%
         dplyr::select(lat,lon,model,variant,model.variant,model.variant.lat.lon,
                       cVeg,MAP,MCWD) %>%
         na.omit(),
       aes(y = MAP, x = -MCWD,z = cVeg)) +
  # geom_line(group = interaction(model.variant,lat,lon),
  #              color = model.variant)) +
  # geom_hex() +
  stat_summary_hex(fun = mean, bins = 25) +
  facet_wrap(~ model.variant) +
  scale_y_continuous(limits = c(0,1500)) +
  # scale_y_continuous(limits = c(0,20)) +
  # scale_fill_viridis_c(trans = "log10") +
  theme_bw() +
  guides(color = "none")

################################################################################

all.together <- global.precip.selected.AS %>%
  # filter(model.variant.lat.lon %in% AbruptShifts[["model.variant.lat.lon"]]) %>%
  group_by(model.variant) %>%
  mutate(timing = case_when(year %in% seq(0,9) ~ "start",
                            year %in% seq(141,150) ~ "end",
                            TRUE ~ "intermediate")) %>%
  dplyr::select(lat,lon,model,variant,model.variant,MAT,MAP,MCWD,cVeg,timing) %>%
  filter(timing %in% c("start","end"),
         !is.na(cVeg),!is.na(MAP)) %>%
  group_by(lat,lon,model,variant,model.variant,timing) %>%
  summarise(MAP = mean(MAP,na.rm = TRUE),
            MCWD = mean(MCWD,na.rm = TRUE),
            MAT = mean(MAT,na.rm = TRUE),
            cVeg = mean(cVeg,na.rm = TRUE),
            .groups = "keep") %>%
  pivot_wider(names_from = "timing",
              values_from = c(MAP,MCWD,MAT,cVeg)) %>%
  mutate(diff_cVeg = cVeg_end - cVeg_start,
         diff_MAP = MAP_end - MAP_start,
         diff_MAT = MAT_end - MAT_start,
         diff_MCWD = MCWD_end - MCWD_start)


ggplot(data = all.together,
       aes(x = diff_MAP,y = diff_cVeg, color = model.variant)) +
  geom_point(alpha = 0.5, size = 0.1) +
  facet_wrap(~ model.variant) +
  geom_hline(yintercept = 0,linetype = 2) +
  geom_vline(xintercept = 0,linetype = 2) +
  stat_smooth(method = "lm",
              formula = "y ~ log(2000 + x)") +
  theme_bw() +
  guides(color = "none")

selected <- all.together %>%
  filter(diff_cVeg < -5) %>%
  mutate(model.variant.lat.lon = paste0(model.variant,".",lat,".",lon))

global.precip.selected.AS.bound <- global.precip.selected.AS %>%
  filter(model.variant.lat.lon %in% selected[["model.variant.lat.lon"]]) %>%
  group_by(model,variant,lat,lon) %>%


  mutate(MAP.bnd = (MAP - min(MAP))/(max(MAP) - min(MAP)),
         cVeg.bnd = (cVeg - min(cVeg))/(max(cVeg) - min(cVeg)),
         MCWD.bnd = (MCWD - min(MCWD))/(max(MCWD) - min(MCWD))) %>%

  mutate(MAP.bnd = MAP/MAP[1],
         cVeg.bnd = cVeg/cVeg[1],
         MCWD.bnd = (MCWD - min(MCWD))/(max(MCWD) - min(MCWD))) %>%

  group_by(model.variant) %>%
  filter(model.variant.lat.lon %in% sample(model.variant.lat.lon,
                                           size = 1, replace = FALSE))


ggplot(data = global.precip.selected.AS.bound) +
  geom_line(aes(x = year, y = MCWD,
                group = interaction(lat,lon)),linetype = 1, color = "black") +
  geom_line(aes(x = year, y = cVeg.bnd,
                group = interaction(lat,lon)),linetype = 2,  color = "red") +
  facet_wrap(~ model.variant,
             nrow = 1) +
  theme_bw() +
  guides(color = "none")


# ggplot(data = global.precip.selected.AS.bound %>%
#          filter(model == "TaiESM1",
#                 model.variant.lat.lon == model.variant.lat.lon[1])) +
#   # geom_line(aes(x = year, y = MAP.bnd,
#   #               group = interaction(lat,lon)),linetype = 1) +
#   geom_line(aes(x = year, y = cVeg,
#                 group = interaction(lat,lon)),linetype = 1,
#             color = "black") +
#   # facet_wrap(~ model.variant) +
#   theme_bw() +
#   guides(color = "none") +
#   theme(text = element_text(size = 24))

