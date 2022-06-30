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

sf_use_s2(FALSE)

continent = "Tropics" # Africa or America or Tropics

# ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_ssp370_r1i1p1f2_gn_201501-204912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_ssp370_r1i1p1f2_gn_205001-210012.nc")
# ncfiles.cfile <- "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_piControl_r1i1p1f2_gn_196001-204912.nc"

# ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_ssp585_r1i1p1f2_gn_201501-204912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_ssp585_r1i1p1f2_gn_205001-210012.nc")
# ncfiles.cfile <- "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_piControl_r1i1p1f2_gn_196001-204912.nc"
#
# ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_ssp126_r1i1p1f2_gn_201501-204912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_ssp126_r1i1p1f2_gn_205001-210012.nc")
# ncfiles.cfile <- "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_piControl_r1i1p1f2_gn_196001-204912.nc"

# ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_1pctCO2_r4i1p1f2_gn_185001-194912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_1pctCO2_r4i1p1f2_gn_195001-199912.nc")
# ncfiles.cfile <- "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_piControl_r1i1p1f2_gn_196001-204912.nc"

# ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_185001-185912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_186001-186912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_187001-187912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_188001-188912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_189001-189912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_190001-190912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_191001-191912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_192001-192912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_193001-193912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_194001-194912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_195001-195912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_196001-196912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_197001-197912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_198001-198912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_1pctCO2_r1i1p1f1_gn_199001-199912.nc")
# ncfiles.cfile <- "/home/femeunier/Downloads/cVeg_Lmon_SAM0-UNICON_piControl_r1i1p1f1_gn_044101_045012.nc"

# ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_NorCPM1_1pctCO2_r1i1p1f1_gn_000102-016412.nc")
# ncfiles.cfile <- c("/home/femeunier/Downloads/cVeg_Lmon_NorCPM1_piControl_r1i1p1f1_gn_040101-050012.nc")

# ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_1pctCO2_r1i1p1f2_gn_185001-194912.nc",
#              "/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_1pctCO2_r1i1p1f2_gn_195001-199912.nc")
# ncfiles.cfile <- c("/home/femeunier/Downloads/cVeg_Lmon_UKESM1-0-LL_piControl_r1i1p1f2_gn_375001-383912.nc")

# ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_TaiESM1_1pctCO2_r1i1p1f1_gn_000102-015012.nc")
# ncfiles.cfile <- c("/home/femeunier/Downloads/cVeg_Lmon_TaiESM1_piControl_r1i1p1f1_gn_060101-070012.nc")

ncfiles <- c("/home/femeunier/Downloads/cVeg_Lmon_IPSL-CM6A-LR_1pctCO2_r1i1p1f1_gr_185001-199912.nc")
ncfiles.cfile <- c("/home/femeunier/Downloads/cVeg_Lmon_IPSL-CM6A-LR_piControl_r1i1p1f1_gr_355001-384912.nc")

world <- ne_countries(scale = "medium", returnclass = "sf")

model.OP <- detect.AS(location = continent,
                      ncfiles,
                      ncfiles.cfile)

###############################################################################################
model.OP[["TS.AS"]] <- model.OP[["TS.AS"]] %>%
  mutate(continent = coord2continent(lon))

saveRDS(model.OP[["TS.AS"]] %>% mutate(lon = round(100*lon)/100,
                                       lat = round(100*lat)/100),
        file = "./outputs/AS.TaiESM1.RDS")

coord.list <- continent2coord(continent)
min.lon.plot <- coord.list[[2]][1]; max.lon.plot <- coord.list[[2]][2]; min.lat.plot <- coord.list[[2]][3]; max.lat.plot <- coord.list[[2]][4]
min.lon.analysis <- coord.list[[1]][1]; max.lon.analysis <- coord.list[[1]][2]; min.lat.analysis <- coord.list[[1]][3]; max.lat.analysis <- coord.list[[1]][4]

################################################################################################
# Plots

Nrows2plot <- ifelse(continent == "Tropics",4,1)

ggplot(data = world) +
  geom_tile(data = model.OP[["criteria"]],
            aes(x = lon, y = lat,fill = value),na.rm = TRUE) +
  geom_sf(fill = NA) +
  coord_sf(xlim = c(min.lon.analysis, max.lon.analysis),
           ylim = c(min.lat.analysis, max.lat.analysis),
           expand = FALSE) +
  facet_wrap(~ var, nrow = Nrows2plot) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(mapping = aes(x = yr,y = cVeg,group = interaction(lat,lon))) +
  geom_line(data = model.OP[["TS.AS"]],
            size = 0.1, color = "darkgrey")+
  geom_line(data = model.OP[["TS.AS"]] %>% filter(AS),
            aes(group = as.factor(interaction(lat,lon)),
                color = Trend)) +
  geom_point(data = model.OP[["TS.AS"]] %>% filter(all.crit),
             aes(colour = as.factor(Trend)), size = 2) +
  facet_wrap(~ continent) +

  theme_bw()

ggplot(data = world) +
  geom_tile(data = model.OP[["cVeg.change"]],
            aes(x = lon, y = lat,fill = cVeg),na.rm = TRUE) +
  geom_sf(fill = NA) +
  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "white") +
  coord_sf(xlim = c(min.lon.analysis, max.lon.analysis),
           ylim = c(min.lat.analysis, max.lat.analysis),
           expand = FALSE) +
  facet_wrap(~ timing,nrow = max(1,(Nrows2plot - 1))) +
  labs(x = "",y = "") +
  theme_bw()

ggplot(data = world) +
  geom_tile(data = model.OP[["TS.AS"]] %>% filter(AS) %>% group_by(lat,lon) %>% slice_head(n = 1),
            aes(x = lon, y = lat,fill = Trend),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  scale_fill_manual(values = c("red","darkgreen"), breaks = c("-","+")) +
  labs(x = "",y = "") +
  theme_bw() +
  guides(fill = "none") +
  theme(text = element_text(size = 24))


cumAS <- model.OP[["TS.AS"]] %>%
  filter.continent(continent = "Tropics") %>%
  group_by(lat, lon, continent) %>%
  mutate(CO2 = yr2CO2(yr)) %>%
  mutate(cumAS = cummax(all.crit)) %>%
  group_by(yr,continent) %>%
  summarise(frac.AS = sum(cumAS)/length(AS),
            total.cVeg = sum(cVeg,na.rm = TRUE),
            CO2 = mean(CO2),
            .groups = "keep")

ggplot(data = cumAS) +
  geom_line(aes(x = yr, y = frac.AS, color = continent)) +
  theme_bw()

df.control <- read.and.filter.ncfile(ncfiles.cfile,
                                     continent2coord("World")[[1]],
                                     var = "cVeg",
                                     aggr = TRUE,
                                     yr.rel = 1:2)

coord <- df.control %>% filter(yr == 0) %>%
  dplyr::select(lat,lon) %>%
  ungroup() %>%
  distinct()

Gridarea <- RCMIP5:::calcGridArea(lon = as.vector(unique(coord$lon)),
                                  lat = as.vector(unique(coord$lat))) %>%
  melt() %>% mutate(Var1 = (as.vector(unique(coord$lon)))[Var1],
                    Var2 = (as.vector(unique(coord$lat)))[Var2]) %>%
  rename(lon = Var1,
         lat = Var2) %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100)

Veg.dyn <- model.OP[["TS.AS"]]  %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100) %>%
  filter.continent(continent = "Tropics") %>%
  left_join(Gridarea %>% rename(area = value),
            by = c("lat","lon")) %>%
  group_by(lat,lon) %>%
  mutate(tot.cVeg = cVeg*area)

ggplot(data = world) +
  geom_tile(data = Veg.dyn %>% filter(yr == 0),
            aes(x = lon, y = lat,fill = area),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  coord_sf(xlim = c(min.lon.plot, max.lon.plot),
           ylim = c(min.lat.plot, max.lat.plot),
           expand = FALSE) +
  scale_fill_viridis_c() +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 24))


ggplot(data =  Veg.dyn %>%
         mutate(cont = coord2continent(lon)) %>%
         group_by(yr,cont) %>%
         summarise(tot.cVeg = sum(tot.cVeg,na.rm = TRUE)/1e12,
                   .groups = "keep") %>%
         group_by(cont) %>%
         mutate(tot.Veg.rol.anomaly = compute.rolling.means(tot.cVeg,yr,Nyears = 10))
       ) +
  geom_line(aes(x = yr, y = tot.Veg.rol.anomaly, color = cont)) +
  theme_bw()

###########################################################################################################
# vs Global warming

Temp.file <- c("/home/femeunier/Documents/projects/CongoAS/data/ts_Amon_IPSL-CM6A-LR_1pctCO2_r1i1p1f1_gr_185001-199912.nc")

Tmp <- read.and.filter.ncfiles(ncfiles = Temp.file,
                               coord.analysis = continent2coord("World"),
                               var = "ts",
                               aggr = TRUE,
                               progressbar = TRUE) %>%

  mutate(tas = ts -273.15) %>%
  group_by(lat, lon) %>%
  mutate(ID = cur_group_id())

Tmp.init <- Tmp %>%
  group_by(lat,lon) %>%
  mutate(tas.mov = compute.rolling.means(tas,yr,Nyears = 10))

ggplot(data = Tmp.init %>% group_by(yr) %>%
         summarise(tas.mov.m  = mean(tas.mov),
                   tas.mov.sd = sd(tas.mov))) +
  geom_line(aes(x = yr, y = tas.mov.m)) +
  geom_ribbon(aes(x = yr, y = tas.mov.m,
                  ymin = tas.mov.m - tas.mov.sd, ymax = tas.mov.m + tas.mov.sd),
              alpha = 0.1) +
  theme_bw()

Tmp.diff <- Tmp.init %>%
  mutate(yr = round(yr)) %>%
  filter(yr %in% c(5,140)) %>%
  mutate(timing = case_when(yr == 5 ~ "Init",
                            TRUE ~ "End")) %>%
  dplyr::select(-yr) %>%
  group_by(lat,lon,timing) %>%
  summarise(tas = mean(tas.mov),
            .groups = "keep") %>%
  ungroup() %>%
  pivot_wider(names_from = timing,
              values_from = tas) %>% mutate(diff = (End - Init))

ggplot(data = world) +
  geom_tile(data = Tmp.diff,
            aes(x = lon, y = lat,fill = diff),
            na.rm = TRUE, alpha = 0.8, color = NA) +
  geom_sf(fill = NA, color = "black") +
  scale_fill_viridis_c() +
  # coord_sf(xlim = c(min.lon.plot, max.lon.plot),
  #          ylim = c(min.lat.plot, max.lat.plot),
  #          expand = FALSE) +
  labs(x = "",y = "") +
  theme_bw() +
  theme(text = element_text(size = 24))


Tmp.GW <- Tmp.init %>%
  mutate(lon = round(100*lon)/100,
         lat = round(100*lat)/100) %>%
  left_join(Gridarea %>% rename(area = value),
            by = c("lat","lon")) %>%
  group_by(yr) %>%
  summarise(tas = sum(tas*area,na.rm = TRUE)/sum(area),
            .groups = "keep") %>%
  ungroup() %>%
  mutate(tas.anomaly = compute.rolling.means(tas,yr,Nyears = 10))

plot(Tmp.GW$yr,Tmp.GW$tas.anomaly,type = "l")

cumAS.GW <- cumAS %>% left_join(Tmp.GW,
                                by = "yr")

ggplot(data = cumAS.GW) +
  geom_line(aes(x = tas.anomaly, y = 100*frac.AS, color = continent)) +
  theme_bw()

# ggplot(data = world) +
#   geom_tile(data = Gridarea,
#             aes(x = lon, y = lat,fill = value),
#             na.rm = TRUE, alpha = 0.8, color = NA) +
#   geom_sf(fill = NA, color = "black") +
#   scale_fill_viridis_c()+
#   labs(x = "",y = "") +
#   theme_bw() +
#   theme(text = element_text(size = 24))



