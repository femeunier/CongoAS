rm(list = ls())

library(dplyr)
library(tidyr)
library(ggplot2)
library(TrENDY.analyses)

# system2("rsync",
#         paste("-avz","hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/Trendy.global.npp.rspld_v11.RDS",
#               "./outputs/"))

Mask.CC <- readRDS("/home/femeunier/Documents/projects/CongoAS/outputs/LUMIP.Map.change.climate.RDS") %>%
  mutate(Delta_x = case_when(type.of.change == "temp" ~ sign(tmp),
                             type.of.change == "precip" ~ sign(MAP),
                             type.of.change == "both" ~ sign(MAP*tmp)),

         Delta_x2 = case_when(type.of.change == "temp" ~ tmp,
                              type.of.change == "precip" ~ MAP,
                              type.of.change == "both" ~ MAP*tmp)) %>%
  mutate(lat = as.numeric(as.vector(lat)),
         lon = as.numeric(as.vector(lon)))

ggplot(data = Mask.CC) +
  geom_bar(aes(x = Delta_x)) +
  facet_wrap(~ type.of.change) +
  theme_bw()


############################################################################################################
biome <- readRDS("/home/femeunier/Documents/projects/CongoAS/outputs/LUMIP.Climate.RDS")

biome.select <- biome %>%
  filter(timing == "beginning",
         model == "CMCC-ESM2",
         scenario == "land-hist")

lat <- biome.select$lat
lon <- biome.select$lon

res <- max(c(diff(sort(unique(lat))),diff(sort(unique(lon)))))
biome.rst <- raster(SpatialPixelsDataFrame(points = biome.select[c("lon","lat")],
                                           data = biome.select["biome"],
                                           tolerance = res/100))

############################################################################################################

LUMIP.ET <- readRDS("/home/femeunier/Documents/projects/CongoAS/outputs/CMIP6.ET.RDS") %>%
  filter(timing == "end") %>%
  group_by(scenario,var,model,lat,lon,timing) %>%
  summarise(value.m = mean(value.m),
            .groups = "keep")

LUMIP.NPP <- readRDS("/home/femeunier/Documents/projects/CongoAS/outputs/CMIP6.NPP.RDS") %>%
  filter(timing == "end") %>%
  group_by(scenario,var,model,lat,lon,timing) %>%
  summarise(value.m = mean(value.m),
            .groups = "keep")

LUMIP.WUE <- LUMIP.NPP %>%
  rename(NPP = value.m) %>%
  ungroup() %>%
  dplyr::select(-var) %>%
  left_join(LUMIP.ET %>% rename(ET = value.m) %>% ungroup() %>% dplyr::select(-var),
            by = c("lon","lat","scenario","model","timing")) %>%
  mutate(value.m = NPP/ET,
         var = "WUE") %>%
  dplyr::select(-c(NPP,ET))

LUMIP2test <- LUMIP.NPP

models <- unique(LUMIP2test$model)
LUMIP.NPP.rspld <- data.frame()

for (cmodel in models){

  print(cmodel)

  cdata <- LUMIP2test %>% filter(model == cmodel)
  keep.model <- length(cdata %>% filter(!is.na(value.m)) %>%
                         pull(scenario) %>% unique()) == 3

  if (!keep.model){
    next()
  }

  LUMIP.NPP.rspld <- bind_rows(list(LUMIP.NPP.rspld,
                                    resample.df.all.col(bigdf = cdata,
                                         raster2resample = biome.rst,
                                         var.names = "value.m",
                                         res = NULL) %>% mutate(model = cmodel)
  ))

}



LUMIP.NPP.wide <- LUMIP.NPP.rspld %>%
  mutate(lon = round(lon*100)/100,
         lat = round(lat*100)/100) %>%
  pivot_wider(names_from = "scenario",
              values_from = "value.m") %>%
  rename(S0 = `land-cCO2`,
         S1 = `land-cClim`,
         S2 = `land-hist`,) %>%
  filter(!(is.na(S0) & is.na(S1) & is.na(S2))) %>%
  left_join(Mask.CC %>% dplyr::select(lat,lon,biome_norm,keep,type.of.change,MAP,tmp,
                                      Delta_x,Delta_x2) %>%
              rename(biome = biome_norm),
            by = c("lat","lon")) %>%
  mutate(lnRR = log(S2/S1/Delta_x),
         diff = (S2 - S1)/Delta_x)    # Or Delta_x2

ggplot(data = LUMIP.NPP.wide %>%
         dplyr::filter(keep)) +
  geom_boxplot(aes(x = as.factor(biome),y = diff, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  # scale_y_continuous(limits = c(-1,1)*0.4) +
  labs(x = "", y = "Change") +
  facet_wrap(~ model,nrow = 1) +
  geom_hline(yintercept = 0,linetype = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(data = LUMIP.NPP.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "temp")) +
  geom_boxplot(aes(x = as.factor(model),y = diff, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  ggtitle("Impact of temp change") +
  # scale_y_continuous(limits = c(-1,1)*0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "Change") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = LUMIP.NPP.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "precip")) +
  geom_boxplot(aes(x = as.factor(model),y = diff, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  ggtitle("Impact of precip change") +
  # scale_y_continuous(limits = c(-1,1)*0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "Change") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = LUMIP.NPP.wide %>%
         dplyr::filter(keep) %>%
         # dplyr::filter(is.finite(lnRR)) %>%
         dplyr::filter(type.of.change == "both")) +
  ggtitle("Impact of precip + temp change") +
  geom_boxplot(aes(x = as.factor(model),y = diff, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  # scale_y_continuous(limits = c(-1,1)*0.4) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "", y = "Change") +
  facet_wrap(~ biome) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


LUMIP.NPP.wide.av <- LUMIP.NPP.wide %>%
  dplyr::filter(keep) %>%
  group_by(biome,type.of.change,lon,lat) %>%
  summarise(lnRR = mean(lnRR,na.rm = TRUE),
            diff.m = mean(diff,na.rm = TRUE),
            .groups = "keep")

ggplot(data = LUMIP.NPP.wide.av) +
  geom_boxplot(aes(x = as.factor(biome),y = diff.m*86400*365, fill = biome),
               outlier.shape = NA,
               show.legend = FALSE) +
  # scale_y_continuous(limits = c(-1,1)*0.152) +
  labs(x = "", y = "NPP change [kgC/m²/yr]") +
  facet_wrap(~type.of.change) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

