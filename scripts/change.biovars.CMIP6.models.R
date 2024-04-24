rm(list = ls())

library(dplyr)
library(ggplot2)
library(dismo)
library(tie)
library(TrENDY.analyses)
library(tidyr)

system2("rsync",paste("-avz",
                      "hpc:/data/gent/vo/000/gvo00074/felicien/R/outputs/Climate.change.CMIP6.RDS",
                      "./outputs/"))

# For regridding
biome.rst <- raster("/home/femeunier/Documents/projects/SoilSensitivity/data/AGBavit_AFR.grd")
e <- as(extent(-10, 45, -15, 10), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
biome.rst.crop <- aggregate(crop(biome.rst, e),2)


CCCMIP6 <- readRDS("./outputs/Climate.change.CMIP6.RDS")

CCCMIP6.wide <- CCCMIP6 %>% pivot_wider(names_from = var,
                                        values_from = value.m) %>%
  distinct() %>%
  mutate(pr = pr*86400*365/12) %>%
  filter(!(is.na(pr) | is.na(tasmin) | is.na(tasmax)))

models <- unique(CCCMIP6.wide$model)
models <- models[models != "CAS-ESM2-0"]   # Missing pr for historical simulation

# CCCMIP6.wide.rspld <- resample.df.all.col(CCCMIP6.wide %>% filter(model == models),
#                                           biome.rst.crop,
#                                           var.names = c("tas","pr","tasmin","tasmax"))
#
# hist(CCCMIP6.wide$pr)

CCCMIP6.2.plot <- CCCMIP6.wide %>%
  group_by(model,lat,lon) %>%
  summarise(pr = mean(pr),
            .groups = "keep")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_raster(data = CCCMIP6.2.plot,
            aes(x = lon, y = lat,
                fill = pr),na.rm = TRUE, alpha = 1) +
  geom_sf(data = world,
          fill = NA) +
  scale_fill_gradient(low = "white",high = "darkgreen",na.value = "transparent") +
  coord_sf(xlim = c(-10, 50),
           ylim = c(-15, 10),
           expand = FALSE) +
  facet_wrap(~ model) +
  labs(x = "",y = "") +
  theme_bw()



CCCMIP6.wide.bio <- readRDS("./outputs/CCCMIP6.wide.bio.RDS")
CCCMIP6.wide.bio.rspld <- readRDS("./outputs/CCCMIP6.wide.bio.rspld.RDS")

# CCCMIP6.wide.bio <- data.frame()
# CCCMIP6.wide.bio.rspld <- data.frame()
#
# for (cmodel in models){
#
#   print(cmodel)
#
#   print("- Computing biovars")
#
#   tmp.df <-
#     CCCMIP6.wide %>%
#     dplyr::filter(model == cmodel) %>%
#     ungroup() %>%
#     # dplyr::filter(scenario %in% c("historical","ssp245","ssp585"),
#     #               timing == "end") %>%
#     group_by(lat,lon,timing,scenario) %>%
#     bow(tie(bio1, bio2, bio3, bio4,
#             bio5, bio6, bio7, bio8,
#             bio9, bio10, bio11, bio12,
#             bio13, bio14, bio15, bio16,
#             bio17, bio18, bio19,MCWD) := c(biovars(pr,
#                                                    (tasmin - 273.15)*10,
#                                                    (tasmax - 273.15)*10)[c(1:19)]))
#   CCCMIP6.wide.bio <- bind_rows(list(CCCMIP6.wide.bio,
#                                      tmp.df %>%
#                                        mutate(model = cmodel)))
#
#   print("- Resampling")
#
#   CCCMIP6.wide.rspld <- resample.df.all.col(tmp.df,
#                                             biome.rst.crop,
#                                             var.names = paste0("bio",seq(1,19)))
#
#   CCCMIP6.wide.bio.rspld <- bind_rows(list(CCCMIP6.wide.bio.rspld,
#                                            CCCMIP6.wide.rspld %>%
#                                              mutate(model = cmodel)))
#
# }

# saveRDS(CCCMIP6.wide.bio,
#         "./outputs/CCCMIP6.wide.bio.RDS")
#
# saveRDS(CCCMIP6.wide.bio.rspld,
#         "./outputs/CCCMIP6.wide.bio.rspld.RDS")


# CCCMIP6.wide %>%
#   ungroup() %>%
#   dplyr::filter(model == "CAS-ESM2-0",
#                 scenario %in% c("historical")) %>%
#   dplyr::filter(lat == lat[1],lon == lon[1],
#                 timing == "beginning") %>%
#   group_by(model,lat,lon,timing,scenario) %>%
#   bow(tie(bio1, bio2, bio3, bio4,
#           bio5, bio6, bio7, bio8,
#           bio9, bio10, bio11, bio12,
#           bio13, bio14, bio15, bio16,
#           bio17, bio18, bio19,MCWD) := c(biovars(pr,
#                                                  (tasmin - 273.15)*10,
#                                                  (tasmax - 273.15)*10)[c(1:19)]))


CCCMIP6.wide.bio.long <- CCCMIP6.wide.bio.rspld %>%
  pivot_longer(cols = -c("lat","lon","model","timing","scenario"),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>%
  mutate(variable = factor(variable,
                           levels = c(paste0("bio",seq(1,19)))))

# Bio.long.sum <- CCCMIP6.wide.bio.long %>%
#   group_by(lat,lon,variable) %>%
#   summarise(value.m = mean(value,na.rm = TRUE),
#             .groups = "keep") %>%
#   group_by(variable) %>%
#   mutate(value.rel = (value.m - min(value.m,na.rm = TRUE))/
#            (max(value.m,na.rm = TRUE) - min(value.m,na.rm = TRUE)))


# CCCMIP6.wide.select <- CCCMIP6.wide %>%
#   group_by(model) %>%
#   dplyr::filter(lat == lat[1],lon == lon[1]) %>%
#   dplyr::select(-pr) %>%
#   pivot_longer(cols = c(tas,tasmin,tasmax),
#                names_to = "var",
#                values_to = "value")
#
#
# ggplot(data = CCCMIP6.wide.select %>%
#          dplyr::filter(scenario == "historical",timing == "beginning")) +
#   geom_line(aes(x = month, y = value, color = var)) +
#   facet_wrap(~model)


ggplot(data = CCCMIP6.wide.bio.long %>%
         dplyr::filter(variable == "bio1",
                       timing == "end")) +
  geom_density(aes(x = value, fill = scenario), alpha = 0.5) +
  facet_wrap(~ model, scales = "free") +
  theme_bw()

ggplot(data = CCCMIP6.wide.bio.long %>%
         dplyr::filter(variable == "bio12",
                       timing == "end")) +
  geom_density(aes(x = value, fill = scenario), alpha = 0.5) +
  facet_wrap(~ model, scales = "free") +
  theme_bw()


CCCMIP6.wide.bio.long.av <- CCCMIP6.wide.bio.long %>%
  group_by(lat,lon,timing,scenario,variable) %>%
  summarise(value.m = mean(value),
            .groups = "keep")


ggplot(data = CCCMIP6.wide.bio.long %>%
         dplyr::filter(timing == "end")) +
  geom_density(aes(x = value, fill = scenario), alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()


