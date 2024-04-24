rm(list = ls())

library(tidyr)
library(dplyr)
library(ggplot2)
library(plotbiomes)
library(raster)
library(ggpubr)
library(cowplot)
library(ggrepel)
library(RColorBrewer)

# system2("scp",paste("hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/CMIP6.OP.RDS",
#                     "./outputs"))

CMIP6.OP <- readRDS("./outputs/CMIP6.OP.RDS")

CMIP6.OP.pp <- CMIP6.OP %>%
  mutate(lon = case_when(lon >= 180 ~ (lon - 360),
                         TRUE ~ lon))

CMIP6.OP.wide <- CMIP6.OP.pp %>%
  dplyr::filter(!is.na(value)) %>%
  mutate(scenario = case_when(scenario == "land-cCO2"~ "cCO2",
                              scenario == "land-hist" ~ "hist",
                              TRUE ~ scenario)) %>%
  pivot_wider(values_from = "value",
              names_from = c("scenario","variable")) %>%
  dplyr::select(-c(cCO2_tas,cCO2_pr)) %>%
  mutate(diff_nep = cCO2_nep - hist_nep,
         diff_npp = cCO2_npp - hist_npp,
         lnRR_npp = log(hist_npp/cCO2_npp)) %>%
  dplyr::filter(!is.na(hist_tas),
                !is.na(hist_pr))

CMIP6.OP.wide.fltrd.long <- CMIP6.OP.wide %>%
  dplyr::select(c("lon","lat","model","hist_tas","hist_pr","diff_nep","diff_npp","lnRR_npp")) %>%
  pivot_longer(cols = c("hist_tas","hist_pr","diff_nep","diff_npp","lnRR_npp"),
               values_to = "value",
               names_to = "variable")

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Models: "CMCC-ESM2" "CNRM-ESM2-1"   "IPSL-CM6A-LR"  "MPI-ESM1-2-LR"
ggplot(data = world) +
  geom_tile(data = CMIP6.OP.wide.fltrd.long %>% filter(model == "CNRM-ESM2-1",
                                                       variable == "hist_tas"),
            aes(x = lon, y = lat,
                fill = value),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  scale_fill_gradient(low = "white",high = "red",na.value = "transparent") +
  labs(x = "",y = "") +
  # facet_wrap( ~ variable, scales = "free") +
  theme_bw()


ggplot(data = CMIP6.OP.wide.fltrd.long ) +
  geom_boxplot(aes(y = value,x = model, fill = model), alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_bw()


polys <- geometry(plotbiomes::Whittaker_biomes_poly)

coords <- cbind(id = 1:nrow(CMIP6.OP.wide),
                CMIP6.OP.wide %>% pull(hist_tas)-273.15,
                CMIP6.OP.wide %>% pull(hist_pr)*86400*365/10)


biome.id <- (plotbiomes::Whittaker_biomes) %>% dplyr::select(biome_id,biome) %>% distinct() %>% mutate(id = 1:9)


sp = SpatialPoints(coords[,c(2,3)])
e <- as.data.frame(raster::extract(polys,sp)) %>%
  rename(point.id = id.y,
         id = id.x) %>% left_join(biome.id %>% dplyr::select(-biome_id),
                                  by = "id")

CMIP6.OP.wide["biome"] <- e$biome

CMIP6.OP.wide.m <- CMIP6.OP.wide %>%
  group_by(model,biome) %>%
  summarise(cCO2_npp_m = mean(cCO2_npp),
                        .groups = "keep")

CMIP6.OP.wide.filter <- CMIP6.OP.wide %>%
  left_join(CMIP6.OP.wide.m,
            by = c("model","biome")) %>%
  dplyr::filter(cCO2_npp > (2.5e-2*cCO2_npp_m))

models <- unique(CMIP6.OP.wide$model)
plots <- list()
for (cmodel in models){

  if (cmodel == models[length(models)]){
    plots[[cmodel]] <- ggplot(data = world) +
      geom_tile(data = CMIP6.OP.wide %>% filter(model == cmodel,
                                                !is.na(biome)),
                aes(x = lon, y = lat,
                    fill = as.factor(biome)),na.rm = TRUE, alpha = 1) +
      geom_sf(fill = NA) +
      labs(x = "",y = "", fill = "Biome") +
      facet_grid( ~ model) +
      theme_bw()
  } else {
    plots[[cmodel]] <- ggplot(data = world) +
      geom_tile(data = CMIP6.OP.wide %>% filter(model == cmodel,
                                                !is.na(biome)),
                aes(x = lon, y = lat,
                    fill = as.factor(biome)),na.rm = TRUE, alpha = 1) +
      geom_sf(fill = NA) +
      labs(x = "",y = "", fill = "Biome") +
      facet_grid( ~ model) +
      theme_bw() +
      guides(fill = "none")
  }

}

ggplot(data = world) +
  geom_tile(data = CMIP6.OP.wide.filter %>% filter(model == cmodel,
                                            !is.na(biome)),
            aes(x = lon, y = lat,
                fill = as.factor(biome)),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Biome") +
  theme_bw()

ggplot(data = world) +
  geom_tile(data = CMIP6.OP.wide.filter %>% filter(model == cmodel,
                                                   !is.na(biome)),
            aes(x = lon, y = lat,
                fill = lnRR_npp),na.rm = TRUE, alpha = 1) +
  geom_sf(fill = NA) +
  labs(x = "",y = "", fill = "Biome") +
  facet_wrap(~ model) +
  theme_bw()

ggplot(data = CMIP6.OP.wide.filter %>% filter(!is.na(biome))) +
  geom_boxplot(aes(x = biome, fill = biome, y = lnRR_npp)) +
  facet_wrap(~ model) +
  # scale_y_continuous(limits = c(0,3)) +
  labs(x = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(data = CMIP6.OP.wide.filter %>% filter(!is.na(biome))) +
  geom_boxplot(aes(x = biome, fill = biome, y = -diff_npp*86400*365)) +
  facet_wrap(~ model) +
  theme_bw() +
  labs(x = "", y = "NPP difference \r\n (cCO2 - hist) \r\n [kg/m²/yr]", fill = "Biome") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = CMIP6.OP.wide.filter %>% filter(!is.na(biome))) +
  geom_boxplot(aes(x = biome, fill = model, y = -100*diff_npp/cCO2_npp)) +
  theme_bw() +
  labs(x = "", y = "NPP relative difference \r\n (hist - cCO2) \r\n [%]", fill = "Model") +
  geom_hline(yintercept = 0,color = "black",linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.12,0.78))

ggplot(data = CMIP6.OP.wide.filter %>% filter(!is.na(biome))) +
  geom_boxplot(aes(x = biome, fill = model, y = -diff_npp*86400*365)) +
  theme_bw() +
  labs(x = "", y = "NPP difference \r\n (hist - cCO2) \r\n [kg/m²/yr]", fill = "Model") +
  geom_hline(yintercept = 0,color = "black",linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.12,0.78))

ggplot(data = CMIP6.OP.wide.filter %>% filter(!is.na(biome))) +
  geom_boxplot(aes(x = biome, fill = model, y = lnRR_npp)) +
  theme_bw() +
  labs(x = "", y = "NPP lnRR \r\n (hist/cCO2)", fill = "Model") +
  geom_hline(yintercept = 0,color = "black",linetype = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = c(0.12,0.78))


ggplot(data = CMIP6.OP.wide.filter %>% filter(!is.na(biome)),
       aes(x = hist_npp*86400*365, color = biome, y = diff_npp*86400*365)) +
  geom_point(size = 0.1) +
  facet_wrap(~ model) +
  stat_smooth(se = FALSE, method = "lm") +
  geom_hline(yintercept = 0, color = "black", linetype = 2) +
  labs( x= "NPP reference \r\n (hist) \r\n [kg/m²/yr]",
        y = "NPP difference \r\n (cCO2 - hist) \r\n [kg/m²/yr]",
        color = "Biome") +
  theme_bw()

# Binning

CMIP6.OP.wide.sum <-
  CMIP6.OP.wide.filter %>%
  filter(!is.na(biome)) %>%
  mutate(tas.cat = round(2*(hist_tas - 273.15))/2,
         pr.cat = round(hist_pr*86400*365/10/50)*50) %>%
  group_by(model,tas.cat,pr.cat) %>%
  summarise(diff_nep.m = mean(diff_nep,na.rm = TRUE),
            diff_npp.m = mean(diff_npp,na.rm = TRUE),
            lnRR_npp.m = mean(lnRR_npp,na.rm = TRUE),
            diff_npp.rel.m = 100*mean(diff_npp/hist_npp,na.rm = TRUE),
            .groups = "keep") %>%
  mutate(diff_npp.rel.m.bnd = case_when(diff_npp.rel.m > 100 ~ 100,
                                        diff_npp.rel.m < -100 ~ -100,
                                        TRUE ~ diff_npp.rel.m))

CMIP6.OP.wide.sum.long <- CMIP6.OP.wide.sum %>%
  pivot_longer(cols = c("diff_nep.m","diff_npp.m",
                        "diff_npp.rel.m.bnd","lnRR_npp.m"))


ggplot() +

  geom_tile(data = CMIP6.OP.wide.sum.long %>% filter(name == "diff_npp.m"),
            aes(x = tas.cat, y = pr.cat,
                fill = value*86400*365),na.rm = TRUE, alpha = 1) +
  labs(x = "",y = "") +
  facet_wrap(~ model, scales = "free") +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  scale_fill_gradient2(low = "red",mid = "white",high = "darkgreen",na.value = "transparent") +
  geom_text(data =Whittaker_biomes %>% group_by(biome) %>%
                    summarise(temp_c = mean(temp_c),
                              precp_cm = mean(precp_cm),
                              .groups = "keep"),
                  aes(x = temp_c, y = precp_cm, label = biome),
                  size = 3.5)  +
  labs(x = "MAT (°C)",y = "MAP (cm)", fill = "NPP difference \r\n (cCO2 - hist) \r\n [kg/m²/yr]") +
  theme_bw()


#####################################################################################################
# Polishing

tas.cat.small <- seq(min(floor(Whittaker_biomes$temp_c))-1,
                     max(ceiling(Whittaker_biomes$temp_c))+1,0.05)

pr.cat.small <- seq(min(floor(Whittaker_biomes$precp_cm))-1,
                    max(ceiling(Whittaker_biomes$precp_cm))+1,0.5)

df.grid.small <- expand.grid(tas.cat.small, pr.cat.small) %>%
  rename(temp_c = Var1,
         precp_cm = Var2) %>%
  mutate(point.id = 1:(length(tas.cat.small)*length(pr.cat.small)))

sp = SpatialPoints(df.grid.small[,c(1,2)])
e <- as.data.frame((raster::extract(polys,sp)))%>%
  rename(point.id = id.y,
         biome.id = id.x) %>%
  left_join(df.grid.small,
            by = "point.id")

rst.small <- rasterFromXYZ(e[,c("temp_c","precp_cm","biome.id")])

df.grid <- CMIP6.OP.wide.sum %>% ungroup()
df.perfect <- data.frame()

for (cmodel in models){

  print(cmodel)

  rst.large <- rasterFromXYZ(df.grid %>% filter(model == cmodel,
                                                !is.na(lnRR_npp.m)) %>%
                               dplyr::select(tas.cat,pr.cat,lnRR_npp.m))

  rst.large2 <- rasterFromXYZ(df.grid %>% filter(model == cmodel,
                                                 !is.na(diff_npp.m)) %>%
                                dplyr::select(tas.cat,pr.cat,diff_npp.m))

  rst.rspld <- resample(rst.large,rst.small,method = "bilinear")
  rst.rspld2 <- resample(rst.large2,rst.small,method = "bilinear")

  # my_window <- extent( -15, 30,0, 400)
  # # my_window <- extent( 18, 20,300, 320)
  # plot(my_window, col=NA)
  # plot(rst.large,add = TRUE)

  # now we cut
  test <- as.data.frame(rst.rspld,xy = TRUE) %>%
    rename(temp_c = x,
           precp_cm = y) %>%
    mutate(temp_c = round(100*temp_c)/100,
           precp_cm = round(10*precp_cm)/10) %>%
    left_join(e ,
              by = c("temp_c","precp_cm")) %>%
    filter(!is.na(biome.id)) %>%
    left_join(as.data.frame(rst.rspld2,xy = TRUE) %>%
                rename(temp_c = x,
                       precp_cm = y) %>%
                mutate(temp_c = round(100*temp_c)/100,
                       precp_cm = round(10*precp_cm)/10),
              by = c("temp_c","precp_cm"))


  test2plot <- test %>% mutate_all(imputeTS::na_interpolation)

  df.perfect  <- bind_rows(list(df.perfect,
                                test2plot %>% mutate(model = cmodel)))

}

Whittaker_biomes.text <- Whittaker_biomes %>% group_by(biome) %>%
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


ggplot() +
  geom_contour_filled(data = df.perfect,
                      aes(x = temp_c, y = precp_cm, z= lnRR_npp.m),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(9, "Greens")) +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "lnRR") +
  facet_wrap(~ model) +
  theme_bw() +
  theme(legend.position = c(0.08,0.75))


df.perfect.sum <- df.perfect %>%
  group_by(temp_c,precp_cm,biome.id) %>%
  summarise(lnRR_npp.m.m = mean(lnRR_npp.m,na.rm = TRUE),
            lnRR_npp.m.CV = 100*sd(lnRR_npp.m,na.rm = TRUE)/mean(lnRR_npp.m,na.rm = TRUE),
            diff_npp.m.m = mean(diff_npp.m,na.rm = TRUE),
            .groups = "keep")

ggplot() +
  geom_contour_filled(data = df.perfect.sum,
                      aes(x = temp_c, y = precp_cm, z= (lnRR_npp.m.m)),
                      alpha = 0.4,
                      bins = 9) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(9, "Greens")) +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "CV lnRR (%)") +
  theme_bw() +
  theme(legend.position = c(0.08,0.75))


ggplot() +
  geom_contour_filled(data = df.perfect.sum,
                      aes(x = temp_c, y = precp_cm, z= (lnRR_npp.m.CV)),
                      alpha = 0.4,
                      bins = 5) +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_text(data = Whittaker_biomes.text,
            aes(x = temp_c, y = precp_cm, label = biome),
            size = 3.5)  +
  scale_fill_manual(values = brewer.pal(5, "Reds")) +
  labs(x = "Temperature (°C)",
       y = "Precipitation (cm)",
       fill = "CV lnRR (%)") +
  theme_bw() +
  theme(legend.position = c(0.08,0.75),
        text = element_text(size = 20))




