rm(list = ls())

library(CongoAS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geodata)
library(ggalluvial)
library(ggrepel)
library(plotbiomes)
library(raster)

df <- read.csv("~/Documents/projects/mesi-db/data/mesi_main.csv") %>%
  mutate(across(c(x_t,x_c),
                as.numeric))

# worldclim_tile("tavg",
#                1, 0, "./data/", version="2.1")

################################################################################
# c = CO2
# d = drought
# f = fertilization
# i = irrigation
# s = Species?
# w = warming

################################################################################
full.df <- df %>%
  filter(experiment_type == "field") %>%
  mutate(treatment.cat = case_when(treatment %in% c("c","d","i","w") ~ "uni",
                                   grepl("s|f",treatment) ~ "other",
                                   TRUE ~ "multi"),
         response.cat = case_when(response %in% c("agb","bgb","lai","total_biomass",
                                                  "soc","fine_root_biomass",
                                                  "soil_total_c") ~ "Cstock",
                                  response %in% c("r_soil",
                                                  "anpp","gpp",
                                                  "npp","nep","nee","r_eco","r_a"
                                  ) ~ "Cflux",
                                  response %in% c("wue",
                                                  "wue_eco",
                                                  "wue_leaf") ~ "WUE",
                                  response %in% c("swc") ~ "SWC",
                                  response %in% c("leaf_n_mass","gs","amax",'asat',
                                                  "anet","leaf_c","vcmax","leaf_cn",
                                                  "jmax") ~ "leaf traits",
                                  TRUE ~ "other response")) %>%
  mutate(treatment.cat = factor(treatment.cat,
                                levels = c("other","multi","uni")),
         response.cat = factor(response.cat,
                               levels = c("other response","leaf traits",
                                          "WUE","SWC","Cflux","Cstock"))) %>%
  mutate(biome = climate2whittaker(mat,map/10,extrapol = TRUE))

full.df.sum <- full.df %>%
  group_by(response.cat,treatment.cat) %>%
  summarise(freq = n(),
            .groups = "keep")

ggplot(data = full.df.sum,
       aes(y = freq, axis1 = treatment.cat, axis2 = response.cat)) +
  geom_alluvium(aes(fill = response.cat),
                width = 1/12,show.legend = FALSE) +
  geom_stratum(fill = "darkgrey",width = 1/12, color = "grey") +
  scale_x_discrete(limits = c("treatment", "response"), expand = c(.05, .05)) +
  scale_fill_brewer(type = "qual", palette = "Pastel1") +
  guides(fill = "none") +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), NA)),
    stat = "stratum", size = 4, direction = "y", nudge_x = -0.1) +  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), NA)),
    stat = "stratum", size = 4, direction = "y", nudge_x = +.1) +
  theme_minimal() +
  labs(y = "Frequency") +
  theme(text = element_text(size = 20))

full.df.whit <- full.df %>%
  filter(treatment.cat != "other",
         !(response.cat %in% c("other response","leaf traits")))


ggplot(data = full.df.whit %>%
         filter(!is.na(biome))) +
  geom_point(aes(y = map/10,x = mat, color = biome, fill = biome)) +
  geom_polygon(data = plotbiomes::Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, linewidth = 0.5) +
  theme_bw() +
  theme(legend.position = c(0.2,0.7),
        text = element_text(size = 22)) +
  labs(x = "MAT (°C)", y = "MAP (cm)")

full.df.whit.sum <- full.df.whit %>%
  filter(!is.na(biome)) %>%
  group_by(response.cat,biome,treatment) %>%
  summarise(freq = n(),
            .groups = "keep") %>%
  group_by(biome) %>%
  mutate(N.biome = sum(freq)) %>%
  group_by(response.cat) %>%
  mutate(N.response = sum(freq)) %>%
  group_by(treatment) %>%
  mutate(N.treatment = sum(freq)) %>%
  ungroup()


full.df.whit.sum <- full.df.whit.sum %>%
  mutate(biome = factor(biome,
                        levels = full.df.whit.sum %>%
                          dplyr::select(biome,N.biome) %>%
                          arrange(desc(N.biome)) %>%
                          distinct() %>%
                          pull(biome) %>% rev())) %>%
  mutate(response.cat = factor(response.cat,
                        levels = full.df.whit.sum %>%
                          dplyr::select(response.cat,N.response) %>%
                          arrange(desc(N.response)) %>%
                          distinct() %>%
                          pull(response.cat) %>% rev())) %>%
  mutate(treatment = factor(treatment,
                            levels = full.df.whit.sum %>%
                              dplyr::select(treatment,N.treatment) %>%
                              arrange(desc(N.treatment)) %>%
                              distinct() %>%
                              pull(treatment) %>% rev()))

ggplot(data = full.df.whit.sum,
       aes(y = freq, axis1 = biome, axis2 = response.cat,axis3 = treatment)) +
  geom_alluvium(aes(fill = biome),
                width = 1/8,show.legend = FALSE) +
  geom_stratum(fill = "darkgrey",width = 1/8, color = "grey") +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 1, as.character(after_stat(stratum)), NA)),
    stat = "stratum", size = 4, direction = "y", nudge_x = -0.7) +
  ggrepel::geom_text_repel(
    aes(label = ifelse(after_stat(x) == 2, as.character(after_stat(stratum)), NA)),
    stat = "stratum", size = 4, direction = "y", nudge_x = +0.) +
  ggrepel::geom_text_repel(
      aes(label = ifelse(after_stat(x) == 3, as.character(after_stat(stratum)), NA)),
      stat = "stratum", size = 4, direction = "y", nudge_x = +.1) +
  theme_minimal() +
  labs(y = "Frequency") +
  theme(text = element_text(size = 20)) +
  scale_x_discrete(limits = c("biome", "response","treatment"), expand = c(.05, .05)) +
  scale_fill_brewer(palette = "Spectral") +
  guides(fill = "none")

################################################################################
# CO2

df.selected.c <- full.df %>%
  filter(treatment == "c") %>%
  filter(!(response.cat %in% c("leaf traits","other response"))) %>%
  mutate(diff = x_t - x_c,
         diff_x = c_t - c_c,

         y.rel = x_t/x_c,
         x.rel = (c_t/c_c),
         x.c = c_c,
         logRR = log(y.rel/x.rel))

ggplot(df.selected.c) +
  geom_density(aes(x = c_c)) +
  geom_density(aes(x = c_t),col = "red")

ggplot(data = df.selected.c %>%
         filter(!is.na(logRR),is.finite(logRR))) +
  geom_boxplot(
    aes(x = biome, y = logRR, fill = biome)) +
  labs(x = "") +
  facet_wrap(~ response,
             nrow = 1) +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

################################################################################
# drought// irrigation

df.selected.d <- full.df %>%
  filter(treatment %in% c("d","i"),
         !(response.cat %in% c("leaf traits","other response"))) %>%
  rowwise() %>%
  mutate(diff = x_t - x_c,
         diff_x = case_when(treatment == "d" & !is.na(d_t2) ~ -d_t2,
                            treatment == "d" ~ -d_t*map,

                            treatment == "i" & !is.na(i_t) & !is.na(treatment_duration) ~
                              sum(c(i_t,i_c),na.rm = TRUE)*treatment_duration,

                            # treatment == "i" & !is.na(i_t) & !is.na(treatment_duration) ~
                            #   sum(c(i_t,i_c),na.rm = TRUE)*(as.Date(end_treatment) - as.Date(start_treatment)),

                             treatment == "i" & !is.na(i_t2) ~ (i_t2 + 1)*i_c*treatment_duration,

                            TRUE ~ NA_real_),

         y.rel = x_t/x_c,
         x.rel = (map+diff_x)/map,
         x.c = map,
         logRR = log(y.rel/x.rel))


ggplot(df.selected.d) +
  geom_density(aes(x = (map + diff_x)/map),alpha = 0.5) +
  facet_wrap(~ treatment, scales = "free")

ggplot(df.selected.d) +
  geom_density(aes(x = (i_c)*(i_t2 + 1)*treatment_duration,
                   fill = treatment),alpha = 0.5)

df.selected.d %>%
  filter((map + diff_x)/map > 2) %>%
  dplyr::select(id,treatment,treatment_duration,d_t2,d_t,i_t2,i_t,map,diff_x)

ggplot(data = df.selected.d %>%
         filter(!is.na(logRR),is.finite(logRR))) +
  geom_boxplot(
    aes(x = biome, y = logRR, fill = biome)) +
  labs(x = "") +
  facet_wrap(~ response,
             nrow = 1) +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

################################################################################
# warming

df.selected.w <- full.df %>%
  filter(treatment %in% c("w"),
         !(response.cat %in% c("leaf traits","other response"))) %>%
  mutate(diff = x_t - x_c,
         diff_x = case_when(!is.na(w_t2) & w_t2 != 0 ~ w_t2,
                            !is.na(w_t3) ~ w_t3,
                            TRUE ~ NA_real_),


         y.rel = x_t/x_c,
         x.rel = (sign(mat)*sign(mat + diff_x)*(mat + diff_x)/(mat)),
         x.c = mat,
         logRR = log(y.rel/x.rel))

df.selected.w %>%
  filter(biome == "Temperate grassland/desert",
         response == "agb") %>%
  dplyr::select(id,treatment,diff_x,mat,w_t2,w_t3,logRR) %>%
  pull(logRR) %>% median(na.rm = T)

ggplot(df.selected.w) +
  geom_point(aes(x = mat,
                 y = sign(mat + diff_x)*sign(mat)*(mat + diff_x)/mat*100, fill = treatment),
             alpha = 0.5) +
  geom_segment(aes(x = mat,xend = mat + diff_x,
                 y = sign(mat + diff_x)*sign(mat)*(mat + diff_x)/mat*100,
                 yend = sign(mat + diff_x)*sign(mat)*(mat + diff_x)/mat*100))


ggplot(data = df.selected.w %>%
         filter(!is.na(logRR),is.finite(logRR))) +
  geom_boxplot(
    aes(x = biome, y = logRR, fill = biome)) +
  labs(x = "") +
  facet_wrap(~ response,
             nrow = 1) +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

################################################################################

df.all <- bind_rows(
  df.selected.d %>%
    mutate(treatment.cat2 = "drought/irrigation"),
  df.selected.w %>%
    mutate(treatment.cat2 = "warming"),
  df.selected.c %>%
    mutate(treatment.cat2 = "eCO2"))


ggplot(data = df.all) +
  geom_bar(aes(x = response, fill = biome)) +
  labs(x = "") +
  theme_bw() +
  facet_wrap(~ treatment.cat2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


ggplot(data = df.all) +
  geom_bar(aes(fill = response, x = biome)) +
  labs(x = "") +
  theme_bw() +
  facet_wrap(~ treatment.cat2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# ggplot(data = df.all) +
#   geom_density(aes(x = diff_x)) +
#   facet_wrap(~treatment.cat2, scales = "free")

df.all %>%
  # filter(biome == "Tropical rain forest") %>%
  group_by(response) %>%
  summarise(N = n()) %>%
  arrange(desc(N))


ggplot(data = df.all %>%
         filter(response == "agb") %>%
         filter(!is.na(logRR),is.finite(logRR))) +
  geom_boxplot(
    aes(x = biome, y = logRR, fill = biome)) +
  labs(x = "") +
  facet_wrap(~ treatment.cat2,
             nrow = 1) +
  theme_bw() +
  geom_hline(yintercept = 0, linetype = 2) +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(data = df.all %>%
         # filter(response == "agb") %>%
         filter(!is.na(logRR),is.finite(logRR),!is.na(biome)),
       aes(x = biome, y = diff_x, fill = biome)) +
  geom_boxplot() +
  # stat_smooth(method = "lm", alpha = 0.5) +
  geom_hline(yintercept = 0,linetype = 2) +
  facet_wrap(~ treatment.cat2,
             scales = "free",
             nrow = 1) +
  theme_bw() +
  guides(fill = "none") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(x = "", y = "Treatment") +
  theme(text = element_text(size = 20))



ggplot() +
  geom_polygon(data = Whittaker_biomes,
               aes(x = temp_c, y = precp_cm, group = biome),
               color = "black",
               fill = NA, size = 0.5) +
  geom_point(data = df.all %>%
               filter(response == "agb") %>%
               filter(treatment.cat2 == "eCO2") %>%
               filter(!is.na(logRR),is.finite(logRR)),
             aes(x = mat, y = map/10, color = diff/1000), alpha = 0.5,
             size = 0.1) +
  scale_color_gradient2(low = "darkred",high = "darkgreen",mid = "black",
                       limits = c(-1,1), oob = scales::squish) +
  theme_bw()

df.all %>%
  filter(response == "agb") %>%
  filter(treatment.cat2 == "eCO2") %>%
  filter(!is.na(logRR),is.finite(logRR)) %>%
  filter(mat == 23.5,map == 220)

