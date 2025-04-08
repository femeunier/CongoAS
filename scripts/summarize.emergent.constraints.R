rm(list = ls())

library(sf)
library(dplyr)
library(raster)
library(ggplot2)

A <- readRDS("./outputs/Emergent.tropics.RDS")

all.df.masked.sum <- A %>%
  # filter(lat >= - 20,lat <= 10,
  #        lon >= -15, lon <= 45) %>%         # Congo Basin
  group_by(model,scenario) %>%
  summarise(nbp.m = mean(nbp),
            .groups = "keep") %>%
  mutate(reference = case_when(scenario == "historical" ~ "yes",
                               TRUE ~ "no")) %>%
  ungroup()

A.all.sum.wide <- all.df.masked.sum %>%
  filter(reference != "yes") %>%
  dplyr::select(-reference) %>%
  left_join(all.df.masked.sum %>%
              ungroup() %>%
              filter(reference == "yes") %>%
              dplyr::select(-c(scenario,reference)) %>%
              rename(historical = nbp.m),
            by = "model")

# all.df.masked.sum %>%
#   filter(model == "E3SM-1-1-ECA")

fac <- 86400*365*10
ggplot(data = A.all.sum.wide  %>%
         filter(historical <= 1e-8),
       aes(x = historical*fac,
           y = nbp.m*fac,
           color = scenario)) +
  geom_point() +
  stat_smooth(method = lm) +
  geom_vline(xintercept = 0, linetype = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_wrap(~ scenario, scales = "free") +
  theme_bw()

A.all.sum.wide  %>%
  filter(historical <= 1e-8) %>%
  group_by(scenario) %>%
  summarise(N = length(historical),
            r2 = summary(lm(nbp.m ~ historical))[["r.squared"]],
            p.val = summary(lm(nbp.m ~ historical))[["coefficients"]][2,4])

A.all.sum.wide  %>%
  filter(historical <= 1e-8) %>%
  summarise(r2 = summary(lm(nbp.m ~ historical))[["r.squared"]],
            p.val = summary(lm(nbp.m ~ historical))[["coefficients"]][2,4])

# scp /home/femeunier/Documents/projects/CongoAS/scripts/summarize.emergent.constraints.R hpc:/data/gent/vo/000/gvo00074/felicien/R/

# Inner Congo Basin,-61.88 ± 100.16,56.17 ± 113.45,2.8 ± 0.085
# Lower Guinea,-48.35 ± 16.79,-22.63 ± 35.35,7.77 ± 0.199
# West-Africa,-21.51 ± 17.85,46.4 ± 46.02,41.94 ± 1.584
# Whole region,-131.74 ± 103.11,79.95 ± 127.43,52.52 ± 1.598
