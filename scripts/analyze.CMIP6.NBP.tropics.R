rm(list = ls())

library(zoo)
library(dplyr)
library(ggplot2)

data.obs <- readRDS("/home/femeunier/Documents/projects/ISIMIP/outputs/Summary.Inversions.RDS")

# CMIP

system2("rsync",
        c("-avz",
          "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/timeseries.CMIP6.tropics.rspld.RDS",
          "./outputs/"))

TS.CMIP6 <- readRDS("./outputs/timeseries.CMIP6.tropics.rspld.RDS") %>%
  ungroup() %>%
  mutate(nbp.m = case_when(model == "E3SM-1-1-ECA" & scenario == "historical" ~ nbp.m/1000,
                           TRUE ~ nbp.m)) %>%               # wront units
  filter(!grepl("GISS",model)) %>%                          # wrong and unknown units
  filter(!(model == "E3SM-1-1" & scenario == "ssp245")) %>% # incomplete
  filter(!(model == "MPI-ESM-1-2-HAM" & scenario == "ssp370")) %>% # incomplete
  filter(!(year > 2014 & scenario == "historical"))

scenarios <- unique(TS.CMIP6$scenario)
TS.CMIP6.all <- TS.CMIP6
for (cscenario in scenarios){
  print(cscenario)

  cmodels <- unique(TS.CMIP6 %>%
                      filter(scenario == cscenario) %>%
                      pull(model))

  TS.CMIP6.all <- bind_rows(TS.CMIP6.all,
                            TS.CMIP6 %>%
                              filter(model %in% cmodels) %>%
                              filter(scenario == "historical") %>%
                              mutate(scenario = cscenario))
}


TS.CMIP6.year <- TS.CMIP6.all %>%
  group_by(year,model,scenario,variant) %>%
  summarise(nbp.m = mean(nbp.m,na.rm = TRUE)*86400*365*10,
            .groups = "keep") %>%
  group_by(model,scenario,variant) %>%
  mutate(nbp.cum = cumsum(nbp.m)) %>%
  ungroup() %>%
  group_by(model,scenario) %>%
  mutate(nbp.m_roll = rollapply(nbp.m, width = 30, FUN = mean, align = "center", partial = TRUE)) %>%
  mutate(scenario = factor(scenario,
                           levels = c(paste0("ssp",c("585","370","245","126")),
                                      "historical")))

ggplot(data = TS.CMIP6.year,
       aes(x = year,
           y = nbp.m_roll,
           color = scenario,
           group = interaction(model,scenario,variant))) +
  geom_line() +
  # stat_smooth(method = "lm", se = FALSE) +
  geom_vline(xintercept = 2015, linetype = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  labs(x = "",y = "NBP (MgC/m²/yr)") +
  scale_color_manual(values = rev(c("darkgrey",
                               rgb(0,52/255,102/255),
                               rgb(112/255,160/255,205/255),
                               rgb(196/255,121/255,0),
                               rgb(153/255,0,2/255)))) +
  facet_wrap(~model) +
  theme_bw() +
  theme(legend.position = c(0.9,0.1))

# Mean
TS.CMIP6.year.m <- TS.CMIP6.year %>%
  group_by(year,scenario) %>%
  summarise(nbp.m.av = mean(nbp.m_roll),
            nbp.cum.av = mean(nbp.cum),
            .groups = "keep")


ggplot() +
  geom_line(data = TS.CMIP6.year %>%
              filter((scenario == "historical") |
                       (scenario != "historical" & year > 2014)),
            aes(x = year,
                y = nbp.m,
                color = scenario,
                group = interaction(model,scenario,variant)),
            size = 0.1) +
  geom_line(data = TS.CMIP6.year.m %>%
              filter((scenario == "historical") |
                       (scenario != "historical" & year > 2014)),
            aes(x = year,
                y = nbp.m.av,
                color = scenario)) +
  geom_vline(xintercept = 2015, linetype = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  scale_color_manual(values = rev(c("darkgrey",
                                    rgb(0,52/255,102/255),
                                    rgb(112/255,160/255,205/255),
                                    rgb(196/255,121/255,0),
                                    rgb(153/255,0,2/255)))) +
  theme_bw() +
  labs(x = "",y = "NBP (MgC/m²/yr)") +
  theme(legend.position = c(0.1,0.8))


ggplot() +
  geom_line(data = TS.CMIP6.year,
            aes(x = year,
                y = nbp.m_roll,
                color = scenario,
                group = interaction(model,scenario,variant)),
            size = 0.1) +
  geom_line(data = TS.CMIP6.year.m ,
            aes(x = year,
                y = nbp.m.av,
                color = scenario)) +
  geom_point(data = TS.CMIP6.year %>%
               filter(year %in% c(1985:2014,2071:2100)) %>%
               filter((scenario == "historical" & year <= 2014) |
                        (year > 2014 & scenario != "historical")) %>%
               group_by(model,scenario) %>%
               summarise(nbp.m.m = mean(nbp.m),
                         .groups = "keep") %>%
               mutate(year = case_when(scenario == "historical" ~ 2015,
                                       TRUE ~ 2100)),
             aes(x = year,y = nbp.m.m, color = scenario),
             shape = 1) +
  geom_vline(xintercept = 2015, linetype = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_wrap(~ scenario, nrow = 1) +
  scale_color_manual(values = rev(c("darkgrey",
                                    rgb(0,52/255,102/255),
                                    rgb(112/255,160/255,205/255),
                                    rgb(196/255,121/255,0),
                                    rgb(153/255,0,2/255)))) +
  theme_bw() +
  labs(x = "",y = "NBP (MgC/m²/yr)") +
  scale_x_continuous(limits = c(1890,2110),expand = c(0,0)) +
  guides(color = "none")

ggplot() +
  geom_line(data = TS.CMIP6.year,
            aes(x = year,
                y = nbp.cum,
                color = scenario,
                group = interaction(model,scenario,variant)),
            size = 0.1) +
  geom_line(data = TS.CMIP6.year.m ,
            aes(x = year,
                y = nbp.cum.av,
                color = scenario)) +
  geom_vline(xintercept = 2015, linetype = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  facet_wrap(~ scenario) +
  scale_color_manual(values = rev(c("darkgrey",
                                    rgb(0,52/255,102/255),
                                    rgb(112/255,160/255,205/255),
                                    rgb(196/255,121/255,0),
                                    rgb(153/255,0,2/255)))) +
  theme_bw()

ggplot() +
  geom_density(data = TS.CMIP6.year.m %>%
                 filter(year %in% c(1985:2014,2071:2100)) %>%
                 filter((scenario == "historical" & year <= 2014) |
                          (year > 2014 & scenario != "historical")),
               aes(x = nbp.m.av,
                   fill = scenario), color = NA, alpha = 0.5) +
  geom_point(data = TS.CMIP6.year %>%
               filter(year %in% c(1985:2014,2071:2100)) %>%
               filter((scenario == "historical" & year <= 2014) |
                        (year > 2014 & scenario != "historical")) %>%
               group_by(model,scenario) %>%
               summarise(nbp.m.m = mean(nbp.m),
                         .groups = "keep"),
             aes(x = nbp.m.m, y = 0, color = scenario)) +
  geom_vline(xintercept = 0, color = "black") +
  facet_wrap(~ scenario, nrow = 1) +
  scale_color_manual(values = rev(c("darkgrey",
                                    rgb(0,52/255,102/255),
                                    rgb(112/255,160/255,205/255),
                                    rgb(196/255,121/255,0),
                                    rgb(153/255,0,2/255)))) +
  scale_fill_manual(values = rev(c("darkgrey",
                                    rgb(0,52/255,102/255),
                                    rgb(112/255,160/255,205/255),
                                    rgb(196/255,121/255,0),
                                    rgb(153/255,0,2/255)))) +
  theme_bw() +
  guides(color = "none", fill = "none")
