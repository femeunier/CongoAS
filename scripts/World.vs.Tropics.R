rm(list = ls())

library(dplyr)
library(ggplot2)
library(zoo)

# system2("rsync",
#         c("-avz",
#           "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/timeseries.CMIP6.all.rspld.RDS",
#           "./outputs/"))
#
# system2("rsync",
#         c("-avz",
#           "hpc:/kyukon/data/gent/vo/000/gvo00074/felicien/R/outputs/timeseries.CMIP5.all.rspld.RDS",
#           "./outputs/"))


Gridarea_tot <- bind_rows(readRDS("./outputs/Gridarea.RDS") %>%
  mutate(region = case_when(abs(lat) <= 23.5 ~ "Tropics",
                            abs(lat) <= 50 ~ "Temperate",
                            TRUE ~ "Boreal")) %>%
  group_by(region) %>%
  summarise(area = sum(area)/1e12),
    readRDS("./outputs/Gridarea.RDS") %>%
    ungroup() %>%
    summarise(region = "World",
              area = sum(area)/1e12))

TS.CMIP6 <- bind_rows(bind_rows(readRDS("./outputs/timeseries.CMIP6.all.rspld.RDS") %>%
                        mutate(region = "World"), # m²
                      readRDS("./outputs/timeseries.CMIP6.all.rspld.RDS")) %>%
  ungroup() %>%
  mutate(nbp.m = case_when(model == "E3SM-1-1-ECA" & scenario == "historical" ~ nbp.m/1000,
                           TRUE ~ nbp.m),
         nbp.w.m = case_when(model == "E3SM-1-1-ECA" & scenario == "historical" ~ nbp.m/1000,
                           TRUE ~ nbp.w.m)) %>%               # wront units
  filter(!grepl("GISS",model)) %>%                          # wrong and unknown units
  filter(!(model == "E3SM-1-1" & scenario == "ssp245")) %>% # incomplete
  filter(!(model == "MPI-ESM-1-2-HAM" & scenario == "ssp370")) %>% # incomplete
  filter(!(year > 2014 & scenario == "historical")) %>%
  filter(!(model == "E3SM-1-1-ECA")) %>%
  left_join(Gridarea_tot,
            by = "region") %>%
    mutate(origin = "CMIP6"),
  bind_rows(readRDS("./outputs/timeseries.CMIP5.all.rspld.RDS") %>%
              mutate(region = "World"),
            readRDS("./outputs/timeseries.CMIP5.all.rspld.RDS")) %>%
    mutate(nbp.m = case_when(model == "CMCC-CESM" ~ nbp.m/1000,
                             TRUE ~ nbp.m),
           nbp.w.m = case_when(model == "CMCC-CESM" ~ nbp.m/1000,
                             TRUE ~ nbp.w.m)) %>%
    ungroup() %>%
    left_join(Gridarea_tot,
              by = "region") %>%
    mutate(origin = "CMIP5"))

A <- readRDS("./outputs/timeseries.CMIP5.all.rspld.RDS")
A %>%
  filter(scenario != "historical") %>%
  group_by(model,scenario) %>%
  summarise(M = max(year)) %>%
  filter(M < 2100)
ggplot(data = A) +
  geom_density(aes(x = nbp.w.m, fill = model), alpha = 0.5) +
  theme_bw()

origins <- unique(TS.CMIP6$origin)

TS.CMIP6.all <- data.frame()
for (corigin in origins){

  cdf <- TS.CMIP6 %>%
    filter(origin == corigin)

  scenarios <- unique(cdf$scenario)
  TS.CMIP6.all <- bind_rows(TS.CMIP6.all,
                            cdf)
  for (cscenario in scenarios){
    print(paste0(corigin," - ",cscenario))

    cmodels <- unique(cdf %>%
                        filter(scenario == cscenario) %>%
                        pull(model))

    TS.CMIP6.all <- bind_rows(TS.CMIP6.all,
                              cdf %>%
                                filter(model %in% cmodels) %>%
                                filter(scenario == "historical") %>%
                                mutate(scenario = cscenario))
  }
}

TS.CMIP6.all <- TS.CMIP6.all %>%
  mutate(scenario = case_when(scenario == "rcp26" ~ "ssp126",
                              scenario == "rcp45" ~ "ssp245",
                              scenario == "rcp60" ~ "ssp370",
                              scenario == "rcp85" ~ "ssp585",
                              TRUE ~ scenario))

TS.CMIP6.year <- TS.CMIP6.all %>%
  group_by(origin,region,year,model,scenario,variant) %>%
  summarise(nbp.w.m = mean(nbp.w.m,na.rm = TRUE)*86400*365,  # kgC/m2/yr
            area = mean(area),
            .groups = "keep") %>%
  group_by(origin,region,model,scenario,variant) %>%
  mutate(nbp.w.cum = (cumsum(nbp.w.m))*area/1e12) %>%   # kgC --> PgC
  ungroup() %>%
  group_by(origin,region,model,scenario,variant) %>%
  mutate(nbp.w.m_roll = rollapply(nbp.w.m, width = 30, FUN = mean, align = "center", partial = TRUE)) %>%
  mutate(scenario = factor(scenario,
                           levels = c(paste0("ssp",c("585","370","245","126")),
                                      "historical")))

# Mean
TS.CMIP6.year.m <- TS.CMIP6.year %>%
  group_by(origin,region,year,scenario) %>%
  summarise(nbp.w.m.av = mean(nbp.w.m_roll),
            nbp.w.cum.av = mean(nbp.w.cum),
            .groups = "keep")

ggplot() +
  geom_line(data = TS.CMIP6.year %>%
              filter((scenario == "historical") |
                       (origin == "CMIP6" & scenario != "historical" & year > 2014) |
                       (origin == "CMIP5" & scenario != "historical" & year > 2005)),
            aes(x = year,
                y = nbp.w.m_roll,
                color = scenario,
                group = interaction(model,scenario,variant)),
            size = 0.1) +
  geom_line(data = TS.CMIP6.year.m,
            aes(x = year,
                y = nbp.w.m.av,
                color = scenario)) +
  geom_vline(data = data.frame(X = c(2006,2015),
                               origin = c("CMIP5","CMIP6")),
             aes(xintercept = X), linetype = 2, color = "black") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  scale_color_manual(values = rev(c("darkgrey",
                                    rgb(0,52/255,102/255),
                                    rgb(112/255,160/255,205/255),
                                    rgb(196/255,121/255,0),
                                    rgb(153/255,0,2/255)))) +
  theme_bw() +
  facet_grid(origin ~ region) +
  labs(x = "",y = "NBP (MgC/ha/yr)")


ref.and.scenarios <- TS.CMIP6 %>%
  filter((origin == "CMIP6" & year %in% c(1985:2014,2015:2044)) |
           (origin == "CMIP5" & year %in% c(1976:2005,2015:2044))) %>%
  mutate(reference = case_when(year < 2015 ~ TRUE,
                               TRUE ~ FALSE)) %>%
  group_by(reference,region,scenario,variant,model,origin) %>%
  summarise(nbp.m = mean(nbp.m,na.rm = TRUE),
            nbp.w.m = mean(nbp.w.m,na.rm = TRUE),
            .groups = "keep") %>%
  ungroup()

ref.vs.scenarios <- ref.and.scenarios %>%
  filter(!reference) %>%
  dplyr::select(-reference) %>%
  left_join(ref.and.scenarios %>%
              filter(reference) %>%
              dplyr::select(-reference) %>%
              rename(ref.m = nbp.m,
                     ref.w.m = nbp.w.m),
            by = c("region","scenario","variant","model","origin")) %>%
  mutate(scenario = case_when(scenario == "rcp26" ~ "ssp126",
                              scenario == "rcp45" ~ "ssp245",
                              scenario == "rcp60" ~ "ssp370",
                              scenario == "rcp85" ~ "ssp585",
                              TRUE ~ scenario))

ggplot(data = ref.vs.scenarios,
       aes(x = ref.w.m, y = nbp.w.m,
           color = scenario, fill = scenario)) +
  geom_point(aes(shape = origin)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 2) +
  stat_smooth(method = "lm", formula = y ~ (x)) +
  facet_grid(region ~ scenario) +
  theme_bw()


