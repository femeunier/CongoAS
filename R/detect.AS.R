detect.AS <- function(location = "Tropics",
                      ncfiles =  "/home/femeunier/Downloads/cVeg_Lmon_TaiESM1_1pctCO2_r1i1p1f1_gn_000102-015012.nc",
                      ncfiles.cfile = "/home/femeunier/Downloads/cVeg_Lmon_TaiESM1_piControl_r1i1p1f1_gn_060101-070012.nc",
                      dt = 1){

  coord.list <- continent2coord(location)

  #######################################################################################################################
  # Control file(s)

  df.control <- read.and.filter.ncfile(ncfiles.cfile,
                                       coord.list[[1]],
                                       var = "cVeg",
                                       aggr = TRUE)

  mask.ocean <- df.control %>% group_by(lat,lon) %>%
    summarise(is.land = !all(cVeg == 0),
              .groups = "keep")

  df.control.mask <- mask(df.control,mask.ocean) %>%
    group_by(lat, lon) %>%
    mutate(ID = cur_group_id())

  df.control.var <-
    df.control.mask %>%
    group_by(ID,lat,lon) %>%
    mutate(diff.cVeg = c(NA,diff(cVeg))) %>%
    summarise(var.cVeg = var(abs(diff.cVeg),na.rm = TRUE),
              .groups = "keep")

  ###########################################################################################################

  df.data.all <- read.and.filter.ncfiles(ncfiles,
                                         coord.list,
                                         var = "cVeg",
                                         aggr = TRUE,
                                         mask.ocean,
                                         progressbar = FALSE)

  df.AS <-
    df.data.all %>%
    left_join(df.control.var,
              by = c("lat","lon")) %>%
    group_by(lat, lon) %>%
    mutate(diff.cVeg = compute.diff.cVeg(cVeg,dt),
           overall.change = compute.overall.change(cVeg,dt)) %>%
    mutate(change.rate.m = compute.change.rate(diff.cVeg,dt)) %>%
    mutate(criteria1 = abs(diff.cVeg) >= 2) %>%
    mutate(criteria2 = abs(diff.cVeg) >= (0.25*abs(overall.change)),
           criteria3 = change.rate.m >= 3*sqrt(var.cVeg)) %>%
    mutate(all.crit = criteria1 & criteria2 & criteria3) %>%
    # mutate(all.crit = shift.beg.AS(all.crit,yr,N = 15)) %>%
    mutate(Trend = case_when(overall.change > 0 ~ "+",
                             TRUE ~ "-"))



  #################################################################################################

  basin.AS <- df.AS %>%
    group_by(lat,lon) %>%
    mutate(AS = any(all.crit,na.rm = TRUE),
           .groups = "keep")

  df2plot.criteria <-
    basin.AS %>% dplyr::select(ID,lat,lon,yr,
                               criteria1,
                               criteria2,
                               criteria3,
                               all.crit,
                               AS) %>%
    group_by(ID,lat,lon) %>%
    summarise(criteria1 = any(criteria1),
              criteria2 = any(criteria2),
              criteria3 = any(criteria3,na.rm = TRUE),
              all.crit = any(all.crit,na.rm = TRUE),
              .groups = "keep") %>%
    pivot_longer(cols = -c(ID,lat,lon),
                 names_to = "var",
                 values_to = "value") %>%
    mutate(var = factor(var,levels = c("criteria1","criteria2","criteria3","all.crit","AS")))


  df.change <- df.data.all %>%
    group_by(lon,lat) %>%
    mutate(cVeg.change = cVeg - cVeg[1]) %>%
    mutate(cVeg.rel.change = cVeg/cVeg[1] - 1) %>%
    mutate(cVeg.rel.change.cache = case_when(cVeg[1] > 5 ~ cVeg.rel.change,
                                             TRUE ~ NA_real_))


  df.data.all.change <-
    bind_rows(list(
      df.data.all %>% filter(yr %in% c(min(yr), max(yr))) %>%
        mutate(timing = case_when(yr == 0 ~ "Init",
                                  TRUE ~ "Final")),
      df.change %>%
        filter(yr == max(yr)) %>%
        dplyr::select(lat,lon,yr,cVeg.change) %>%
        rename(cVeg = cVeg.change) %>%
        mutate(timing = "Change")
    )) %>% mutate(timing = factor(timing,levels = c("Init","Final","Change")))

  basin.map <- basin.AS %>%
    group_by(lat,lon) %>%
    summarise()

  return(list(cVeg.change = df.data.all.change,
              TS.AS = basin.AS,
              criteria = df2plot.criteria))

}
