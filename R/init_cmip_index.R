init_cmip_index <- function (activity = "ScenarioMIP", variable = c("tas", "tasmax",
                                                 "tasmin", "hurs", "hursmax", "hursmin", "pr", "rsds", "rlds",
                                                 "psl", "sfcWind", "clt"), frequency = "day", experiment = c("ssp126",
                                                                                                             "ssp245", "ssp370", "ssp585"), source = c("AWI-CM-1-1-MR",
                                                                                                                                                       "BCC-CSM2-MR", "CESM2", "CESM2-WACCM", "EC-Earth3", "EC-Earth3-Veg",
                                                                                                                                                       "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "MPI-ESM1-2-HR",
                                                                                                                                                       "MRI-ESM2-0"), variant = "r1i1p1f1", replica = FALSE, latest = TRUE,
          resolution = c("100 km", "50 km"), limit = 10000L, data_node = NULL,
          years = NULL, save = FALSE,
          project = "CMIP6")
{
  assert_integerish(years, lower = 1900, unique = TRUE, sorted = TRUE,
                    any.missing = FALSE, null.ok = TRUE)
  assert_flag(save)
  verbose("Querying CMIP6 Dataset Information")
  qd <- esgf_query_project(activity = activity, variable = variable,
                   frequency = frequency, experiment = experiment, source = source,
                   replica = replica, latest = latest, variant = variant,
                   resolution = resolution, limit = limit, type = "Dataset",
                   data_node = data_node,
                   project = project)
  if (!nrow(qd))
    return(qd)
  if (nrow(qd) == 10000L) {
    warning("The dataset query returns 10,000 results which ",
            "hits the maximum record limitation of a single query using ESGF search RESTful API. ",
            "It is possible that the returned Dataset query responses are not complete. ",
            "It is suggested to examine and refine your query.")
  }
  dt <- data.table::set(qd, NULL, "file_url", NA_character_)
  file_url <- NULL
  attempt <- 0L
  retry <- 10L
  while (nrow(nf <- dt[is.na(file_url)]) && attempt <= retry) {
    attempt <- attempt + 1L
    verbose("Querying CMIP6 File Information [Attempt ",
            attempt, "]")
    .SD <- NULL
    q <- unique(nf[, .SD, .SDcols = c("activity_drs", "source_id",
                                      "member_id", "experiment_id", "nominal_resolution",
                                      "table_id", "frequency", "variable_id")])
    qf <- esgf_query_project(activity = unique(q$activity_drs),
                     variable = unique(q$variable_id), frequency = unique(q$frequency),
                     experiment = unique(q$experiment_id), source = unique(q$source_id),
                     variant = unique(q$member_id), resolution = unique(q$nominal_resolution),
                     replica = replica, latest = latest, type = "File",
                     data_node = data_node, project = project)
    data.table::set(qf, NULL, value = NULL, setdiff(intersect(names(qd),
                                                              names(qf)), c("dataset_id", "file_url")))
    data.table::set(nf, NULL, value = NULL, setdiff(intersect(names(qf),
                                                              names(nf)), c("dataset_id")))
    dt <- data.table::rbindlist(list(dt[!nf, on = "dataset_id"],
                                     qf[nf, on = "dataset_id"]), fill = TRUE)
  }
  verbose("Checking if data is complete")
  if (anyNA(dt$file_url)) {
    warning("There are still ", length(unique(dt$dataset_id[is.na(dt$file_url)])),
            " Dataset that ", "did not find any matched output file after ",
            retry, " retries.")
  }
  data.table::setcolorder(dt, c("file_id", "dataset_id", "mip_era",
                                "activity_drs", "institution_id", "source_id", "experiment_id",
                                "member_id", "table_id", "frequency", "grid_label",
                                "version", "nominal_resolution", "variable_id", "variable_long_name",
                                "variable_units", "datetime_start", "datetime_end",
                                "file_size", "data_node", "file_url", "dataset_pid",
                                "tracking_id"))
  if (!is.null(years)) {
    exp <- data.table::data.table(expect_start = ISOdatetime(years -
                                                               1L, 1, 1, 0, 0, 0, "UTC"), expect_end = ISOdatetime(years +
                                                                                                                     1L, 12, 31, 0, 0, 0, "UTC"))
    dt[, `:=`(expect_start = datetime_start, expect_end = datetime_end)]
    dt <- dt[exp, on = c("expect_start<=expect_end", "expect_end>=expect_start")][,
                                                                                  `:=`(expect_start = NULL, expect_end = NULL)]
  }
  dt <- unique(dt, by = "file_id")
  if (save) {
    data.table::fwrite(dt, file.path(.data_dir(TRUE), "cmip6_index.csv"))
    verbose("Data file index saved to '", normalizePath(file.path(.data_dir(TRUE),
                                                                  "cmip6_index.csv")), "'")
    EPWSHIFTR_ENV$index_db <- data.table::copy(dt)
  }
  dt
}
