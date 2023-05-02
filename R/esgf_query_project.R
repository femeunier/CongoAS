esgf_query_project <- function (activity = "ScenarioMIP", variable = c("tas", "tasmax",
                                                 "tasmin", "hurs", "hursmax", "hursmin", "pr", "rsds", "rlds",
                                                 "psl", "sfcWind", "clt"), frequency = "day", experiment = c("ssp126",
                                                                                                             "ssp245", "ssp370", "ssp585"), source = c("AWI-CM-1-1-MR",
                                                                                                                                                       "BCC-CSM2-MR", "CESM2", "CESM2-WACCM", "EC-Earth3", "EC-Earth3-Veg",
                                                                                                                                                       "GFDL-ESM4", "INM-CM4-8", "INM-CM5-0", "MPI-ESM1-2-HR",
                                                                                                                                                       "MRI-ESM2-0"), variant = "r1i1p1f1", replica = FALSE, latest = TRUE,
          resolution = c("100 km", "50 km"), type = "Dataset", limit = 10000L,
          data_node = NULL,
          project = "CMIP5")
{
  assert_subset(activity, empty.ok = FALSE, choices = c("AerChemMIP",
                                                        "C4MIP", "CDRMIP", "CFMIP", "CMIP", "CORDEX", "DAMIP",
                                                        "DCPP", "DynVarMIP", "FAFMIP", "GMMIP", "GeoMIP", "HighResMIP",
                                                        "ISMIP6", "LS3MIP", "LUMIP", "OMIP", "PAMIP", "PMIP",
                                                        "RFMIP", "SIMIP", "ScenarioMIP", "VIACSAB", "VolMIP"))
  assert_character(variable, any.missing = FALSE, null.ok = TRUE)
  assert_subset(frequency, empty.ok = TRUE, choices = c("1hr",
                                                        "1hrCM", "1hrPt", "3hr", "3hrPt", "6hr", "6hrPt", "day",
                                                        "dec", "fx", "mon", "monC", "monPt", "subhrPt", "yr",
                                                        "yrPt"))
  assert_character(experiment, any.missing = FALSE, null.ok = TRUE)
  assert_character(source, any.missing = FALSE, null.ok = TRUE)
  assert_character(variant, any.missing = FALSE, pattern = "r\\d+i\\d+p\\d+f\\d+",
                   null.ok = TRUE)
  assert_character(resolution, any.missing = FALSE, null.ok = TRUE)
  assert_flag(replica)
  assert_flag(latest)
  assert_count(limit, positive = TRUE)
  assert_choice(type, choices = c("Dataset", "File"))
  assert_character(data_node, any.missing = FALSE, null.ok = TRUE)
  url_base <- "http://esgf-node.llnl.gov/esg-search/search/?"

  dict <- c(activity = "activity_id", experiment = "experiment_id",
            source = "source_id", variable = "variable_id", resolution = "nominal_resolution",
            variant = "variant_label")

  if (project == "CMIP5"){
    dict <- c(activity = "activity_id", experiment = "experiment",
              source = "source_id", variable = "variable", resolution = "nominal_resolution",
              variant = "variant_label",frequency = "time_frequency")
  }

  pair <- function(x, first = FALSE) {
    var <- deparse(substitute(x))
    if (is.null(x) || length(x) == 0)
      return()
    key <- dict[names(dict) == var]
    if (!length(key))
      key <- var
    if (is.logical(x))
      x <- tolower(x)
    s <- paste0(key, "=", paste0(x, collapse = "%2C"))
    if (first)
      s
    else paste0("&", s)
  }
  `%and%` <- function(lhs, rhs){
    if (is.null(rhs)){
      return(lhs)
    }  else {
      return(paste0(lhs, rhs))
    }
  }

  format <- "application%2Fsolr%2Bjson"
  resolution <- c(gsub(" ", "", resolution, fixed = TRUE),
                  gsub(" ", "+", resolution, fixed = TRUE))
  q <- url_base %and% pair(project, TRUE) %and% pair(activity) %and%
    pair(experiment) %and% pair(source) %and% pair(variable) %and%
    pair(resolution) %and% pair(variant) %and% pair(data_node) %and%
    pair(frequency) %and% pair(replica) %and% pair(latest) %and%
    pair(type) %and% pair(limit) %and% pair(format)

  q <- url_base %and% pair(project, TRUE) %and%
    pair(variable) %and%
    pair(source) %and%
    pair(experiment) %and%
    pair(frequency) %and%
    pair(replica) %and%
    pair(latest) %and%
    pair(resolution) %and%
    pair(type) %and%
    pair(limit) %and%
    pair(data_node) %and%
    pair(format)

  q <- tryCatch(jsonlite::read_json(q), warning = function(w) w,
                error = function(e) e)

  q

  if (inherits(q, "warning") || inherits(q, "error")) {
    message("No matched data. Please check network connection and the availability of LLNL ESGF node.")
    dt <- data.table::data.table()
  } else if (q$response$numFound == 0L) {
    message("No matched data. Please examine the actual response using 'attr(x, \"response\")'.")
    dt <- data.table::data.table()
  } else if (type == "Dataset") {
    dt <- extract_query_dataset(q)
  } else if (type == "File") {
    dt <- extract_query_file(q)
  }
  data.table::setattr(dt, "response", q)
  dt
}
