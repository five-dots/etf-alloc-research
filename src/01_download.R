library("ProjectTemplate")
setwd(here::here())
load.project()


get_last_bizday <- function(dt) {
  while (!RQuantLib::isBusinessDay("UnitedStates/NYSE", dt))
    dt <- dt -1
  dt
}

need_updated <- function(path, universe) {
  if (!fs::file_exists(path)) return(TRUE)

  data <- data.table::fread(path, data.table = FALSE)
  end_date <- max(ymd(data$date))
  symbols <- unique(data$symbol)

  nyc_time <- now(tz = "America/New_York")
  last_date <- if_else(hour(nyc_time) >= 17,
                       get_last_bizday(date(nyc_time)),
                       get_last_bizday(date(nyc_time) - 1))

  not_latest <- end_date < last_date
  not_all_sym <- !setequal(universe, symbols)

  not_latest || not_all_sym
}

download <- function(universe, from, path) {
  data <- map_dfr(universe, function(symbol) {
    tq_get(symbol, from = from) %>%
      mutate(symbol = !!symbol) %>%
      select_at(vars(symbol, everything()))
  })

  data.table::fwrite(data, path)
  invisible(path)
}


path <- glue("{config$prj_dir}/data/rawdata.csv")

if (need_updated(path, config$universe)) {
  download(universe, config$from, path)
}
