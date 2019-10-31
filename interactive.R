
### {renv} operation
renv::restore()
renv::install("tsibble")
renv::snapshot(confirm = FALSE)
renv::status()


### {ProjectTemplate} operations
library("ProjectTemplate")
setwd(here::here())
load.project() # config -> lib -> cache/data -> munge

run.project()
test.project()

list.data()
clear.cache()
cache()
project.config()
project.info


### Data analysis scripts
source("src/01_download.R")

clear.cache("fits")
source("src/02_fits.R")

clear.cache("weights")
source("src/03_weights.R")


### Objects
{
  rawdata %>% tail()
  data
  data_dim_check
  nested_data
  xts_data

  params
  params %>%
    group_by(model, dist, lookback) %>%
    summarize(count = n())

  fits
  print(fits, n = 100)
  fits_dim_check

  weights
  weights %>%
    group_by(model, dist, type, lookback) %>%
    summarize(count = n())
  weights_by_type
  perfs_by_type
  perf_summary
  perf_summary %>% arrange(desc(Sharpe))

  plot_perf_by_symbol
  plot_perf_by_type$cum_ret
  plot_perf_by_type$drawdown
  plot_weight_by_type$plot[[1]]
  plot_sharpe_by_param$type
  plot_rshape_history
}


### EDA

