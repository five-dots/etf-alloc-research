#!/usr/bin/env Rscript

library("ProjectTemplate")
setwd(here::here())
## delete cache to load the latest data from data folder
clear.cache("fits")
## Load the latest "fits" from data/ not cache/
load.project()


get_ugarch_spec <- function(model = "eGARCH", order = c(1, 1), dist = "norm", ...) {
  ugarchspec(
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),
    variance.model = list(model = model, garchOrder = order),
    distribution.model = dist,
    fixed.pars = list(...))
}

fits_multi_garch <- function(xts_data, model = "eGARCH", order = c(1, 1), dist = "norm") {
  spec <- get_ugarch_spec(model, order, dist)
  specs <- multispec(replicate(ncol(xts_data), spec))

  fits <- tryCatch({
    multifit(specs, xts_data)
  }, error = function(e) NA)

  list(specs = specs, fits = fits)
}

fit_mgarch <- function(xts_data, spec, ..., model = "dcc", fit = NULL, solver = "solnp", timeout = 60) {

  ## Error in solve.default(A) :
  ##   system is computationally singular: reciprocal condition number = 1.15485e-17
  ## https://stackoverflow.com/questions/50928796/system-is-computationally-singular-reciprocal-condition-number-in-r
  ## https://stackoverflow.com/questions/57609790/dcc-model-estimation-with-t-student-distribution
  ## https://quant.stackexchange.com/questions/7260/r-arma-garch-rugarch-package-doesnt-always-converge

  tryCatch({
    withTimeout({
      args <- list(spec = spec, data = xts_data, solver = solver,
                   fit = fit, fit.control = list(...))
      switch(model,
        "dcc" = do.call(dccfit, args),
        "copula" = do.call(cgarchfit, args),
        stop("model must be dcc or copula"))
    }, timeout = timeout, onTimeout = "error")
  }, error = function(e) NA)
}

fit_dcc_mvnorm <- function(xts_data) {
  uspec <- get_ugarch_spec("eGARCH", c(1, 1), "norm")
  mspec <- dccspec(uspec = multispec(replicate(ncol(xts_data), uspec)),
                   dccOrder = c(1,1), distribution = "mvnorm")
  ## fit_mgarch(xts_data, mspec, model = "dcc")

  ## Iterate solvers till suceeded
  solvers <- c("solnp", "nlminb", "gosolnp", "lbfgs")
  for (solver in solvers) {
    print(solver)
    fit <- fit_mgarch(xts_data, mspec, model = "dcc", solver = solver)
    if (class(fit) == "DCCfit") break
  }
  fit
}

fit_dcc_mvt <- function(xts_data, max_iters = 5) {
  ## Base ugarch multifits by eGARCH + norm
  multi <- fits_multi_garch(xts_data)
  if (!class(multi$fits) == "uGARCHmultifit")
    return(NULL)

  ## 1-Stage: DCC fit by multifit object (eGARCH-norm + DCC-mvt)
  init_spec <- dccspec(multi$specs, dccOrder = c(1, 1), distribution = "mvt")
  init_fit <- fit_mgarch(xts_data, init_spec, eval.se = FALSE, model = "dcc", fit = multi$fits)

  ## 2-Statge: Iterate fits by previous fitted shape with eval.se (eGARCH-std + DCC-mvt)
  shape <- ifelse(class(init_fit) == "DCCfit", rshape(init_fit), 20)
  fits <- list()
  iter <- 1

  while (length(fits) < 3 && iter <= max_iters) {
    uspec <- get_ugarch_spec("eGARCH", c(1, 1), "std", shape = shape)
    mspec <- dccspec(uspec = multispec(replicate(ncol(xts_data), uspec)),
                     dccOrder = c(1,1), distribution = "mvt")
    fit <- fit_mgarch(xts_data, mspec, eval.se = TRUE, model = "dcc")

    if (class(fit) == "DCCfit") {
      fits <- append(fits, fit)
      ## Use the latest shape for the next fit
      shape <- rshape(fit)
    } else {
      ## Randomly change shape in case of failure
      shape <- shape + rnorm(1, 0, sqrt(shape))
    }
    iter <- iter + 1
  }

  ## Check if there is at least one fit succeeded
  if (length(fits) == 0) return(NULL)

  ## Return best fit
  logliks <- map_dbl(fits, likelihood)
  fits[[which.max(logliks)]]
}

fit <- function(target) {
  cur_plan <- plan(multiprocess)
  on.exit(plan(cur_plan))

  future_map_dfr(transpose(target), function(p) {
    xts_data <- slice_data(nested_data, p$weekend, p$lookback) %>%
      convert_to_longer(ret_log) %>%
      tk_xts(-date, date)

    ## Dynamicall change function
    fun <- glue("fit_{p$model}_{p$dist}")
    fit <- do.call(fun, args = list(xts_data = xts_data))

    p$fit <- I(list(fit))
    p
  }, .progress = TRUE) %>%
    ## Convert number to Date
    mutate(weekend = as.Date(weekend, origin = "1970-01-01"))
}


## if (exists("fits", mode = "list")) {
##   ## Subset params by already fitted rows
##   target <- anti_join(params, fits)
##   if (nrow(target) > 0) {
##     fitted <- fit(target)
##     fits <- bind_rows(fits, fitted)
##   }

## } else {
##   target <- params
##   fits <- fit(target)
## }

target <- params
fits <- fit(target)


## TODO use bucher to reduce size
if (nrow(fits) > 0) {
  fits %>%
    arrange_at(vars(-fit)) %>%
    saveRDS("data/fits.rds")
}
