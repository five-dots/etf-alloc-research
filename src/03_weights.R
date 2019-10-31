
library("ProjectTemplate")
setwd(here::here())
## delete cache to load the latest data from data folder
clear.cache("weights")
## Load the latest "fits" from data/ not cache/
load.project()


fcst_dcc_cov <- function(fit) {
  fcst <- dccforecast(fit)
  rcov(fcst)[[1]][,,1]
}

fcst_ccc_cov <- function(fit) {
  fcst <- dccforecast(fit)
  D <- diag(sigma(fcst)[,,1])
  R <- cor(fit@model$modeldata$data)
  H <- D %*% R %*% D

  universe <- fit@model$modeldata$asset.names
  colnames(H) <- universe
  rownames(H) <- universe
  H
}

fcst_deco_cov <- function(fit) {
  universe <- fit@model$modeldata$asset.names
  fcst <- dccforecast(fit)

  N    <- length(universe)
  D    <- diag(sigma(fcst)[,,1])
  one  <- matrix(1, N, N)
  iota <- rep(1, N)
  Q    <- rcor(fcst, type="Q")[[1]][,,1]
  rho  <- as.numeric((N * (N-1))^(-1) * (t(iota) %*% Q %*% iota - N))
  R    <- (1 - rho) * diag(1, N, N) + rho * one
  H    <- D %*% R %*% D

  colnames(H) <- universe
  rownames(H) <- universe
  H
}

fcst_cov <- function(fits) {
  cols <- colnames(select(fits, -fit))
  fits %>%
    ## TODO add mvnorm
    filter(dist == "mvt") %>%
    ## Delete failed model
    filter(map_lgl(fit, ~ class(.) == "DCCfit")) %>%
    ## Extract cov
    mutate(hist = map(fit, ~ cov(.@model$modeldata$data)),
           ccc  = map(fit, fcst_ccc_cov),
           dcc  = map(fit, fcst_dcc_cov),
           deco = map(fit, fcst_deco_cov)) %>%
    ## Delete fit objects to reduce size
    select(-fit) %>%
    ## Unnecessary
    pivot_longer(-cols, names_to = "type", values_to = "cov")
    ## FIXME distinct "hist" rows
    ## dplyr::distinct(weekend, lookback, model, .keep_all = TRUE) %>%
    ## hist is not based on any multivariate GARCH model
    ## mutate(model = if_else(type == "hist", NA_character_, model)) %>%
    ## mutate(dist = if_else(type == "hist", NA_character_, dist))
}

calc_weight <- function(cov_fcsts) {
  mu <- rep(0, 4)
  control <- list(type = "minvol", constraint = "lo")

  mutate(cov_fcsts, weight = map(cov, ~ {
    w <- optimalPortfolio(., mu, control = control)
    set_names(w, colnames(.))
  }))
}


stopifnot(exists("fits"))

if (exists("weights", mode = "list")) {
  target <- anti_join(weights, fits)
  if (nrow(target) > 0) {
    weights2 <- target %>%
      fcst_cov() %>%
      calc_weight()
    weights <- bind_rows(weights, weights2)
  }

} else {
  weights <- fits %>%
    fcst_cov() %>%
    calc_weight()
}


if (nrow(weights) > 0) {
  weights %>%
    arrange_at(vars(-cov, -weight)) %>%
    saveRDS("data/weights.rds")
}
