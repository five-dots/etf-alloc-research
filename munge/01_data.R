### Data
cache("universe", {
  c("QQQ", "VNQ", "TLT", "GLD")
})

cache("data", depends = c("rawdata"), {
  rawdata %>%
    ## Assert before mutate
    verify(has_all_names("symbol", "date", "adjusted")) %>%
    verify(adjusted > 0) %>%
    ## Mutate data
    mutate(date = ymd(date)) %>%
    group_by(symbol) %>%
    mutate(ret = (adjusted - lag(adjusted)) / lag(adjusted),
           ret_log = log(adjusted) - lag(log(adjusted))) %>%
    slice(-1) %>%
    ungroup() %>%
    arrange(symbol, date) %>%
    ## Assert after mutate
    assert(not_na, ret, ret_log)
})

cache("data_dim_check", depends = c("data"), {
  data %>%
    group_by(symbol) %>%
    summarise(count = n(), start = min(date), end = max(date)) %>%
    ## Check if dim is the same
    verify(length(unique(count)) == 1) %>%
    verify(length(unique(start)) == 1) %>%
    verify(length(unique(end)) == 1)
})

cache("nested_data", depends = c("data"), {
  data %>%
    ## Group by ISO week
    group_by(year = year(date), week = isoweek(date)) %>%
    mutate(weekend = max(date)) %>%
    ungroup() %>%
    select(-year, -week) %>%
    ## Filter out the most recent week if is not ended
    group_modify(~ {
      wday <- wday(today(tzone = "America/New_York"), week_start = 1)
      now <- now(tzone = "America/New_York")
      if (wday >= 5 | (wday == 1 & hour(now) < 17))
        .x
      else
        filter(.x, weekend != max(weekend))
    }) %>%
    group_nest(weekend)
})


### Parameters
cache("lookbacks", {
  c(52, 104)
})

cache("models", {
  c("dcc")
})

cache("dists", {
  c("mvnorm", "mvt")
})

cache("rebalances", {
  c(1, 4)
})

cache("cost", {
  c(0.00005)
})

## TODO Consider dials/tune
cache("params", depends = c("nested_data", "lookbacks", "models", "dists"), {
  crossing(weekend = nested_data$weekend,
           lookback = lookbacks,
           model = models,
           dist = dists) %>%
    ## Delete heading rows of each group
    group_split(lookback, model, dist) %>%
    map_dfr(~ {
      slice(., .$lookback[1]:n())
    })
})


### Used only for interactive analysis
cache("xts_data", depends = c("nested_data", "params"), {
  p <- params[1, ]
  slice_data(nested_data, p$weekend, p$lookback) %>%
    convert_to_longer(ret_log) %>%
    tk_xts(-date, date)
})


### Generate weight and performance from fits
## TODO Add dcc-mvnorm, copula
## TODO Other optimization method, return forecast (not only gmv)

cache("fits_dim_check", depends = c("fits"), {
  fits %>%
    group_by(model, dist, lookback) %>%
    summarize(count = n(),
              fail = sum(map_lgl(fit, ~ class(.) != "DCCfit")))
})

cache("weights_by_type", depends = c("weights"), {
  weights %>%
    select(-cov) %>%
    group_nest(model, dist, type, lookback) %>%
    mutate(weight = map(data, ~ {
      bind_cols(data.frame(date = .$weekend),
                map_dfr(.$weight, ~ as_tibble(t(.))))
    })) %>%
    select(-data)
})

cache("perfs_by_type", depends = c("weights_by_type", "rebalances", "cost"), {

  calc_perf <- function(weight, rebalance, cost, ...) {
    ## Subset rows only on rebalance timing
    idx <- (1:nrow(weight) -1) %% rebalance == 0
    w <- weight[idx, ]

    ## Commission
    c <- w
    c[, 2:ncol(c)] <- cost

    ## Fill weights gap and convert to xts
    w_f <- left_join(all_dates, w, by = "date") %>%
      mutate_at(vars(-date), lag) %>%
      fill(-date, .direction = "down") %>%
      filter(complete.cases(.))

    ## Fill commission gap with 0
    replace_rules <- map(vector("list", ncol(c) - 1), ~ 0) %>%
      set_names(colnames(c[, -1]))
    c_f <- left_join(all_dates, c, by = "date") %>%
      mutate_at(vars(-date), lag) %>%
      filter(date >= min(w_f$date)) %>%
      replace_na(replace_rules)

    ## Return by symbol
    ret <- convert_to_longer(data, ret) %>%
      filter(date >= min(w_f$date))

    ## Portfolio return
    port_ret <- ret[, -1] * w_f[, -1] - c_f[, -1]

    ## Return combined data.frame
    bind_cols(
      data.frame(date = ret$date),
      port_ret,
      data.frame(ret = rowSums(port_ret))
    ) %>%
      ## Add cumulative return and drawdown
      mutate(cum_ret = cumprod(1 + ret) - 1,
             drawdown = suppressWarnings(Drawdowns(ret)))
  }

  ## the first date of the longest lookback
  first_date <- map_dbl(weights_by_type$weight, ~ min(.$date)) %>% max() %>% as_date()
  all_dates <- data.frame(date = sort(unique(data$date)))

  ## Duplicate by rebalance
  map(rebalances, ~ mutate(weights_by_type, rebalance = .)) %>%
    bind_rows() %>%
    ## Align with the first date of the longest lookback
    mutate(weight = map(weight, ~ filter(., date >= !!first_date))) %>%
    mutate(perf = pmap(., calc_perf, cost = cost)) %>%
    select(-weight)
})

cache("perf_summary", depends = c("perfs_by_type"), {
  pmap_dfr(perfs_by_type, function(model, dist, type, lookback, rebalance, perf) {
    model_dist <- str_split(model, "_")[[1]]
    r <- perf %>% select(date, ret) %>% tk_xts(ret, date)

    data.frame(
      Model     = model,
      Dist      = dist,
      Type      = type,
      Lookback  = lookback,
      Rebalance = rebalance,
      Sharpe    = round(as.numeric(SharpeRatio(r, annualize = TRUE, FUN = "StdDev")), 2),
      Avg_Ret   = as.numeric(Return.annualized(r)),
      Cum_Ret   = as.numeric(Return.cumulative(r)),
      Std_Dev   = as.numeric(StdDev.annualized(r)),
      Max_DD    = as.numeric(maxDrawdown(r)),
      stringsAsFactors = FALSE)
  })
})


### Plots
cache("plot_perf_by_symbol", depends = c("data"), {

  perf <- suppressWarnings(data %>%
    group_by(symbol) %>%
    mutate(cum_ret = cumprod(1 + ret) - 1,
           drawdown = Drawdowns(ret)) %>%
    ungroup())

  cum_ret  <- plot_lines(perf, cum_ret, symbol) + ggtitle("Performance summary by symbol")
  ret      <- plot_lines(perf, ret, symbol)
  drawdown <- plot_lines(perf, drawdown, symbol)
  cum_ret + ret + drawdown + plot_layout(ncol = 1, heights = c(2, 1, 1))
})

cache("plot_perf_by_type", depends = c("perfs_by_type"), {

  extract_data <- function(model, dist, type, lookback, rebalance, perf, col) {
    col <- enquo(col)
    perf2 <- select(perf, date, !!col)
    perf2 %>%
      mutate(model = !!model, dist = !!dist, type = !!type,
             lookback = !!lookback, rebalance = !!rebalance) %>%
      select(-!!col, everything(), !!col)
  }

  cum_rets <- pmap_dfr(perfs_by_type, extract_data, col = cum_ret)
  drawdowns <- suppressWarnings(pmap_dfr(perfs_by_type, extract_data, col = drawdown))
  p <- list()

  p$cum_ret <- plot_lines(cum_rets, cum_ret, type, offset = 0.2) +
    ggtitle("Cumulative returns by model") +
    facet_grid(rebalance ~ lookback, labeller = label_both)

  p$drawdown <- plot_lines(drawdowns, drawdown, type, offset = 0.2) +
    ggtitle("Drawdown") +
    facet_grid(rebalance ~ lookback, labeller = label_both)
  p
})

cache("plot_sharpe_by_param", depends = c("perf_summary"), {
  plot_box_violin_point <- function(perf_summary, param) {
    param <- enquo(param)
    perf_summary %>%
      mutate(!!param := as.factor(!!param)) %>%
      select(!!param, Sharpe) %>%
      ggplot(aes(x = !!param, y = Sharpe, color = !!param)) +
      geom_violin(aes(fill = !!param), alpha = 0.3) +
      geom_boxplot(color = "black", width = 0.25) +
      geom_point(position = position_jitter(width = 0.4, height = 0.0), alpha = 0.5) +
      stat_summary(fun.y = mean, geom = "point", color = "black", shape = 4, size = 4) +
      theme_my()
  }

  p <- list()
  p$model <- plot_box_violin_point(perf_summary, Model) +
    ggtitle("Model vs. Sharpe")

  p$dist <- plot_box_violin_point(perf_summary, Dist) +
    ggtitle("Dist vs. Sharpe")

  p$type <- plot_box_violin_point(perf_summary, Type) +
    ggtitle("Type vs. Sharpe")

  p$lookback <- plot_box_violin_point(perf_summary, Lookback) +
    ggtitle("Lookback vs. Sharpe")

  p$rebalance <- plot_box_violin_point(perf_summary, Rebalance) +
    ggtitle("Rebalance vs. Sharpe")
  p
})

cache("plot_weight_by_type", depends = c("weights_by_type"), {
  weights_by_type %>%
    mutate(plot = pmap(., function(type, lookback, weight, ...) {
      title <- glue("{str_to_upper(type)} (Lookback = {lookback})")
      weight %>%
        gather(key = "Symbol", value = "Weight", -date) %>%
        ggplot(aes(x = date, y = Weight, fill = Symbol)) +
        geom_area(color = "black", size = 0.1) +
        ggtitle(title) +
        theme_my() +
        theme(legend.position = "right")
    })) %>%
    select(-weight)
})

cache("plot_rshape_history", depends = c("fits"), {
  fits %>%
    filter(dist == "mvt") %>%
    mutate(shape = map_dbl(fit, rshape)) %>%
    select(-fit) %>%
    ggplot(aes(x = weekend, y = shape)) +
    geom_point() +
    facet_grid(model ~ lookback, labeller = label_both) +
    theme_my()
})
