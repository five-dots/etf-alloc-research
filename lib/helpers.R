
### Data utils
slice_data <- function(nested_data, weekend, lookback) {
  row_num <- which(nested_data$weekend == weekend)
  nested_data %>%
    slice((row_num - lookback + 1):row_num) %>%
    pull(data) %>%
    bind_rows()
}

convert_to_longer <- function(data, col) {
  col <- enquo(col)
  data %>%
    select(symbol, date, !!col) %>%
    pivot_wider(names_from = symbol, values_from = !!col)
}


mean_weight <- function(weight) {
  weight %>%
    gather(key = "symbol", value = "weight", -date) %>%
    group_by(symbol) %>%
    summarise(avg_weight = percent(mean(weight)))
}


### Plot
theme_my <- function() {
  theme_light() + theme(
    plot.title = element_text(face = "bold.italic"),
    axis.title = element_blank(),
    legend.position = "none")
}

plot_lines <- function(perf, y_var, color_var, offset = 0.1) {
  y_var <- enquo(y_var)
  color_var <- enquo(color_var)

  min_date <- min(perf$date)
  max_date <- max(perf$date)
  range <- c(min_date, max_date + ((max_date - min_date) * offset))

  perf %>%
    group_by(!!color_var) %>%
    mutate(label = if_else(date == max(date), !!color_var, NA_character_)) %>%
    ggplot(aes(x = date, y = !!y_var, color = !!color_var)) +
    geom_line() +
    geom_label_repel(aes(label = label), na.rm = TRUE, xlim = c(max_date, NA), size = 3) +
    scale_x_date(limits = range) +
    theme_my()
}
