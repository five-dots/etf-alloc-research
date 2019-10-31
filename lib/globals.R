# Add any project specific configuration here.
add.config(
  apply.override = FALSE
)

# Add project specific configuration that can be overridden from load.project()
add.config(
  apply.override = TRUE,
  from = ymd("2005-07-29"),
  prj_dir = here::here()
)
