source("func/spss_empty2na.R")

a <- haven::read_sav("data/子女_预.sav") |>
  spss_empty2na()
