spss_empty2na <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        .cols = where(is.character),
        .fns = ~ ifelse(is.na(.x) | .x == "", NA_character_, .x)
      )
    )
}
