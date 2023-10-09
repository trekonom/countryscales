library(bubble)

ct <- NodeSession$new()

ct$eval(
  paste0(
    readLines("data-raw/numberFormats.js"),
    collapse = "\n"
  )
)

locs <- locales$locale

testlocale <- lapply(locs, \(x) {
  ct$eval(sprintf("var foo = numberFormats('%s');", x))
  as.data.frame(ct$get(foo))
}) |>
  dplyr::bind_rows() |>
  mutate(
    across(
      c(number_neg, percent_neg, currency_neg),
      ~ gsub("\u200e", "", .x, fixed = TRUE)
    ),
    across(
      c(number_neg, percent_neg, currency_neg),
      ~ gsub("\u200f", "", .x, fixed = TRUE)
    )
  )

saveRDS(testlocale, "inst/extdata/testloc.rds")
