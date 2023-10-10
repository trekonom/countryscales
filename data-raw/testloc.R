library(bubble)

ct <- NodeSession$new()

ct$eval(
  paste0(
    readLines("data-raw/numberFormats.js"),
    collapse = "\n"
  )
)

locs <- locales$locale

number <- 123456
percent <- .789
currency <- 123456

testlocale <- lapply(locs, \(x) {
  ct$eval(
    sprintf(
      "var foo = numberFormats('%s', %f, %f, %f);",
      x, number, percent, currency
    )
  )
  as.data.frame(ct$get(foo))
}) |>
  dplyr::bind_rows() |>
  mutate(
    across(
      c(
        number_pos, percent_pos, currency_pos,
        number_neg, percent_neg, currency_neg
      ),
      ~ gsub("\u200e", "", .x, fixed = TRUE)
    ),
    across(
      c(
        number_pos, percent_pos, currency_pos,
        number_neg, percent_neg, currency_neg
      ),
      ~ gsub("\u200f", "", .x, fixed = TRUE)
    )
  )

saveRDS(testlocale, "inst/extdata/testloc.rds")
