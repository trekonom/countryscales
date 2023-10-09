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
  dplyr::bind_rows()

saveRDS(testlocale, "inst/extdata/testloc.rds")
