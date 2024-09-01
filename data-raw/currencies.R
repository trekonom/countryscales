## Re-Import currencies data to avoid errors from R CMD check
data(currencies, package = "i18n")

usethis::use_data(currencies, overwrite = TRUE)
