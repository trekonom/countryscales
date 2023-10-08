library(rvest)
library(tidyverse)

countries <- data.frame(
  country = c("Germany", "United States", "Switzerland", "France")
)

countries <- locales[c("locale", "cntry")]
countries$cntry <- gsub(", .*$", "", countries$cntry)
countries$url <- paste0("https://en.wikipedia.org/wiki/", gsub(" ", "_", countries$cntry))

countries$demonym <- map_chr(
  countries$url,
  function(url) {
    print(url)

    html <- read_html(url)

    demonym <- html |>
      html_element(xpath = "//tr[./th/a[@title='Demonym']]")

    if (!inherits(demonym, "xml_missing")) {
      demonym |>
        html_element("td > a") |>
        html_text2()
    } else {
      NA_character_
    }
  }
)

writexl::write_xlsx(countries, "data-raw/demonyms.xlsx")
