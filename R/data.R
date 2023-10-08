#' Gapminder Dataset
#'
#' A dataset containing data on GDP per capita, life expectancy and population
#' size of 183 countries for 1800 to 2015
#'
#' @format A data frame with 14,823 rows and 7 variables:
#' \describe{
#'   \item{country}{country name}
#'   \item{code}{country code}
#'   \item{region}{world region}
#'   \item{year}{year}
#'   \item{life_exp}{life expectancy at birth, in years}
#'   \item{gdp_per_cap}{GDP per capita, in dollars PPP}
#'   \item{pop}{population size, in persons}
#' }
#' @source \url{http://www.gapminder.org/}
"gapminder"

#' @rdname gapminder
"gapminder15"

#' Locales
#'
#' A dataset containing specification of formatting styles to label numbers,
#'    percentages and currencies for 574 locales based on
#'    CLDR (Common Locale Data Repository) data provided by the
#'    [i18n](https://rich-iannone.github.io/i18n/) package.
#'
#' @source \url{https://rich-iannone.github.io/i18n/}
"locales"
