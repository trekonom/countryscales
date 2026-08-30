# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## What this package does

`countryscales` extends `scales` and `ggplot2` with functions to format
numbers, percentages and currencies — and label ggplot2 axes — using
country- or locale-specific conventions (thousands separator, decimal
mark, currency symbol placement, sign placement, percent-sign
placement/spacing, etc.), sourced from CLDR data via the `i18n` package
([`countryscales::locales`](https://trekonom.github.io/countryscales/reference/locales.md),
[`countryscales::currencies`](https://trekonom.github.io/countryscales/reference/currencies.md)).

Not all locales are supported: locales using the Indian numbering system
(grouping by threes then twos) are excluded because they deviate from
the standard grouping `countryscales` implements.

## Common commands

Development follows standard R package conventions
(devtools/testthat/roxygen2).

``` r

devtools::load_all()      # load package for interactive dev
devtools::document()      # regenerate NAMESPACE/man/*.Rd from roxygen comments
devtools::test()          # run full test suite
devtools::check()         # full R CMD check
testthat::test_file("tests/testthat/test-label-de.R")  # run a single test file
```

There is no CI workflow that runs `R CMD check` or tests
(`.github/workflows/` only builds the pkgdown site) — running
tests/checks locally is the only verification.

## Architecture

### Three-layer function family, repeated per locale/country

Every formatting concept (`number`, `percent`, `currency`) is
implemented once as a “locale” version, then thin country-specific
wrappers just pin the `locale` argument:

- **[`label_number_locale()`](https://trekonom.github.io/countryscales/reference/label-locale.md)
  /
  [`label_percent_locale()`](https://trekonom.github.io/countryscales/reference/label-locale.md)
  /
  [`label_currency_locale()`](https://trekonom.github.io/countryscales/reference/label-locale.md)**
  (`R/label-locale.R`) — the actual logic. Each takes a `locale` string
  (e.g. `"de-DE"`), looks up formatting rules via `check_locale()`
  (`R/utils.R`, which reads
  [`countryscales::locales`](https://trekonom.github.io/countryscales/reference/locales.md)),
  and returns a `scales`-style labelling function (a function that
  formats a numeric vector to a character vector).
- **Country-specific wrappers** (`R/label-de.R`, `R/label-ch.R`,
  `R/label-us.R`) —
  e.g. [`label_number_de()`](https://trekonom.github.io/countryscales/reference/label-de.md),
  `number_de()` — just call the `_locale` version with a fixed `locale`.
  The `xxx_de()` (no `label_` prefix) variants are the same thing
  applied directly to a vector `x`, i.e. `label_xxx_de(...)(x)`.
- **`scale_x/y_number_locale()` / `..._percent_locale()` /
  `..._currency_locale()`** (`R/scale-locale.R`) and their
  `_de`/`_ch`/`_us` counterparts (`R/scale-de.R`, `R/scale-ch.R`,
  `R/scale-us.R`) — ggplot2 `continuous_scale()` constructors that use
  the corresponding `label_*` function to build axis labels. They funnel
  through the shared internal constructor `number_scale()` in
  `R/scale-constructor.R`.

When adding a new country (e.g. `_fr`), follow the `_de`/`_ch` files as
the template: add a `label-xx.R` with
`label_number_xx`/`number_xx`/`label_percent_xx`/`percent_xx`/
`label_currency_xx`/`currency_xx`, and a `scale-xx.R` with the six
`scale_x/y_*_xx` constructors — all delegating to the `_locale` versions
with a fixed locale string.

### Number formatting core

`R/label-number.R` and `R/label-currency.R` are internal (`@noRd`)
reimplementations of
[`scales::label_number()`](https://scales.r-lib.org/reference/label_number.html)/`label_currency()`,
vendored and modified because upstream `scales` doesn’t support custom
minus/plus symbols or the currency sign positioning/spacing needed for
correct locale output (see README “Credits” section for the rationale —
Unicode control characters for bidi text, custom sign positioning).
Don’t reach for
[`scales::label_number`](https://scales.r-lib.org/reference/label_number.html)
directly when locale-correct output is needed; use these internal
versions via the `label_*_locale()` functions.

### Data

`data/*.rda` (loaded via `LazyData: true`, documented in `R/data.R`) are
generated from scripts in `data-raw/` (not part of the built package —
see `.Rbuildignore`):

- `locales.R` builds `locales` (per-locale formatting rules: separators,
  decimal marks, sign position/spacing) from CLDR data via `i18n`.
- `currencies.R` builds `currencies` (re-imported from `i18n` to avoid
  `R CMD check` NOTEs about undocumented imports).
- `countries.R` / `g20.R` build country/locale lookup tables used in
  examples.
- `gapminder.R` builds the example datasets (`gapminder`, `gapminder15`)
  used in README/vignette examples.
- `testloc.R` / `numberFormats.js` generate `inst/extdata/testloc.rds`,
  the fixture tests are checked against (see Testing below).

### Testing against JS `Intl.NumberFormat`

The `label_xxx_locale` family is tested for correctness against the
output of JavaScript’s `Intl.NumberFormat` for each supported locale,
not just against hand-written expected strings.
`inst/extdata/testloc.rds` holds precomputed reference output (generated
from `data-raw/numberFormats.js` via `data-raw/testloc.R`); tests in
`tests/testthat/test-label-*.R` load it, filter to the relevant
`locale`, and compare
`label_number_*`/`label_percent_*`/`label_currency_*` output against the
`number_pos`/`number_neg`/`percent_pos`/… columns. When changing
formatting logic, this comparison — not just hardcoded expectations
later in the same test files — is the primary correctness check.
