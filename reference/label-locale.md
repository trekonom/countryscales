# Formatting numbers

The label_xxx_locale family of functions makes it easy to format numbers
in decimal format, as percentages or as currencies.

## Usage

``` r
label_number_locale(
  accuracy = NULL,
  scale = 1,
  prefix = "",
  suffix = "",
  big.mark = NULL,
  decimal.mark = NULL,
  locale = "en-US",
  trim = TRUE,
  ...
)

label_percent_locale(
  accuracy = NULL,
  scale = 100,
  prefix = NULL,
  suffix = NULL,
  big.mark = NULL,
  decimal.mark = NULL,
  locale = "en-US",
  trim = TRUE,
  ...
)

label_currency_locale(
  accuracy = NULL,
  scale = 1,
  prefix = NULL,
  suffix = NULL,
  big.mark = NULL,
  decimal.mark = NULL,
  p_sep_by = NULL,
  n_sep_by = NULL,
  currency = "USD",
  locale = "en-US",
  trim = TRUE,
  ...
)
```

## Arguments

- accuracy:

  A number to round to. Use (e.g.) `0.01` to show 2 decimal places of
  precision. If `NULL`, the default, uses a heuristic that should ensure
  breaks have the minimum number of digits needed to show the difference
  between adjacent values.

  Applied to rescaled data.

- scale:

  A scaling factor: `x` will be multiplied by `scale` before formatting.
  This is useful if the underlying data is very small or very large.

- prefix:

  Additional text to display before the number. The suffix is applied to
  absolute value before `style_positive` and `style_negative` are
  processed so that `prefix = "$"` will yield (e.g.) `-$1` and `($1)`.

- suffix:

  Additional text to display after the number.

- big.mark:

  Character used between every 3 digits to separate thousands. The
  default (`NULL`) retrieves the setting from the [number
  options](https://scales.r-lib.org/reference/number_options.html).

- decimal.mark:

  The character to be used to indicate the numeric decimal point. The
  default (`NULL`) retrieves the setting from the [number
  options](https://scales.r-lib.org/reference/number_options.html).

- locale:

  locale string. Defaults to "en-US"

- trim:

  Logical, if `FALSE`, values are right-justified to a common width (see
  [`base::format()`](https://rdrr.io/r/base/format.html)).

- ...:

  Other arguments passed on to
  [`base::format()`](https://rdrr.io/r/base/format.html).

- p_sep_by:

  separator between currency symbol and positive monteary value

- n_sep_by:

  separator between currency symbol and negative monteary value

- currency:

  currency symbol

## Details

- `label_number_locale` formats numbers in decimal format.

- `label_percent_locale` formats numbers as percentages.

- `label_currency_locale` formats numbers as currencies.

## Examples

``` r
if (FALSE) { # \dontrun{
require(scales)
demo_continuous(
  c(-1e6, 1e6),
  labels = label_number_locale(locale = "fr-FR")
)
demo_continuous(
  c(-1, 1),
  label_percent_locale(locale = "it-IT", accuracy = .01)
)
demo_continuous(
  c(-1, 1),
  labels = label_currency_locale(
    locale = "ja-JP", accuracy = .1,
    currency = "JPY"
  )
)
} # }
```
