# US Style Formatting of Numbers

The label_xxx_xxx family of functions makes it easy to format numbers in
decimal format, as percentages or as currencies.

## Usage

``` r
label_number_us(
  accuracy = 1,
  scale = 1,
  prefix = "",
  suffix = "",
  big.mark = NULL,
  decimal.mark = NULL,
  trim = TRUE,
  ...
)

number_us(
  x,
  accuracy = 1,
  scale = 1,
  prefix = "",
  suffix = "",
  big.mark = NULL,
  decimal.mark = NULL,
  trim = TRUE,
  ...
)

label_percent_us(
  accuracy = 1,
  scale = 100,
  prefix = "",
  suffix = NULL,
  big.mark = NULL,
  decimal.mark = NULL,
  trim = TRUE,
  ...
)

percent_us(
  x,
  accuracy = 1,
  scale = 100,
  prefix = "",
  suffix = NULL,
  big.mark = NULL,
  decimal.mark = NULL,
  trim = TRUE,
  ...
)

label_currency_us(
  accuracy = 1,
  scale = 1,
  currency = "USD",
  prefix = NULL,
  suffix = NULL,
  big.mark = NULL,
  decimal.mark = NULL,
  trim = TRUE,
  ...
)

currency_us(
  x,
  accuracy = 1,
  scale = 1,
  currency = "USD",
  prefix = NULL,
  suffix = NULL,
  big.mark = NULL,
  decimal.mark = NULL,
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

- trim:

  Logical, if `FALSE`, values are right-justified to a common width (see
  [`base::format()`](https://rdrr.io/r/base/format.html)).

- ...:

  Other arguments passed on to
  [`base::format()`](https://rdrr.io/r/base/format.html).

- x:

  a numeric

- currency:

  currency symbol

## Details

- `label_number_us` formats numbers in decimal format.

- `label_percent_us` formats numbers as percentages.

- `label_currency_us` formats numbers as currencies.

## Examples

``` r
require(scales)

demo_continuous(c(-1e6, 1e6),
  labels = label_number_us()
)
#> scale_x_continuous(labels = label_number_us())

demo_continuous(c(-1, 1),
  labels = label_percent_us(accuracy = .1)
)
#> scale_x_continuous(labels = label_percent_us(accuracy = 0.1))

demo_continuous(c(-1, 1),
  labels = label_currency_us(accuracy = .1)
)
#> scale_x_continuous(labels = label_currency_us(accuracy = 0.1))
```
