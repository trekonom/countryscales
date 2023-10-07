function numberFormats(locale) {
    const numberFormatter = new Intl.NumberFormat(locale);
    const percentFormatter = new Intl.NumberFormat(locale, {
        style: 'percent'
    });
    const currencyFormatter = new Intl.NumberFormat(locale, {
        style: 'currency',
        currency: 'USD'
    });

    const number_pos = numberFormatter.format(123456.789);
    const number_neg = numberFormatter.format(-123456.789);
    const percent_pos = percentFormatter.format(.789);
    const percent_neg = percentFormatter.format(-.789);
    const currency_pos = currencyFormatter.format(123456.789);
    const currency_neg = currencyFormatter.format(-123456.789);

    return {
      "locale": locale,
      "number_pos": number_pos,
      "number_neg": number_neg,
      "percent_pos": percent_pos,
      "percent_neg": percent_neg,
      "currency_pos": currency_pos,
      "currency_neg": currency_neg
    };
}
