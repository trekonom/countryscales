function numberFormats(locale, vn = 123456, vp = .789, vc = 123456) {
    const numberFormatter = new Intl.NumberFormat(locale);
    const percentFormatter = new Intl.NumberFormat(locale, {
        style: 'percent'
    });
    const currencyFormatter = new Intl.NumberFormat(locale, {
        style: 'currency',
        currency: 'USD',
        minimumFractionDigits: 0
    });

    const number_pos = numberFormatter.format(vn);
    const number_neg = numberFormatter.format(-vn);
    const percent_pos = percentFormatter.format(vp);
    const percent_neg = percentFormatter.format(-vp);
    const currency_pos = currencyFormatter.format(vc);
    const currency_neg = currencyFormatter.format(-vc);

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
