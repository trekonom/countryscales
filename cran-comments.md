## Resubmission

This is a resubmission. In this version I have:

* Added a `<https://cldr.unicode.org>` link for the CLDR webservice named in
  the Description field, per CRAN feedback.
* Replaced an unsuppressible `cat()` call in an internal helper
  (`demo_ggplot()`, used by the exported `demo_number()`) with `message()`,
  per CRAN feedback, so it can be silenced with `suppressMessages()`.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.2
* GitHub Actions (macOS-latest, windows-latest, ubuntu-latest), R release, devel and oldrel-1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes locally and on the GitHub Actions CI matrix.

<!-- TODO before resubmitting: re-run devtools::check_win_devel() /
     check_win_release() against these fixes and refresh the numbers below —
     the DESCRIPTION change may shift the word-position in the "possibly
     misspelled words" note below (it should still fire, since CLDR is still
     an unrecognized word), but this needs to be confirmed against a real
     win-builder run rather than assumed. -->

win-builder (both devel and release) previously reported 0 errors | 0
warnings | 1 note:

> Possibly misspelled words in DESCRIPTION: CLDR (20:17)

CLDR is the Unicode Common Locale Data Repository, the data source this
package is built on; it's spelled correctly and referenced elsewhere in the
DESCRIPTION as well.

## Downstream dependencies

There are currently no downstream dependencies for this package, as this is
its first release.
