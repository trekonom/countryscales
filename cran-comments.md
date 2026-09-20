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

With `remote = TRUE` (i.e. CRAN's own incoming checks enabled), `devtools::check()`
additionally reports the standard note expected for any first submission:

> New submission

win-builder (both devel and release, re-checked 2026-09-19/20 against these
fixes) reports 0 errors | 0 warnings | 1 note:

> Possibly misspelled words in DESCRIPTION: CLDR (20:17)

CLDR is the Unicode Common Locale Data Repository, the data source this
package is built on; it's spelled correctly and referenced elsewhere in the
DESCRIPTION as well.

## Downstream dependencies

There are currently no downstream dependencies for this package, as this is
its first release.
