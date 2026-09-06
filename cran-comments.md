## Submission

This is a new release — countryscales' first CRAN submission.

## Test environments

* local macOS (aarch64-apple-darwin20), R 4.5.2
* GitHub Actions (macOS-latest, windows-latest, ubuntu-latest), R release, devel and oldrel-1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 0 notes locally and on the GitHub Actions CI matrix.

win-builder (both devel and release) reports 0 errors | 0 warnings | 1 note:

> Possibly misspelled words in DESCRIPTION: CLDR (20:17)

CLDR is the Unicode Common Locale Data Repository, the data source this
package is built on; it's spelled correctly and referenced elsewhere in the
DESCRIPTION as well.

## Downstream dependencies

There are currently no downstream dependencies for this package, as this is
its first release.
