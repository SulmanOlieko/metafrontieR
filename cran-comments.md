# cran-comments.md

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  - "New submission" is expected for a first-time CRAN submission.
  - The `tidy` HTML validation tool and the `V8` package are not installed
    on this local machine; CRAN check servers have both. These skipped
    checks are therefore not a concern.

## win-builder (R-devel, x86_64-w64-mingw32)

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  - "New submission" — first-time submission, expected.
  - "Possibly misspelled words in DESCRIPTION" — the flagged words (`Dakpo`, `Metafrontier`, `al`, `et`, `metafrontier`) are correct authors' names and domain-specific terminology.
  - All other sub-items in the NOTE have been addressed.

## Platform

* macOS Ventura 13.7.8, R 4.5.2 (x86_64-apple-darwin20)
* Windows Server 2022, R-devel (win-builder)
