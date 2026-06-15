# Message
Re-Updating navigation to CRAN after it was removed from CRAN as it required archived package `rbenchmark`.
Removed this dependency and updating new version.


# Local check on Ubuntu 22.04
==> Rcpp::compileAttributes()

* Updated R/RcppExports.R

==> devtools::check()

══ Documenting ════════════════════════════════════════════════
ℹ Updating navigation documentation
ℹ Loading navigation
Loading required package: plotly
Loading required package: ggplot2

Attaching package: ‘plotly’

The following object is masked from ‘package:ggplot2’:

    last_plot

The following object is masked from ‘package:stats’:

    filter

The following object is masked from ‘package:graphics’:

    layout

Loading required package: magrittr
Loading required package: simts

Attaching package: ‘simts’

The following object is masked from ‘package:plotly’:

    select


══ Building ═══════════════════════════════════════════════════
Setting env vars:
• CFLAGS    : -Wall -pedantic -fdiagnostics-color=always
• CXXFLAGS  : -Wall -pedantic -fdiagnostics-color=always
• CXX11FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX14FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX17FLAGS: -Wall -pedantic -fdiagnostics-color=always
• CXX20FLAGS: -Wall -pedantic -fdiagnostics-color=always
── R CMD build ────────────────────────────────────────────────
✔  checking for file ‘/home/lionel/github_repo/navigation/DESCRIPTION’ ...
─  preparing ‘navigation’:
✔  checking DESCRIPTION meta-information ...
─  cleaning src
─  installing the package to build vignettes
✔  creating vignettes (1m 35.7s)
─  cleaning src
─  checking for LF line-endings in source and make files and shell scripts (613ms)
─  checking for empty or unneeded directories
─  building ‘navigation_0.0.2.tar.gz’
   
══ Checking ═══════════════════════════════════════════════════
Setting env vars:
• _R_CHECK_CRAN_INCOMING_USE_ASPELL_           : TRUE
• _R_CHECK_CRAN_INCOMING_REMOTE_               : FALSE
• _R_CHECK_CRAN_INCOMING_                      : FALSE
• _R_CHECK_FORCE_SUGGESTS_                     : FALSE
• _R_CHECK_PACKAGES_USED_IGNORE_UNUSED_IMPORTS_: FALSE
• NOT_CRAN                                     : true
── R CMD check ────────────────────────────────────────────────
─  using log directory ‘/home/lionel/github_repo/navigation.Rcheck’
─  using R version 4.5.2 (2025-10-31)
─  using platform: x86_64-pc-linux-gnu
─  R was compiled by
       gcc (Ubuntu 11.4.0-1ubuntu1~22.04.2) 11.4.0
       GNU Fortran (Ubuntu 11.4.0-1ubuntu1~22.04.2) 11.4.0
─  running under: Ubuntu 22.04.5 LTS
─  using session charset: UTF-8
─  using options ‘--no-manual --as-cran’ (383ms)
✔  checking for file ‘navigation/DESCRIPTION’
─  checking extension type ... Package
─  this is package ‘navigation’ version ‘0.0.2’
─  package encoding: UTF-8
✔  checking package namespace information ...
✔  checking package dependencies (1.1s)
✔  checking if this is a source package
✔  checking if there is a namespace
✔  checking for executable files ...
✔  checking for hidden files and directories
✔  checking for portable file names
✔  checking for sufficient/correct file permissions ...
─  checking whether package ‘navigation’ can be installed ... [41s/41s] OK (41.2s)
─  used C++ compiler: ‘g++ (Ubuntu 11.4.0-1ubuntu1~22.04.3) 11.4.0’
─  checking installed package size ... INFO
     installed size is 14.4Mb
     sub-directories of 1Mb or more:
       data   1.5Mb
       doc    6.8Mb
       libs   5.6Mb
✔  checking package directory
N  checking for future file timestamps (1m 0.4s)
   unable to verify current time
✔  checking ‘build’ directory
✔  checking DESCRIPTION meta-information (425ms)
✔  checking top-level files ...
✔  checking for left-over files
✔  checking index information ...
✔  checking package subdirectories (860ms)
✔  checking code files for non-ASCII characters ...
✔  checking R files for syntax errors ...
✔  checking whether the package can be loaded (2.5s)
✔  checking whether the package can be loaded with stated dependencies (2.3s)
✔  checking whether the package can be unloaded cleanly (2.3s)
✔  checking whether the namespace can be loaded with stated dependencies (2.2s)
✔  checking whether the namespace can be unloaded cleanly (2.2s)
✔  checking loading without being on the library search path (2.5s)
✔  checking dependencies in R code (4.5s)
✔  checking S3 generic/method consistency (2.1s)
✔  checking replacement functions (2.1s)
✔  checking foreign function calls (2.3s)
─  checking R code for possible problems ... [18s/18s] OK (17.6s)
✔  checking Rd files ...
✔  checking Rd metadata ...
✔  checking Rd line widths ...
✔  checking Rd cross-references ...
✔  checking for missing documentation entries (2.6s)
✔  checking for code/documentation mismatches (6.6s)
✔  checking Rd \usage sections (2.6s)
✔  checking Rd contents ...
✔  checking for unstated dependencies in examples ...
✔  checking contents of ‘data’ directory ...
✔  checking data for non-ASCII characters (357ms)
✔  checking LazyData
✔  checking data for ASCII and uncompressed saves ...
✔  checking line endings in C/C++/Fortran sources/headers
✔  checking line endings in Makefiles
✔  checking compilation flags in Makevars ...
─  checking for GNU extensions in Makefiles ... INFO
   GNU make is a SystemRequirements.
✔  checking for portable use of $(BLAS_LIBS) and $(LAPACK_LIBS)
✔  checking use of PKG_*FLAGS in Makefiles
✔  checking use of SHLIB_OPENMP_*FLAGS in Makefiles ...
✔  checking pragmas in C/C++ headers and code
✔  checking compilation flags used
✔  checking compiled code ...
✔  checking installed files from ‘inst/doc’ ...
✔  checking files in ‘vignettes’ ...
─  checking examples ... [28s/28s] OK (28s)
✔  checking for unstated dependencies in vignettes (467ms)
✔  checking package vignettes ...
─  checking re-building of vignette outputs ... [87s/87s] OK (1m 27.3s)
✔  checking for non-standard things in the check directory
✔  checking for detritus in the temp directory
   
   See
     ‘/home/lionel/github_repo/navigation.Rcheck/00check.log’
   for details.
   
── R CMD check results ────────────────── navigation 0.0.2 ────
Duration: 4m 39.5s

❯ checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✔ | 0 warnings ✔ | 1 note ✖

R CMD check succeeded

## R-CMD-check on GitHub actions 

All jobs pass on 

- macOS-latest (release)
- ubuntu-latest (devel)
- ubuntu-latest (oldrel-1)
- ubuntu-latest (release)
- windows-latest (release)

see https://github.com/SMAC-Group/navigation/actions/workflows/R-CMD-check.yaml

# Downstream dependencies
There are currently no downstream dependencies for this package.
