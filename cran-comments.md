# Local check on Ubuntu 22.04

   
── R CMD check results ──────── navigation 0.0.1 ────
Duration: 15m 27.2s

❯ checking installed package size ... NOTE
    installed size is 20.3Mb
    sub-directories of 1Mb or more:
      data   7.6Mb
      doc    6.7Mb
      libs   5.5Mb

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

0 errors ✔ | 0 warnings ✔ | 2 notes ✖

R CMD check succeeded


# Rhub check

── navigation 0.0.1: NOTE

  Build ID:   navigation_0.0.1.tar.gz-47625ba2fa3a4347822508d9ff0c97e8
  Platform:   Windows Server 2022, R-devel, 64 bit
  Submitted:  2h 4m 53.1s ago
  Build time: 11m 24.4s

❯ checking CRAN incoming feasibility ... [11s] NOTE
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    GNSS (16:58)
    Kalman (16:31)
  
  Size of tarball: 10480971 bytes
  Maintainer: 'Lionel Voirol <lionelvoirol@hotmail.com>'

❯ checking installed package size ... NOTE
    installed size is 16.0Mb
    sub-directories of 1Mb or more:
      data   7.6Mb
      libs   1.2Mb
      doc    6.7Mb

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

❯ checking examples ... [158s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                                user system elapsed
  navigation                   46.75   0.78   47.55
  compute_mean_orientation_err 21.80   0.33   22.13
  plot.navigation              13.01   0.75   13.92
  compare.navigation           12.33   0.22   12.58
  compute_coverage             11.27   0.11   11.37
  compute_nees                 10.99   0.17   11.16
  compute_mean_position_err    10.21   0.24   10.44
  plot_imu_err_with_cov         8.05   0.13    8.21
  plot_nav_states_with_cov      7.88   0.13    8.00

❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

0 errors ✔ | 0 warnings ✔ | 6 notes ✖

── navigation 0.0.1: NOTE

  Build ID:   navigation_0.0.1.tar.gz-b6d95f117c8047b380bdb611a14bfbf7
  Platform:   Ubuntu Linux 20.04.1 LTS, R-release, GCC
  Submitted:  2h 4m 53.3s ago
  Build time: 42m 29.1s

❯ checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Lionel Voirol <lionelvoirol@hotmail.com>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    GNSS (16:58)
    Kalman (16:31)
  
  Size of tarball: 10480971 bytes

❯ checking installed package size ... NOTE
    installed size is 25.6Mb
    sub-directories of 1Mb or more:
      data   7.6Mb
      doc    6.7Mb
      libs  10.7Mb

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

❯ checking examples ... [127s/127s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                                 user system elapsed
  navigation                   40.920  0.192  41.114
  compute_mean_orientation_err 18.876  0.055  18.933
  plot.navigation              10.371  0.072  10.398
  compare.navigation            9.716  0.052   9.768
  compute_nees                  9.502  0.064   9.567
  compute_coverage              9.521  0.024   9.546
  compute_mean_position_err     8.995  0.032   9.027
  plot_nav_states_with_cov      6.549  0.036   6.586
  plot_imu_err_with_cov         6.486  0.036   6.523

❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 5 notes ✖

── navigation 0.0.1: NOTE

  Build ID:   navigation_0.0.1.tar.gz-9e568afce60045e797e076f2d6617744
  Platform:   Fedora Linux, R-devel, clang, gfortran
  Submitted:  2h 4m 53.5s ago
  Build time: 38m 9.2s

❯ checking CRAN incoming feasibility ... [4s/11s] NOTE
  Maintainer: ‘Lionel Voirol <lionelvoirol@hotmail.com>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    GNSS (16:58)
    Kalman (16:31)
  
  Size of tarball: 10480971 bytes

❯ checking installed package size ... NOTE
    installed size is 18.7Mb
    sub-directories of 1Mb or more:
      data   7.6Mb
      doc    6.7Mb
      libs   3.9Mb

❯ checking for future file timestamps ... NOTE
  unable to verify current time

❯ checking for GNU extensions in Makefiles ... NOTE
  GNU make is a SystemRequirements.

❯ checking examples ... [134s/132s] NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                                 user system elapsed
  navigation                   42.298  0.172  42.471
  compute_mean_orientation_err 19.548  0.108  19.657
  plot.navigation              11.055  0.112  10.495
  compare.navigation           10.415  0.080  10.496
  compute_nees                  9.933  0.028   9.962
  compute_coverage              9.800  0.076   9.876
  compute_mean_position_err     9.190  0.024   9.213
  plot_nav_states_with_cov      6.750  0.060   6.811
  plot_imu_err_with_cov         6.723  0.028   6.752

❯ checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

0 errors ✔ | 0 warnings ✔ | 6 notes ✖

## R-CMD-check on GitHub actions 

All jobs pass on 

- macOS-latest (release)
- ubuntu-latest (devel)
- ubuntu-latest (oldrel-1)
- ubuntu-latest (release)
- windows-latest (release)


# Downstream dependencies
There are currently no downstream dependencies for this package.
