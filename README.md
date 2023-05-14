![](https://github.com/SMAC-Group/navigation/actions/workflows/R-CMD-check.yaml/badge.svg)
![](https://img.shields.io/github/last-commit/SMAC-Group/navigation) 
[![Licence](https://img.shields.io/badge/licence-AGPL--3.0-blue.svg)](https://opensource.org/licenses/AGPL-3.0)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.0.0-6666ff.svg)](https://cran.r-project.org/)


# 🛰️ `navigation` Overview <a href="https://smac-group.com/"><img src="man/figures/logo.png" align="right" style="width: 20%; height: 20%"/></a>

The `navigation` `R` package allows to analyze the
impact of sensor error modeling on performance of integrated navigation
(sensor fusion) based on IMU, GPS (generally speaking, GNSS), and
barometer data. The package allows for one of the two major tasks:

  - **Sensor model evaluation:** The user shall provide a reference
    trajectory, along which the navigation performance is being
    evaluated using different sensor error models. Perfect sensor data
    along that reference trajectory are generated, and then corrupted by
    sensor error coming from either simulation based on the error models
    provided by user, or directly from user input *(option to be
    added)*. Integrated navigation is then performed, whit a separately
    provided error model to be used within the Extended Kalman Filter
    (EKF). The user can easily introduce GPS outage periods, and there
    is a growing number of tools to visualize and summarize the results.

  - **Integrated navigation (sensor fusion)** As a natural by-product of
    the first main application, integrated navigation is also available
    to users. Providing only the sensor data and the sensor error model
    to be used within the navigation filter, the user is able to perform
    integrated navigation using the package and also benefit from a
    subset of visualization tools.

**Caution** A flat non-rotating Earth model is assumed throughout the
package. We consider this not to be of major impact on sensor model
evaluation, as the main contributor there are match/mismatch between the
additive sensor errors and the provided error models to the navigation
filter. For absolute navigation results though, is long distances and
high speeds are involved, such simplifications start to have measurable
impact on results. Also, attitude parameterization is done via Euler
angles at the moment, bringing their interinsic limitations, such as the
singularity at pitch $=\pm \pi/2$. This limitation may be resolved in
future using other attitude parameterizations such as quaternions.

# Installation Instructions

The `navigation` package is currently only available on GitHub.

Furthermore, the package is currently in an early development phase. Some
functions are stable and some are still in development. Moreover, the
GitHub version is subject to modifications/updates which may lead to
installation problems or broken functions.

You can install the latest
version of the `navigation` package with:

```
# Install devtools package if not already installed
if (!require("devtools")) {
  install.packages("devtools")
}

# Install package from GitHub
devtools::install_github('https://github.com/SMAC-Group/navigation')
``` 


### External `R` libraries

The `navigation` package relies on a limited number of external libraries, but notably on `Rcpp` and `RcppArmadillo` which require a `C++` compiler for installation, such as for example `gcc`.

# Usage

Find detailled usage instructions, examples and the user's manual at the [package website](https://smac-group.github.io/navigation/index.html).

# License

This source code is released under is the GNU AFFERO GENERAL PUBLIC LICENSE (AGPL) v3.0. 

# References

[D. A. Cucci, L. Voirol, M. Khaghani and S. Guerrier, "On Performance Evaluation of Inertial Navigation Systems: the Case of Stochastic Calibration," in IEEE Transactions on Instrumentation and Measurement, doi: 10.1109/TIM.2023.3267360.](https://ieeexplore.ieee.org/document/10104150)
