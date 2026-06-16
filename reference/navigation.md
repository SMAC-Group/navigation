# Runs "IMU model evaluation" or "INS-GPS-Baro integrated navigation (sensor fusion)"

This function performs of the two following main tasks, base on the
provided input. If a reference trajectory (`traj.ref`) is provided, it
generates sensor data (IMU, GPS, Baro) corrupted by additive errors
according to `snsr.mdl`, and performs navigation using `KF.mdl` as the
sensor error model within the Kalman filter to evaluate how this
particular model performs when navigating.

## Usage

``` r
navigation(
  traj.ref,
  timing,
  snsr.mdl,
  KF.mdl,
  g = 9.8056,
  num.runs = 1,
  results.system = "ned",
  x_o = NULL,
  noProgressBar = FALSE,
  IC = NULL,
  imu_data = NULL,
  gps_data = NULL,
  baro_data = NULL,
  input.seed = 0,
  PhiQ_method = "exact",
  P_subsampling = 1,
  compute_PhiQ_each_n = 1,
  parallel.ncores = detectCores(all.tests = FALSE, logical = TRUE),
  tmpdir = tempdir()
)
```

## Arguments

- traj.ref:

  A `trajectory` object (see the documentation for `make_trajectory`),
  serving as the reference trajectory for generating sensor data and
  evaluating the error in navigation once performed. Only position and
  attitude data are required/considered, and velocity will be calculated
  from position.

- timing:

  A `timing` object (see the documentation for `make_timing`) containing
  timing information such as start and end of navigation.

- snsr.mdl:

  A `sensor` object (see the documentation for `make_sensor`) containing
  additive sensor error model to generate realistic sensor data.

- KF.mdl:

  A `sensor` object (see the documentation for `make_sensor`) containing
  additive sensor error model to be used within the Kalman filter for
  navigation.

- g:

  Gravitational acceleration.

- num.runs:

  Number of times the sensor data generation and navigation is performed
  (Monte-Carlo simulation).

- results.system:

  The coordinate system (`ned`/`ellipsoidal`) in which the results are
  reported (see the documentation for `make_trajectory`).

- x_o:

  Origin of the fixed `ned` frame.

- noProgressBar:

  A `bolean` specifying if there should not be a progress bar.

- IC:

  Initial conditions. See the examples for the format.

- imu_data:

  IMU data. See the examples for the format.

- gps_data:

  GPS data. See the examples for the format.

- baro_data:

  Baro data. See the examples for the format.

- input.seed:

  Seed for the random number generator. Actual seed is computed as
  `input.seed * num.runs + run`

- PhiQ_method:

  String that specify the method to compute Phi and Q matrices, can be
  "exact" or the order of the Taylor expansions to use.

- P_subsampling:

  (memory optimization) store only one sample of the P matrix each
  `P_subsampling` time instants.

- compute_PhiQ_each_n:

  Specify the interval of IMU measurements between each computation of
  PhiQ.

- parallel.ncores:

  The number of cores to be used for parallel Monte-Carlo runs.

- tmpdir:

  Where to store temporary navigation output. It should not be mapped on
  a filesystem which lives in RAM.

## Value

An object of `navigation` class containing the reference trajectory,
fused trajectory, sensor data, covariance matrix, and time.

## Author

Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier

## Examples

``` r
# load data
data("lemniscate_traj_ned")
head(lemniscate_traj_ned)
#>         t          x          y z         roll     pitch_sm       yaw
#> [1,] 0.00 0.00000000 0.00000000 0 0.0000000000 0.000000e+00 0.7853979
#> [2,] 0.01 0.05235987 0.05235984 0 0.0001821107 8.255405e-05 0.7853971
#> [3,] 0.02 0.10471968 0.10471945 0 0.0003642249 1.650525e-04 0.7853946
#> [4,] 0.03 0.15707937 0.15707860 0 0.0005463461 2.474976e-04 0.7853905
#> [5,] 0.04 0.20943890 0.20943706 0 0.0007284778 3.298918e-04 0.7853847
#> [6,] 0.05 0.26179819 0.26179460 0 0.0009106235 4.122374e-04 0.7853773
traj <- make_trajectory(data = lemniscate_traj_ned, system = "ned")
timing <- make_timing(
  nav.start = 0, # time at which to begin filtering
  nav.end = 15,
  freq.imu = 100, # frequency of the IMU, can be slower wrt trajectory frequency
  freq.gps = 1, # GNSS frequency
  freq.baro = 1, # barometer frequency (to disable, put it very low, e.g. 1e-5)
  gps.out.start = 8, # to simulate a GNSS outage, set a time before nav.end
  gps.out.end = 13
)
# create sensor for noise data generation
snsr.mdl <- list()
# this uses a model for noise data generation
acc.mdl <- WN(sigma2 = 5.989778e-05) +
  AR1(phi = 9.982454e-01, sigma2 = 1.848297e-10) +
  AR1(phi = 9.999121e-01, sigma2 = 2.435414e-11) +
  AR1(phi = 9.999998e-01, sigma2 = 1.026718e-12)
gyr.mdl <- WN(sigma2 = 1.503793e-06) +
  AR1(phi = 9.968999e-01, sigma2 = 2.428980e-11) +
  AR1(phi = 9.999001e-01, sigma2 = 1.238142e-12)
snsr.mdl$imu <- make_sensor(
  name = "imu",
  frequency = timing$freq.imu,
  error_model1 = acc.mdl,
  error_model2 = gyr.mdl
)
# RTK-like GNSS
gps.mdl.pos.hor <- WN(sigma2 = 0.025^2)
gps.mdl.pos.ver <- WN(sigma2 = 0.05^2)
gps.mdl.vel.hor <- WN(sigma2 = 0.01^2)
gps.mdl.vel.ver <- WN(sigma2 = 0.02^2)
snsr.mdl$gps <- make_sensor(
  name = "gps",
  frequency = timing$freq.gps,
  error_model1 = gps.mdl.pos.hor,
  error_model2 = gps.mdl.pos.ver,
  error_model3 = gps.mdl.vel.hor,
  error_model4 = gps.mdl.vel.ver
)
# Barometer
baro.mdl <- WN(sigma2 = 0.5^2)
snsr.mdl$baro <- make_sensor(
  name = "baro",
  frequency = timing$freq.baro,
  error_model1 = baro.mdl
)
# define sensor for Kalmna filter
KF.mdl <- list()
# make IMU sensor
KF.mdl$imu <- make_sensor(
  name = "imu",
  frequency = timing$freq.imu,
  error_model1 = acc.mdl,
  error_model2 = gyr.mdl
)
KF.mdl$gps <- snsr.mdl$gps
KF.mdl$baro <- snsr.mdl$baro
# perform navigation simulation
num.runs <- 5 # number of Monte-Carlo simulations
res <- navigation(
  traj.ref = traj,
  timing = timing,
  snsr.mdl = snsr.mdl,
  KF.mdl = KF.mdl,
  num.runs = num.runs,
  noProgressBar = TRUE,
  PhiQ_method = "1",
  # order of the Taylor expansion of the matrix exponential
  # used to compute Phi and Q matrices
  compute_PhiQ_each_n = 10,
  # compute new Phi and Q matrices every n IMU steps (execution time optimization)
  parallel.ncores = 1,
  P_subsampling = timing$freq.imu
) # keep one covariance every second
```
