# Package index

## Coordinate transformation

- [`X_ellips2ned()`](https://smac-group.github.io/navigation/reference/X_ellips2ned.md)
  : Transform position from ellipsoidal to NED coordinates
- [`X_ned2ellips()`](https://smac-group.github.io/navigation/reference/X_ned2ellips.md)
  : Transform position from NED to ellipsoidal coordinates

## Create objects

- [`make_sensor()`](https://smac-group.github.io/navigation/reference/make_sensor.md)
  :

  Construct a `sensor` object

- [`make_timing()`](https://smac-group.github.io/navigation/reference/make_timing.md)
  :

  Construct a `timing` object

- [`make_trajectory()`](https://smac-group.github.io/navigation/reference/make_trajectory.md)
  :

  Construct a `trajectory` object

## Perform navigation simulation

- [`navigation()`](https://smac-group.github.io/navigation/reference/navigation.md)
  : Runs "IMU model evaluation" or "INS-GPS-Baro integrated navigation
  (sensor fusion)"

## Compute Statistics

- [`compute_coverage()`](https://smac-group.github.io/navigation/reference/compute_coverage.md)
  : Compute Coverage
- [`compute_mean_orientation_err()`](https://smac-group.github.io/navigation/reference/compute_mean_orientation_err.md)
  : Compute mean orientation error
- [`compute_mean_position_err()`](https://smac-group.github.io/navigation/reference/compute_mean_position_err.md)
  : Compute mean position error
- [`compute_nees()`](https://smac-group.github.io/navigation/reference/compute_nees.md)
  : Compute Normalized Estimation Errror Squared (NEES)

## Inspect objects

- [`print(`*`<sensor>`*`)`](https://smac-group.github.io/navigation/reference/print.sensor.md)
  :

  Print a `sensor` object parameters (name, frequency and error model)

## Visualize objects and results

- [`plot(`*`<trajectory>`*`)`](https://smac-group.github.io/navigation/reference/plot.trajectory.md)
  :

  Plot a `trajectory` object

- [`plot(`*`<navigation>`*`)`](https://smac-group.github.io/navigation/reference/plot.navigation.md)
  :

  Plot a `navigation` object

- [`plot_nav_states_with_cov()`](https://smac-group.github.io/navigation/reference/plot_nav_states_with_cov.md)
  : Plot navigation states with covariance

- [`plot_imu_err_with_cov()`](https://smac-group.github.io/navigation/reference/plot_imu_err_with_cov.md)
  : Plot IMU error with covariances

- [`plot(`*`<nees.stat>`*`)`](https://smac-group.github.io/navigation/reference/plot.nees.stat.md)
  :

  Plot multiple `nees.stat` objects

- [`plot(`*`<coverage.stat>`*`)`](https://smac-group.github.io/navigation/reference/plot.coverage.stat.md)
  :

  Plot multiple `coverage.stat` objects

- [`plot(`*`<navigation.stat>`*`)`](https://smac-group.github.io/navigation/reference/plot.navigation.stat.md)
  :

  Plot multiple `navigation.stat` objects

## Data

- [`example_1_traj_ned`](https://smac-group.github.io/navigation/reference/example_1_traj_ned.md)
  : Example trajectory 1 in NED coordinates
- [`example_1_traj_ellipsoidal`](https://smac-group.github.io/navigation/reference/example_1_traj_ellipsoidal.md)
  : Example trajectory 1 in ellipsoidal coordinates
- [`example_2_traj_ned`](https://smac-group.github.io/navigation/reference/example_2_traj_ned.md)
  : Example trajectory 2 in NED coordinates
- [`example_2_traj_ellipsoidal`](https://smac-group.github.io/navigation/reference/example_2_traj_ellipsoidal.md)
  : Example trajectory 2 in ellipsoidal coordinates
- [`lemniscate_traj_ned`](https://smac-group.github.io/navigation/reference/lemniscate_traj_ned.md)
  : Lemniscate trajectory in NED coordinates
