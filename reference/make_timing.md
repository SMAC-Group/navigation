# Construct a `timing` object

Construct a `timing` object controlling the timing and frequencies for
navigation, making sure about the consistency and feasibility of
provided information.

## Usage

``` r
make_timing(
  nav.start = NULL,
  nav.end = NULL,
  freq.imu = NULL,
  freq.gps = NULL,
  freq.baro = NULL,
  gps.out.start = NULL,
  gps.out.end = NULL
)
```

## Arguments

- nav.start:

  Time at which navigation starts

- nav.end:

  Time at which navigation ends

- freq.imu:

  Frequency of generated IMU data (and hence that of navigation)

- freq.gps:

  Frequency of generated GPS data

- freq.baro:

  Frequency of generated Baro data

- gps.out.start:

  Time at which GPS outage starts

- gps.out.end:

  Time at which GPS outage ends

## Value

An object of class `timing` containing sensor name and its additive
error model along with the frequency associated to that model

## Author

Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier

## Examples

``` r
timing <- make_timing(
  nav.start = 0,
  nav.end = 50,
  freq.imu = 10,
  freq.gps = 1,
  freq.baro = 1e-5,
  gps.out.start = 25.1,
  gps.out.end = 45
)
```
