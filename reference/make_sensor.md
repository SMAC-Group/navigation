# Construct a `sensor` object

Construct a `sensor` object for IMU, GPS, and Baro from error model of
class `ts.model`

## Usage

``` r
make_sensor(
  name,
  frequency = 1,
  error_model1 = NULL,
  error_model2 = NULL,
  error_model3 = NULL,
  error_model4 = NULL,
  error_data1 = NULL
)
```

## Arguments

- name:

  Name of the sensor

- frequency:

  Frequency associated with the error model

- error_model1:

  Error model of class `ts.model` for either accelerometer (as part of
  imu), horizontal components of GPS position, or Barometer

- error_model2:

  Error model of class `ts.model` for either gyroscope (as part of imu)
  or vertical component of GPS position

- error_model3:

  Error model of class `ts.model` for horizontal components of GPS
  velocity

- error_model4:

  Error model of class `ts.model` for vertical component of GPS velocity

- error_data1:

  Vector of error observations.

## Value

An object of class `sensor` containing sensor name and its additive
error model along with the frequency associated to that model

## Author

Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier

## Examples

``` r
# IMU:
imu.freq <- 250
acc.mdl <- WN(sigma2 = 1.535466e-04) + RW(gamma2 = 1.619511e-10)
gyr.mdl <- WN(sigma2 = 1.711080e-03) + RW(gamma2 = 1.532765e-13)
imu.mdl <- make_sensor(
  name = "imu",
  frequency = imu.freq,
  error_model1 = acc.mdl,
  error_model2 = gyr.mdl
)

# GPS:
gps.freq <- 1
gps.mdl.pos.hor <- WN(sigma2 = 2^2)
gps.mdl.pos.ver <- WN(sigma2 = 4^2)
gps.mdl.vel.hor <- WN(sigma2 = 0.04^2)
gps.mdl.vel.ver <- WN(sigma2 = 0.06^2)
gps.mdl <- make_sensor(
  name = "gps", frequency = gps.freq,
  error_model1 = gps.mdl.pos.hor,
  error_model2 = gps.mdl.pos.ver,
  error_model3 = gps.mdl.vel.hor,
  error_model4 = gps.mdl.vel.ver
)

# Baro:
baro.freq <- 1
baro.mdl <- WN(sigma2 = 0.5^2)
baro.mdl <- make_sensor(
  name = "baro",
  frequency = baro.freq,
  error_model1 = baro.mdl
)
```
