# Print a `sensor` object parameters (name, frequency and error model)

Print method for a `sensor` object

## Usage

``` r
# S3 method for class 'sensor'
print(x, ...)
```

## Arguments

- x:

  A `sensor` object.

- ...:

  Further arguments passed to or from other methods.

## Value

Print the `sensor` object name and specifications in the console.

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
print(imu.mdl)
#> $name
#> [1] "imu"
#> 
#> $frequency
#> [1] 250
#> 
#> $error_model
#> $error_model$acc
#> $error_model$acc$X
#> 
#> Guess Starting Values: FALSE 
#>  Terms Initial.Values
#>     WN   1.535466e-04
#>     RW   1.619511e-10
#> The model will be initiated using the initial values you supplied.
#> 
#> $error_model$acc$Y
#> 
#> Guess Starting Values: FALSE 
#>  Terms Initial.Values
#>     WN   1.535466e-04
#>     RW   1.619511e-10
#> The model will be initiated using the initial values you supplied.
#> 
#> $error_model$acc$Z
#> 
#> Guess Starting Values: FALSE 
#>  Terms Initial.Values
#>     WN   1.535466e-04
#>     RW   1.619511e-10
#> The model will be initiated using the initial values you supplied.
#> 
#> 
#> $error_model$gyr
#> $error_model$gyr$X
#> 
#> Guess Starting Values: FALSE 
#>  Terms Initial.Values
#>     WN   1.711080e-03
#>     RW   1.532765e-13
#> The model will be initiated using the initial values you supplied.
#> 
#> $error_model$gyr$Y
#> 
#> Guess Starting Values: FALSE 
#>  Terms Initial.Values
#>     WN   1.711080e-03
#>     RW   1.532765e-13
#> The model will be initiated using the initial values you supplied.
#> 
#> $error_model$gyr$Z
#> 
#> Guess Starting Values: FALSE 
#>  Terms Initial.Values
#>     WN   1.711080e-03
#>     RW   1.532765e-13
#> The model will be initiated using the initial values you supplied.
#> 
#> 
#> 
```
