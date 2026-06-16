# Print trajectory Objects

Pretty formatting for `trajectory` objects.

## Usage

``` r
# S3 method for class 'trajectory'
print(x, obs = 5L, ...)
```

## Arguments

- x:

  A `trajectory` object.

- obs:

  A `integer` the specifies how many from the beginning and end of the
  data set to show.

- ...:

  Further arguments passed to or from other methods.

## Value

Print `trajectory` objects.

## Author

Stephane Guerrier, Mehran Khaghani, and Lionel Voirol

## Examples

``` r
n <- 100
dat <- cbind(
  seq(from = 0, to = 60 * 60, length.out = n),
  46.204391 * pi / 180 + cumsum(rnorm(n)) / 10^5,
  6.143158 * pi / 180 + cumsum(rnorm(n)) / 10^5,
  375 + cumsum(rnorm(n))
)
traj <- make_trajectory(data = dat, name = "My cool data")
traj
#> Data Name: My cool data 
#> 
#> Data preview:
#> 
#>                 time               lat               lon              alt
#>   1                0 0.806418937287848 0.107221249247555  374.78940204542
#>   2 36.3636363636364 0.806416695883202 0.107215058522159 373.884228867694
#>   3 72.7272727272727 0.806414045292529 0.107231556712995 374.252150076295
#>   4 109.090909090909  0.80641256068772 0.107236456938788  373.99714183413
#>   5 145.454545454545 0.806431919605826 0.107238892718648 375.759683755779
#> ---                                                                      
#>  96 3454.54545454545 0.806334273980386 0.107387208107035 357.381470786951
#>  97 3490.90909090909 0.806321696646505  0.10740147910003 357.596868397007
#>  98 3527.27272727273 0.806341260868329  0.10740459699039 356.996712491451
#>  99 3563.63636363636 0.806352317886719 0.107404099912501 359.608309760093
#> 100             3600 0.806330070506908  0.10738326418482 360.645098965827
#> 
```
