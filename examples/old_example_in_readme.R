library(navigation)
# example

## Model Evaluation

# First of all, we need to load the reference trajectory and create a `trajectory` object out of it (see `?make_trajectory` for details).

# Reference trajectory
data("lemniscate_traj_ned.RData") # trajectory in proper format as shown bellow
head(lemniscate_traj_ned)
traj = make_trajectory(data = lemniscate_traj_ned, system = "ned")
traj


# Plotting the reference trajectory
plot(traj, n_split = 10)


# Then we  need to make a `timing` object (see `?make_timing` for details) where we specify

# * the start and the end of navigation,
# * the frequencies of different sensors,
# * the beginning and end of the GPS outage period.

# Timing and sampling frequencies
timing = make_timing(nav.start     = 0,
                     nav.end       = 50,
                     freq.imu      = 100,
                     freq.gps      = 1,
                     freq.baro     = .5,
                     gps.out.start = 40,
                     gps.out.end   = 50)

# Now we need to creat the sensor error models for error generation as a list of `sensor` objects (see `?make_sensor` for details). These are the models that will be used in generating the sensor errors, and not the ones necessarilly used within the navigation filter.

# sensor model for data generation
snsr.mdl=list()

imu.freq = 250
acc.mdl = WN(sigma2 = 1.535466e-04) + RW(gamma2 = 1.619511e-10) # DR is not supported anymore + DR(omega = 1.276475e-08)
gyr.mdl = WN(sigma2 = 1.711080e-03) + RW(gamma2 = 1.532765e-13)
snsr.mdl$imu = make_sensor(name="imu", frequency=imu.freq, error_model1=acc.mdl, error_model2=gyr.mdl)

gps.freq = 1
gps.mdl.pos.hor = WN(sigma2 = 2^2)
gps.mdl.pos.ver = WN(sigma2 = 4^2)
gps.mdl.vel.hor = WN(sigma2 = 0.04^2)
gps.mdl.vel.ver = WN(sigma2 = 0.06^2)
snsr.mdl$gps = make_sensor(name="gps", frequency=gps.freq,
                           error_model1=gps.mdl.pos.hor,
                           error_model2=gps.mdl.pos.ver,
                           error_model3=gps.mdl.vel.hor,
                           error_model4=gps.mdl.vel.ver)
baro.freq = 1
baro.mdl = WN(sigma2=0.5^2)
snsr.mdl$baro = make_sensor(name="baro", frequency=baro.freq, error_model1=baro.mdl)

# Then we need to creat the sensor error models for filetring as a list of `sensor` objects (see `?make_sensor` for details). These are the models that will be used within the navigation filter (an extended Kalman filter), which may or mey not be the same as the ones used in generating the sensor errors. In this example, we have chosen them to be the same.

# sensor model for the KF
KF.mdl = list()

KF.mdl$imu  = snsr.mdl$imu
KF.mdl$gps  = snsr.mdl$gps
KF.mdl$baro = snsr.mdl$baro

# Finally, we can call the `navigation` function, which first simulates realistic sensor data based on the reference trajectory and provided sensor error models, and then performs the navigation. The whole process can be done in a Monte-Carlo fassion, by only specifying the number of desired runs as the `num.runs` input to `navigation` function. For detailed documentation, see `?navigation`.
# Running the navigation (Monte-Carlo)
num.runs = 5
res = navigation(traj.ref = traj,
                 timing = timing,
                 snsr.mdl = snsr.mdl,
                 KF.mdl = KF.mdl,
                 num.runs = num.runs,
                 noProgressBar = FALSE)

# Now we can see how the results look like. 

#Plot the results
plot(res, plot3d = F, time_interval = .5, emu_for_covmat = 1,
     emu_to_plot = 1, time_interval_simu = 1, error_analysis = T,
     plot_mean_traj = T, plot_CI = F, nsim = num.runs)

