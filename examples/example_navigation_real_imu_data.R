library(navigation)

# Reference trajectory-----------------
data("lemniscate_traj_ned") # trajectory in proper format as shown bellow
head(lemniscate_traj_ned)
traj = make_trajectory(data = lemniscate_traj_ned, system = "ned")

# Timing and sampling frequencies-----------------
timing = make_timing(nav.start     = 0,
                     nav.end       = 300,
                     freq.imu      = 100,
                     freq.gps      = 1,
                     freq.baro     = .5,
                     gps.out.start = 300,
                     gps.out.end   = 300)

# real data from MTI-g IMU
# 6 independent acquisitions lasting approximately 3 hours
library(imudata)
data("mtig100hrz")

# create a simts::imu object with data from the i-th acquisition in the mtig100hrz dataset
i_acq = 1
mtig_imu = simts::imu(
  data = cbind(
    mtig100hrz[[4]][[i_acq]]$data, 
    mtig100hrz[[5]][[i_acq]]$data,
    mtig100hrz[[6]][[i_acq]]$data,
    mtig100hrz[[1]][[i_acq]]$data,
    mtig100hrz[[2]][[i_acq]]$data,
    mtig100hrz[[3]][[i_acq]]$data),
  gyros = c(1,2,3),
  accels = c(4,5,6),
  axis = c('X', 'Y', 'Z', 'X', 'Y', 'Z'),
  freq = 100,
  name = "mti-g")

imu.freq = 100

# sensor model for data generation----------------
snsr.mdl=list()
snsr.mdl$imu = make_sensor(name="imu", frequency = imu.freq, error_data1 = mtig_imu)

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

# sensor model for the KF------------------------
KF.mdl = list()

acc.mdl = WN(sigma2 = 5.989778e-05) + AR1(phi = 9.982454e-01, sigma2 = 1.848297e-10) + AR1(phi = 9.999121e-01, sigma2 = 2.435414e-11) + AR1(phi = 9.999998e-01, sigma2 = 1.026718e-12)
gyr.mdl = WN(sigma2 = 1.503793e-06) + AR1(phi = 9.968999e-01, sigma2 = 2.428980e-11) + AR1(phi = 9.999001e-01, sigma2 = 1.238142e-12)

KF.mdl$imu  = make_sensor(name="imu", frequency=imu.freq, error_model1=acc.mdl, error_model2=gyr.mdl)
KF.mdl$gps  = snsr.mdl$gps
KF.mdl$baro = snsr.mdl$baro

# Running the navigation (Monte-Carlo)----------------------------
num.runs = 36
res = navigation(traj.ref = traj,
                 timing = timing,
                 snsr.mdl = snsr.mdl,
                 KF.mdl = KF.mdl,
                 num.runs = num.runs,
                 noProgressBar = FALSE,
                 printMaxErrors = FALSE,
                 PhiQ_method=2,
                 parallel.ncores=8,
                 P_subsampling = 100)

plot(res, plot3d = F, time_interval = .5, emu_for_covmat = 1,
     emu_to_plot = 1, time_interval_simu = 1, error_analysis = T,
     plot_mean_traj = T, plot_CI = F, nsim = num.runs)

nees = compute_nees(res, idx=1:6, step = 100)
plot_nees(list(nees), num.runs, 6)

coverage = compute_coverage(res, alpha = 0.7, step = 100, idx = 1:6)
plot_coverage(list(coverage), alpha = 0.7, nruns = num.runs)
