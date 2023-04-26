library(navigation)

# Reference trajectory-----------------
data("hexagon_t1_ned") # trajectory in proper format as shown bellow
head(hexagon_t1_ned)
traj = make_trajectory(data = hexagon_t1_ned, system = "ned")

# Timing and sampling frequencies-----------------
timing = make_timing(nav.start     = 0,
                     nav.end       = 150,
                     freq.imu      = 200,
                     freq.gps      = 1,
                     freq.baro     = .5,
                     gps.out.start = 300,
                     gps.out.end   = 300)

# sensor model for data generation----------------
snsr.mdl=list()

# GMWM fit of MTIg IMU
imu.freq = 200

acc.mdl = WN(sigma2 = 5.989778e-05) + AR1(phi = 9.982454e-01, sigma2 = 1.848297e-10) + AR1(phi = 9.999121e-01, sigma2 = 2.435414e-11) + AR1(phi = 9.999998e-01, sigma2 = 1.026718e-12)
gyr.mdl = WN(sigma2 = 1.503793e-06) + AR1(phi = 9.968999e-01, sigma2 = 2.428980e-11) + AR1(phi = 9.999001e-01, sigma2 = 1.238142e-12)

snsr.mdl$imu = make_sensor(name="imu", frequency=imu.freq, error_model1=acc.mdl, error_model2=gyr.mdl)

gps.freq = 1
gps.mdl.pos.hor = WN(sigma2 = 0.025^2)
gps.mdl.pos.ver = WN(sigma2 = 0.05^2)
gps.mdl.vel.hor = WN(sigma2 = 0.01^2)
gps.mdl.vel.ver = WN(sigma2 = 0.02^2)
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

KF.mdl$imu  = snsr.mdl$imu
KF.mdl$gps  = snsr.mdl$gps
KF.mdl$baro = snsr.mdl$baro

# Running the navigation (Monte-Carlo)----------------------------
num.runs = 4
res = navigation(traj.ref = traj,
                 timing = timing,
                 snsr.mdl = snsr.mdl,
                 KF.mdl = KF.mdl,
                 num.runs = num.runs,
                 noProgressBar = TRUE,
                 printMaxErrors = FALSE,
                 PhiQ_method=2,
                 parallel.ncores=4,
                 P_subsampling = 100)

plot(res, plot3d = F, time_interval = .5, emu_for_covmat = 1,
     emu_to_plot = 1, time_interval_simu = 1, error_analysis = F,
     plot_mean_traj = T, plot_CI = F, nsim = num.runs)


plot_imu_err_with_cov(res, error=F)
plot_nav_states_with_cov(res,error=T)

nees = compute_nees(res, idx=1:6, step = 100)
plot_nees(list(nees), num.runs, 6)

coverage = compute_coverage(res, alpha = 0.7, step = 100, idx = 1:6)
plot_coverage(list(coverage), alpha = 0.7, nruns = num.runs)
