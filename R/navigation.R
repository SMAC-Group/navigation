#' @title Runs "IMU model evaluation" or "INS-GPS-Baro integrated navigation (sensor fusion)"
#' @description This function performs of the two following main tasks, base on the provided input. If a reference trajectory (\code{traj.ref}) is provided, it generates sensor data (IMU, GPS, Baro) corrupted by additive errors according to \code{snsr.mdl}, and performs navigation using \code{KF.mdl} as the sensor error model within the Kalman filter to evaluate how this particular model performs when navigating.
#' @param traj.ref A \code{trajectory} object (see the documentation for \code{make_trajectory}), serving as the reference trajectory for generating sensor data and evaluating the error in navigation once performed. Only position and attitude data are required/considered, and velocity will be calculated from position.
#' @param timing A \code{timing} object (see the documentation for \code{make_timing}) containing timing information such as start and end of navigation.
#' @param snsr.mdl A \code{sensor} object (see the documentation for \code{make_sensor}) containing additive sensor error model to generate realistic sensor data.
#' @param KF.mdl A \code{sensor} object (see the documentation for \code{make_sensor}) containing additive sensor error model to be used within the Kalman filter for navigation.
#' @param g Gravitational acceleration.
#' @param num.runs Number of times the sensor data generation and navigation is performed (Monte-Carlo simulation).
#' @param results.system The coordinate system (\code{ned}/\code{ellipsoidal}) in which the results are reported (see the documentation for \code{make_trajectory}).
#' @param x_o Origin of the fixed \code{ned} frame.
#' @param IC Initial conditions. See the examples for the format.
#' @param imu_data IMU data. See the examples for the format.
#' @param gps_data GPS data. See the examples for the format.
#' @param baro_data Baro data. See the examples for the format.
#' @param input.seed Seed for the random number generator. Actual seed is computed as \code{input.seed * num.runs + run}
#' @param PhiQ_method String that specify the method to compute Phi and Q matrices, can be "exact" or the order of the Taylor expansions to use
#' @param P_subsampling (memory optimization) store only one sample of the P matrix each \code{P_subsampling} time instants
#' @param parallel.ncores The number of cores to be used for parallel Monte-Carlo runs
#' @param tmpdir Where to store temporary navigation output. It should not be mapped on a filesystem which lives in RAM.
#' @param compute_PhiQ_each_n Specify the interval of IMU measurements between each computation of PhiQ.
#' @param noProgressBar A \code{bolean} specifying if there should not be a progress bar
#' @return An object of \code{navigation} class containing the reference trajectory, fused trajectory, sensor data, covariance matrix, and time.
#' @export
#' @import parallel
#' @import pbmcapply
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#'
navigation <- function(traj.ref, timing, snsr.mdl, KF.mdl, g = 9.8056, num.runs = 1, results.system = "ned", x_o = NULL, noProgressBar = FALSE,# Inputs needed for navigation with a reference trajectory
                       IC = NULL, imu_data = NULL, gps_data = NULL, baro_data= NULL, input.seed = 0, PhiQ_method = "exact", P_subsampling = 1, # Inputs needed for sensor fusion only (+ KF.mdl, )
                       compute_PhiQ_each_n = 1, # each n measurment to compute PhiQ
                       parallel.ncores = detectCores(all.tests = FALSE, logical = TRUE), tmpdir = tempdir()) { 
  
  # define out_raw before
  out_raw = NULL
  
  #verify the coordinate system
  if(results.system != "ned") stop('This function currently support only NED coordinates system')
  
  # conver KF model
  
  # convert IMU models to continuous time
  KF.mdl$imu$error_model$acc$X = model_to_ekf(KF.mdl$imu$error_model$acc$X, timing$freq.imu)
  KF.mdl$imu$error_model$acc$Y = model_to_ekf(KF.mdl$imu$error_model$acc$Y, timing$freq.imu)
  KF.mdl$imu$error_model$acc$Z = model_to_ekf(KF.mdl$imu$error_model$acc$Z, timing$freq.imu)
  
  KF.mdl$imu$error_model$gyr$X = model_to_ekf(KF.mdl$imu$error_model$gyr$X, timing$freq.imu)
  KF.mdl$imu$error_model$gyr$Y = model_to_ekf(KF.mdl$imu$error_model$gyr$Y, timing$freq.imu)
  KF.mdl$imu$error_model$gyr$Z = model_to_ekf(KF.mdl$imu$error_model$gyr$Z, timing$freq.imu)
  

  # Check if only sensor fusion is intended
  # (Detailed input check and error handling to be done)
  if (is.null(traj.ref)) {
    traj.calc = INS_GPS_EKF(IC = IC, imu_data = imu_data, gps_data = gps_data, 
                            baro_data = baro_data, KF.mdl = KF.mdl, g = g, PhiQ_method = PhiQ_method, 
                            P_subsampling = P_subsampling, compute_PhiQ_each_n = compute_PhiQ_each_n)

    out = list()
    out$traj.fused = make_trajectory(data = t(rbind(traj.calc$t, traj.calc$X[1:9,])), system = "ned")
    if (results.system == "ellipsoidal") {
      out$traj.fused = X_ned2ellips(out$traj.fused, x_o)
    }
    out$Cov.Nav = traj.calc$P
    out$t = traj.calc$t

    return(out)
  }



  if (traj.ref$system!="ned") {
    stop("Support for non-ned trajectory systems will be added soon.")
  }

  if (!("roll" %in% names(traj.ref$trajectory))) {
    stop("Calculation of attitude based on position will be added soon. For the moment, it has to be provided within traj.ref.")
  }


  # check_traj checks traj for size consistency, pitch staying below pi/2, and regular sampling. Downsamples traj to imu_freq if necessary, sets imu_freq according to traj otherwise. Clips traj according to nav_start and nav_end if provided and consistent, assigns t(1) and t(and) to them otherwise.
  traj.ref = check_traj(traj.ref, timing)
  timing   = traj.ref$timing
  traj.ref = traj.ref$traj


  # completes traj (adds linear and rotational velocities)
  traj.ref = complete_traj(traj.ref)
  
  
  # define the function that would generate data and compute one solution
  do_one <- function (iii) {
    set.seed(input.seed*num.runs + iii)
    # generates IMU data
    imu_gen = gen_snsr_data(snsr.name="imu", traj=traj.ref, timing=timing, snsr.mdl=snsr.mdl$imu, g=g)
    timing = imu_gen$timing
    imu_data = imu_gen$snsr.data
    imu_error = imu_gen$snsr.err
    
    # generates GPS data (for now works only with sensor error model and not with sensor error data)
    gps_gen = gen_snsr_data(snsr.name="gps", traj=traj.ref, timing=timing, snsr.mdl=snsr.mdl$gps)
    timing = gps_gen$timing
    gps_data = gps_gen$snsr.data
    gps_error = gps_gen$snsr.err
    
    # generates Baro data (for now works only with sensor error model and not with sensor error data)
    baro_gen = gen_snsr_data(snsr.name="baro", traj=traj.ref, timing=timing, snsr.mdl=snsr.mdl$baro)
    timing = baro_gen$timing
    baro_data = baro_gen$snsr.data
    baro_error = baro_gen$snsr.err
    
    # Build IC (initial conditions)
    IC = cnstr.IC(traj.ref, KF.mdl)
    
    
    # runs navigation (discard initialization error for now)
    traj.calc = INS_GPS_EKF(IC = IC, imu_data = imu_data, gps_data = gps_data, 
                            baro_data = baro_data, KF.mdl = KF.mdl, g = g, noProgressBar = noProgressBar, 
                            PhiQ_method = PhiQ_method, P_subsampling = P_subsampling, compute_PhiQ_each_n = compute_PhiQ_each_n)
    
    # Post processing
    traj = list()
    traj$calc = traj.calc
    traj$ref = traj.ref
    
    out_raw = list(
      traj = traj,
      imu_data = imu_data,
      imu_error = imu_error,
      gps_data = gps_data,
      gps_error = gps_error,
      baro_data = baro_data,
      baro_error = baro_error
    )
    
    file_name = sprintf("%s/navigation_%05d.RData", tmpdir, iii)
    save(out_raw, file=file_name)
    
    return(file_name)
  }
  
  # evaluate solutions in parallel
  cat("Monte-Carlo runs...\n")
  if (parallel.ncores > 1) {
    out_raw_files = pbmclapply(
      1:num.runs, 
      do_one, 
      mc.cores = parallel.ncores,
      mc.set.seed = FALSE)
  } else {
    out_raw_files = lapply(
      1:num.runs,
      do_one
    )
  }

  out = list("traj.fused"=list(), "data.imu"=list(), "data.gps"=list(), "data.baro"=list(), "Cov.Nav" = list())

  for (iii in 1:num.runs) {
    load(out_raw_files[[iii]])
    
    if (iii == 1) { # the same for all runs
      state_order = 1:dim(out_raw$traj$calc$X)[1]
      state_order[4:9] = c(7:9, 4:6) # swap velocity and orientation
      
      out$t = out_raw$traj$calc$t
      out$t_p = out_raw$traj$calc$t_p
      out$state_names = out_raw$traj$calc$state_names[state_order]  # reordering
      out$Cov_subsampling = P_subsampling
    }
    
    out$traj.ref = traj.ref
    out$traj.fused[[iii]] = make_trajectory(
      data = t(
        rbind(out_raw$traj$calc$t, 
              out_raw$traj$calc$X[state_order[1:9],]) # only navigation states
        ), system = "ned")
    if (results.system == "ellipsoidal") {
      out$traj.fused[[iii]] = X_ned2ellips(out$traj.fused[[iii]], x_o)
      warning("The covariance matrix \'Cov.Nav\' is still associated with \'ned\' system.")
    }
    out$data.imu[[iii]]   = out_raw$imu_data
    out$err.imu[[iii]]   = out_raw$imu_error
    out$data.gps[[iii]]   = out_raw$gps_data
    out$err.gps[[iii]]   = out_raw$gps_error
    out$data.baro[[iii]]  = out_raw$baro_data
    out$err.baro[[iii]]  = out_raw$baro_error
    out$Cov.Nav[[iii]]    = out_raw$traj$calc$P[state_order,state_order,]
    if (dim(out_raw$traj$calc$X)[1] > 9) {
      out$est.imu.states[[iii]] = out_raw$traj$calc$X[10:dim(out_raw$traj$calc$X)[1], ]
    } else {
      out$est.imu.states[[iii]] = matrix(NA, nrow = 0, ncol= dim(out_raw$traj$calc$X)[2])
    }
    out$inn.gps[[iii]] = out_raw$traj$calc$dZgps
    
    rm(out_raw) # clean up some memory
    file.remove(out_raw_files[[iii]]) # remove temporary file
  }

  out = structure(out, class = 'navigation')
  return(out)
}

#' @title TO DO
#' @description TO DO
#' @return TO DO
#' @author Stephane Guerrier, Mehran Khaghani, and Lionel Voirol
#'
#' @noRd
cnstr.IC = function(traj.ref, KF.mdl) {
  
  # X0[1:9] = traj.ref[2:10,1]
  # X0[1:9] = traj.ref$trajectory[colnames(traj$trajectory)==c("x_N","x_E","x_D","v_N","v_E","v_D","roll","pitch","yaw"), 1]
  X0 = c(traj.ref$trajectory$x_N[1],
         traj.ref$trajectory$x_E[1],
         traj.ref$trajectory$x_D[1],
         traj.ref$trajectory$v_N[1],
         traj.ref$trajectory$v_E[1],
         traj.ref$trajectory$v_D[1],
         traj.ref$trajectory$roll[1],
         traj.ref$trajectory$pitch[1],
         traj.ref$trajectory$yaw[1])

  P0 = matrix(0, nrow=9, ncol=9)

  IC = list("X0"=X0, "P0"=P0)
  return(IC)
}
