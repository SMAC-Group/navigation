#' @title stochastic model conversion for the EFK
#' @description This function converts a discrete time \code{ts.model} to the
#' equivalent continuous time representation, using \code{model_disc_to_cont},
#' and adds a \code{WN(sigma2 = 0)} to the model if this is not present in the
#' original one
#' @param model The model to be converted.
#' @param frequency The frequency at which this model is supposed to be sampled in Hz.
#' @return a \code{ts.model} suitable to be used in \code{INS_GPS_EKF}.
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#' @noRd
model_to_ekf <- function(model, frequency) {
  if (!inherits(model, "ts.model")) {
    stop("model must be of class ts.model")
  }

  model <- model_disc_to_cont(model, frequency)

  # ensure that we have a white noise
  i_wn <- match("WN", model$process.desc)
  if (is.na(i_wn)) {
    model <- WN(sigma2 = 0) + model
  }

  return(model)
}

#' @title stochastic model conversion from discrete to continuous time units
#' @description This function converts a discrete time \code{ts.model} to the
#' equivalent continuous time representation, suitable to be employed
#' in \code{INS_GPS_EKF}
#' @param model The model to be converted.
#' @param frequency The frequency of the original data used to fit the model.
#' @return a \code{ts.model} suitable to be used in \code{INS_GPS_EKF}.
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#'
#' @importFrom simts AR1 WN RW QN DR GM
#' @noRd
model_disc_to_cont <- function(model, frequency) {
  if (!inherits(model, "ts.model")) {
    stop("model must be of class ts.model")
  }

  single_models <- list()
  n_mdls <- 1

  th_i <- 1
  for (i in seq_along(model$desc)) {
    if (model$desc[i] == "WN") {
      single_models[[n_mdls]] <- WN(sigma2 = model$theta[th_i] / frequency)
    } else if (model$desc[i] == "AR1") {
      ar1_phi <- model$theta[th_i]
      ar1_sigma2 <- model$theta[th_i + 1]

      beta <- -log(ar1_phi) * frequency
      psd <- 2 * beta * ar1_sigma2 / (1 - exp(-2 * beta / frequency))

      single_models[[n_mdls]] <- GM(beta = beta, sigma2_gm = psd)
    } else if (model$desc[i] == "RW") {
      single_models[[n_mdls]] <- RW(gamma2 = model$theta[th_i] * frequency)
    } else {
      stop(paste("model", model$desc[i], "unsupported"))
    }

    n_mdls <- n_mdls + 1
    th_i <- th_i + length(model$obj.desc[[i]])
  }

  return(Reduce("+", single_models))
}

#' @title stochastic model conversion from continuous to discrete time units
#' @description This function converts a continuous time \code{ts.model} to the
#' equivalent discrete time representation, e.g., suitable for sampling
#' with \code{simts::gen_gts}
#' @param model The model to be converted.
#' @param frequency The frequency at which this model is supposed to be sampled in Hz.
#' @return a \code{ts.model} suitable to be used in \code{simts::gen_gts}.
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#'
#' @noRd
model_cont_to_disc <- function(model, frequency) {
  if (!inherits(model, "ts.model")) {
    stop("model must be of class ts.model")
  }

  single_models <- list()
  n_mdls <- 1

  th_i <- 1
  for (i in seq_along(model$desc)) {
    if (model$desc[i] == "WN") {
      single_models[[n_mdls]] <- WN(sigma2 = model$theta[th_i] * frequency)
    } else if (model$desc[i] == "GM") {
      gm_beta <- model$theta[th_i]
      gm_psd <- model$theta[th_i + 1]

      phi <- exp(-gm_beta / frequency)
      sigma2 <- gm_psd / gm_beta / 2 * (1 - exp(-2 * gm_beta / frequency))

      single_models[[n_mdls]] <- AR1(phi = phi, sigma2 = sigma2)
    } else if (model$desc[i] == "RW") {
      single_models[[n_mdls]] <- RW(gamma2 = model$theta[th_i] / frequency)
    } else {
      stop(paste("model", model$desc[i], "unsupported"))
    }

    n_mdls <- n_mdls + 1
    th_i <- th_i + length(model$obj.desc[[i]])
  }

  return(Reduce("+", single_models))
}

remove_qn <- function(model) {
  if (!inherits(model, "ts.model")) {
    stop("model must be of class ts.model")
  }

  ret_model <- NA

  th_i <- 1
  for (i in seq_along(model$desc)) {
    to_add <- NA

    if (model$desc[i] == "WN") {
      to_add <- WN(sigma2 = model$theta[match("WN", model$process.desc)])
    } else if (model$desc[i] == "AR1") {
      to_add <- AR1(phi = model$theta[th_i], sigma2 = model$theta[th_i + 1])
    } else if (model$desc[i] == "RW") {
      to_add <- RW(gamma2 = model$theta[th_i])
    } else if (model$desc[i] == "DR") {
      to_add <- DR(omega = model$theta[th_i])
    } else if (model$desc[i] == "QN") {
      # do nothing
    } else {
      stop(paste("model", model$desc[i], "unsupported"))
    }

    if (!is.na(to_add[1])) {
      if (is.na(ret_model)) {
        ret_model <- to_add
      } else {
        ret_model <- ret_model + to_add
      }
    }

    th_i <- th_i + length(model$obj.desc[[i]])
  }

  return(ret_model)
}

#' @title convert smallest phi AR1 to WN
#' @description TODO
#' @param model The model to be converted.
#' @return a \code{ts.model}
#' @author Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
#'
#' @noRd
conv_smallest_phi_ar1_to_wn <- function(model) {
  if (!inherits(model, "ts.model")) {
    stop("model must be of class ts.model")
  }

  i_ar1s <- which(model$process.desc %in% "AR1")
  if (length(i_ar1s) == 0) {
    warning("no AR1 in the given model")
    return(model)
  }

  phis <- model$theta[i_ar1s]
  sigma2s <- model$theta[i_ar1s + 1]

  i_min <- match(min(phis), phis)

  i_wn <- match("WN", model$process.desc)
  if (is.na(i_wn)) {
    wn_model <- WN(sigma2 = sigma2s[i_min] / (1 - phis[i_min]^2))
  } else {
    wn_model <- WN(sigma2 = model$theta[i_wn] + sigma2s[i_min] / (1 - phis[i_min]^2))
  }

  ret_model <- NA
  th_i <- 1
  i_ar <- 1
  for (i in seq_along(model$desc)) {
    th_model <- NA
    if (model$desc[i] != "WN") {
      if (model$desc[i] == "AR1") {
        if (i_ar != i_min) {
          th_model <- AR1(phi = model$theta[th_i], sigma2 = model$theta[th_i + 1])
        }
        i_ar <- i_ar + 1
      } else if (model$desc[i] == "RW") {
        th_model <- RW(gamma2 = model$theta[th_i])
      } else if (model$desc[i] == "QN") {
        th_model <- QN(q2 = model$theta[th_i])
      } else if (model$desc[i] == "DR") {
        th_model <- DR(omega = model$theta[th_i])
      } else {
        stop(paste("model", model$desc[i], "unsupported"))
      }
    }


    if (inherits(th_model, "ts.model")) {
      if (!inherits(ret_model, "ts.model")) {
        ret_model <- th_model
      } else {
        ret_model <- ret_model + th_model
      }
    }

    th_i <- th_i + length(model$obj.desc[[i]])
  }

  # have the WN at the end because of bugs in gmwm
  if (!inherits(ret_model, "ts.model")) {
    ret_model <- wn_model
  } else {
    ret_model <- ret_model + wn_model
  }

  return(ret_model)
}

overbound_smallest_phi_ar1_with_wn <- function(model) {
  if (!inherits(model, "ts.model")) {
    stop("model must be of class ts.model")
  }

  i_ar1s <- which(model$process.desc %in% "AR1")
  if (length(i_ar1s) == 0) {
    warning("no AR1 in the given model")
    return(model)
  }

  phis <- model$theta[i_ar1s]
  sigma2s <- model$theta[i_ar1s + 1]

  i_min <- match(min(phis), phis)

  sigma2_ov <- sigma2s[i_min] / (1 - phis[i_min])^2

  i_wn <- match("WN", model$process.desc)
  if (is.na(i_wn)) {
    wn_model <- WN(sigma2 = sigma2_ov)
  } else {
    wn_model <- WN(sigma2 = model$theta[i_wn] + sigma2_ov)
  }

  ret_model <- NA
  th_i <- 1
  i_ar <- 1
  for (i in seq_along(model$desc)) {
    th_model <- NA
    if (model$desc[i] != "WN") {
      if (model$desc[i] == "AR1") {
        if (i_ar != i_min) {
          th_model <- AR1(phi = model$theta[th_i], sigma2 = model$theta[th_i + 1])
        }
        i_ar <- i_ar + 1
      } else if (model$desc[i] == "RW") {
        th_model <- RW(gamma2 = model$theta[th_i])
      } else if (model$desc[i] == "QN") {
        th_model <- QN(q2 = model$theta[th_i])
      } else if (model$desc[i] == "DR") {
        th_model <- DR(omega = model$theta[th_i])
      } else {
        stop(paste("model", model$desc[i], "unsupported"))
      }
    }

    if (inherits(th_model, "ts.model")) {
      if (!inherits(ret_model, "ts.model")) {
        ret_model <- th_model
      } else {
        ret_model <- ret_model + th_model
      }
    }

    th_i <- th_i + length(model$obj.desc[[i]])
  }

  # have the WN at the end because of bugs in gmwm
  if (!inherits(ret_model, "ts.model")) {
    ret_model <- wn_model
  } else {
    ret_model <- ret_model + wn_model
  }

  return(ret_model)
}
