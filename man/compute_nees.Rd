% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{compute_nees}
\alias{compute_nees}
\title{Compute Normalized Estimation Errror Squared (NEES)}
\usage{
compute_nees(sols, step = 50, idx = 1:6)
}
\arguments{
\item{sols}{The set of solutions returned by the \code{navigation} function}

\item{step}{do it for one sample out of \code{step}}

\item{idx}{Components of the states to be considered (default: position and orientation)}
}
\description{
Compute Normalized Estimation Errror Squared (NEES)
}
\author{
Stephane Guerrier, Mehran Khaghani, Lionel Voirol and Davide A. Cucci
}
