#' @title Print a \code{sensor} object parameters (name, frequency and error model)
#' @description Print method for a \code{sensor} object
#' @return Print the \code{sensor} object name and specifications in the console.
#' @param x A \code{sensor} object.
#' @author Davide Cucci, Lionel Voirol, Mehran Khaghani, Stéphane Guerrier
#'
print.sensor = function(x) {
  cat("Senor name is: ", x$name, "\b.\n\n")
  cat("Sensor frequency is: ", x$frequency, "Hz.\n\n")
  cat("Sensor error model:\n")
  print(x$error_model) # To be modified to print only the relevant information.
}
