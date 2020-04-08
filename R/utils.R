#' @export
find_peaks <- function(x) {
  # Diff series
  xd <- diff(x)
  # DS to save peaks
  peaks <- c()
  last_sign <- sign(xd[1])
  # Loop over diff values
  for (i in seq(1:length(xd))) {
    if (last_sign != sign(xd[i]) && sign(xd[i]) < 0) {
      peaks <- c(peaks, i)
    }
    last_sign = sign(xd[i])
  }
  peaks # return peaks index
}

#' @export
prepare_periodicities_dataset <- function(list) {
  # DS to save all data
  dt <-
    setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("fInv", "b", "label"))
  # Prepare data
  ranges <- names(list)
  for (range in ranges) {
    dt <- rbind(dt,
                data.frame(
                  "fInv" = list[[as.character(range)]][["fInv"]],
                  "b" = list[[as.character(range)]][["b"]],
                  "label" = paste(list[[as.character(range)]][["label"]], " freqs")
                ))
  }
  dt
}

#' @export
prepare_echelles_dataset <- function(list) {
  # DS to save all data
  dt <-
    setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("x", "y", "label"))
  # Prepare data
  ranges <- names(list)
  for (range in ranges) {
    dt <- rbind(dt,
                data.frame(
                  "x" = list[[as.character(range)]][["modDnuStacked"]],
                  "y" = list[[as.character(range)]][["freMas"]],
                  "h" = list[[as.character(range)]][["amplitudes"]],
                  "label" = paste(range, " freqs")
                ))
  }
  dt
}

#' @export
getRhoFromDnu <- function(Dnu, e_Dnu = 0) {
  # Function to calculate rho or Dnu from the formula by
  # Garcia Hernandez et al. (2017) for Delta Scuti stars:
  # rho = 1.50(0.09,-0.1)*(Dnu/Dnusun)^2.04(0.04,-0.04)
  # Constants
  rho_sun <- 1.409
  dnu_sun = 134.8
  a = 1.501
  e_a = 0.096
  b = 2.0373
  e_b = 0.0405
  # Calculate rho
  rho = (a * (Dnu / dnu_sun) ** b) * rho_sun
  e_rho = rho * sqrt((e_a / a) ** 2 + (b * e_Dnu / (Dnu * dnu_sun)) ** 2 + (log(Dnu /
                                                                                  dnu_sun) * e_b) ** 2)
  return(list("rho" = rho, "e_rho" = e_rho))
}

#' @export
getDnuFromRho <- function(rho, e_rho = 0) {
  # Function to calculate rho or Dnu from the formula by
  # Garcia Hernandez et al. (2017) for Delta Scuti stars:
  # rho = 1.50(0.09,-0.1)*(Dnu/Dnusun)^2.04(0.04,-0.04)
  # Constants
  rho_sun <- 1.409
  dnu_sun = 134.8
  a = 1.501
  e_a = 0.096
  b = 2.0373
  e_b = 0.0405
  dnu <- (((rho / rho_sun) / a) ** (1 / b)) * dnu_sun
  dnu_err <-
    dnu * sqrt((e_rho / (b * rho)) ** 2 + ((e_a / (a * b))) ** 2 + (log(rho /
                                                                          (a ** rho_sun)) * e_b) ** 2)
  return(list("dnu" = dnu, "dnu_err" = dnu_err))
}
