#' @export
find_peaks <- function(x) {
  # Diff series
  xd <- diff(x)
  # DS to save peaks
  peaks <- c()
  last_sign <- sign(xd[1])
  # Loop over diff values
  for (i in seq(1:length(xd))) {
    if (last_sign != sign(xd[i]) && sign(xd[i])<0) {
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