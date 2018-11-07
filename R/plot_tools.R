#' @export
plot_spectrum <- function(min, max, dt) {
  max_amplitude <- dt[which.max(dt$amplitude), ]
  # Plot
  ggplot(aes(frequency, amplitude), data = dt) +
    geom_point(aes(alpha = 0.6)) +
    geom_point(aes(frequency, amplitude, colour = "#009988"),
               data = max_amplitude) +
    geom_line() +
    theme_bw() + ylab("Amplitude") + xlab("Frecuency") +
    ggtitle(paste(
      "First Frecuency (F:",
      round(max_amplitude$frequency, 4),
      ", A:",
      round(max_amplitude$amplitude, 4),
      ")",
      sep = ""
    )) +
    xlim(min, max) +
    theme(legend.position = "none")
}
