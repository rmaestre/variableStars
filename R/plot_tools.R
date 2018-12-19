#' @export
plot_spectrum <- function(min, max, dt) {
  max_amplitude <- dt[which.max(dt$amplitude), ]
  # Plot
  p <- ggplot(aes(frequency, amplitude), data = dt) +
    geom_point(aes(alpha = 0.6)) +
    geom_point(aes(frequency, amplitude, colour = "#009988"),
               data = max_amplitude) +
    #geom_line() +
    geom_bar(aes(alpha=0.6), stat="identity") +
    theme_bw() + ylab("Amplitude") + xlab("Frecuency") +
    ggtitle(paste(
      "First Frequency (F:",
      round(max_amplitude$frequency, 4),
      ", A:",
      round(max_amplitude$amplitude, 4),
      ")",
      sep = ""
    )) +
    xlim(min, max) +
    theme(legend.position = "none")
  return(p)
}


#' @export
plot_apodization <- function(dt) {
  p <- ggplot(aes(x = frequences, y = amplitude),
              data = dt) +
    geom_point() +
    geom_line() +
    ggtitle("Apodization- Frequences and amplitudes") +
    theme_bw()
  return(p)
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
plot_periodicities <- function(dt) {
  # Plot frecuency and amplitude
  ggplot(aes(
    x = fInv,
    y = b,
    group = label,
    colour = label
  ), data = dt) +
    #geom_point(alpha=0.2) +
    geom_line(alpha = 0.8) +
    ggtitle(expression(paste("Periodicities (", d ^ -1, ")"))) +
    xlab(expression(paste("Periodicities (", mu, "hz)"))) +
    ylab("Amplitude") +
    theme_bw() +
    scale_color_lancet()
}


#' @export
plot_echelle <- function(dt) {
  # colour palette
  rf <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
  r <- rf(32)
  # plot
  ggplot(aes(x = x, y = y), data = dt) +
    stat_density2d(
      geom = "raster",
      aes(fill = ..density..),
      n = 200,
      #h = 10,
      contour = FALSE
    ) +
    geom_point(aes(size=h, color=h), shape="+") + 
    theme_bw() +
    scale_fill_gradientn(colours = r) +
    ggtitle("Echelle diagram") +
    xlab(expression(paste("Frequencies mod ", Delta, nu))) +
    ylab("Frequencies") +
    scale_color_gradientn(colours = r)
}
