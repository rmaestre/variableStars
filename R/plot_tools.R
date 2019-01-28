#' @export
plot_spectrum <- function(min, max, dt) {
  max_amplitude <- dt[which.max(dt$amplitude),]
  # Plot
  p <- ggplot(aes(frequency, amplitude), data = dt) +
    geom_point(aes(alpha = 0.6)) +
    geom_point(aes(frequency, amplitude, colour = "#009988"),
               data = max_amplitude) +
    #geom_line() +
    geom_bar(aes(alpha = 0.6), stat = "identity") +
    ylab("Amplitude") + xlab("Frecuency") +
    ggtitle(paste(
      "First Frequency (F:",
      round(max_amplitude$frequency, 4),
      ", A:",
      round(max_amplitude$amplitude, 4),
      ")",
      sep = ""
    )) +
    theme_bw() +
    theme(
      legend.position = "none",
      text = element_text(size = 20),
      plot.title = element_text(size = rel(0.8), face = "bold"),
      axis.title.x = element_text(size = rel(0.8)),
      axis.title.y = element_text(size = rel(0.8))
    )
  return(p)
}


#' @export
plot_apodization <- function(dt) {
  p <- ggplot(aes(x = frequences, y = amplitude),
              data = dt) +
    geom_point() +
    geom_line() +
    ggtitle("Apodization") +
    xlab("Frequencies") +
    ylab("Normalized amplitudes") +
    theme_bw() +
    theme(
      text = element_text(size = 20),
      plot.title = element_text(size = rel(0.8), face = "bold"),
      axis.title.x = element_text(size = rel(0.8)),
      axis.title.y = element_text(size = rel(0.8))
    )
  return(p)
}

#' @export
plot_periodicities <- function(dt) {
  # # Calculate maxs
  # inds <- aggregate(b ~ label, data = dt, max)
  # maxs <- dt[dt$b %in% inds$b,]
  # # Plot frecuency and amplitude
  # p <- ggplot(aes(
  #   x = fInv,
  #   y = b,
  #   group = label,
  #   colour = label
  # ), data = dt) +
  #   #geom_point(alpha=0.2) +
  #   geom_line(alpha = 0.8) +
  #   geom_vline(
  #     data = maxs,
  #     mapping = aes(xintercept = fInv, colour = label),
  #     linetype = 2
  #   ) +
  #
  #   annotate(geom = "text",
  #     x = maxs$fInv, y = maxs$b, label=maxs$fInv
  #   ) +
  #
  #   annotate(geom = "text",
  #            x = maxs$fInv, y = maxs$b, label=maxs$fInv
  #   ) +
  #
  #   ggtitle("Periodicities") +
  #   xlab(expression(paste("Periodicities (", mu, "hz)"))) +
  #   ylab("Amplitude") +
  #   theme_bw() +
  #   theme(
  #     plot.margin = unit(c(1, 1, 4, 1), "lines"),
  #     axis.title.x = element_blank(),
  #     axis.text.x = element_blank(),
  #     panel.grid.major.x = element_blank(),
  #     panel.grid.minor.x = element_blank()
  #   ) +
  #   scale_color_lancet() +
  #   scale_x_continuous(breaks = round(seq(
  #     from = 0, to = 100, by = 25
  #   )))
  # g2 <- ggplot_gtable(ggplot_build(p))
  # g2$layout$clip[g2$layout$name == "panel"] <- "off"
  # grid::grid.draw(g2)
  # return(p)
  #
  plot_ly(
    dt,
    x = ~ fInv,
    y = ~ b,
    type = 'scatter',
    mode = 'lines',
    color = ~ label
  ) %>%
    layout(
      xaxis = list(title = "Periodicitie (mHz)"),
      yaxis = list(title = "Amplitude")
    ) %>%
    config(mathjax = 'cdn')
}


#' @export
plot_echelle <- function(dt, dnu, dnuD) {
  plot_ly(
    dt,
    x = ~ x,
    y = ~ y,
    type = 'scatter',
    mode = 'markers',
    color = ~ h,
    symbol = ~ label,
    opacity = 0.5,
    marker = list(size = 15)
  ) %>%
    layout(
      xaxis = list(title = "Frequencies mod", range = c(0, 1)),
      yaxis = list(title = "Frequencies")
    ) %>%
    config(mathjax = 'cdn')
}

#' @export
plot_histogram <- function(dt) {
  plot_ly(dt,
          name = 'Histogram of differences') %>%
    add_bars(x = ~ bins,
             y = ~ values,
             width = 2,
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(8,48,107)',
                                       width = 1.5))) %>%
    layout(
      xaxis = list(title = "Differences (mHz)"),
      yaxis = list(title = "Frequencies")
    )
}

#' @export
plot_crosscorrelation <- function(dt) {
  plot_ly(
    dt,
    x = ~ index,
    y = ~ autocorre,
    type = 'scatter',
    mode = 'lines'
  ) %>%
    layout(
      xaxis = list(title = "Frequencies (mHz)"),
      yaxis = list(title = "Crosscorrelation")
    ) %>%
    config(mathjax = 'cdn')
}