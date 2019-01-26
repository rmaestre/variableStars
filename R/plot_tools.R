#' @export
plot_spectrum <- function(min, max, dt) {
  max_amplitude <- dt[which.max(dt$amplitude), ]
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
      layout(xaxis = list(title = "Periodicitie (mHz)"), 
            yaxis = list(title = "Amplitude")) %>%
      config(mathjax = 'cdn')
}


#' @export
plot_echelle <- function(dt, dnu, dnuD) {
  # # colour palette
  # library(RColorBrewer)
  # rf <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
  # r <- rf(32)
  # # plot
  # dt$label <- as.factor(dt$label)
  # p <- ggplot(aes(x = x, y = y), data = dt) +
  #   #stat_density2d(
  #   #  geom = "raster",
  #   #  aes(fill = ..density..),
  #   #  n = 200,
  #   #  #h = 10,
  #   #  contour = FALSE
  #   #) +
  #   geom_point(aes(
  #     size = h,
  #     shape = label,
  #     colour = label,
  #     alpha = 0.6
  #   )) +
  #   theme_bw() +
  #   #scale_fill_gradientn(colours = r) +
  #   ggtitle("Echelle diagram") +
  #   xlab(substitute(
  #     paste(
  #       title,
  #       " ",
  #       Delta,
  #       nu,
  #       " (",
  #       dnu,
  #       " ",
  #       mu,
  #       "Hz =",
  #       dnuD,
  #       " ",
  #       d ^ -1,
  #       ")",
  #       sep = " "
  #     ),
  #     list(
  #       title = "Frequencies mod",
  #       dnu = dnu,
  #       dnuD = dnuD
  #     )
  #   )) +
  #   ylab("Frequencies") +
  #   #scale_color_gradientn(colours = r)  +
  #   theme(
  #     text = element_text(size = 20),
  #     plot.title = element_text(size = rel(0.8), face = "bold"),
  #     axis.title.x = element_text(size = rel(0.8)),
  #     axis.title.y = element_text(size = rel(0.8)),
  #     legend.title = element_text(size = 11),
  #     legend.text = element_text(size = 10)
  #   ) +
  #   scale_colour_manual(
  #     name = "label",
  #     values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
  #   ) +
  #   xlim(0, 1)
  # return(p)
  
  plot_ly(
    dt,
    x = ~ x,
    y = ~ y,
    type = 'scatter',
    mode = 'markers',
    color = ~ h,
    symbol = ~ label,
    opacity = 0.5
  ) %>%
    layout(xaxis = list(title = "Frequencies mod", range = c(0, 1)), 
           yaxis = list(title = "Frequencies")) %>%
    config(mathjax = 'cdn')
}

#' @export
plot_histogram <- function(dt) {
  p <- ggplot(aes(x = bins, y = values), data = dt) +
    geom_bar(stat = "identity") +
    ggtitle("Histogram of differences") +
    xlab(expression(paste("Differences (", mu, "hz)"))) +
    ylab("Frecuency") +
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
plot_crosscorrelation <- function(dt) {
  # p <- ggplot(aes(x = index, y = autocorre), data = dt) +
  #   geom_line(stat = "identity") +
  #   ggtitle("Crosscorrelation") +
  #   xlab(expression(paste("Periodicities (", mu, "hz)"))) +
  #   ylab("Autocorrelation") +
  #   theme_bw() +
  #   theme(
  #     text = element_text(size = 20),
  #     plot.title = element_text(size = rel(0.8), face = "bold"),
  #     axis.title.x = element_text(size = rel(0.8)),
  #     axis.title.y = element_text(size = rel(0.8))
  #   )
  # return(p)
  
  plot_ly(
    dt,
    x = ~ index,
    y = ~ autocorre,
    type = 'scatter',
    mode = 'lines'
  ) %>%
    layout(xaxis = list(title = "Frequencies (mHz)"), 
           yaxis = list(title = "Crosscorrelation")) %>%
    config(mathjax = 'cdn')
}