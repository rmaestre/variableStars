#' @export
plot_spectrum <- function(min, max, dt) {
  dt$pattern <- paste("pattern ", dt$pattern, sep = "")
  p <-
    plot_ly(
      dt,
      x = ~ frequency,
      y = ~ amplitude,
      name = 'signal',
      type = 'scatter',
      mode = 'lines'
    ) %>%
    add_trace(
      symbol = ~ pattern,
      name = ~ pattern,
      color = ~ pattern,
      type = 'scatter',
      mode = 'markers',
      marker = list(size = 15)
    )  %>%
    layout(xaxis = list(title = "Frequency"),
           yaxis = list(title = "Amplitude"),
           title = "Frequencies and amplitudes")
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
      yaxis = list(title = "Amplitude",
                   title = "Periodicities")
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
      yaxis = list(title = "Frequencies",
                   title = "Echelle")
    ) %>%
    config(mathjax = 'cdn')
}

#' @export
plot_histogram <- function(dt) {
  plot_ly(dt,
          name = 'Histogram of differences') %>%
    add_bars(
      x = ~ bins,
      y = ~ values,
      width = 2,
      marker = list(
        color = 'rgb(158,202,225)',
        line = list(color = 'rgb(8,48,107)',
                    width = 1.5)
      )
    ) %>%
    layout(
      xaxis = list(title = "Differences (mHz)"),
      yaxis = list(title = "Frequencies"),
      title = "Differences Histogram"
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
      yaxis = list(title = "Crosscorrelation"),
      title = "Crosscorrelation"
    ) %>%
    config(mathjax = 'cdn')
}












#' @export
plot_spectrum_ggplot <- function(min, max, dt) {
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
plot_histogram_ggplot <- function(dt) {
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
plot_crosscorrelation_ggplot <- function(dt) {
  p <- ggplot(aes(x = index, y = autocorre), data = dt) +
    geom_line(stat = "identity") +
    ggtitle("Autocorrelacion (Crosscorrelation)") +
    xlab(expression(paste("Periodicities (", mu, "hz)"))) +
    ylab("Autocorrelation") +
    theme_bw()
  return(p)
}

#' @export
plot_apodization_ggplot <- function(dt) {
  p <- ggplot(aes(x = frequences, y = amplitude),
              data = dt) +
    geom_point() +
    geom_line() +
    ggtitle("Apodization- Frequences and amplitudes") +
    theme_bw()
  return(p)
}

#' @export
plot_periodicities_ggplot <- function(dt) {
  # Calculate maxs
  inds <- aggregate(b~label, data = dt, max)
  maxs <- dt[dt$b %in% inds$b,]
  # Plot frecuency and amplitude
  ggplot(aes(
    x = fInv,
    y = b,
    group = label,
    colour = label
  ), data = dt) +
    #geom_point(alpha=0.2) +
    geom_line(alpha = 0.8) +
    geom_vline(data=maxs, 
               mapping=aes(xintercept=fInv, colour=label), linetype=2) +
    ggtitle(expression(paste("Periodicities (", d ^ -1, ")"))) +
    xlab(expression(paste("Periodicities (", mu, "hz)"))) +
    ylab("Amplitude") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_color_lancet() +
    scale_x_continuous(breaks=round(c(maxs$fInv,seq(0,max(dt$fInv),10)), 1))
}


#' @export
plot_echelle_ggplot <- function(dt) {
  # colour palette
  library(RColorBrewer)
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