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