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
    theme(legend.position = "none") +
    theme(text = element_text(size=20))
  return(p)
}


#' @export
plot_apodization <- function(dt) {
  p <- ggplot(aes(x = frequences, y = amplitude),
              data = dt) +
    geom_point() +
    geom_line() +
    ggtitle("Apodization- Frequences and amplitudes") +
    theme_bw() +
    theme(text = element_text(size=20))
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
plot_periodicities <- function(dt) {
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
    scale_x_continuous(breaks=round(c(maxs$fInv,seq(0,max(dt$fInv),10)), 1)) +
    theme(text = element_text(size=20))
}


#' @export
plot_echelle <- function(dt, dnu, dnuD) {
  # colour palette
  library(RColorBrewer)
  rf <- colorRampPalette(rev(brewer.pal(11, 'Spectral')))
  r <- rf(32)
  # plot
  dt$label <- as.factor(dt$label)
  ggplot(aes(x = x, y = y), data = dt) +
    #stat_density2d(
    #  geom = "raster",
    #  aes(fill = ..density..),
    #  n = 200,
    #  #h = 10,
    #  contour = FALSE
    #) +
    geom_point(aes(size=h, shape=label, colour=label, alpha=0.6)) + 
    theme_bw() +
    #scale_fill_gradientn(colours = r) +
    ggtitle("Echelle diagram") +
    xlab(substitute(paste(title," ",Delta,nu," (",dnu," ",mu,"Hz =",dnuD," ",d^-1,")", sep=" "), 
                    list(title="Frequencies mod", dnu=dnu, dnuD=dnuD))) +
    ylab("Frequencies") +
    #scale_color_gradientn(colours = r)  +
    theme(text = element_text(size=20)) +
    scale_colour_manual(name = "label",
                        values = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")) +
    xlim(0, 1)
}

#' @export
plot_histogram <- function(dt) {
  ggplot(aes(x = bins, y = values), data = dt) +
    geom_bar(stat = "identity") +
    ggtitle("Histogram of differences") +
    theme_bw() +
    theme(text = element_text(size=20))
}

#' @export
plot_crosscorrelation <- function(dt) {
  ggplot(aes(x = index, y = autocorre), data = dt) +
    geom_line(stat = "identity") +
    ggtitle("Autocorrelacion (Crosscorrelation)") +
    xlab(expression(paste("Periodicities (", mu, "hz)"))) +
    ylab("Autocorrelation") +
    theme_bw() +
    theme(text = element_text(size=20))
}

