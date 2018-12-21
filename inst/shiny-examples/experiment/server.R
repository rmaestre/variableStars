library(variableStars)
library(data.table)
library(ggplot2)
library(ggsci)

server <- function(input, output, session) {
  globals <-
    reactiveValues(mainResult = NULL, processedDatasets = list())
  
  # Read data from input
  filedata <- function(){
    # Create number of frecuences
    n <- round(input$numFreqs / 2)
    dis <- input$distance
    # Create data.frame
    dt <- data.frame("x"=seq(from=0, to=2*n*dis, by=dis), "y"=input$baseAMplitudeFirst)
    dt$pos <- seq(1,nrow(dt)) # Secucencial integers
    # Even elements with other amplitude value
    dt[lapply(dt$pos, "%%", 2) == 0,]$y <- input$baseAMplitudeSecond
    # Apply random 
    set.seed(input$seed)
    dt$y <- abs(rnorm(nrow(dt), dt$y, input$ampRandRange))
    dt$pos <- NULL
    dt
  }

  
  # -----------------------------------------------------------
  main_process <- function() {
    return(
      process(
        t(filedata()[c("x")]),
        t(filedata()[c("y")]),
        filter = input$apodization,
        gRegimen = input$gRegimen,
        minDnu = input$minDnu,
        maxDnu = input$maxDnu,
        dnuValue = input$dnuValue,
        dnuGuessError = input$dnuGuessError,
        dnuEstimation = input$dnuEstimation,
        numFrequencies = input$numFrequencies,
        debug = input$debug
      )
    )
  }
  
  write_csv <- function(dt, fileName) {
    tmpdir <- tempdir()
    setwd(tempdir())
    write.table(dt,
                fileName,
                quote = F,
                row.names = F,
                sep = " ")
  }
  
  # --------------------------------------------------------
  # Create a zip to download all processed dataset that are
  # already saved into globals$processedDatasets
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("output", "zip", sep = ".")
    },
    content = function(fname) {
      # ---------------------------------------------------
      # Save all processed datasets
      fs <- c()
      for (name in names(globals$processedDatasets)) {
        completeName <- paste0(name, ".csv")
        write_csv(globals$processedDatasets[[name]], completeName)
        fs <- c(fs, completeName)
      }
      # Compress all files and force download
      zip(zipfile = fname, files = fs)
    },
    contentType = "application/zip"
  )
  
  
  # --------------------------------------------------------
  # Process for main process
  spectrum <- eventReactive(input$process, {
    # All process is only computed once
    globals$mainResult <- main_process()
    # ------------------------------
    dt <- data.frame(filedata()[c("x")],
                     filedata()[c("y")])
    colnames(dt) <- c("frequency", "amplitude")
    # Save global dataset
    globals$processedDatasets[["spectrum"]] = dt
    # Return plot
    plot_spectrum(min(dt$frequency), max(dt$frequency), dt)
  })
  apodization <- eventReactive(input$process, {
    dt <- data.frame(
      "frequences" = globals$mainResult$apodization$frequences,
      "amplitude" = globals$mainResult$apodization$amp
    )
    # Save global dataset
    globals$processedDatasets[["apodization"]] = dt
    # Return plot
    plot_apodization(dt)
  })
  periodicities <- eventReactive(input$process, {
    dt <- prepare_periodicities_dataset(globals$mainResult$fresAmps)
    # Save global dataset
    globals$processedDatasets[["periodicities"]] = dt
    plot_periodicities(dt)
  })
  echelle <- eventReactive(input$process, {
    dt <- data.frame(
      "x" = globals$mainResult$echelle$modDnuStacked,
      "y" = globals$mainResult$echelle$freMas,
      "h" = globals$mainResult$echelle$amplitudes
    )
    # Save global dataset
    globals$processedDatasets[["echelle"]] = dt
    plot_echelle(dt)
  })
  histogramDiff <- eventReactive(input$process, {
    dt <- data.frame(globals$mainResult$diffHistogram$histogram)
    # Save global dataset
    globals$processedDatasets[["histogramDiff"]] = dt
    # Return plot
    ggplot(aes(x = bins, y = values), data = dt) +
      geom_bar(stat = "identity") +
      ggtitle("Histogram of differences") +
      theme_bw()
  })
  autocorrelation <- eventReactive(input$process, {
    dt <- data.frame(globals$mainResult$crossCorrelation)
    # Save global dataset
    globals$processedDatasets[["autocorrelation"]] = dt
    ggplot(aes(x = index, y = autocorre), data = dt) +
      geom_line(stat = "identity") +
      ggtitle("Autocorrelacion (Crosscorrelation)") +
      xlab(expression(paste("Periodicities (", mu, "hz)"))) +
      ylab("Autocorrelation") +
      ylim(c(-0.1, 0.25)) +
      theme_bw()
  })
  
  
  output$plotSpectrum = renderPlot({
    spectrum()
  })
  output$plotApodization = renderPlot({
    apodization()
  })
  output$plotPeriodicities = renderPlot({
    periodicities()
  })
  output$plotEchelle = renderPlot({
    echelle()
  })
  output$plotHistogramDiff = renderPlot({
    histogramDiff()
  })
  output$plotAutocorrelation = renderPlot({
    autocorrelation()
  })
  
  
}