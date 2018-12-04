library(variableStars)
library(data.table)
library(ggplot2)
library(ggsci)

server <- function(input, output, session) {
  globals <- reactiveValues(cluster = NULL)
  
  # Read data from input
  filedata <- reactive({
    infile <- input$datafile
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    read.csv(infile$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  # This previews the CSV data file
  output$filetable <- renderTable({
    head(filedata(), 3)
  })
  
  # Dynamically generate UI input when data is uploaded ----
  output$frecuencyVar <- renderUI({
    selectInput(
      inputId = "selectedFrecuency",
      label = "Select Frecuency",
      choices = names(filedata())
    )
  })
  output$amplitudeVar <- renderUI({
    selectInput(
      inputId = "selectedAmplitude",
      label = "Select Amplitude",
      choices = names(filedata())
    )
  })
  
  # -----------------------------------------------------------
  main_process <- function() {
    return(
      variableStars::process(
        t(filedata()[c(input$selectedFrecuency)]),
        t(filedata()[c(input$selectedAmplitude)]),
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
  
  spectrum <- eventReactive(input$process, {
    globals$cluster <- main_process()
    dt <- data.frame(filedata()[c(input$selectedFrecuency)],
                     filedata()[c(input$selectedAmplitude)])
    colnames(dt) <- c("frequency", "amplitude")
    plot_spectrum(min(dt$frequency), max(dt$frequency), dt)
    
  })
  apodization <- eventReactive(input$process, {
    plot_apodization(globals$cluster)
  })
  periodicities <- eventReactive(input$process, {
    plot_periodicities(globals$cluster$fresAmps)
  })
  histogramDiff <- eventReactive(input$process, {
    dt <- data.frame(globals$cluster$diffHistogram$histogram)
    ggplot(aes(x = bins, y = values), data = dt) +
      geom_bar(stat = "identity") +
      ggtitle("Histogram of differences") +
      theme_bw()
  })
  autocorrelation <- eventReactive(input$process, {
    dt <- data.frame(globals$cluster$crossCorrelation)
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
  output$plotHistogramDiff = renderPlot({
    histogramDiff()
  })
  output$plotAutocorrelation = renderPlot({
    autocorrelation()
  })
  
  
}