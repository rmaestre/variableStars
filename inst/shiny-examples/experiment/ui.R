ui <- pageWithSidebar(
  headerPanel('Variable Stars'),
  sidebarPanel(
    sliderInput("numFreqs", "Frequences number",
                min = 0, max = 1000, value = 10
    ),
    sliderInput("ampRandRange", "Random amplitudes range",
                min = 0, max = 1000, value = 10
    ),
    sliderInput("obs", "Distance between patterns",
                min = 0, max = 10, value = 10
    ),
    tags$hr(),
    h2("Experiment parameters"),
    selectInput(
      inputId = 'apodization',
      'Apodization',
      c(
        "uniform",
        "bartlett",
        "blackman",
        "connes",
        "cosine",
        "gaussian",
        "hamming",
        "hanning",
        "welch"
      ),
      selected = "uniform"
    ),
    numericInput(
      inputId = 'gRegimen',
      'G Regimen',
      0,
      min = 0,
      max = 100
    ),
    numericInput(
      inputId = 'minDnu',
      'Minimum DNU value',
      15,
      min = 0,
      max = 1000
    ),
    numericInput(
      inputId = 'maxDnu',
      'Maximum DNU value',
      15,
      min = 0,
      max = 1000
    ),
    numericInput(
      inputId = 'dnuValue',
      'DNU value',
      -1,
      min = -10,
      max = 1000
    ),
    numericInput(
      inputId = 'dnuGuessError',
      'DNU guess error',
      -1,
      min = -10,
      max = 1000
    ),
    checkboxInput(
      inputId = 'dnuEstimation',
      label = 'DNU estimation',
      value = TRUE
    ),
    numericInput(
      inputId = 'numFrequencies',
      'Number of frecuencies selected',
      30,
      min = 0,
      max = 1000
    ),
    checkboxInput(
      inputId = 'debug',
      label = 'In terminal debug information',
      value = TRUE
    ),
    actionButton(inputId = 'process', 'Process'),
    br(),
    p("Click the button to perfomr the calculations.")
    ,
    downloadButton("downloadData", "Download experiment data")
  ),
  mainPanel(
    h2("Results"),
    fluidRow(splitLayout(
      cellWidths = c("50%", "50%"),
      plotOutput("plotSpectrum"),
      plotOutput("plotApodization")
    )),
    fluidRow(splitLayout(
      cellWidths = c("100%"),
      plotOutput("plotPeriodicities")
    )),
    fluidRow(splitLayout(
      cellWidths = c("50%", "50%"),
      plotOutput("plotHistogramDiff"),
      plotOutput("plotAutocorrelation")
    ))
  )
)
