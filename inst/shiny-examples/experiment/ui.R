library(plotly)

ui <- pageWithSidebar(
  headerPanel('Variable Stars'),
  sidebarPanel(
    actionButton(inputId = 'process', 'Process'),
    h2("Synthetic data generation"),
    numericInput(
      inputId = 'seed',
      'Random seed',
      1234,
      min = 0,
      max = 10000
    ),
    tags$hr(),
    sliderInput("numFreqs", "Frequences number",
                min = 0, max = 100, value = 10
    ),
    numericInput(
      inputId = 'periodF',
      'Period for 1º pattern',
      min = 0, max = 100, value = 1, step = 0.1
    ),
    numericInput(
      inputId = 'periofS',
      'Period for 2º pattern',
      min = 0, max = 100, value = 1, step = 0.1
    ),
    sliderInput("distance", "Distance between patterns",
                min = 0, max = 20, value = 0.5, step = 0.1
    ),
    tags$hr(),
    numericInput(
      inputId = 'baseAMplitudeFirst',
      'Base amplitude 1º pattern',
      10,
      min = 0,
      max = 100,
      step = 0.1
    ),
    numericInput(
      inputId = 'baseAMplitudeSecond',
      'Base amplitude 2º pattern',
      10,
      min = 0,
      max = 100,
      step = 0.1
    ),
    tags$hr(),
    sliderInput("ampRandRange", "Random amplitudes range",
                min = 0, max = 100, value = 0
    ),
    sliderInput("freqOneRandRange", "Random frecuence 1º range",
                min = 0, max = 2, value = 0.0, step = 0.01
    ),
    sliderInput("freqTwoRandRange", "Random frecuence 2º range",
                min = 0, max = 2, value = 0.0, step = 0.01
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
      10,
      min = 0,
      max = 1000
    ),
    checkboxInput(
      inputId = 'debug',
      label = 'In terminal debug information',
      value = TRUE
    ),
    downloadButton("downloadData", "Download experiment data")
  ),
  mainPanel(
    fluidRow(splitLayout(
      cellWidths = c("50%", "50%"),
      plotlyOutput("plotSpectrum"),
      plotOutput("plotApodization")
    )),
    fluidRow(splitLayout(
      cellWidths = c("50%", "50%"),
      plotlyOutput("plotPeriodicities"),
      plotlyOutput("plotEchelle")
    )),
    fluidRow(splitLayout(
      cellWidths = c("50%", "50%"),
      plotlyOutput("plotHistogramDiff"),
      plotlyOutput("plotAutocorrelation")
    ))
  )
)
