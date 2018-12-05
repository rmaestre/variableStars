ui <- pageWithSidebar(
  headerPanel('Variable Stars'),
  sidebarPanel(
    h4("Download data examples"),
    helpText(   
      a("HD 174966",     
        href="https://raw.githubusercontent.com/rmaestre/variableStars/master/data/freqs.dat"),
      a("HD 174936",     
        href="https://raw.githubusercontent.com/rmaestre/variableStars/master/data/table1.dat")
      ),
    tags$hr(),
    #Selector for file upload
    h2("Data Input"),
    checkboxInput(
      inputId = 'header',
      label = 'Header',
      value = TRUE
    ),
    radioButtons(
      inputId = 'sep',
      label = 'Separator',
      choices = c(
        Comma = ',' ,
        Semicolon = ';' ,
        Tab = '\t',
        Space = ''
      ),
      selected = ','
    ),
    fileInput(
      inputId = 'datafile',
      'Choose CSV file with frecuencies and amplitudes',
      accept = c('text/csv', 'text/comma-separated-values,text/plain')
    ),
    tags$hr(),
    h2("Columns selector"),
    uiOutput("frecuencyVar"),
    uiOutput("amplitudeVar"),
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
    h2("Data input"),
    h6("Only 5 first lines are showed"),
    tableOutput("filetable"),
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
