library(shiny)
library(plotly)
library(shinyjs)


  fluidPage(theme = "bootstrap.css",
    headerPanel("Hyp predict"),
    sidebarPanel(
      tags$head(
      tags$style("#plot1{width: 25vw !important;}"),
      tags$style(".progress-bar{background-color: #8C8C8C;}"),
      tags$style(".irs-bar {border-top: 1px solid #2E2E2E;  border-bottom: 1px solid #2E2E2E; background: #8C8C8C}"),
      tags$style(".irs-bar-edge {border: 1px solid #2E2E2E; border-right: 0; background: #8C8C8C}"),
      tags$style(".irs-single {background: #8C8C8C}"),
      tags$style(".irs-grid-pol {background: #2E2E2E;}"),
      tags$style("a {color:  #2E2E2E; font-weight: bold;}"),
      tags$style("a:hover {color:  #2E2E2E; font-weight: bold;}"),
      tags$style(".btn-default:focus  {border-color: #8C8C8C;  box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.075), 0 0 8px rgba(255, 255, 255, 0.6); outline: #FFF;}"),
      tags$style(".btn-default:active  {border-color: #8C8C8C;  box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.075), 0 0 8px rgba(20, 20, 20, 0.6); outline: #FFF;}"),
      tags$style(".btn-default:active:hover  {border-color: #8C8C8C;  box-shadow: inset 0 1px 1px rgba(0, 0, 0, 0.075), 0 0 8px rgba(10, 10, 10, 0.6); outline: #FFF;}")
      ),
      fileInput("file1", "Upload FASTA File",
                accept=c(".fa", ".fas", ".fasta", ".FA", ".FAS", ".FASTA", ".txt")),
      helpText("Hyp predict is a hydroxyproline site prediction algorithm",
               "for secreted plant proteins, based on the", 
               a(href="https://github.com/dmlc/xgboost", "xgboost",target="_blank"),
               "distributed gradient boosting library.",
               "It was trained on plant sequences with experimentally determined 4-hydroxyprolines from",
               a(href="http://www.uniprot.org/", "UniProt",target="_blank"),
               "data base.",
               "To start upload a FASTA formatted file with protein sequences of interest.",
               "The results will be displayed graphically on the right.",
               "Hovering over the points will provide more detail.",
               "Prediction is not possible for prolines which are within 10 N-terminal",
               "and 6 C-terminal amino acids. Currently there is a limitation of 5 MB per file upload.", 
               "For faster computation consider instaling the R package",
               a(href="https://github.com/missuse/ragp", "ragp",target="_blank"),
               "which offers the same functionality via the function predict_hyp",
               "The results can also be downloaded as a .csv file."),
      sliderInput("slide", "Probability", min = 0, max = 1,  value = 0.3, step = 0.01),
      helpText("The default threshold probability for classifying HYP is set at 0.3",
               "providing a solid tradeoff between specificity and sensitivity in our simulations,",
               "setting it higher will increase specificity at the cost of sensitivity"),
      useShinyjs(),
      actionButton("hideshow", "treshold explained"),
      shinyjs::hidden(plotly::plotlyOutput('plot1')),
      br(),
      shinyjs::hidden(tableOutput("stats")),
      br(),
      downloadButton('downloadTrain', 'train data'),
      downloadButton('downloadTest', 'test data')),
    mainPanel(
      plotly::plotlyOutput("plot"),
      uiOutput('manyIDs'),
      uiOutput('ui.action')
    )
  )
