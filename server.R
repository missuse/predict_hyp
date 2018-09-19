train <- read.csv("train.csv")
test <- read.csv("test.csv")
probs <- readRDS("probs.rds")


library(xgboost)
library(plotly)
library(shiny)
library(shinyjs)
library(ragp)




ReadFasta <- function(file){
  # Read the file line by line
  fasta <- readLines(file)
  # Identify header lines
  ind <- grep(">", fasta)
  nseq <- length(ind)
  if (nseq == 0) {
    stop("no line starting with a > character found")
  }
  # Identify the sequence lines
  s <- data.frame(ind=ind, from=ind+1, to=c((ind-1)[-1], length(fasta)))
  # Process sequence lines
  seqs <- apply(s, 1, function(x) paste(fasta[x[2]:x[3]], collapse=""))
  # Create a data frame 
  DF <- data.frame(id = gsub("/.*", "", gsub(" .*", "", gsub(">", "", fasta[ind]))),
                 sequence = seqs)
  # Return the data frame as a result object from the function
  return(DF)
}



function(input, output, session) {
  options(warn =-1) 
  style <- isolate(input$style)
  a <- reactive({
    inFile <- input$file1
    req(inFile)
    ReadFasta(inFile$datapath)
  })
  
  output$plot1 <- renderPlotly({ 
    do.call(rbind, probs) %>%
      mutate(Jouden = sens + spec -1 ) %>%
      filter(class == "protein") %>%
      plot_ly(x = ~ prob, source = "a") %>%
      add_lines(y = ~sens, name = "Sensitivity") %>%
      add_lines(y = ~spec, name = "Specificity") %>%
      add_lines(y = ~kappa, name = "Cohen's kappa") %>%
      add_lines(y = ~Jouden, name = "Youden's J" ) %>%
      add_lines(y = ~mcc, name = "Matthews CC" ) %>%
      config(displayModeBar = F) %>%
      layout(legend = list(x = 0, y= -0.15, orientation = 'h', showlegend = F),
             xaxis = list(title = "", showgrid = T, zeroline = T, showticklabels = T),
             yaxis = list(title = "", showgrid = T, zeroline = T, showticklabels = T, range = c(0.3, 1))) %>%
      add_lines(x = c(input$slide, input$slide), y = c(0.3, 1), color = I("grey"), name = "probability", line = list(dash = 'dash'), showlegend = F) %>%
      layout(plot_bgcolor = 'transparent') %>% 
      layout(paper_bgcolor = 'transparent')
  })
  
  
  outputOptions(output, 'plot1', suspendWhenHidden = FALSE)

  observe({event.data <- event_data("plotly_click", source = "a")
  if(is.null(event.data)){
    updateSliderInput(session, "slide", value = 0.3, min = 0, max = 1, step = 0.01)
    } else {updateSliderInput(session, "slide", value = event.data[[3]], min = 0, max = 1, step = 0.01)}
  })
  
  observeEvent(input$hideshow, {
    # every time the button is pressed, alternate between hiding and showing the plot
    toggle("plot1")
    toggle("stats")
  })
  
  output$stats <- renderTable({
    do.call(rbind, probs) %>%
      filter(class == "protein") %>%
      mutate("Youden's J" = sens + spec - 1) %>%
      mutate(Sensitivity = sens) %>%
      mutate(Specificity = spec) %>%
      mutate("Cohen's kappa" = kappa) %>%
      mutate("Matthews CC" = mcc) %>%
      subset(prob == input$slide) %>%
      select(-prob, -sens, -spec, -kappa, -mcc, - class, -1)%>%
      t()}, digits = 4, rownames = T, colnames = F)
  
  output$downloadTrain <- downloadHandler(
    filename = "train.csv",
    content = function(file) {
      write.csv(train, file)
    }
  )
  
  output$downloadTest <- downloadHandler(
    filename = "test.csv",
    content = function(file) {
      write.csv(test, file)
    }
  )
  
  output$ui.action <- renderUI({
    if (is.null(a())) return()
    downloadButton('downloadData', 'result')
  })

  b <- reactive({
    a <- a()
    withProgress(message =  "building features", style = style, value = 0, {
      sequence <- as.character(a$sequence)
      id <- as.character(a$id)
      pred <- ragp::predict_hyp(sequence = sequence, id = id)
      incProgress(0.95, detail = "predicting")
      return(pred$prediction)
      })
  })
  
  output$manyIDs = renderUI({
    req(b())
    selectInput('ids', 'Protein id', choices = unique(b()$id), multiple = F)
  })

  output$plot <- renderPlotly({ 
    req(input$ids)
    b <- b()
    b$HYP <- ifelse(b$prob >= input$slide, "Yes", "No")
    g <- b[b$id == input$ids,]
    g$P_pos <- as.numeric(g$P_pos)
    g$prob<- as.numeric(g$prob)
    g$HYP <- as.character(g$HYP)
    colP <- ifelse(g$HYP == "Yes", "red", "grey")
    colN <- unique(colP)
    minP <- min(g$P_pos)
    maxP <- max(g$P_pos)
    HYPname <- NULL
    if(length(colN) == 2) HYPname <- as.factor(ifelse(g$HYP == "Yes", "Hyp", "Pro"))
    if(length(colN) == 2) HYPname <- relevel(HYPname, ref = "Pro")
    g %>%
        mutate(stringerino = gsub('^([A-Z]{10})([A-Z]+)$', '\\1-\\2', substr)) %>%
        mutate(stringerino = gsub('^(.{12})([A-Z]+)$', '\\1-\\2', stringerino)) %>%
        mutate(id = gsub('_.*', '', id))%>%
        plot_ly(x= ~P_pos, y= ~prob, text = ~paste("position: ", P_pos, '<br>substring:', stringerino), type = 'scatter', 
                mode = "markers", marker = list(color = "rgba(51,51,51, 0.2)", size = 10, line = list(color = colP, width=2)), hoverinfo = "text", showlegend=F)%>%
        config(displayModeBar = F)%>%
        layout(xaxis = list(range=c(minP-10,maxP+10), title = "Pro position", showgrid = F, zeroline = T, showticklabels = T),
               yaxis = list(range=c(-0.02,1.02),title = "xgb probability", showgrid = F, zeroline = T, showticklabels = T))
    
  })
  output$downloadData <- downloadHandler(
    filename <- function() { paste("result_", gsub("[^0-9]", "_", Sys.time()), ".csv", sep="") },
    content <- function(file) {
      b <- b()
      b$HYP <-  ifelse(b$prob >= input$slide, "Yes", "No")
      write.csv(b, file)
    }
  )
}
