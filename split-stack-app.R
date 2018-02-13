library(DT)
library(tidyverse)
library(shinythemes)
library(magrittr)
library(readr)
library(shiny)
library(data.table)
library(purrr)
library(stringr)

ui <- bootstrapPage(
  theme = shinytheme("cosmo"),
  titlePanel("Upload Data to split-stack"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        'dataset',
        'Choose CSV File',
        accept = c('text/csv',
                   'text/comma-separated-values,text/plain',
                   '.csv')
      ),
      # selectInput("columns", "Which Column Contains the Identifier", ""),
      selectInput("splits", "Which Column Contains the Data to Split?", ""),
      # textInput("usr", "Enter Your email address", placeholder = "yours@yours.com"),
      
      downloadButton('downloadData', 'Download Report')
    ),
    mainPanel(
      
      tabsetPanel(
        tabPanel(title = "Original",
                 shiny::dataTableOutput("contents")),
        tabPanel(title = "Preview",
                 shiny::dataTableOutput("preview"))
        
      )
    )
  )
)

server <- function(input, output, session) {
  dat <- reactive({
    inFile <- input$dataset
    if (is.null(inFile)) {
      return(NULL)
    } else {
      fread(inFile$datapath, encoding = 'UTF-8')
    }
  })
  
  
  outVar = reactive({
    mydata = dat()
    names(mydata)
  })
  
  outVar2 = reactive({
    mydata = dat()
    names(mydata)
  })
  
  observe({
    updateSelectInput(session, "columns",
                      choices = outVar())
  })
  
  observe({
    updateSelectInput(session, "splits",
                      choices = outVar2())
  })
  
  output$contents <- shiny::renderDataTable({
    
    # input$dataset will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    
    inFile <- input$dataset
    
    if (is.null(inFile))
      return(NULL)
    
    read_csv(inFile$datapath)
  })
  
  
  final_file <- reactive({
    
    loaded_data <- reactive({
      inFile <- input$dataset
      if (is.null(inFile)) {
        return(NULL)
      } else {
        tryCatch({
          read_csv(inFile$datapath)
        }, error = function(e) {
          print("Invalid Formatting - Check for UTF8 encoding")
        })
      }
      
    })
    
    tmp <-
      loaded_data()[which(colnames(loaded_data()) == input$splits)] %>% as.list() %>%
      map(str_split, pattern = ",", simplify = T) %>% 
      map(trimws)
    
    tmp <- tmp[[1]] %>% as_data_frame()
    
    names(tmp) <-
      paste("split_value", 1:ncol(tmp), sep = "_")
    
    result <-
      cbind(loaded_data(), tmp)
    
    stop <- ncol(result)
    start <- which(names(result) == "split_value_1")
    
    result %<>%
      tidyr::gather(data = ., split_type, split_value, start:stop) %>%
      dplyr::select(-c(split_type)) %>%
      unique %>%
      dplyr::filter(split_value != "")
    
    result
    
  })
  
  output$preview <- shiny::renderDataTable({
    final_file()
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(),
            "split-stack",
            '.csv', sep = '-')
    },
    content = function(file) {
      
      write_csv(final_file(), file)
    }
  )
  
}

shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port= 8118))

