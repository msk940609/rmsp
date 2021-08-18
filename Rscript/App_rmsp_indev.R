library(shiny)
library(plotly)
library(dplyr)
library(data.table)
options(shiny.maxRequestSize=100*1024^2)
getOption("digits")
options("digits" = 15)
library(RColorBrewer)
options(scipen=1000)
library(rsconnect)
library(tidyverse)
library(bit64)
library(shinyFiles)
library(xlsx)
library(writexl)
library(ggplot2)
library(RCurl)
Sys.setlocale("LC_TIME","english")

urlfile<-'https://raw.githubusercontent.com/msk940609/rmsp/main/Datafile/ex_spectrum1.csv'

ui <- fluidPage(  ##fluid page start
  navbarPage( "MS test app" ,#navbar start
              tabPanel( "tab page name 1", ##tab panel start && name of first tap
                        sidebarLayout(       ##side bar design start
                          sidebarPanel( width=3,      ##side panel design start
                            fileInput("file1", "Choose File",
                                      buttonLabel=list(icon("folder"),"Browse"),
                                      multiple = TRUE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")), ### first option: file select
                            checkboxInput("example", "browse ex spectrum", FALSE), ## ui of check box
                            tags$hr(),
                            sliderInput(inputId = "slider", 
                                        label = "Error ppm range",
                                        min = 0, 
                                        max = 0.5, 
                                        sep = "",
                                        step = 0.01,
                                        value = c(0.03)), ###span value (slider ui)
                            uiOutput("variable_x"), ## variable of x axis
                            uiOutput("variable_y"), ## variable of y axis
                            
                            actionButton("action", "loess regresstion") ##ui of action button
                            
                          ), ##side panel design end
                          
                          mainPanel(  ##design main panel
                            splitLayout(style = "border: 1px solid silver:", cellWidths = c(700,450), 
                                        plotlyOutput('raw',  width = "500px", height = "500px"),
                                        DT::dataTableOutput('sptable',width = "400px", height = "400px")) ## show table & plot at main paenl first raw
                            
                          )  ##end main panel
                        
                          
                        ) ##side bar end
                        
                
              ) ##tab panel end
    
  ) #navbar end
  
  
) ##fluid page end (ui end)


server <- function(input, output, session) {
  
  data <- reactive({
    if(input$example==F){
      df <- fread(input$file1$datapath)
    }
    else{
      df <- fread(urlfile)
    }
    return(df)
  })
  
  
  output$variable_x <- renderUI({
    selectInput("variableNames_x", label = "Variable_X", choices = names(data())) 
  })
  
  output$variable_y <- renderUI({
    selectInput("variableNames_y", label = "Variable_Y", choices = names(data()) ) 
  })
  
  
  dat <- reactive({
    test <- data.frame(data()[[input$variableNames_x]], data()[[input$variableNames_y]])
    colnames(test) <- c("X", "Y")
    return(test)
  })
  
  output$raw <- renderPlotly({
    plotheight <- 600
    fig <- plot_ly(dat(), color = I("gray20"),
                   width = 600) %>% 
      add_trace(x = ~`X`,
                y = ~`Y`,
                type = 'scatter',
                mode = 'markers')
    
    
    fig <- fig %>% config(
      displaylogo = FALSE
    )
    fig
  })
  
  output$sptable =  DT::renderDataTable(
    
    DT::datatable(data = data()),
    filter="none", options = list(dom="t"),selection="multiple", escape=FALSE
  )  
  
  observeEvent(input$action, {
    
    
    
  })
}

shinyApp(ui, server)
