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
library(writexl)
library(ggplot2)
library(RCurl)
Sys.setlocale("LC_TIME","english")

urlfile<-'https://raw.githubusercontent.com/msk940609/rmsp/main/Datafile/ex_spectrum1.csv'

ui <- fluidPage( ##fluid page start
  navbarPage( "MS test app" ,#navbar start
              tabPanel( "Regularize spectrum", ##tab panel start && name of first tap
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
                            sliderInput(inputId = "span", 
                                        label = "Set span value",
                                        min = 0, 
                                        max = 0.2, 
                                        sep = "",
                                        step = 0.002,
                                        value = c(0.03)), ###span value (slider ui)
                            uiOutput("variable_x"), ## variable of x axis
                            uiOutput("variable_y"), ## variable of y axis
                            sliderInput(inputId = "range", 
                                        label = "Select export wave number range",
                                        min = 401, 
                                        max = 1800, 
                                        sep = "",
                                        step = 10,
                                        value = c(401, 1800)), ###span value (slider ui)
                            selectInput(inputId = "gap",  label = "Select gap export wave number",
                                        choices = c(0.1,1)),
                            
                            downloadButton("export", "Export regularized spectrum") ##ui of down button
                            
                          ), ##side panel design end
                          
                          mainPanel(  ##design main panel
                            splitLayout(style = "border: 1px solid silver:", cellWidths = c(900,450), 
                                        plotlyOutput('raw',  width = "700px", height = "500px"),
                                        DT::dataTableOutput('sptable',width = "400px", height = "400px")), ## show table & plot at main panel first raw
                            tags$hr(),
                            splitLayout(style = "border: 1px solid silver:", cellWidths = c(900,450), 
                                        plotlyOutput('smooth',  width = "700px", height = "500px"),
                                        DT::dataTableOutput('smsp',width = "400px", height = "400px")), ## show table & plot at main panel first raw
                            tags$hr(),
                            splitLayout(style = "border: 1px solid silver:", cellWidths = c(900,450), 
                                        plotlyOutput('regexport',  width = "700px", height = "500px"),
                                        DT::dataTableOutput('regtable',width = "400px", height = "400px"))
                          )  ##end main panel
                        
                          
                        ) ##side bar end
                
              ), ##tab panel end
    tabPanel( "Spectrum deconvolution",  ##start second tab panel 
      sidebarLayout(  #start sidebar
        sidebarPanel( ##start sidebar panel
          
        ), ##end sidebar panel
        mainPanel(  ##start main panel
          
        ) ##end main panel
      ) ##end side bar
    )
  ) #navbar end
  
  
) ##fluid page end (ui end)


server <- function(input, output, session) {
  
  data <- reactive({
    
    validate(
      if(input$example==F) {
        need(input$file1 != "", "Please select a data set") ## if not use example data and input data was not selected, showed text
      }
      else {
        NULL
      }
    )
    
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
    fig <- plot_ly(dat(),x = ~`X`, y = ~`Y`, marker=list(size=2, color = 'rgb(22, 96, 167)'), type = 'scatter',
                   mode = 'markers',
                   width = 800) %>% 
      add_trace(x = ~`X`,
                y = ~`Y`,
                line=list(width=2, color='rgb(22, 96, 167)'),
                type = 'scatter',
                mode = 'lines') %>% 
      layout(title="Raw raman spectrum",
             margin=list(l=50, r=20, b=70, t=50))
    
    
    fig <- fig %>% config(
      displaylogo = FALSE
    )
    fig
  })
  
  output$sptable =  DT::renderDataTable(
    
    DT::datatable(data = data()),
    filter="none", options = list(dom="t"),selection="multiple", escape=FALSE
  )  
  
  loess <- reactive({
    
    x=data()[[input$variableNames_x]]
    y=data()[[input$variableNames_y]]
    
    fit <- stats::loess(formula = y ~ x, span=input$span)
    
    y2=fit$fitted
    
    lssm2 <- data.frame(x,y2)
    colnames(lssm2) <- c("X", "new_Y")
    
    return(lssm2)
  })
  
  
  output$smooth <- renderPlotly({
    dist=round(dist(rbind(dat()$Y, loess()$new_Y)),digits = 3)
    
    
    plotheight <- 600
    fig <- plot_ly(loess(),x = ~`X`, y = ~`new_Y`, marker=list(size=2, color = 'rgb(22, 96, 167)'), type = 'scatter',
                   mode = 'markers',
                   width = 800) %>% 
      add_trace(x = ~`X`,
                y = ~`new_Y`,
                line=list(width=2, color='rgb(22, 96, 167)'),
                type = 'scatter',
                mode = 'lines') %>% 
      layout(title=paste("Loess Regression& smoothing (Euclidean dist:",dist,")"),
             margin=list(l=50, r=20, b=70, t=50))
    
    fig <- fig %>% config(
      displaylogo = FALSE
    )
    fig
  })
  
  output$smsp =  DT::renderDataTable(
    DT::datatable(data = loess()),
    filter="none", options = list(dom="t"),selection="multiple", escape=FALSE
  )
  
  export <- reactive({
    
    xrange=seq(input$range[1],input$range[2], as.numeric(input$gap))
    
    x=data()[[input$variableNames_x]]
    y=data()[[input$variableNames_y]]
    
    fit <- stats::loess(formula = y ~ x, span=input$span)
    
    y_pred=predict(fit,xrange)
    
    reg <- data.frame(xrange,y_pred)
    colnames(reg) <- c("re_X", "re_Y")
    
    return(reg)
  })  
  
  output$regexport <- renderPlotly({
    
    plotheight <- 600
    fig <- plot_ly(export(),x = ~`re_X`, y = ~`re_Y`, marker=list(size=2, color = 'rgb(22, 96, 167)'), type = 'scatter',
                   mode = 'markers',
                   width = 800) %>% 
      add_trace(x = ~`re_X`,
                y = ~`re_Y`,
                line=list(width=2, color='rgb(22, 96, 167)'),
                type = 'scatter',
                mode = 'lines') %>% 
      layout(title="export intensity from regression according to selected wave number range",
             margin=list(l=50, r=20, b=70, t=50))
    
    fig <- fig %>% config(
      displaylogo = FALSE
    )
    fig
  })
  
  output$regtable =  DT::renderDataTable(
    DT::datatable(data = export()),
    filter="none", options = list(dom="t"),selection="multiple", escape=FALSE
  )
  
  output$export <- downloadHandler(
    filename = function() {
      paste(format(Sys.time(),"%y%m%d","tempname"))
    },
    content=function(file){
      fwrite(export(), file, row.names = F)
    }
  )
}

shinyApp(ui, server)
