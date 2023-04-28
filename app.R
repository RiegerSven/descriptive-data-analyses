#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel(""),
  navbarPage("(Descriptive) Data Analyses with Shiny & R",
             tabPanel("Analyses",
                      sidebarLayout(
                        sidebarPanel(
                          tryCatch(
                            fileInput("file1", "Data File ( !not ready to use...:(! )", 
                                      accept=c(".csv"))
                          ),
                          helpText('Select a .csv file to be uploaded. The R function read.csv with the default settings for arguments.'),
                          br(),
                          selectizeInput(inputId="exdata", label="Example Data", selected="",
                                         choices= c("",
                                                    "no data",
                                                    "Holzinger1939 (lavaan package)",
                                                    "Highschool and beyond"),
                                         options = list(placeholder = 'choose example data'),
                                         width='50%'),  
                          hr(),
                          conditionalPanel(condition = "input.tabs == 0",
                                           checkboxInput(inputId = "showDatInfo", "str(data)", FALSE)),
                          conditionalPanel(condition = "input.tabs == 1",
                                           varSelectInput(inputId = "vars", label = "Variables (required)",
                                                          character(0),
                                                          multiple = TRUE),
                                           hr(),
                                           h4("Which statistics?"),
                                           fluidRow(
                                             column(4, HTML("<b>Measures of <br> central tendency</b>"),
                                                    checkboxInput(inputId = "mean", "Mean", TRUE),
                                                    checkboxInput(inputId = "median", "Median", FALSE),
                                                    checkboxInput(inputId = "mode", "Mode", FALSE)),
                                           column(4, HTML("<b>Measures of <br> variability</b>"),
                                           checkboxInput(inputId = "sd", "Standard deviation", TRUE),
                                           checkboxInput(inputId = "var", "Variance", FALSE),
                                           checkboxInput(inputId = "min", "Minimum", FALSE),
                                           checkboxInput(inputId = "max", "Maximum", FALSE)),
                                           column(4, HTML("<b>Measures of <br> shape</b>"),
                                           checkboxInput(inputId = "skew", "Skewness", FALSE),
                                           checkboxInput(inputId = "kurt", "Kurtosis", FALSE))),
                                           hr(),
                                           checkboxInput(inputId = "hist", "Density plots?", FALSE),
                                           uiOutput("histPlot")),
                          conditionalPanel(condition = "input.tabs == 2",
                                           fluidRow(
                                             column(6,
                                                    varSelectInput(inputId = "varY", label = "Variable Y (required)",
                                                                   character(0),
                                                                   multiple = FALSE)),
                                             column(6,
                                                    varSelectInput(inputId = "varX", label = "Variable X (required)",
                                                                   character(0),
                                                                   multiple = FALSE))),
                                           fluidRow(
                                             column(4,
                                                    selectInput(inputId = "method ", label = "Method :",
                                                                choices = c("pearson", "kendall", "spearman"),
                                                                selected = "pearson"#,
                                                                #width = '30%'
                                                                )),
                                             column(4,
                                                    selectInput(inputId = "alternative", label = "Direction of hypothesis:",
                                                                choices = c("two.sided", "less", "greater"),
                                                                selected = "two.sided"#,
                                                                #width = '30%'
                                                                )),
                                             column(4,
                                                    numericInput(inputId = "conf", label = "Confidence interval:",
                                                                 value = 0.95, min = 0.001, max = .999, step = 0.01#,
                                                                 #width = '30%'
                                                                 ))),
                                           hr(),
                                           checkboxInput(inputId = "scatterPlot1", "Scatterplot", FALSE),
                                           uiOutput("smooth1")),
                          conditionalPanel(condition = "input.tabs == 3",
                                           fluidRow(
                                             column(6,
                                                    varSelectInput(inputId = "varY2", label = "Variable Y (required)",
                                                                   character(0),
                                                                   multiple = FALSE),
                                                    selectInput(inputId = "scaleLevel", label = "Scale level of Y:",
                                                                choices = c("metric", "ordinal", "binary"),
                                                                selected = "metric",
                                                                width = '100%')),
                                             column(6,
                                                    varSelectInput(inputId = "varX2", label = "Variable X (required)",
                                                                   character(0),
                                                                   multiple = TRUE),
                                                    varSelectInput(inputId = "cluster", label = "Cluster (optional)",
                                                                   character(0),
                                                                   selected = NULL,
                                                                   multiple = TRUE))),
                                           hr(),
                                           checkboxInput(inputId = "scatterPlot2", "Scatterplot", FALSE),
                                           uiOutput("smooth2"))
                        ),
                        
                        mainPanel(
                          tabsetPanel(id = "tabs",
                                      tabPanel("Data", value = 0,
                                               DT::dataTableOutput("tbl"),
                                               hr(),
                                               verbatimTextOutput(outputId = "showDatInfo")),  
                                      tabPanel("Descriptive statistics", value = 1,
                                               verbatimTextOutput(outputId = "descr"),
                                               hr(),
                                               plotOutput(outputId = "hist"), style='width: 75%'),
                                      tabPanel("Correlation", value = 2,
                                               verbatimTextOutput(outputId = "cor"),
                                               hr(),
                                               plotOutput(outputId = "scatter1"), style='width: 65%'),
                                      tabPanel("Regression Modeling", value = 3,
                                               verbatimTextOutput(outputId = "regOutput"),
                                               hr(),
                                               plotOutput(outputId = "scatter2"), style='width: 65%')
                                      )
                                      
                                      
                          )
                        )
                      ),
             tabPanel("Information",
                      br(),
                      h4("This Shiny App is designed to calculate basic descriptive statistics."),
                      )
             )
  )



server <- function(input, output) {
  
  # Reactive Data Input ####
  dataInput <- reactive({
    inFile <- input$file1
    exdata <- input$exdata
    
    if(is.null(inFile)){      
      if(exdata==""){
        return(NULL)        
      }else if(exdata=="Holzinger1939 (lavaan package)"){
        dat <- lavaan::HolzingerSwineford1939
        return(dat)  
      } else if(exdata == "Highschool and beyond"){
        dat <- merTools::hsb
        return(dat)
      }
    }
    
    if(!is.null(inFile)){
      
      dat <- data.table::data.table ( read.csv2(inFile$datapath, header = T) )
      return(dat)
      
    }
    
    
  })
  # Update Select Input ####
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "vars", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varY", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varY2", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varX", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varX2", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "cluster", choices = colnames(dataInput()))
  })
  
  
  # Calculate Descriptive Statistics ####
  
  calcDescr <- reactive({
    # https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode/8189441#8189441
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    tempDescr <- data.table::rbindlist(
      lapply( 1:length(as.character(input$vars)),
              function(x) {
                if ( is.numeric( dataInput()[,as.character(input$vars)[x]]) == TRUE |
                     is.integer( dataInput()[,as.character(input$vars)[x]]) == TRUE ) {
                  
                mean <- mean(dataInput()[,as.character(input$vars)[x]], na.rm = T)
                median <- stats::median(dataInput()[,as.character(input$vars)[x]], na.rm = T)
                mode <- Mode(dataInput()[,as.character(input$vars)[x]])
                sd <- sd(dataInput()[,as.character(input$vars)[x]], na.rm = T)
                var <- var(dataInput()[,as.character(input$vars)[x]], na.rm = T)
                min <- min(dataInput()[,as.character(input$vars)[x]], na.rm = T)
                max <- max(dataInput()[,as.character(input$vars)[x]], na.rm = T)
                skew <- moments::skewness(dataInput()[,as.character(input$vars)[x]], na.rm = T)
                kurt <- moments::kurtosis(dataInput()[,as.character(input$vars)[x]], na.rm = T)
                dat <- data.frame(Mean = mean, Median = median, Mode = mode,
                                  SD = sd, Var = var,
                                  Min = min, Max = max, Skew = skew, Kurtosis = kurt)
                dat <- round(dat, 3) } else {
                  stop("Variable must be of type numeric to calculate descriptive statistics.")
                }
                
                return(dat)
              }), 
      idcol = "Variable")
    
    tempDescr$Variable <- as.character(input$vars)
    tempDescr
    
  
    
  })
  
  output$descr <- renderPrint({
    
    if ( length(as.character(input$vars)) == 0 ) {
      
      HTML( "Please select variables") 
      
    } else {
    
      selDescr <- c("Variable",
                    "Mean", "Median", "Mode", "SD", "Var",
                    "Min", "Max", "Skew", "Kurtosis")[c(TRUE,
                                                        input$mean,
                                                        input$median,
                                                        input$mode,
                                                        input$sd,
                                                        input$var,
                                                        input$min, 
                                                        input$max,
                                                        input$skew,
                                                        input$kurt)]
        
    calcDescr()[,..selDescr]
     
    }
    
  })
  
  
  
  output$hist <- renderPlot({ 
    
    histPlot <- function( data,
                          item,
                          title = "",
                          xLab = "",
                          yLab = "Density",
                          facetNcol = 3) {
      
      #data <- data.table::as.data.table(data)
      if (length(item) > 1) {
        
        data <- reshape(data[,item],
                        varying = item,
                        v.names = "val",
                        timevar = "item",
                        times = item,
                        direction = "long")
        
      } else {
        data$val <- data[,item]
        data$item <- ""
      }
      
      tempPlot <- ggplot(data = data,
                         aes( x = val)) +
        geom_histogram(aes(y=..density..),
                       fill = "lightgrey", color = "black", bins = 30) +
        geom_density(aes(y=..density..), fill = "lightblue", alpha = .5) +
        facet_wrap(~item, scales = "free", ncol = facetNcol) +
        labs(x = xLab, y = yLab) +
        theme_minimal() +
        theme(axis.text = element_text(size = 15),
              axis.title = element_text(size = 15),
              plot.title = element_text(size = 25, hjust = 0.5),
              strip.text.x = element_text(size = 20))
      
      return(tempPlot)
      
    }
    
    if ( input$hist == TRUE) {
    
      if ( length(as.character(input$vars)) == 0) {
        stop("You need to select variables.")
      }
      
    tempHist <- histPlot(data = dataInput(),
                         item = as.character(input$vars))
    tempHist
    }
      

    })
  
  calcCorr <- reactive({
    
    cor.test(x = dataInput()[,as.character(input$varX)],
             y = dataInput()[,as.character(input$varY)],
             alternative = input$alternative,
             method = input$method,
             conf.level = input$conf)
    
  })
  
  output$cor <- renderPrint({
    
    if ( length(input$varY) == 0 |  length(input$varX) == 0) {
      
      HTML( "Please select variables") 
      
    } else {
      
      calcCorr()
      
    }
    
  })
  
  calcReg <- reactive({
    
    
    
    if ( length(as.character(input$cluster)) > 1) {
      stop("only one cluster is supported")
    
      } else if (length(as.character(input$cluster)) == 0) {
      
      svyDesign <- survey::svydesign(ids =~ 1, probs =~ NULL, data = dataInput())
      
      } else {
        Cluster <- dataInput()[,as.character(input$cluster)]
        svyDesign <- survey::svydesign(ids = Cluster, probs =~ NULL, data = dataInput())
        
      }
    
    if ( input$scaleLevel == "metric") {
      
      myFormula <- as.formula( paste(input$varY2, "~", paste(input$varX2, collapse = "+") ) )
      summary(
        survey::svyglm(formula = myFormula, design = svyDesign,
                       family=stats::gaussian())
      )
      
    } else if ( input$scaleLevel == "binary") {
      
      myFormula <- as.formula( paste(input$varY2, "~", paste(input$varX2, collapse = "+") ) )
      summary(
        survey::svyglm(formula = myFormula, design = svyDesign,
                       family=stats::binomial(link = "logit"))
      )
      
    } else if ( input$scaleLevel == "ordinal") {
      
      myFormula <- as.formula( paste("as.factor(",input$varY2,")", "~", paste(input$varX2, collapse = "+") ) )
      summary(
        survey::svyolr(formula = myFormula, design = svyDesign)
      )
      
    }
    
    
    
  })
  
  output$regOutput <- renderPrint({
    
    if ( length(input$varY2) == 0 |  length(input$varX2) == 0) {
      
      HTML( "Please select variables") 
      
    } else {
      
      calcReg()
      
    }
    
  })
  
  output$smooth1 = renderUI({
    
    if (input$scatterPlot1 == FALSE) {
      return(NULL)
      
    } else {
      
      loesCheck <- checkboxInput(inputId = "smooth1", "Loess function?", FALSE)
      
      sc1LabX <- textInput(inputId = "labX", "Label of x-Axis:", value = input$varX, width = '30%', placeholder = NULL)
      sc1LabY <- textInput(inputId = "labY", "Label of y-Axis:", value = input$varY, width = '30%', placeholder = NULL)
      sc1title <- textInput(inputId = "title", "Title of plot:", value = "", width = '30%', placeholder = NULL)
    
      sc1Ui <- list(loesCheck, hr(), h4("Cosmetics"), sc1LabX, sc1LabY, sc1title)
      sc1Ui
      }
    
    
    
    })
  
  output$smooth2 = renderUI({
    
    if (input$scatterPlot2 == FALSE) {
      return(NULL)
      
    } else {
      
      checkboxInput(inputId = "smooth2", "Loess function?", FALSE)
      
    }
    
    
    
  })
  
  
  output$scatter1 <- renderPlot({
    
    if ( input$scatterPlot1 == TRUE ) {
    
      #yMean <- mean(dataInput()[,as.character(input$varY)], na.rm = T)
      #xMean <- mean(dataInput()[,as.character(input$varX)], na.rm = T)
      
    sc1 <- ggplot(data = dataInput(),
           aes(y = .data[[input$varY]], x = .data[[input$varX]])) +
      geom_point(color = "black", fill = "white", linewidth = 3, alpha = .25) +
      labs(x = input$labX, y = input$labY, title = input$title) +
      #geom_hline(yintercept = yMean, color = "black") +
      #geom_vline(yintercept = xMean, color = "black") +
      theme_minimal() + theme(axis.text = element_text(size = 20),
                              axis.title = element_text(size = 20),
                              plot.title = element_text(size = 25, hjust = 0.5))
    
    if ( input$smooth1 == FALSE ) {
      
      sc1
      
      
    } else {
    
    sc1 <- sc1 + geom_smooth(method = "loess", formula = 'y ~ x',
                             se = FALSE, color = "darkred",
                             linewidth = 2)
    sc1
    
    }
    }
    
  })
  
  
  
  output$scatter2 <- renderPlot({
    
    if ( input$scatterPlot2 == TRUE ) {
      
      if ( length(input$varX2) > 1 ) {
        stop("visualizing is only available for 2 variables in total")
      }
      
      sc2 <- ggplot(data = dataInput(),
             aes(y = .data[[input$varY2]],
                 x = .data[[as.character(input$varX2)[1]]])) +
        geom_point(color = "black", fill = "white", size = 3, alpha = .25) +
        geom_smooth(method = "lm", formula = 'y ~ x', se = FALSE) + 
        theme_minimal() + theme(axis.text = element_text(size = 20),
                                axis.title = element_text(size = 20))
      
      if ( input$smooth2 == FALSE ) {
        
        sc2
        
        
      } else {
        
        sc2 <- sc2 + geom_smooth(method = "loess", formula = 'y ~ x',
                                 se = FALSE, color = "darkred")
        sc2
      
    }
      
    }
    
  })
  
  
  output$tbl = DT::renderDataTable({
    
    DT::datatable(dataInput(), options = list(lengthChange = FALSE))
      
  })
  
  output$showDatInfo = renderPrint({
    
    if (input$showDatInfo == TRUE) {
      
      str(dataInput())
      
    }
    
    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
