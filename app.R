#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel(""),
  navbarPage("(Descriptive) Data Analyses with Shiny & R",
             tabPanel("Analyses",
                      sidebarLayout(
                        sidebarPanel(
                          conditionalPanel(condition = "input.tabs == 0",
                                           fileInput("file1", "Select a .csv file to be uploaded:",
                                                     multiple = FALSE,
                                                     accept = c("text/csv",
                                                                "text/comma-separated-values,text/plain",
                                                                ".csv")),
                                           checkboxInput("header", "Header", TRUE),
                                           fluidRow(
                                             column(4,
                                                    radioButtons("sep", "Separator",
                                                                 choices = c(Comma = ",",
                                                                             Semicolon = ";",
                                                                             Tab = "\t"),
                                                                 selected = ";")),
                                             column(4,
                                                    radioButtons("dec", "Decimal",
                                                                 choices = c(Comma = ",",
                                                                             Period = "."),
                                                                 selected = ".")),
                                             column(4,
                                                    radioButtons("quote", "Quote",
                                                                 choices = c(None = "",
                                                                             "Double Quote" = '"',
                                                                              "Single Quote" = "'"),
                                                                 selected = '"'))),
                                           br(),
                                           selectizeInput(inputId="exdata", label="Example Data", selected="",
                                                          choices= c("",
                                                                     "Diamonds (ggplot2 package)",
                                                                     "Holzinger1939 (lavaan package)",
                                                                     "Highschool and beyond (merTools package)",
                                                                     "Now, I want to upload data!"),
                                                          options = list(placeholder = 'choose example data'),
                                                          width='50%'),
                                           hr(),
                                           checkboxInput(inputId = "showDatInfo", "Show information about the data set: str(data)", FALSE)
                                           ),
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
                                                    selectInput(inputId = "method", label = "Method:",
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
                                           uiOutput("smooth1"),
                                           hr(),
                                           downloadButton("downloadCor", "Download correlation analysis")),
                          conditionalPanel(condition = "input.tabs == 3",
                                           fluidRow(
                                             column(6,
                                                    varSelectInput(inputId = "varY2", label = "Variable Y (required)",
                                                                   character(0),
                                                                   multiple = FALSE)),
                                             column(6,
                                                    varSelectInput(inputId = "varX2", label = "Group-Variable X (required)",
                                                                   character(0),
                                                                   multiple = FALSE))),
                                           fluidRow(
                                             column(4,
                                                    numericInput(inputId = "mu", label =  "True value \u03BC",
                                                                 value = 0, step = .1)),
                                             column(4,
                                                    selectInput(inputId = "alternative2", label = "Direction of hypothesis:",
                                                                choices = c("two.sided", "less", "greater"),
                                                                selected = "two.sided"#,
                                                                #width = '30%'
                                                                )),
                                             column(4,
                                                    numericInput(inputId = "conf2", label = "Confidence interval:",
                                                                 value = 0.95, min = 0.001, max = .999, step = 0.01#,
                                                                 #width = '30%'
                                                                 ))),
                                           checkboxInput(inputId = "var.equal", "Equal variances", FALSE),
                                           checkboxInput(inputId = "paired", "Paired t-test", FALSE),
                                           hr(),
                                           checkboxInput(inputId = "boxplot", "Boxplot", FALSE),
                                           uiOutput("boxUi"),
                                           hr(),
                                           downloadButton("downloadtTest", "Download t-Test analysis")),
                          conditionalPanel(condition = "input.tabs == 4",
                                           fluidRow(
                                             column(6,
                                                    varSelectInput(inputId = "varYreg", label = "Variable Y (required)",
                                                                   character(0),
                                                                   multiple = TRUE),
                                                    selectInput(inputId = "scaleLevel", label = "Scale level of Y:",
                                                                choices = c("metric", "ordinal", "binary"),
                                                                selected = "metric",
                                                                width = '100%')),
                                             column(6,
                                                    varSelectInput(inputId = "varsX", label = "Variable X (required)",
                                                                   character(0),
                                                                   multiple = TRUE),
                                                    varSelectInput(inputId = "cluster", label = "Cluster (optional)",
                                                                   character(0),
                                                                   selected = NULL,
                                                                   multiple = FALSE),
                                                    varSelectInput(inputId = "weight", label = "Weight (optional)",
                                                                   character(0),
                                                                   selected = NULL,
                                                                   multiple = FALSE))),
                                           hr(),
                                           checkboxInput(inputId = "scatterPlot2", "Scatterplot", FALSE),
                                           uiOutput("smooth2"),
                                           hr(),
                                           downloadButton("downloadReg", "Download regression analysis")),
                          conditionalPanel(condition = "input.tabs == 5",
                                           fluidRow(
                                             column(6,
                                                    varSelectInput(inputId = "varYLav", label = "Variable Y (required)",
                                                                   character(0),
                                                                   multiple = TRUE),
                                                    selectInput(inputId = "scaleLevelLav", label = "Scale level of Y:",
                                                                choices = c("metric"),
                                                                selected = "metric",
                                                                width = '100%')
                                                    ),
                                             column(6,
                                                    varSelectInput(inputId = "varsXLav", label = "Predictors X (required)",
                                                                   character(0),
                                                                   multiple = TRUE),
                                                    varSelectInput(inputId = "clusterLav", label = "Cluster (optional)",
                                                                   character(0),
                                                                   selected = NULL,
                                                                   multiple = FALSE))
                                             ),
                                           fluidRow(
                                             column(6, HTML("<b>Model specification</b>"),
                                                    checkboxInput(inputId = "fixedx", "Fixed Predictors (fixed.x)?", TRUE),
                                                    checkboxInput(inputId = "meanStr", "Meanstructure?", TRUE),
                                                    selectInput(inputId = "estimator", label = "Estimator:",
                                                                choices = c("ML", "MLR", "PML", "MLM", "MLMVS", "MLMV",
                                                                            "WLS", "DWLS", "GLS", "ULS"),
                                                                selected = "ML",
                                                                width = '50%'),
                                                    selectInput(inputId = "miss", label = "Missing data:",
                                                                choices = c("default", "direct", "ml", "fiml"),
                                                                selected = "default",
                                                                width = '50%')),
                                             column(6, HTML("<b>Output</b>"),
                                                    checkboxInput(inputId = "fit", "Fit measures?", FALSE),
                                                    checkboxInput(inputId = "std", "Standardized solution?", FALSE),
                                                    checkboxInput(inputId = "r2", "Explained Variance", FALSE))
                                             ))
                                             
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
                                               #withMathJax("$$\\text{Display formula in heading }...$$"),
                                               #hr(),
                                               verbatimTextOutput(outputId = "cor"),
                                               hr(),
                                               plotOutput(outputId = "scatter1"), style='width: 65%'),
                                      tabPanel("t-Test", value = 3,
                                               verbatimTextOutput(outputId = "tTest"),
                                               hr(),
                                               plotOutput(outputId = "boxplot"), style='width: 65%'),
                                      tabPanel("Regression Modeling", value = 4,
                                               verbatimTextOutput(outputId = "regOutput"),
                                               hr(),
                                               plotOutput(outputId = "scatter2"), style='width: 65%'),
                                      tabPanel("Path analysis", value = 5,
                                               verbatimTextOutput(outputId = "lavOutput"),
                                               hr(),
                                               #plotOutput(outputId = "scatter2")
                                               style='width: 65%')
                                      )
                                      
                                      
                          )
                        )
                      ),
             tabPanel("Information",
                      br(),
                      h4("This Shiny App is designed to calculate basic descriptive statistics."),
                      )
             ))

  

  



server <- function(input, output) {
  
  # Reactive Data Input ####
  dataInput <- reactive({
    
      if (is.null(input$file1$datapath)) {
        
        if(input$exdata == ""){
          return(NULL)
          } else if(input$exdata == "Now, I want to upload data!"){
            return(NULL)
            } else if(input$exdata == "Diamonds (ggplot2 package)"){
            dat <- ggplot2::diamonds
            return(dat)
            } else if(input$exdata == "Holzinger1939 (lavaan package)"){
              dat <- lavaan::HolzingerSwineford1939
              return(dat)
              } else if(input$exdata == "Highschool and beyond (merTools package)"){
                dat <- merTools::hsb
                return(dat)
                }
        
        
      } else {
       
        
        
        dat <- tryCatch(data.table::data.table ( read.csv(input$file1$datapath,
                                               header = input$header,
                                               sep = input$sep,
                                               quote = input$quote,
                                               dec = input$dec) 
                                      ))
        updateSelectInput(session = getDefaultReactiveDomain(), "exdata", choices = "You uploaded data.") 
      
      return(dat)
      }
  })
  
  
  # Update Select Input ####
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "vars", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varY", choices = c("", colnames(dataInput())),
                      selected = "")
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varY2", choices = c("", colnames(dataInput())),
                      selected = "")
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varYreg", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varYLav", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varX", choices = c("", colnames(dataInput())),
                      selected = "")
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varX2", choices = c("", colnames(dataInput())),
                      selected = "")
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varsX", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "varsXLav", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "cluster", choices = c("no clustering", colnames(dataInput())),
                      selected = "no clustering")
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "weight", choices = c("no weighting", colnames(dataInput())),
                      selected = "no weighting")
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "clusterLav", choices = c("no clustering", colnames(dataInput())),
                      selected = "no clustering")
  })
  
  
  # Calculate Descriptive Statistics ####
  
  calcDescr <- reactive({
    # https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode/8189441#8189441
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    myVars <- as.character(input$vars)
    
    tempDescr <- data.table::rbindlist(
      lapply( 1:length(myVars),
              function(x) {
                tempDat <- as.data.frame(dataInput()) # ugly ...
                mean <- mean(tempDat[,myVars[x]], na.rm = T)
                median <- stats::median(tempDat[,myVars[x]], na.rm = T)
                mode <- Mode(tempDat[,myVars[x]])
                sd <- sd(tempDat[,myVars[x]], na.rm = T)
                var <- var(tempDat[,myVars[x]], na.rm = T)
                min <- min(tempDat[,myVars[x]], na.rm = T)
                max <- max(tempDat[,myVars[x]], na.rm = T)
                skew <- moments::skewness(tempDat[,myVars[x]], na.rm = T)
                kurt <- moments::kurtosis(tempDat[,myVars[x]], na.rm = T)
                dat <- data.frame(Mean = mean, Median = median, Mode = mode,
                                  SD = sd, Var = var,
                                  Min = min, Max = max, Skew = skew, Kurtosis = kurt)
                dat <- round(dat, 3) 
                
                return(dat)
              }), 
      idcol = "Variable")
    
    
    tempDescr$Variable <- myVars
    tempDescr
    
  
    
  })
  
  output$descr <- renderPrint({
    
    if ( is.null(dataInput())) {
      
      HTML( "Please upload or choose an example data set.") 
      
    } else {
    
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
        geom_histogram(aes(y=after_stat(density)),
                       fill = "lightgrey", color = "black", bins = 30) +
        geom_density(aes(y=after_stat(density)), fill = "lightblue", alpha = .5) +
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
      
    tempHist <- histPlot(data = as.data.frame(dataInput()),
                         item = as.character(input$vars))
    tempHist
    }
      

    })
  # Calculate correlation ####
  calcCorr <- reactive({
    
    cor.test(x = dataInput()[,as.character(input$varX)],
             y = dataInput()[,as.character(input$varY)],
             alternative = input$alternative,
             method = paste0(input$method),
             conf.level = input$conf)
    
  })
  
  output$cor <- renderPrint({
    
    if ( is.null(dataInput())) {
      
      HTML( "Please upload or choose an example data set.") 
      
    } else {
    
    if ( length(input$varY) == 0 |  length(input$varX) == 0) {
      
      HTML( "Please select variables") 
      
    } else {
      
      calcCorr()
      
    }
    }
    
  })
  
  # Calculate t-Test ####
  
  calctTest <- reactive({
    
    myFormula <- as.formula( paste(input$varY2, "~", input$varX2 ) )
    
    t.test(formula = myFormula,
           data = dataInput(),
           alternative = input$alternative2,
           conf.level = input$conf2,
           paired = input$paired,
           var.equal = input$var.equal,
           mu = input$mu)
    
    
  })
  
  output$tTest <- renderPrint({
    
    if ( is.null(dataInput())) {
      
      HTML( "Please upload or choose an example data set.") 
      
    } else {
      
      if ( length(input$varY2) == 0 |  length(input$varX2) == 0) {
        
        HTML( "Please select variables") 
        
      } else {
        
        calctTest()
        
      }
      
      
    }
    
    
    
  })
  # Calculate Regression ####
  calcReg <- reactive({
    
    if (input$cluster == "no clustering") {
      
      Cluster <- input$cluster
      Cluster <- ~ 1 
      
      } else {
        
        Cluster <- dataInput()[,paste0(input$cluster)]
        
      }
    
    if (input$weight == "no weighting") {
      
      Weight <- input$weight
      Weight <- NULL 
      
    } else {
      
      Weight <- dataInput()[,paste0(input$weight)]
      
    }
    
      svyDesign <- survey::svydesign(ids = Cluster, weight = Weight, data = dataInput())
      
     
    if ( input$scaleLevel == "metric") {
      
      if (length(as.character(input$varYreg)) > 1 ) {
        
        modelRes <- lapply(1:length(as.character(input$varYreg)),
               function(x) {
                 tempFormula <- as.formula( paste(as.character(input$varYreg)[x], "~", paste(input$varsX, collapse = "+") ) )
                 
                 tempMod <- survey::svyglm(formula = tempFormula, design = svyDesign,
                                           family=stats::gaussian())
                 return(tempMod)
                 
               })
        
        modelRes
        
      } else {
        
        myFormula <- as.formula( paste(input$varYreg, "~", paste(input$varsX, collapse = "+") ) )
        
        modelRes <- survey::svyglm(formula = myFormula, design = svyDesign,
                                   family=stats::gaussian())
        modelRes
      }
      
      
    } else if ( input$scaleLevel == "binary") {
      
      myFormula <- as.formula( paste(input$varYreg, "~", paste(input$varsX, collapse = "+") ) )
      summary(
        survey::svyglm(formula = myFormula, design = svyDesign,
                       family=stats::binomial(link = "logit"))
      )
      
    } else if ( input$scaleLevel == "ordinal") {
      
      myFormula <- as.formula( paste("as.factor(",input$varYreg,")", "~", paste(input$varsX, collapse = "+") ) )
      summary(
        survey::svyolr(formula = myFormula, design = svyDesign)
      )
      
    }
    
    
    
  })
  
  output$regOutput <- renderPrint({
    
    if ( is.null(dataInput())) {
      
      HTML( "Please upload or choose an example data set.") 
      
    } else {
      
      if ( length(input$varYreg) == 0 |  length(input$varsX) == 0) {
      
      HTML( "Please select variables") 
        
        } else {
          
          if ( length(input$varYreg) > 1 ) {
            
            lapply(calcReg(),
                   function(x) jtools::summ( x ) )
            
            
          } else {
            
            jtools::summ( calcReg() )
            
          }
          
          
          
        }
      }
    })
  
  # Calculate path ####
  
  calcLav <- reactive({ 
    
    myModel <- paste(input$varYLav, "~", paste(input$varsXLav, collapse = "+") )
    
    if ( input$clusterLav == "no clustering" ) { 
      
      lavCluster <- input$clusterLav
      lavCluster <- NULL
    } else {
      lavCluster <- paste0(input$clusterLav)
      }
    
    tempLav <- lavaan::sem(myModel,
                           data = dataInput(),
                           cluster = lavCluster,
                           fixed.x = input$fixedx,
                           meanstructure = input$meanStr,
                           estimator = input$estimator,
                           missing = input$miss)
    lavaan::summary(tempLav,
                    fit = input$fit,
                    std = input$std,
                    rsq = input$r2)
    
    })
  
  output$lavOutput <- renderPrint({
    
    if ( is.null(dataInput())) {
      
      HTML( "Please upload or choose an example data set.") 
      
    } else {
      
      if ( length(input$varYLav) == 0 |  length(input$varsXLav) == 0) {
        
        HTML( "Please select variables") 
        
      } else {
        
        calcLav()
        
      }
    }
    
  })
  
  output$smooth1 = renderUI({
    
    if (input$scatterPlot1 == FALSE) {
      return(NULL)
      
    } else {
      
      loesCheck <- checkboxInput(inputId = "smooth1", "Loess function?", FALSE)
      
      sc1LabX <- textInput(inputId = "labX", "Label of x-Axis:", value = input$varX, width = '100%', placeholder = NULL)
      sc1LabY <- textInput(inputId = "labY", "Label of y-Axis:", value = input$varY, width = '100%', placeholder = NULL)
      sc1title <- textInput(inputId = "title", "Title of plot:", value = "", width = '100%', placeholder = NULL)
      sc1textSize <- numericInput(inputId = "sc1AxisTextSize", label =  "Axis text size:",
                                  value = 20, step = 1, min = 1, max = 35, width = '100%')
      sc1TitleTextSize <- numericInput(inputId = "sc1TitleTextSize", label =  "Title text size:",
                                       value = 25, step = 1, min = 1, max = 35, width = '100%')
      sc1Ui <- list(loesCheck, hr(),
                    h4("Cosmetics"),
                    fluidRow(
                      column(8,
                             sc1LabX,
                             sc1LabY),
                      column(4,
                             sc1textSize)),
                    fluidRow(
                      column(8,
                             sc1title),
                      column(4,
                             sc1TitleTextSize)
                             )
                    )
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
  
  output$boxUi = renderUI({
    
    if (input$boxplot == FALSE) {
      return(NULL)
      
    } else {
      
      boxLabX <- textInput(inputId = "boxlabX", "Label of x-Axis:", value = input$varX2, width = '100%', placeholder = NULL)
      boxLabY <- textInput(inputId = "boxlabY", "Label of y-Axis:", value = input$varY2, width = '100%', placeholder = NULL)
      boxtitle <- textInput(inputId = "boxtitle", "Title of plot:", value = "", width = '100%', placeholder = NULL)
      boxtextSize <- numericInput(inputId = "boxAxisTextSize", label =  "Axis text size:",
                                  value = 20, step = 1, min = 1, max = 35, width = '100%')
      boxTitleTextSize <- numericInput(inputId = "boxTitleTextSize", label =  "Title text size:",
                                       value = 25, step = 1, min = 1, max = 35, width = '100%')
      boxUi <- list(h4("Cosmetics"),
                    fluidRow(
                      column(8,
                             boxLabX,
                             boxLabY),
                      column(4,
                             boxtextSize)),
                    fluidRow(
                      column(8,
                             boxtitle),
                      column(4,
                             boxTitleTextSize)
                    )
      )
      boxUi
    }
    
    
    
  })
  
  
  scatter1Reac <- reactive({
    
    if ( input$scatterPlot1 == TRUE ) {
      
      if ( length(as.character(input$varY)) == 0 | length(as.character(input$varX)) == 0) {
        stop("You need to select variables.")
      }
      
      sc1 <- ggplot(data = dataInput(),
                    aes(y = .data[[input$varY]], x = .data[[input$varX]])) +
        geom_point(color = "black", fill = "white", size = 3, alpha = .25) +
        labs(x = input$labX, y = input$labY, title = input$title) +
        theme_minimal() + theme(axis.text = element_text(size = input$sc1AxisTextSize),
                                axis.title = element_text(size = input$sc1AxisTextSize),
                                plot.title = element_text(size = input$sc1TitleTextSize, hjust = 0.5))
      
      if ( input$smooth1 == FALSE ) {
        
        sc1
        
        
      } else {
        
        sc1 <- sc1 + geom_smooth(method = "loess", formula = 'y ~ x',
                                 se = FALSE, color = "darkred",
                                 linewidth = 1.5)
        sc1
        
      }
    }
    
  })
  
  output$scatter1 <- renderPlot({
    
    scatter1Reac()
    
  })
  
  output$scatter2 <- renderPlot({
    
    if ( input$scatterPlot2 == TRUE ) {
      
      if ( length(as.character(input$varYreg)) == 0 | length(as.character(input$varsX)) == 0) {
        stop("You need to select variables.")
      }
      
      if ( length(input$varX2) > 1 ) {
        stop("visualizing is only available for 2 variables in total (Y and X)")
      }
      
      sc2 <- ggplot(data = dataInput(),
             aes(y = .data[[input$varYreg]],
                 x = .data[[as.character(input$varsX)]])) +
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
  
  boxplotReac <- reactive({
    
    if ( input$boxplot == TRUE ) {
      
      if ( length(as.character(input$varY2)) == 0 | length(as.character(input$varX2)) == 0) {
        stop("You need to select variables.")
      }
      
      tempBox <- ggplot(data = dataInput(),
                        aes(y = .data[[input$varY2]],
                            x = factor(.data[[input$varX2]]))) +
        geom_boxplot(color = "black", width = .3, outlier.colour="red") +
        #geom_jitter(color = "lightgrey", alpha = .25, shape=16, position=position_jitter(0.1)) +
        stat_summary(fun=mean, geom="point", shape=4, color ="black", size = 4) +
        labs(x = input$boxlabX, y = input$boxlabY, title = input$boxtitle) +
        theme_minimal() + theme(axis.text = element_text(size = 20),
                                axis.title = element_text(size = 20))
      tempBox
    } 
    
  })
  
  output$boxplot <- renderPlot({
      
    boxplotReac()
    
  })
  
  output$downloadCor <- downloadHandler(
    filename = "correlation-output.docx",
    content = function(file) {
      tempCor <- file.path(tempdir(), "cor-analysis.Rmd")
      file.copy("cor-analysis.Rmd", tempCor, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = dataInput(),
                     varX = input$varX,
                     varY = input$varY,
                     scatterPlot = scatter1Reac())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempCor, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    
    }
  )
  
  output$downloadtTest <- downloadHandler(
    filename = "tTest-output.docx",
    content = function(file) {
      temptTest <- file.path(tempdir(), "tTest-analysis.Rmd")
      file.copy("tTest-analysis.Rmd", temptTest, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = dataInput(),
                     plot = boxplotTest())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(temptTest, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  output$downloadReg <- downloadHandler(
    filename = "regression-output.docx",
    content = function(file) {
      tempReg <- file.path(tempdir(), "reg-analysis.Rmd")
      file.copy("reg-analysis.Rmd", tempReg, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data = dataInput(),
                     regOut = calcReg())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReg, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  
  output$tbl = DT::renderDataTable({
    
    DT::datatable(dataInput(), options = list(lengthChange = FALSE))
      
  })
  
  output$showDatInfo = renderPrint({
    
    if (input$showDatInfo == TRUE) {
      
      if ( is.null(dataInput())) {
        HTML("Please upload data or select an example data set.")
      } else {
        
        str(dataInput())
        
      }
      
      
    }
    
    
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
