#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(survey)
library(ggplot2)

ui <- fluidPage(
  titlePanel(""),
  navbarPage("(Descriptive) Data Analyses with Shiny & R",
             tabPanel("Analyses",
                      sidebarLayout(
                        sidebarPanel(
                          conditionalPanel(condition = "input.tabs == 0", # data ui ####
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
                                           uiOutput("dataOpt"),
                                           hr(),
                                           selectizeInput(inputId="exdata", label="Example Data", selected="",
                                                          choices= c("",
                                                                     "Diamonds (ggplot2 package)",
                                                                     "Holzinger1939 (lavaan package)",
                                                                     "Highschool and beyond (merTools package)",
                                                                     "Now, I want to upload data!"),
                                                          options = list(placeholder = 'choose example data'),
                                                          width='50%'),
                                           hr(),
                                           fluidRow(
                                             column(4,
                                                    varSelectInput(inputId = "filtVar",
                                                                   label = "Filter variable (optional)",
                                                                   character(0),
                                                                   selected = "no filter",
                                                                   multiple = FALSE)),
                                             column(4,
                                                    selectInput(inputId = "filtOp",
                                                                label = "Select operator",
                                                                selected = "==",
                                                                choices = c("==",
                                                                            ">",
                                                                            "<",
                                                                            ">=",
                                                                            "<="))
                                                    ),
                                             column(4,
                                                    textInput(inputId = "filtVal", "Filter value",
                                                              placeholder = "Enter value.",
                                                              value = ""))
                                             #uiOutput("filtVal")
                                             ),
                                           checkboxInput(inputId = "showDatInfo",
                                                         "Show information about the data set: str(data)",
                                                         value = TRUE)
                                           ),
                          conditionalPanel(condition = "input.tabs == 1", # descriptive ui ####
                                           varSelectInput(inputId = "vars", label = "Continuous Variables",
                                                          character(0),
                                                          multiple = TRUE),
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
                                           checkboxInput(inputId = "hist",
                                                         "Show Histogram(s)",
                                                         value = FALSE),
                                           uiOutput("histPlot"),
                                           uiOutput("histUp"),
                                           hr(),
                                           varSelectInput(inputId = "catVars", label = "Categorical Variables",
                                                          character(0),
                                                          multiple = TRUE),
                                           hr(),
                                           downloadButton("downloadDescr", "Download descriptive statistics")),
                          conditionalPanel(condition = "input.tabs == 2", # correlation ui ####
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
                          conditionalPanel(condition = "input.tabs == 3", # t-Test ui ####
                                           fluidRow(
                                             column(6,
                                                    varSelectInput(inputId = "varY2", label = "Variable Y (required)",
                                                                   character(0),
                                                                   multiple = TRUE)),
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
                                           hr(),
                                           #checkboxInput(inputId = "tTestResFormat", label = "Table?", FALSE),
                                           #hr(),
                                           checkboxInput(inputId = "dotplot", "Dotplot", FALSE),
                                           uiOutput("dotUi"),
                                           hr(),
                                           downloadButton("downloadtTest", "Download t-Test analysis")),
                          conditionalPanel(condition = "input.tabs == 4", # regression ui ####
                                           fluidRow(
                                             column(6,
                                                    varSelectInput(inputId = "varYreg", label = "Variable Y (required)",
                                                                   character(0),
                                                                   multiple = TRUE),
                                                    selectInput(inputId = "scaleLevel", label = "Scale level of Y:",
                                                                choices = c("metric", "binary"),
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
                                           downloadButton("downloadReg", "Download regression analysis")),
                          conditionalPanel(condition = "input.tabs == 5", # path analysis ui ####
                                           fluidRow(
                                             column(6,
                                                    varSelectInput(inputId = "varYLav", label = "Variable Y (required)",
                                                                   character(0),
                                                                   multiple = TRUE),
                                                    varSelectInput(inputId = "orderedLav", label = "Ordered Y variables:",
                                                                   character(0),
                                                                   multiple = TRUE)
                                                    ),
                                             column(6,
                                                    varSelectInput(inputId = "varsXLav", label = "Predictors X (required)",
                                                                   character(0),
                                                                   multiple = TRUE),
                                                    varSelectInput(inputId = "clusterLav", label = "Cluster (optional)",
                                                                   character(0),
                                                                   selected = NULL,
                                                                   multiple = FALSE),
                                                    varSelectInput(inputId = "weightLav", label = "Weight (optional)",
                                                                   character(0),
                                                                   selected = NULL,
                                                                   multiple = FALSE))
                                             ),
                                           hr(),
                                           fluidRow(
                                             column(6, HTML("<b>Model specification</b>"),
                                                    checkboxInput(inputId = "fixedx", "Fixed Predictors (fixed.x)?", TRUE),
                                                    checkboxInput(inputId = "meanStr", "Meanstructure?", TRUE),
                                                    selectInput(inputId = "estimator", label = "Estimator:",
                                                                choices = c("ML", "MLR", "PML", "MLM", "MLMVS", "MLMV",
                                                                            "WLS", "WLSMV", "DWLS", "GLS", "ULS", "ULSM", "ULSMV"),
                                                                selected = "ML",
                                                                width = '55%'),
                                                    selectInput(inputId = "miss", label = "Missing data:",
                                                                choices = c("default", "direct", "ml", "fiml", "listwise", "pairwise",
                                                                            "available.cases"),
                                                                selected = "default",
                                                                width = '70%')),
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
                                               plotOutput(outputId = "hist"),
                                               hr(),
                                               verbatimTextOutput(outputId = "descrCat"),
                                               style='width: 75%'),
                                      tabPanel("Correlation", value = 2,
                                               #withMathJax("$$\\text{Display formula in heading }...$$"),
                                               #hr(),
                                               verbatimTextOutput(outputId = "cor"),
                                               hr(),
                                               plotOutput(outputId = "scatter1"), style='width: 65%'),
                                      tabPanel("t-Test", value = 3,
                                               verbatimTextOutput(outputId = "tTest"),
                                               #uiOutput("tTestTab"),
                                               hr(),
                                               plotOutput(outputId = "dotplot"), style='width: 65%'),
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
  dataInput0 <- reactive({
    
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
        
        
        if ( !is.null(input$missVal) ) {
          missVals <- gsub(" ", "", paste0(input$missVal), fixed = TRUE)
          missVals <- unlist(strsplit(x = missVals, split = ";" ))
          missVals
        } else {
          missVals <- NULL
        }
        
        
        dat <- tryCatch(data.table::data.table ( read.csv(input$file1$datapath,
                                                          header = input$header,
                                                          sep = input$sep,
                                                          quote = input$quote,
                                                          dec = input$dec
                                                          ,na.strings = c(missVals)
                                                          ) 
                                      ))
        
        for (col in names(dat)) {
          if (is.integer(dat[[col]])) {
            dat[[col]] <- as.numeric(dat[[col]])
          }
        }
        
        
        updateSelectInput(session = getDefaultReactiveDomain(),
                          "exdata",
                          choices = "You already uploaded data.") 
      
      return(dat)
      }
  })
  
  
  dataInput <- reactive({
    
    if (is.null(dataInput0())) {
      
      dat <- dataInput0()
      
    } else {
    
    if ( input$filtVal != "" ) {
      #eval(parse(text = paste0(input$filtOp)))
      if ( input$filtVar == "no filter") {
        dat <- dataInput0()
        showNotification("Please select a filter variable first; no subsetting was done.",
                         type = "warning")
      } else {
      
      tempFiltVar <- paste0(input$filtVar)
      
      if ( input$filtOp == "==" ) {
        
        dat <- subset(dataInput0(), dataInput0()[,tempFiltVar] == input$filtVal)
        
      } else if (input$filtOp == ">") {
        
        dat <- subset(dataInput0(), dataInput0()[,tempFiltVar] > input$filtVal)
        
      } else if (input$filtOp == "<") {
        
        dat <- subset(dataInput0(), dataInput0()[,tempFiltVar] < input$filtVal)
        
      } else if (input$filtOp == ">=") {
        
        dat <- subset(dataInput0(), dataInput0()[,tempFiltVar] >= input$filtVal)
        
      } else if (input$filtOp == "<=") {
        
        dat <- subset(dataInput0(), dataInput0()[,tempFiltVar] <= input$filtVal)
        
      }
      }

      
    } else {
      
      dat <- dataInput0()
      
    }
    }
    
    
    return(dat)
      
    
  })
  
  
  # Update Select Input ####
  
  observeEvent(dataInput0(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "filtVar", choices = c("no filter", colnames(dataInput())),
                      selected = "no filter")
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "vars", choices = colnames(dataInput()))
  })
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "catVars", choices = colnames(dataInput()))
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
    updateSelectInput(session = getDefaultReactiveDomain(), "orderedLav", choices = c("", colnames(dataInput())),
                      selected = "")
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
  
  observeEvent(dataInput(), {
    updateSelectInput(session = getDefaultReactiveDomain(), "weightLav", choices = c("no weighting", colnames(dataInput())),
                      selected = "no weighting")
  })
  
  
  # Calculate Descriptive Statistics ####
  
  calcDescr <- reactive({
    # https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode/8189441#8189441
    Mode <- function(x) {
      ux <- unique(x)
      ux[which.max(tabulate(match(x, ux)))]
    }
    myVars <- as.character(input$vars)
    
    if ( length(myVars) > 0 ) {
    
    tempDescr <- data.table::rbindlist(
      lapply( 1:length(myVars),
              function(x) {
                tempDat <- as.data.frame(dataInput()) # ugly ...
                n <- sum(!is.na(tempDat[,myVars[x]]))
                mean <- mean(tempDat[,myVars[x]], na.rm = T)
                median <- stats::median(tempDat[,myVars[x]], na.rm = T)
                mode <- Mode(tempDat[,myVars[x]])
                sd <- sd(tempDat[,myVars[x]], na.rm = T)
                var <- var(tempDat[,myVars[x]], na.rm = T)
                min <- min(tempDat[,myVars[x]], na.rm = T)
                max <- max(tempDat[,myVars[x]], na.rm = T)
                skew <- moments::skewness(tempDat[,myVars[x]], na.rm = T)
                kurt <- moments::kurtosis(tempDat[,myVars[x]], na.rm = T)
                dat <- data.frame(N = n,
                                  Mean = mean, Median = median, Mode = mode,
                                  SD = sd, Var = var,
                                  Min = min, Max = max, Skew = skew, Kurtosis = kurt)
                dat <- round(dat, 3) 
                
                return(dat)
              }), 
      idcol = "Variable")
    
    
    tempDescr$Variable <- myVars
    
    selDescr <- c("Variable", "N",
                  "Mean", "Median", "Mode", "SD", "Var",
                  "Min", "Max", "Skew", "Kurtosis")[c(TRUE, TRUE,
                                                      input$mean,
                                                      input$median,
                                                      input$mode,
                                                      input$sd,
                                                      input$var,
                                                      input$min, 
                                                      input$max,
                                                      input$skew,
                                                      input$kurt)]
    
    tempDescr[,..selDescr]
    } else {
      return(NULL)
    }
    
  
    
  })
  
  calcDescrCat <- reactive({
    
    catVar <- function ( variable,
                         data,
                         table = TRUE) {
      
      
      # calculate freq. as function
      calcFreq <- function ( variable, data) {
        
        # calc abs and rel freq.
        tempFreq <- data.frame( abs = table(data[,variable],
                                            useNA = "always"))
        
        tempFreq$rel <- table(data[,variable],
                              useNA = "always")/length(data[,variable])*100
        
        colnames(tempFreq) <- c("var",
                                "Absolute",
                                "Relative")
        
        # calculate sum
        tempFreq <- rbind(tempFreq,
                          data.frame(var = "Sum",
                                     t(colSums(tempFreq[!is.na(tempFreq$var),-1])))
        )
        
        
        # cosmetics
        
        tempFreq$Relative <- paste0(
          sprintf(tempFreq$Relative,
                  fmt = '%#.1f'),
          "%")
        
        tempTab <- as.data.frame(t(tempFreq[,-1]))
        colnames(tempTab) <- tempFreq[,1]
        colnames(tempTab)[is.na(colnames(tempTab))] <- "Miss"
        
        tempTab <- as.data.frame(
          t(sapply(colnames(tempTab),
                   function(x) paste0(tempTab[1,x],
                                      " (",
                                      tempTab[2,x], ")"),
                   simplify = TRUE)
          )
        )
        
        return(tempTab)
      }
      
      tempFreq <- data.table::rbindlist(
        sapply(variable,
               function(x) 
                 calcFreq(variable = x, data = data),
               simplify = FALSE),
        idcol = "Variable", use.names = T, fill = T)
      
      # get colnames to sort
      colToSort <- c("Variable", colnames(tempFreq)[!colnames(tempFreq) %in% 
                                                      c("Variable",
                                                        "Miss",
                                                        "Sum"
                                                        #"Sum (miss)"
                                                      )],
                     "Sum",
                     "Miss"
                     #"Sum (miss)",
      )
      
      tempFreq <- tempFreq[,..colToSort]
      
      
      if ( table == TRUE ) {
        
        tempTab <- kableExtra::kbl(
          x = tempFreq,
          align = "c"
        ) |>
          kableExtra::kable_styling(
            full_width = TRUE,
            bootstrap_options = c("hover", "responsive")
            #,latex_options = "HOLD_position"
          ) 
        
        tempOut <- tempTab
        tempOut
        
      } else {
        
        tempOut <- tempFreq[,..colToSort]
        
        return(tempOut)
        
      }
      
    }
    if ( length(as.character(input$catVars)) > 0 ) {
    catVar(variable = as.character(input$catVars),
           data = dataInput(),
           table = FALSE) } 
    else {
      return(NULL)
    }
    
    
  })
  
  output$descr <- renderPrint({
    
    if ( is.null(dataInput())) {
      
      HTML( "Please upload or choose an example data set.") 
      
    } else {
    
    if ( length(as.character(input$vars)) == 0 ) {
      
      HTML( "Please select continuous variables") 
      
    } else {
        
    calcDescr()
     
    }
    }
    
  })
  
  output$descrCat <- renderPrint({
    
    if ( is.null(dataInput())) {
      
      HTML( "") 
      
    } else {
      
      if ( length(as.character(input$catVars)) == 0 ) {
        
        HTML( "Please select categorical variables") 
        
      } else {
        
        calcDescrCat()
        
      }
    }
    
  })
  
  hist <- reactive({
    
    histPlot <- function( data,
                          item,
                          title = "",
                          xLab = input$histlabX,
                          yLab = input$histlabY,
                          facetNcol = 2) {
      
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
        geom_histogram(#aes(y=after_stat(density)),
                       fill = "lightgrey", color = "black", bins = 30) +
        #geom_density(aes(y=after_stat(density)), fill = "lightblue", alpha = .5) +
        facet_wrap(~item, scales = "free", ncol = facetNcol) +
        labs(x = xLab, y = yLab) +
        theme_minimal() +
        theme(axis.text = element_text(size = input$histAxisTextSize),
              axis.title = element_text(size = input$histAxisTextSize),
              plot.title = element_text(size = 25, hjust = 0.5),
              strip.text.x = element_text(size = input$histAxisTextSize))
      
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
  
  output$hist <- renderPlot({ 
    
    
    hist()

    })
  
  # Calculate correlation ####
  calcCorr <- reactive({
    
    tempFormula <- as.formula( paste("~", input$varY, "+", input$varX ) )
    
    cor.test(
      tempFormula,
      data = dataInput(),     
      alternative = input$alternative,
      method = paste0(input$method),
      conf.level = input$conf
      )
    
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
    
    
    if (length(as.character(input$varY2)) > 1 ) {
      
      tTestRes <- lapply(1:length(as.character(input$varY2)),
                         function(x) {
                           
                           tempFormula <- as.formula( paste(input$varY2[x], "~", input$varX2 ) )
                           temptTest <- t.test(formula = tempFormula,
                                               data = dataInput(),
                                               alternative = input$alternative2,
                                               conf.level = input$conf2,
                                               paired = FALSE,
                                               var.equal = input$var.equal,
                                               mu = input$mu)
                           return(temptTest)
                           
                         })
      tTestRes
    
    } else {
      
    myFormula <- as.formula( paste(input$varY2, "~", input$varX2 ) )
    
    tTestRes <- t.test(formula = myFormula,
           data = dataInput(),
           alternative = input$alternative2,
           conf.level = input$conf2,
           paired = FALSE,
           var.equal = input$var.equal,
           mu = input$mu)
    
    tTestRes
    }
    
    
  })
  
  output$tTest <- renderPrint({
      
      if ( is.null(dataInput())) {
        
        HTML( "Please upload or choose an example data set.") 
        
      } else {
        
        if ( length(input$varY2) == 0 |  length(input$varX2) == 0) {
          
          HTML( "Please select variables") 
          
        } else {
          
          if ( length(input$varY2) > 1 ) {
            
            lapply(calctTest(),
                   function(x) x)
            
          } else {
            
            calctTest()
          }
          
        }
        }
    }) 
  
  tTestRes <- reactive({
    
    if ( length(input$varY2) > 1 ) {
            
            tTestRes <- lapply(calctTest(),
                               function(x) {
                                 
                                 tempRes <- as.data.frame(
                                   t(
                                     c(x$estimate,
                                       x$estimate[1]-x$estimate[2],
                                       x$statistic,
                                       x$parameter,
                                       x$p.value,
                                       x$conf.int)
                                   )
                                 )
                                 colnames(tempRes) <- c("M G0", "M G1", "MeanDiff",
                                                        "t", "df", "p", "CILow", "CIUp")
                                 return(tempRes)
                                 
                               })
            tTestRes <- data.table::rbindlist(tTestRes, idcol = "Variable")
            tTestRes$Variable <- as.character(input$varY2)
            tTestRes
          } else {
            
            tTestRes <- as.data.frame(
              t(
                c(calctTest()$estimate,
                  calctTest()$estimate[1]-calctTest()$estimate[2],
                  calctTest()$statistic,
                  calctTest()$parameter,
                  calctTest()$p.value,
                  calctTest()$conf.int)
              )
            )
            colnames(tTestRes) <- c("M G0", "M G1", "MeanDiff",
                                    "t", "df", "p", "CILow", "CIUp")
            tTestRes$Variable <- as.character(input$varY2)
            tTestRes
           }
      })
  
  output$tTestTab <- renderUI({
    
    if ( input$tTestResFormat == TRUE ) {
    
    if ( length(input$varY2) > 1 ) {
            
            tTestRes <- lapply(calctTest(),
                               function(x) {
                                 
                                 tempRes <- as.data.frame(
                                   t(
                                     c(x$estimate,
                                       x$estimate[1]-x$estimate[2],
                                       x$statistic,
                                       x$parameter,
                                       x$p.value,
                                       x$conf.int)
                                   )
                                 )
                                 colnames(tempRes) <- c("M G0", "M G1", "MeanDiff",
                                                        "t", "df", "p", "CI Low", "CI Up")
                                 return(tempRes)
                                 
                               })
            tTestRes <- data.table::rbindlist(tTestRes, idcol = "Variable")
            tTestRes$Variable <- as.character(input$varY2)
            
            HTML(kableExtra::kable( tTestRes,
                                    format = "html",
                                    digits = 3) |>
                   kableExtra::kable_styling(full_width = TRUE))
            
          } else {
            
            tTestRes <- as.data.frame(
              t(
                c(calctTest()$estimate,
                  calctTest()$estimate[1]-calctTest()$estimate[2],
                  calctTest()$statistic,
                  calctTest()$parameter,
                  calctTest()$p.value,
                  calctTest()$conf.int)
              )
            )
            colnames(tTestRes) <- c("M G0", "M G1", "MeanDiff",
                                    "t", "df", "p", "CI Low", "CI Up")
            
            HTML(kableExtra::kable( tTestRes,
                                    format = "html",
                                    digits = 3) |>
                   kableExtra::kable_styling(full_width = TRUE))
            
          }
    } else {
      
      return(NULL)
      
    }
          
      })
  
  dotplotReac <- reactive({
    
    if ( input$dotplot == TRUE ) {
      
      if ( length(as.character(input$varY2)) == 0 | length(as.character(input$varX2)) == 0) {
        stop("You need to select variables.")
      }
      
      tempPlot <- ggplot(data = tTestRes(),
              aes(y = MeanDiff, x = Variable)) +
        geom_point(color = "black", size = 3) +
        geom_errorbar(aes(ymin = CILow, ymax = CIUp), width = .1) +
        scale_y_continuous(expand = c(0, 0)) +
        geom_hline(yintercept = input$mu, linetype = "dashed", color = "darkred") +
        labs(x = input$dotlabX, y = input$dotlabY, title = input$dottitle) +
        facet_wrap(~Variable, scales = "free") +
        theme_minimal() + theme(axis.text.x = element_blank(),
                                axis.ticks.x = element_blank(),
                                strip.text.x = element_text(size = input$dotAxisTextSize),
                                axis.title = element_text(size = input$dotAxisTextSize),
                                axis.text.y = element_text(size = input$dotAxisTextSize),
                                plot.title = element_text(size = input$dotTitleTextSize, hjust = 0.5))
        theme_classic()
        
      tempPlot
      
    } 
    
  })
  
  output$dotplot <- renderPlot({
    
    dotplotReac()
    
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
      
      if (length(as.character(input$varYreg)) > 1 ) {
        
        modelRes <- lapply(1:length(as.character(input$varYreg)),
                           function(x) {
                             tempFormula <- as.formula( paste(as.character(input$varYreg)[x], "~", paste(input$varsX, collapse = "+") ) )
                             
                             tempMod <- survey::svyglm(formula = tempFormula, design = svyDesign,
                                                       family=stats::binomial(link = "logit"))
                             return(tempMod)
                             
                           })
        
        modelRes
        
      } else {
      
      myFormula <- as.formula( paste(input$varYreg, "~", paste(input$varsX, collapse = "+") ) )
      
      modelRes <- survey::svyglm(formula = myFormula, design = svyDesign,
                                 family=stats::binomial(link = "logit"))
      modelRes
      
      }
      
    } else if ( input$scaleLevel == "ordinal") {
      
      # does not work properly... restrict selection above
      
      if (length(as.character(input$varYreg)) > 1 ) {
        
        modelRes <- lapply(1:length(as.character(input$varYreg)),
                           function(x) {
                             
                             tempFormula <- as.formula( paste("as.factor(",input$varYreg,")"[x], "~", paste(input$varsX, collapse = "+") ) )
                             tempMod <- survey::svyolr(formula = tempFormula, design = svyDesign)
                             return(tempMod)
                             
                           })
        
        modelRes
        
      } else {
      
      myFormula <- as.formula( paste("as.factor(",input$varYreg,")", "~", paste(input$varsX, collapse = "+") ) )
      
      modelRes <- survey::svyolr(formula = myFormula, design = svyDesign)
      modelRes
      
      }
      
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
    
    if ( input$weightLav == "no weighting" ) { 
      
      lavWeight <- input$weightLav
      lavWeight <- NULL
    } else {
      lavWeight <- paste0(input$lavWeight)
    }
    
    
    
    tempLav <- lavaan::sem(myModel,
                           data = dataInput(),
                           cluster = lavCluster,
                           sampling.weights = lavWeight,
                           fixed.x = input$fixedx,
                           meanstructure = input$meanStr,
                           estimator = input$estimator,
                           missing = input$miss,
                           ordered = paste0(input$orderedLav))
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
  
  # UI Stuff ####

  output$dataOpt <- renderUI({
    
    if ( !is.null( input$file1$datapath ) ) {
    
    dataMis <- textInput("missVal", "Missing values",
                         placeholder = "Enter missing values separated by a semicolon.",
                         width = '60%')
    
    dataOptUi <- list(dataMis)
    dataOptUi
    } else {
      
      dataOptUi <- list()
    }
      
    
  })

  output$histUp = renderUI({
    
    if (input$hist == FALSE) {
      return(NULL)
      
    } else {
      
      histLabX <- textInput(inputId = "histlabX", "Label of x-Axis:", value = "Values", width = '100%', placeholder = NULL)
      histLabY <- textInput(inputId = "histlabY", "Label of y-Axis:", value = "Count", width = '100%', placeholder = NULL)
      
      histtextSize <- numericInput(inputId = "histAxisTextSize", label =  "Axis text size:",
                                   value = 20, step = 1, min = 1, max = 35, width = '100%')
      
      histUi <- list(h4("Cosmetics"),
                     fluidRow(
                      column(8,
                             histLabX,
                             histLabY),
                      column(4,
                             histtextSize))
      )
      histUi
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
  
  output$dotUi = renderUI({
    
    if (input$dotplot == FALSE) {
      return(NULL)
      
    } else {
      
      dotLabX <- textInput(inputId = "dotlabX", "Label of x-Axis:", value = "", width = '100%', placeholder = "Optional description of the x-axis.")
      dotLabY <- textInput(inputId = "dotlabY", "Label of y-Axis:", value = "Mean difference", width = '100%', placeholder = NULL)
      dottitle <- textInput(inputId = "dottitle", "Title of plot:", value = "", width = '100%', placeholder = NULL)
      dottextSize <- numericInput(inputId = "dotAxisTextSize", label =  "Axis text size:",
                                  value = 20, step = 1, min = 1, max = 35, width = '100%')
      dotTitleTextSize <- numericInput(inputId = "dotTitleTextSize", label =  "Title text size:",
                                       value = 25, step = 1, min = 1, max = 35, width = '100%')
      dotUi <- list(h4("Cosmetics"),
                    fluidRow(
                      column(8,
                             dotLabX,
                             dotLabY),
                      column(4,
                             dottextSize)),
                    fluidRow(
                      column(8,
                             dottitle),
                      column(4,
                             dotTitleTextSize)
                    )
      )
      dotUi
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
  
  
  
  
  # Download buttos ####
  
  output$downloadDescr <- downloadHandler(
    filename = "descriptive-output.docx",
    content = function(file) {
      tempDescr <- file.path(tempdir(), "descr-analysis.Rmd")
      file.copy("descr-analysis.Rmd", tempDescr, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(descrOut = calcDescr(),
                     hist = hist(),
                     descrCatOut = calcDescrCat())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempDescr, 
                        output_file = file,
                        output_format = "word_document",
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  
  output$downloadCor <- downloadHandler(
    filename = "correlation-output.docx",
    content = function(file) {
      tempCor <- file.path(tempdir(), "cor-analysis.Rmd")
      file.copy("cor-analysis.Rmd", tempCor, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(corOut = calcCorr(),
                     scatterPlot = scatter1Reac())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempCor, 
                        output_file = file,
                        output_format = "word_document",
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
      params <- list(tTestOut = calctTest(),
                     plot = dotplotReac())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(temptTest, 
                        output_file = file,
                        output_format = "word_document",
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
      rmarkdown::render(tempReg, 
                        output_file = file,
                        output_format = "word_document",
                        params = params,
                        envir = new.env(parent = globalenv())
      )
      
    }
  )
  
  
  output$tbl = DT::renderDataTable({
    
    # https://stackoverflow.com/questions/58526047/customizing-how-datatables-displays-missing-values-in-shiny
    rowCallback <- c(
      "function(row, data){",
      "  for(var i=0; i<data.length; i++){",
      "    if(data[i] === null){",
      "      $('td:eq('+i+')', row).html('NA')",
      "        .css({'color': 'rgb(237, 12, 12)', 'font-style': 'italic'});",
      "    }",
      "  }",
      "}"  
    )
    
    DT::datatable(dataInput(),
                  options = list(lengthChange = FALSE,
                                 rowCallback = DT::JS(rowCallback)),
                  editable = FALSE)

  })
  
  output$showDatInfo = renderPrint({
    
    if ( !is.null(dataInput()) & input$showDatInfo == TRUE) {
      str(dataInput())
    } 
    
  })
  

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
