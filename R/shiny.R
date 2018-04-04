#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2018 Michael Hahsler et al
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
#
# Adapted from code written by Andrew Brooks and found at 
#                 https://github.com/brooksandrew/Rsenal
#

### TODO:
#
# * scatterplot: presets for x, y, and z are not kept for other plots
# * Error with 0 rules
# * support of 0 
# * Can the log go into a frame in shiny?
# * add shading selector to grouped and matrix
# * error when using a plot for the first time then reloads.
# * suppress warning messages?


shiny_arules <- function(x, support = 0.1, confidence = 0.8) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package shiny is required to run this method.", call. = FALSE)
  }
  roundUp <- function(x,digits = 3) round(x+.5*10^-digits, digits)
  roundDown <- function(x,digits = 3) round(x-.5*10^-digits, digits)
  
  ### dataset can be rules or transactions
  dataset <- x
  supp <- support
  conf <- confidence
  
  ### make sure we have transactions or rules
  if(is(dataset, "data.frame")) {
    dataset <- discretizeDF(dataset)
    dataset <- as(dataset, "transactions")
  }
  
  ### default measures to use
  xIndexCached <- "support"
  yIndexCached <- "confidence"
  zIndexCached <- "lift"

  if(is(dataset, "rules")) {
    minSupp <- roundDown(min(quality(dataset)$support), 3)
    maxSupp <- roundUp(max(quality(dataset)$support), 3)
    minConf <- roundDown(min(quality(dataset)$confidence), 3)
    maxConf <- roundUp(max(quality(dataset)$confidence), 3)
    minLift <- floor(min(quality(dataset)$lift))
    maxLift <- ceiling(max(quality(dataset)$lift))
    
    supp <- minSupp
    conf <- minConf
    lift <- minLift
  } else {  
    ### transactions
    minSupp <- 0
    maxSupp <- 1
    minConf <- 0
    maxConf <- 1
    minLift <- 0
    maxLift <- 1
    lift <- 0
  
  }
  
  ## create Shiny UI and server code
  shiny::shinyApp(ui = shiny::shinyUI(shiny::pageWithSidebar(
    shiny::headerPanel("Association Rules"),
    
    shiny::sidebarPanel(
      
      shiny::conditionalPanel(
        condition = "input.mytab %in%' c('grouped', 'graph', 'datatable', 'scatter', 'paracoord', 'matrix')",
        shiny::uiOutput("kSelectInput"),
        shiny::uiOutput("xAxisSelectInput"),
        shiny::uiOutput("yAxisSelectInput"),
        shiny::uiOutput("cAxisSelectInput"),
        shiny::br(),
        shiny::sliderInput("supp", "Support:", min = minSupp, max = maxSupp, value = supp , step = (maxSupp-minSupp)/1000, sep =""),
        shiny::sliderInput("conf", "Confidence:", min = minConf, max = maxConf, value = conf , step =  (maxConf-minConf)/1000, sep = ""), 
        shiny::sliderInput("lift", "Lift:", min = minLift, max = maxLift, value = lift , step =  (maxLift-minLift)/1000, sep = ""), 
        shiny::numericInput("minL", "Min. items in rule:", 2), 
        shiny::numericInput("maxL", "Max. items in rule:", 10), 
        shiny::br(),
        shiny::uiOutput("choose_columns"), 
        shiny::uiOutput("choose_lhs"), 
        shiny::uiOutput("choose_rhs"), 
        shiny::br(),
        shiny::downloadButton('downloadData', 'Download Rules as CSV')
      )
      
      
    ),
    
    shiny::mainPanel(
      shiny::tabsetPanel(id='mytab',
                         shiny::tabPanel('Data Table', value='datatable', shiny::dataTableOutput("rulesDataTable")),
                         shiny::tabPanel('Scatter', value='scatter', plotlyOutput("scatterPlot", width='100%', height='100%')),
                         shiny::tabPanel('Matrix', value='matrix', plotlyOutput("matrixPlot", width='100%', height='100%')),
                         shiny::tabPanel('Grouped', value='grouped', shiny::plotOutput("groupedPlot", width='100%', height='100%')),
                         shiny::tabPanel('Graph', value='graph', visNetworkOutput("graphPlot", width='100%', height='800px'))
      )
    )
    
  )),
  
  server = function(input, output, session) {
    
    output$kSelectInput <- shiny::renderUI({
      if(input$mytab == 'grouped') {
        shiny::sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15)
      }
    })
    
    output$xAxisSelectInput <- shiny::renderUI({
      if(input$mytab == 'scatter') {
        shiny::selectInput("xAxis","X Axis:",xNames(), selected=xIndexCached)
      }
    })
    
    output$yAxisSelectInput <- shiny::renderUI({
      if(input$mytab == 'scatter') {
        shiny::selectInput("yAxis","Y Axis:",yNames(), selected=yIndexCached)
      }
    })
    
    output$cAxisSelectInput <- shiny::renderUI({
      if(input$mytab == 'scatter' || input$mytab == 'matrix') {
        shiny::selectInput("cAxis","Shading:",cNames(), selected=zIndexCached)
      }
    })
    
    output$choose_columns <- shiny::renderUI({
        shiny::selectizeInput('cols','Remove rules including:',
                              itemLabels(dataset),
                              multiple = TRUE)
    })
    
    
    output$choose_lhs <- shiny::renderUI({
      shiny::selectizeInput('colsLHS','Remove rules with LHS including:',
                            itemLabels(dataset),
                            multiple = TRUE)
    })
    
    output$choose_rhs <- shiny::renderUI({
      shiny::selectizeInput('colsRHS','Remove rules with RHS including:',
                            itemLabels(dataset),
                            multiple = TRUE)
    })
    
    ## caching data
    cachedRules <- NULL
    cachedSupp <- supp
    cachedConf <- conf
    cachedLift <- lift
    cachedMinL <- minLift
    cachedMaxL <- maxLift
    
    if(is(dataset, "rules")) {
      cachedRules <- dataset
      cachedSupp <<- info(dataset)$support
      cachedConf <<- info(dataset)$confidence
      cachedLift <<- min(quality(dataset)$Lift)
      cachedMinL <<- min(size(dataset))
      cachedMaxL <<- max(size(dataset))
    }
    
    ### remine rules if necessary dataset is transactions!
    remineRules <- shiny::reactive({
      
      rules <- apriori(dataset, parameter=list(
        support=as.numeric(input$supp), 
        confidence=as.numeric(input$conf), 
        minlen=input$minL, 
        maxlen=input$maxL),
        control = list(verbose = T))
      quality(rules) <- interestMeasure(rules, transactions = dataset)
      
      message("Remined ", length(rules), " rules.")
      
      cachedRules <<- rules
      cachedSupp <<- input$supp
      cachedConf <<- input$conf
      cachedLift <<- input$lift
      cachedMinL <<- input$minL
      cachedMaxL <<- input$maxL
    }
    )
    
    rules <- shiny::reactive({
      
      ### recalculate rules?
      if(is(dataset, 'transactions')) {
        if(is.null(cachedRules)) remineRules()
        if(input$supp < cachedSupp || input$conf < cachedConf) remineRules()
        if(input$minL < cachedMinL || input$maxL > cachedMaxL) remineRules()
      }
      
      ar <- cachedRules
      
      if(input$supp > cachedSupp) {
        ar <- subset(ar, subset = support > input$supp)
      }
      
      if(input$conf > cachedConf) {
        ar <- subset(ar, subset = confidence > input$conf)
      }
      
      if(input$lift > cachedLift) {
        ar <- subset(ar, subset= lift > input$lift)
      }
      
      if(input$minL > cachedMinL) {
        ar <- ar[size(ar) >= input$minL]   
      }
      
      if(input$maxL < cachedMaxL) {
        ar <- ar[size(ar) <= input$maxL]   
      }
      
      if(length(input$cols) > 0) {
        ar <- subset(ar, subset=!(items %in% input$cols))
      }

      
      if(length(input$colsLHS) > 0) {
        ar <- subset(ar, subset=!(lhs %in% input$colsLHS))
      }
      
      if(length(input$colsRHS) > 0) {
        ar <- subset(ar, subset=!(rhs %in% input$colsRHS))
      }
      
      
      ### update lift slider
      #maxLift <<- ceiling(max(quality(ar)$lift))
      #shiny::updateSliderInput(session,"lift",value = input$lift, min=minLift, max=maxLift, step = 1/10000)
      
      shiny::validate(
        shiny::need(length(ar) > 0, "No matching rules found!")
      )
      
      ar
      
    }
    )
    
    # Rule length
    xNames <- shiny::reactive({ colnames(quality(rules())) })
    yNames <- shiny::reactive({ colnames(quality(rules())) })
    cNames <- shiny::reactive({ colnames(quality(rules())) })
    
    # this sets them to support
    #shiny::observe({ xIndexCached <<- input$xAxis })
    #shiny::observe({ yIndexCached <<- input$yAxis })
    #shiny::observe({ zIndexCached <<- input$cAxis })
    
    
    # Present errors nicely to the user
   handleErrors <- shiny::reactive({
#      ar <- rules()
#      shiny::validate(
#        shiny::need(nR() <= length(ar), 'Please decrease the number of rules')
#      )
    })
    
    
    ## Grouped Plot #########################
    output$groupedPlot <- shiny::renderPlot({
      handleErrors()
      
      plot(rules(), method='grouped', control=list(k=input$k))
    }, height=800, width=800)
    
    
    ## Graph Plot ##########################
    output$graphPlot <- renderVisNetwork({
      handleErrors()
      
      plt <- plot(rules(), method='graph', engine='htmlwidget')
      
      plt$sizingPolicy <- htmlwidgets::sizingPolicy(
        viewer.paneHeight=1000,
        browser.defaultHeight=1000,
        knitr.defaultHeight=1000,
        defaultHeight=1000,defaultWidth=1000,
        browser.fill=TRUE
      )
      plt$height <- 1000
      plt$x$height <- 1000
      plt
    })
    
    
    ## Scatter Plot ##########################
    output$scatterPlot <- renderPlotly({
      handleErrors()
      
      plotly_arules(rules(), method = 'scatterplot',
                    measure=c(input$xAxis, input$yAxis), shading = input$cAxis)
      })
    
    
    ## Parallel Coordinates Plot ###################
    output$paracoordPlot <- shiny::renderPlot({
      handleErrors()
      
      plot(rules(), method='paracoord')
    }, height=800, width=800)
    
    
    ## Matrix Plot ###################
    output$matrixPlot <- renderPlotly({
      handleErrors()
      
      plotly_arules(rules(), method='matrix', shading=input$cAxis)
    })
    
    ## Rules Data Table ##########################
    output$rulesDataTable <- shiny::renderDataTable({
      handleErrors()
      
      
      x <- rules()
      data.frame(LHS = labels(lhs(x)), RHS = labels(rhs(x)), quality(x))
    }, options = list(filter = "top", rownames = paste0('[', 1:length(x), ']'))
    )
    
    ## Download data to csv ########################
    output$downloadData <- shiny::downloadHandler(
      filename = 'arules_data.csv',
      content = function(file) { write.csv(as(rules(), "data.frame"), file) }
    )
    
    
  }
  )
}
