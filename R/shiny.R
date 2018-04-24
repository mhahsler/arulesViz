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

ruleExplorer <- function(x, parameter = NULL) {
  
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package shiny is required to run this method.", call. = FALSE)
  }
  roundUp <- function(x,digits = 3) round(x+.5*10^-digits, digits)
  roundDown <- function(x,digits = 3) round(x-.5*10^-digits, digits)
  
  ### dataset can be rules or transactions
  dataset <- x
  aparameter <- as(parameter,'APparameter')
  supp <- aparameter@support
  conf <- aparameter@confidence
  
  ### make sure we have transactions or rules
  if(is(dataset, "data.frame")) {
    dataset <- discretizeDF(dataset)
    dataset <- as(dataset, "transactions")
  }
  
  ### default measures to use
  xIndexCached <- "support"
  yIndexCached <- "confidence"
  zIndexCached <- "lift"
  
  #logOutput <- shiny::reactiveVal('Output log')
  
  if(is(dataset, "rules")) {
    if(length(dataset) < 1) stop("Zero rules provided!")
    
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
    maxLift <- 25
    lift <- 0
    
  }
  
  ## create Shiny UI and server code
  shiny::shinyApp(ui = shiny::shinyUI(shiny::fluidPage(
    #shiny::titlePanel("Association Rules"),
    
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        
        shiny::htmlOutput('numRulesOutput'),
        shiny::br(),
        shiny::uiOutput("kSelectInput"),
        shiny::uiOutput("xAxisSelectInput"),
        shiny::uiOutput("yAxisSelectInput"),
        shiny::uiOutput("cAxisSelectInput"),
        shiny::br(),
        shiny::sliderInput("supp", "Minimum Support:", min = minSupp, max = maxSupp, value = supp , step = (maxSupp-minSupp)/1000, sep =""),
        shiny::sliderInput("conf", "Minimum Confidence:", min = minConf, max = maxConf, value = conf , step =  (maxConf-minConf)/1000, sep = ""), 
        shiny::sliderInput("lift", "Minimum Lift:", min = minLift, max = maxLift, value = lift , step =  (maxLift-minLift)/1000, sep = ""), 
        shiny::numericInput("minL", "Min. items in rule:", 2), 
        shiny::numericInput("maxL", "Max. items in rule:", 10), 
        shiny::br(),
        shiny::HTML('<b>Filter rules by items:</b>'),
        shiny::br(),
        shiny::selectInput('colsType',NULL,c('Exclude items:'='rem','Require items:'='req')),
        shiny::uiOutput("choose_columns"), 
        shiny::selectInput('colsLHSType',NULL,c('Exclude items from LHS:'='rem','Require items in LHS:'='req')),
        shiny::uiOutput("choose_lhs"), 
        shiny::selectInput('colsRHSType',NULL,c('Exclude items from RHS:'='rem','Require items in RHS:'='req')),
        shiny::uiOutput("choose_rhs"), 
        shiny::br(),
        shiny::downloadButton('rules.csv', 'Download Rules as CSV')
        #,
      #  shiny::br(),
      #  shiny::verbatimTextOutput('logOutput')
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
    ) 
  )),
    
    server = function(input, output, session) {
      
      
      output$numRulesOutput <- shiny::renderUI({
        shiny::HTML(paste('<b>', 'Rules selected: ', length(rules()), '</b>'))
      })

      output$kSelectInput <- shiny::renderUI({
        if(input$mytab == 'grouped') {
          shiny::sliderInput('k', label='Choose # of rule clusters', min=1, max=50, step=1, value=15)
        }
      })
      
      output$xAxisSelectInput <- shiny::renderUI({
        if(input$mytab == 'scatter') {
          shiny::selectInput("xAxis","X Axis:", colnames(quality(rules())), selected=xIndexCached)
        }
      })
      
      output$yAxisSelectInput <- shiny::renderUI({
        if(input$mytab == 'scatter') {
          shiny::selectInput("yAxis","Y Axis:", colnames(quality(rules())), selected=yIndexCached)
        }
      })
      
      output$cAxisSelectInput <- shiny::renderUI({
        if(input$mytab %in% c('scatter', 'matrix', 'graph', 'grouped')) {
          shiny::selectInput("cAxis","Shading:", colnames(quality(rules())), selected=zIndexCached)
        }
      })
      
      output$choose_columns <- shiny::renderUI({
        shiny::selectizeInput('cols',NULL,
          itemLabels(dataset),
          multiple = TRUE)
      })
      
      
      output$choose_lhs <- shiny::renderUI({
        shiny::selectizeInput('colsLHS',NULL,
          itemLabels(dataset),
          multiple = TRUE)
      })
      
      output$choose_rhs <- shiny::renderUI({
        shiny::selectizeInput('colsRHS',NULL,
          itemLabels(dataset),
          multiple = TRUE)
      })
      
      #output$logOutput <- shiny::renderText({
      #  logOutput()
      #})
      
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
        cachedLift <<- min(quality(dataset)$lift)
        cachedMinL <<- min(size(dataset))
        cachedMaxL <<- max(size(dataset))
      }
      
      ### remine rules if necessary dataset is transactions!
      remineRules <- shiny::reactive({
       
        ### use a minimum of 1 absolute support!
        supp <- input$supp
        if(supp == 0) supp <- 1/length(dataset)
         
        rules <- apriori(dataset, parameter = list(
          support = as.numeric(supp), 
          confidence = as.numeric(input$conf), 
          minlen = input$minL, 
          maxlen = input$maxL),
          control = list(verbose = TRUE))
        quality(rules) <- interestMeasure(rules, transactions = dataset)
        
        message("Remined ", length(rules), " rules.")
        #lo <- paste(logOutput(),'\nRemined',length(rules),'rules.')
        #logOutput(lo)
        
        cachedRules <<- rules
        cachedSupp <<- input$supp
        cachedConf <<- input$conf
        cachedLift <<- input$lift
        cachedMinL <<- input$minL
        cachedMaxL <<- input$maxL
      })
     
      ### handle warning for too low support
      override <- shiny::reactiveVal(FALSE)
      
      shiny::observeEvent(input$cancel, {
        shiny::removeModal() 
        # reset the slider (Note this does not change input$supp!)
        shiny::updateSliderInput(session, "supp", value = cachedSupp)
      })

      shiny::observeEvent(input$continue, { 
        shiny::removeModal() 
        override(TRUE)
      })
          
      rules <- shiny::reactive({
        ### recalculate rules?
        
        if(is(dataset, 'transactions')) {
          
        #  cat("input supp: ", input$supp, "\n")  
        #  cat("cached supp: ", cachedSupp, "\n")  
        #  cat("abs supp: ", input$supp*length(dataset), "\n")  
        #  cat("override: ", override(), "\n")  
          
          # check for low minimum support first
          if(input$supp*length(dataset) > 10 || override()) {
        
            if(is.null(cachedRules)) remineRules()
            if((input$supp < cachedSupp) || input$conf < cachedConf) remineRules()
            if(input$minL < cachedMinL || input$maxL > cachedMaxL) remineRules()
            
          } else { 
              shiny::showModal(shiny::modalDialog(
                title='Warning',
                'Very low minimum support! Too low values can result in long wait times and memory issues.',
                footer=shiny::tagList(
                  shiny::actionButton('cancel','cancel'),
                  shiny::actionButton('continue','proceed')
                )
              ))
          }
        }
        
        ar <- cachedRules
        
        if(input$supp > cachedSupp) {
          ar <- subset(ar, subset = support > input$supp)
        }
       
        if(input$conf > cachedConf) {
          ### FIXME: R CMD check complains about: 
          ### shiny_arules : <anonymous>: no visible binding for global variable 'confidence' 
          #ar <- subset(ar, subset = quality(ar)$confidence > input$conf)
          ar <- subset(ar, subset = quality(ar)$confidence > input$conf)
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
        
        if(input$colsType == 'rem' && length(input$cols) > 0) {
          ar <- subset(ar, subset=!(items %in% input$cols))
        }
        
        if(input$colsType == 'req' && length(input$cols) > 0) {
          ar <- subset(ar, subset=items %in% input$cols)
        }
        
        
        if(input$colsLHSType == 'rem' && length(input$colsLHS) > 0) {
          ar <- subset(ar, subset=!(lhs %in% input$colsLHS))
        }
        
        if(input$colsLHSType == 'req' && length(input$colsLHS) > 0) {
          ar <- subset(ar, subset=lhs %in% input$colsLHS)
        }
        
        if(input$colsRHSType == 'rem' && length(input$colsRHS) > 0) {
          ar <- subset(ar, subset=!(rhs %in% input$colsRHS))
        }
        
        if(input$colsRHSType == 'req' && length(input$colsRHS) > 0) {
          ar <- subset(ar, subset=rhs %in% input$colsRHS)
        }
        
        
        ### update lift slider
        #maxLift <<- ceiling(max(quality(ar)$lift))
        #shiny::updateSliderInput(session,"lift",value = input$lift, min=minLift, max=maxLift, step = 1/10000)
        
        shiny::validate(
          #shiny::need(length(ar) > 0, "No matching rules found!")
        )
        
        ar
        
      }
      )
      
      # remember settings for other plots
      shiny::observe({ shiny::req(input$xAxis); xIndexCached <<- input$xAxis })
      shiny::observe({ shiny::req(input$yAxis); yIndexCached <<- input$yAxis })
      shiny::observe({ shiny::req(input$cAxis); zIndexCached <<- input$cAxis })
      
      
      # Present errors nicely to the user
      handleErrors <- shiny::reactive({
        shiny::validate(
          shiny::need(length(rules())>0, 'No rules to visualize! Decrease support, confidence or lift.')
        )
      })
      
      
      ## Grouped Plot #########################
      output$groupedPlot <- shiny::renderPlot({
        shiny::req(input$cAxis, input$k)
        handleErrors()
        
        plot(rules(), method='grouped', shading = input$cAxis, control=list(k=input$k))
      }, height=800, width=800)
      
      
      ## Graph Plot ##########################
      output$graphPlot <- renderVisNetwork({
        shiny::req(input$cAxis)
        handleErrors()
        
        plt <- plot(rules(), method='graph', shading = input$cAxis, engine='htmlwidget')
        
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
        shiny::req(input$xAxis, input$yAxis, input$cAxis)
        handleErrors()
        
        .plotly_arules(rules(), method = 'scatterplot',
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
        
        .plotly_arules(rules(), method='matrix', shading=input$cAxis)
      })
      
      ## Rules Data Table ##########################
      output$rulesDataTable <- shiny::renderDataTable({
        handleErrors()
        
        
        x <- rules()
        data.frame(LHS = labels(lhs(x)), RHS = labels(rhs(x)), quality(x))
      }, options = list(filter = "top", rownames = paste0('[', 1:length(x), ']'))
      )
      
      ## Download data to csv ########################
      output$rules.csv <- shiny::downloadHandler(
        filename = 'rules.csv',
        content = function(file) { 
          x <- rules()
          data <- data.frame(LHS = labels(lhs(x)), RHS = labels(rhs(x)), quality(x))
          write.csv(data, file) 
        }
      )
      
      
    }
  )
}
