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
# do remove/must contain for rule selection


# * support of 0 for dataset may hang the system 
#   -> include warning in docs? looks like that's what apriori does
#      -> see if shiny has popups
# * put the output (log) into a frame in shiny?
#   -> Logs when rules are remined
# * suppress warning messages? widget IDs will be fixed by plotly soon.
#   -> tried capture.output and that didn't work - not sure how to do this
# * should the parametes be parameters of apriori?
#   -> does not use "AParameter" object but can
# * name of the function?
#   -> Don't know
# * show number of rules somewhere (sidebar)?
#   -> Done
# * toggle remove/require/can only contain items.
#   -> mix remove with must contain at least one of
#
# control verbose false for apriori
# 

#shiny_arules <- function(x, support = 0.1, confidence = 0.8) {
shiny_arules <- function(x, parameter = NULL) {
  
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

  logOutput <- shiny::reactiveVal('Output log')

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
    maxLift <- 1
    lift <- 0
  
  }
  
  ## create Shiny UI and server code
  shiny::shinyApp(ui = shiny::shinyUI(shiny::pageWithSidebar(
    shiny::headerPanel("Association Rules"),
    
    shiny::sidebarPanel(
      
      shiny::htmlOutput('numRulesOutput'),
      shiny::conditionalPanel(
        condition = "input.mytab %in% c('grouped', 'graph', 'datatable', 'scatter', 'paracoord', 'matrix')",
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
        shiny::selectInput('colsType',NULL,c('Remove rules including:'='rem','Require rules to include:'='req')),
        shiny::uiOutput("choose_columns"), 
        shiny::selectInput('colsLHSType',NULL,c('Remove rules with LHS including:'='rem','Require rules to have LHS include:'='req')),
        shiny::uiOutput("choose_lhs"), 
        shiny::selectInput('colsRHSType',NULL,c('Remove rules with RHS including:'='rem','Require rules to have RHS include:'='req')),
        shiny::uiOutput("choose_rhs"), 
        shiny::br(),
        shiny::downloadButton('downloadData', 'Download Rules as CSV'),
        shiny::verbatimTextOutput('logOutput')
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
    

    output$numRulesOutput <- shiny::renderUI({
        HTML(paste('<b>',length(rules()),'rules selected</b>'))
    })
    output$logOutput <- shiny::renderText({
        logOutput()
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
      
      rules <- apriori(dataset, parameter=list(
        support=as.numeric(input$supp), 
        confidence=as.numeric(input$conf), 
        minlen=input$minL, 
        maxlen=input$maxL),
        control = list(verbose = T))
      quality(rules) <- interestMeasure(rules, transactions = dataset)
      
      message("Remined ", length(rules), " rules.")
      lo <- paste(logOutput(),'\nRemined',length(rules),'rules.')
      logOutput(lo)
      
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
        if((tempSupp == 0 && input$supp < cachedSupp) || input$conf < cachedConf) remineRules()
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
        shiny::need(length(ar) > 0, "No matching rules found!")
      )
      
      ar
      
    }
    )
    
    # remember settings for other plots
    shiny::observe({ shiny::req(input$xAxis); xIndexCached <<- input$xAxis })
    shiny::observe({ shiny::req(input$yAxis); yIndexCached <<- input$yAxis })
    shiny::observe({ shiny::req(input$cAxis); zIndexCached <<- input$cAxis })

    tempSupp <- 0
    warn <- TRUE
    shiny::observeEvent(input$supp, {
        if(input$supp <= 0.2) {
            if(warn) {
                tempSupp <<- input$supp
                shiny::updateSliderInput(session,"supp",value = cachedSupp, min=minSupp, max=maxSupp, step = (maxSupp-minSupp)/10000)
                showModal(modalDialog(
                    title='Warning',
                    'Warning - low support could result in long computation time.
                    Click cancel to reset and continue to continue.',
                    footer=tagList(
                        actionButton('cancel','cancel'),
                        actionButton('continue','continue')
                    )
                    ))
            } else {
                warn <<- TRUE
            }
        }
    });
    shiny::observeEvent(input$continue, {
            shiny::updateSliderInput(session,"supp",value = tempSupp, min=minSupp, max=maxSupp, step = (maxSupp-minSupp)/10000)
            warn <<- FALSE
            tempSupp <<- 0
            removeModal()
        }
    )
    shiny::observeEvent(input$cancel, {
            tempSupp <<- 0
            removeModal()
        }
    )
    
    
    # Present errors nicely to the user
   handleErrors <- shiny::reactive({
#      ar <- rules()
#      shiny::validate(
#        shiny::need(nR() <= length(ar), 'Please decrease the number of rules')
#      )
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
