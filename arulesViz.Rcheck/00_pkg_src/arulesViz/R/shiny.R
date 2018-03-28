#######################################################################
# arulesViz - Visualizing Association Rules and Frequent Itemsets
# Copyrigth (C) 2011 Michael Hahsler and Sudheer Chelluboina
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
# Adapted from code written by Andrew Brooks and found at https://github.com/brooksandrew/Rsenal


# Used to export rules as CSV file and put rules into datatable
rules2df <- function(rules, list=F){
  df <- as(rules, 'data.frame')
  df[,1] <- as.character(df[,1])
  df$lhs <- sapply(df[,1], function(x) strsplit(x, split=' => ')[[1]][1])
  df$rhs <- sapply(df[,1], function(x) strsplit(x, split=' => ')[[1]][2])
  df$lhs <- gsub(pattern='\\{', replacement='', x=df$lhs)
  df$lhs <- gsub(pattern='}', replacement='', x=df$lhs)
  df$rhs <- gsub(pattern='\\{', replacement='', x=df$rhs)
  df$rhs <- gsub(pattern='}', replacement='', x=df$rhs)

  if(list==T){
    p <- rules@lhs@data@p
    i <- rules@lhs@data@i+1
    lhsItems <- unlist(rules@lhs@itemInfo@.Data)
    lhsL <- list()
    for(j in 2:length(p)) lhsL[[j-1]] <- lhsItems[i[(p[j-1]+1):(p[j])]]
    df$lhs <- lhsL

    p <- rules@rhs@data@p
    i <- rules@rhs@data@i+1
    rhsItems <- unlist(rules@rhs@itemInfo@.Data)
    rhsL <- list()
    for(j in 2:length(p)) rhsL[[j-1]] <- rhsItems[i[(p[j-1]+1):(p[j])]]
    df$rhs <- rhsL
  }
  return(df)
}

# Used for binning of numeric data
depthbin <- function(ser, nbins=10, qtype=7, digits=10, labelRange=T, labelPct=F, labelOrder=F) {
  cutpts <- quantile(ser, probs=seq(0, 1, 1/nbins), na.rm=T, type=qtype)
  if(length(unique(cutpts))==nbins+1) {
    returnser <- cut(ser, breaks=cutpts, right=T, include.lowest=T)
  } else {
    alldup <- vector()
    while(length(unique(cutpts))+length(alldup) < nbins+1) {
      dup <- cutpts[duplicated(cutpts)]
      dups <- unique(dup)
      alldup <- c(alldup, dups)
      dupL <- length(alldup) + length(dups)
      ser2 <- ser[which(!ser %in% alldup)]
      cutpts <- quantile(ser2, probs=seq(0, 1, 1/(nbins-length(dups))), na.rm=T, type=qtype)
    }
    cutpts <- c(unique(cutpts), alldup)
    returnser <- cut(ser, breaks=cutpts, include.lowest=T, dig.lab=digits, right=F)
  }
  if(sum(labelRange, labelPct, labelOrder)==0) {
    labelRange <- T
    warning('arguments labelRange, labelOrder, labelPct should not all be set to FALSE. Setting labelRange to TRUE.')
  }
  rawlev <- levels(returnser)
  if (labelRange==T) levels(returnser) <- paste0(levels(returnser), rawlev)
  if (labelOrder==T) levels(returnser) <- paste0(levels(returnser), ' ', 1:length(rawlev), '/', length(rawlev))
  if (labelPct==T) levels(returnser) <- paste0(levels(returnser), ' ', paste0('(', as.character(round(table(returnser)/length(returnser)*100, 1)), '%)'))
  for(i in 1:length(levels(returnser))) levels(returnser)[i] <- substr(levels(returnser)[i], nchar(rawlev[i])+1, nchar(levels(returnser)[i]))
  return(returnser)
}

shiny_arules <- function (dataset, vars=0, supp=0.1, conf=0.5, lift=0.0) {

  if (!requireNamespace("shiny",quietly=TRUE)) {
      stop("Package shiny is required to run this method.",call.=FALSE)
  }

  numVars <- length(colnames(dataset))
  minSupp <- 0
  maxSupp <- 1
  minConf <- 0
  maxConf <- 1
  minLift <- 0
  maxLift <- 1
  roundUp <- function(x,y) ceiling(x*(10^y))/(10^y)
  roundDown <- function(x,y) floor(x*(10^x))/(10^y)
  xIndexCached <- "support"
  yIndexCached <- "confidence"
  zIndexCached <- "lift"

  if(class(dataset)=='rules') {
      minSupp <- roundDown(min(dataset@quality$support),3)
      maxSupp <- roundUp(max(dataset@quality$support),3)
      minConf <- roundDown(min(dataset@quality$confidence),3)
      maxConf <- roundUp(max(dataset@quality$confidence),3)
      minLift <- floor(min(dataset@quality$lift))
      maxLift <- ceiling(max(dataset@quality$lift))
      if(vars==0) {
          vars <- length(itemLabels(dataset))
      }
      supp <- minSupp
      conf <- minConf
      lift <- minLift
      numVars <- length(itemLabels(dataset))
  }
  ## binning numeric data
  if(class(dataset)=='transactions'){
      if(vars==0){
          vars <- length(colnames(dataset))
      }
      for(i in 1:ncol(dataset)) {
        if(class(dataset[,i]) %in% c('numeric', 'integer')) dataset[,i] <- depthbin(dataset[,i], nbins=10)
      }
  }

  ## calling Shiny App
  shiny::shinyApp(ui = shiny::shinyUI(shiny::pageWithSidebar(
    shiny::headerPanel("Association Rules"),

    shiny::sidebarPanel(

      shiny::conditionalPanel(
        condition = "input.samp=='Sample'",
        shiny::numericInput("nrule", 'Number of Rules', 5, min=2), shiny::br()
      ),


      shiny::conditionalPanel(
        condition = "input.lhsv=='Subset'",
        shiny::uiOutput("choose_lhs"),shiny::br()
      ),

      shiny::conditionalPanel(
        condition = "input.rhsv=='Subset'",
        shiny::uiOutput("choose_rhs"), shiny::br()
      ),

      shiny::conditionalPanel(
        condition = "input.mytab=='grouped'",
        shiny::sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15), shiny::br()
      ),
      shiny::conditionalPanel(
        condition = "input.mytab=='itemFreq'",
        shiny::sliderInput('itemFreqN', label='Choose # of items', min=1, max=numVars, step=1, value=30), shiny::br()
      ),

      shiny::conditionalPanel(
        condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')",
        shiny::uiOutput("choose_columns"), shiny::br(),
        shiny::uiOutput("xAxisSelectInput"),
        shiny::uiOutput("yAxisSelectInput"),
        shiny::uiOutput("cAxisSelectInput"),
        shiny::sliderInput("supp", "Support:", min = minSupp, max = maxSupp, value = supp , step = 1/10000),
        shiny::sliderInput("conf", "Confidence:", min = minConf, max = maxConf, value = conf , step = 1/10000), 
        shiny::sliderInput("lift", "Lift:", min = minLift, max = maxLift, value = lift , step = 1/10000), 
        shiny::numericInput("minL", "Min. items per set:", 2), 
        shiny::numericInput("maxL", "Max. items per set:", 3), 
        shiny::radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), shiny::br(),
        shiny::radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), shiny::br(),
        shiny::downloadButton('downloadData', 'Download Rules as CSV')
      )

    ),

    shiny::mainPanel(
      shiny::tabsetPanel(id='mytab',
                  shiny::tabPanel('Data Table', value='datatable', shiny::dataTableOutput("rulesDataTable")),
                  shiny::tabPanel('Scatter', value='scatter', plotlyOutput("scatterPlot", width='100%', height='100%')),
                  shiny::tabPanel('Graph', value='graph', visNetworkOutput("graphPlot", width='100%', height='800px')),
                  shiny::tabPanel('ItemFreq', value='itemFreq', shiny::plotOutput("itemFreqPlot", width='100%', height='100%')),
                  shiny::tabPanel('Grouped', value='grouped', shiny::plotOutput("groupedPlot", width='100%', height='100%')),
                  shiny::tabPanel('Matrix', value='matrix', plotlyOutput("matrixPlot", width='100%', height='100%'))
      )
    )

   )),

   server = function(input, output, session) {

     output$xAxisSelectInput <- shiny::renderUI({
        if(input$mytab == 'scatter') {
            shiny::selectInput("xAxis","X Axis:",xNames(),selected=xIndexCached)
        }
     })

     output$yAxisSelectInput <- shiny::renderUI({
        if(input$mytab == 'scatter') {
            shiny::selectInput("yAxis","Y Axis:",yNames(),selected=yIndexCached)
        }
     })

     output$cAxisSelectInput <- shiny::renderUI({
        if(input$mytab == 'scatter' || input$mytab == 'matrix') {
            shiny::selectInput("cAxis","Shading:",cNames(),selected=zIndexCached)
        }
     })

     output$choose_columns <- shiny::renderUI({
       if(class(dataset)=='transactions'){
           shiny::selectizeInput('cols','Choose variables to remove:',
                          colnames(dataset),
                          multiple = TRUE)
       } else if(class(dataset)=='rules'){
           shiny::selectizeInput('cols','Choose variables:',
                          colnames(dataset),
                          multiple = TRUE)
       }
     })


     shiny::observe({
         xIndexCached <<- input$xAxis
     })
     shiny::observe({
         yIndexCached <<- input$yAxis
     })
     shiny::observe({
         zIndexCached <<- input$cAxis
     })

     output$choose_lhs <- shiny::renderUI({
       cols <- colnames(dataset)
       cols <- cols[!(cols %in% input$cols)]
       shiny::selectizeInput('colsLHS','Choose LHS variables to remove:',
                      cols,
                      multiple = TRUE)
     })

     output$choose_rhs <- shiny::renderUI({
       cols <- colnames(dataset)
       cols <- cols[!(cols %in% input$cols)]
       shiny::selectizeInput('colsRHS','Choose RHS variables to remove:',
                      cols,
                      multiple = TRUE)
     })

     ## Extracting and Defining arules
     cachedRules <- new("rules",lhs=new("itemMatrix"),rhs=new("itemMatrix"))
     cachedVars <- vars
     cachedSupp <- 0
     cachedConf <- 0
     cachedLift <- 0
     cachedMinL <- 0
     cachedMaxL <- 0
     recalculateRules <- shiny::reactive({
       cols <- colnames(dataset)
       cols <- cols[!(cols %in% input$cols)]
       tr <- as(dataset[,cols], 'transactions')
       cachedRules <<- apriori(tr, parameter=list(support=as.numeric(input$supp), confidence=as.numeric(input$conf), minlen=input$minL, maxlen=input$maxL))
       cachedVars <<- length(cols)
       cachedSupp <<- input$supp
       cachedConf <<- input$conf
       cachedLift <<- input$lift
       cachedMinL <<- input$minL
       cachedMaxL <<- input$maxL
     })
     rules <- shiny::reactive({
       if(class(dataset)=='transactions') {
           cols <- colnames(dataset)
           cols <- cols[!(cols %in% input$cols)]
           tr <- as(dataset[,cols], 'transactions')

           if(length(cachedRules)==0  || length(cols) > cachedVars) {
               recalculateRules()
           }
           arAll <- cachedRules
           if(length(cols) < cachedVars) {
               arAll <- subset(arAll, subset=lhs %in% cols & rhs %in% cols)
           }
           if(input$supp > cachedSupp) {
               arAll <- subset(arAll, subset=arAll@quality$support>input$supp)
           } else if(input$supp < cachedSupp) {
               recalculateRules()
               arAll <- cachedRules
           }
           if(input$conf > cachedConf) {
               arAll <- subset(arAll, subset=arAll@quality$confidence>input$conf)
           } else if(input$conf < cachedConf) {
               recalculateRules()
               arAll <- cachedRules
           }
           if(input$lift != cachedLift) {
               arAll <- subset(arAll, subset=lift>input$lift)
               cachedLift <<- input$lift
           }
           if(input$minL != cachedMinL) {
               recalculateRules()
               arAll <- cachedRules
           }
           if(input$maxL != cachedMaxL) {
               recalculateRules()
               arAll <- cachedRules
           }
           if(input$rhsv=='Subset' & input$lhsv!='Subset'){
             rhsCols <- colnames(dataset)
             rhsCols <- rhsCols[!(rhsCols %in% input$cols)]
             rhsCols <- rhsCols[!(rhsCols %in% input$colsRHS)]
             ar <- subset(arAll, subset=rhs %in% rhsCols)

           } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
             lhsCols <- colnames(dataset)
             lhsCols <- lhsCols[!(lhsCols %in% input$cols)]
             lhsCols <- lhsCols[!(lhsCols %in% input$colsLHS)]
             ar <- subset(arAll, subset=lhs %in% lhsCols)

           } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
             rhsCols <- colnames(dataset)
             rhsCols <- rhsCols[!(rhsCols %in% input$cols)]
             rhsCols <- rhsCols[!(rhsCols %in% input$colsRHS)]
             lhsCols <- colnames(dataset)
             lhsCols <- lhsCols[!(lhsCols %in% input$cols)]
             lhsCols <- lhsCols[!(lhsCols %in% input$colsLHS)]
             ar <- subset(arAll, subset=lhs %in% lhsCols & rhs %in% rhsCols)

           } else {
             ar <- arAll
           }

           quality(ar) <- interestMeasure(ar, transactions=tr)
           maxLift <<- ceiling(max(quality(ar)$lift))
           shiny::updateSliderInput(session,"lift",value = input$lift, min=minLift, max=maxLift, step = 1/10000)
           ar
       } else if(class(dataset)=='rules') {
           cols <- itemLabels(dataset)
           cols <- cols[!(cols %in% input$cols)]
           cat(file=stderr(),'I was given rules and running the rules() method','\n')
           arAll <- subset(dataset, subset=support>input$supp)
           arAll <- subset(arAll, subset=lhs %in% cols & rhs %in% cols)
           arAll <- subset(arAll, subset=arAll@quality$confidence>input$conf)
           arAll <- subset(arAll, subset=arAll@quality$lift>input$lift)
           if(input$rhsv=='Subset' & input$lhsv!='Subset'){
             arAll <- subset(arAll, subset=rhs %in% input$colsRHS)
           } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
             arAll <- subset(arAll, subset=lhs %in% input$colsLHS)
           } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
                 arAll <- subset(arAll, subset=lhs %in% input$colsLHS & rhs %in% input$colsRHS)
           }
           shiny::validate(
               shiny::need( length(items(arAll))!=0,"No matching rules found")
           )
           maxLift <<- ceiling(max(quality(ar)$lift))
           shiny::updateSliderInput(session,"lift",value = input$lift, min=minLift, max=maxLift, step = 1/10000)
            arAll
       }
     })

     # Rule length
     nR <- shiny::reactive({
       #TODO: change if allowing rule sampling
       #nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
       nRule <- length(rules())
     })

     xNames <- shiny::reactive({
         colnames(quality(rules()))
     })
     yNames <- shiny::reactive({
         colnames(quality(rules()))
     })
     cNames <- shiny::reactive({
         colnames(quality(rules()))
     })

     # Present errors nicely to the user
     handleErrors <- shiny::reactive({
       ar <- rules()
       shiny::validate(
                shiny::need(nR()<=length(ar),'Please decrease the number of rules')
       )
     })

     ## Grouped Plot #########################
     output$groupedPlot <- shiny::renderPlot({
       ar <- rules()
       handleErrors()
       #TODO: make sure this is legit
       #plot(sort(ar, by=input$sort)[1:nR()], method='grouped', control=list(k=input$k))
       cat(file=stderr(),length(ar),'\n')
       #plot(sort(ar)[1:nR()], method='grouped', control=list(k=input$k), engine='interactive')
       plot(ar, method='grouped', control=list(k=input$k))
     }, height=800, width=800)

     ## Graph Plot ##########################
     output$graphPlot <- renderVisNetwork({
       ar <- rules()
       handleErrors()
       #plot(sort(ar)[1:nR()], method='graph', control=list(type=input$graphType))
       plt <- plot(sort(ar)[1:nR()], method='graph', control=list(type=input$graphType),engine='htmlwidget')
       sz <- htmlwidgets::sizingPolicy(
            viewer.paneHeight=1000,
            browser.defaultHeight=1000,
            knitr.defaultHeight=1000,
            defaultHeight=1000,defaultWidth=1000,
            browser.fill=TRUE
            )
       plt$sizingPolicy <- sz
       plt$height <- 1000
       plt$x$height <- 1000
       plt
     })

     ## Scatter Plot ##########################
     
     output$scatterPlot <- renderPlotly({
       ar <- rules()
       handleErrors()
       #plotly_arules(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
       plotly_arules(sort(ar)[1:nR()], method='scatterplot',
                     measure=c(input$xAxis,input$yAxis),shading=input$cAxis)
     })
     

     ## Parallel Coordinates Plot ###################
     output$paracoordPlot <- shiny::renderPlot({
       ar <- rules()
       handleErrors()
       #plot(sort(ar, by=input$sort)[1:nR()], method='paracoord')
       plot(sort(ar)[1:nR()], method='paracoord')
     }, height=800, width=800)

     ## Matrix Plot ###################
     output$matrixPlot <- renderPlotly({
       ar <- rules()
       handleErrors()
       #plotly_arules(sort(ar, by=input$sort)[1:nR()], method='matrix', control=list(reorder='similarity'))
       plotly_arules(sort(ar)[1:nR()], method='matrix',shading=input$cAxis)
     })

     ## Item Frequency Plot ##########################
     output$itemFreqPlot <- shiny::renderPlot({
       if(class(dataset)=='transactions'){
           cols <- colnames(dataset)
           cols <- cols[!(cols %in% input$cols)]
           trans <- as(dataset[,cols], 'transactions')
           itemFrequencyPlot(trans,topN=input$itemFreqN)
       } else if(class(dataset)=='rules'){
           itemFrequencyPlot(items(rules()),topN=input$itemFreqN)
       }
     }, height=800, width=800)

     ## Rules Data Table ##########################
     output$rulesDataTable <- shiny::renderDataTable({
       ar <- rules()
       handleErrors()
       rulesdt <- rules2df(ar)
       rulesdt
     })

     ## Rules Printed ########################
     output$rulesTable <- shiny::renderPrint({
       ar <- rules()
       handleErrors()
       inspect(sort(ar))
     })

     ## Download data to csv ########################
     output$downloadData <- shiny::downloadHandler(
       filename = 'arules_data.csv',
       content = function(file) {
         write.csv(rules2df(rules()), file)
       }
     )


   }
  )
}
