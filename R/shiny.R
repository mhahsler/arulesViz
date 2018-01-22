#TODO: cite and document and change
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

shiny_arules <- function (dataset, vars=5, supp=0.1, conf=0.5) {

  ## binning numeric data
  for(i in 1:ncol(dataset)) {
    if(class(dataset[,i]) %in% c('numeric', 'integer')) dataset[,i] <- depthbin(dataset[,i], nbins=10)
  }

  ## calling Shiny App
  shinyApp(ui = shinyUI(pageWithSidebar(

    headerPanel("Association Rules"),

    sidebarPanel(

      conditionalPanel(
        condition = "input.samp=='Sample'",
        numericInput("nrule", 'Number of Rules', 5), br()
      ),

      conditionalPanel(
        condition = "input.mytab=='graph'",
        radioButtons('graphType', label='Graph Type', choices=c('itemsets','items'), inline=T), br()
      ),

      conditionalPanel(
        condition = "input.lhsv=='Subset'",
        uiOutput("choose_lhs"), br()
      ),

      conditionalPanel(
        condition = "input.rhsv=='Subset'",
        uiOutput("choose_rhs"), br()
      ),

      conditionalPanel(
        condition = "input.mytab=='grouped'",
        sliderInput('k', label='Choose # of rule clusters', min=1, max=150, step=1, value=15), br()
      ),

      conditionalPanel(
        condition = "input.mytab %in%' c('grouped', 'graph', 'table', 'datatable', 'scatter', 'paracoord', 'matrix', 'itemFreq')",
        radioButtons('samp', label='Sample', choices=c('All Rules', 'Sample'), inline=T), br(),
        uiOutput("choose_columns"), br(),
        sliderInput("supp", "Support:", min = 0, max = 1, value = supp , step = 1/10000), br(),
        sliderInput("conf", "Confidence:", min = 0, max = 1, value = conf , step = 1/10000), br(),
        selectInput('sort', label='Sorting Criteria:', choices = c('lift', 'confidence', 'support')), br(), br(),
        numericInput("minL", "Min. items per set:", 2), br(),
        numericInput("maxL", "Max. items per set::", 3), br(),
        radioButtons('lhsv', label='LHS variables', choices=c('All', 'Subset')), br(),
        radioButtons('rhsv', label='RHS variables', choices=c('All', 'Subset')), br(),
        downloadButton('downloadData', 'Download Rules as CSV')
      )

    ),

    mainPanel(
      tabsetPanel(id='mytab',
                  tabPanel('Grouped', value='grouped', plotOutput("groupedPlot", width='100%', height='100%')),
                  tabPanel('Graph', value='graph', plotOutput("graphPlot", width='100%', height='100%')),
                  tabPanel('Scatter', value='scatter', plotOutput("scatterPlot", width='100%', height='100%')),
                  tabPanel('Parallel Coordinates', value='paracoord', plotOutput("paracoordPlot", width='100%', height='100%')),
                  tabPanel('Matrix', value='matrix', plotOutput("matrixPlot", width='100%', height='100%')),
                  tabPanel('ItemFreq', value='itemFreq', plotOutput("itemFreqPlot", width='100%', height='100%')),
                  tabPanel('Table', value='table', verbatimTextOutput("rulesTable")),
                  tabPanel('Data Table', value='datatable', dataTableOutput("rulesDataTable"))
      )
    )

   )),

   server = function(input, output) {

     output$choose_columns <- renderUI({
       checkboxGroupInput("cols", "Choose variables:",
                          choices  = colnames(dataset),
                          selected = colnames(dataset)[1:vars])
     })


     output$choose_lhs <- renderUI({
       checkboxGroupInput("colsLHS", "Choose LHS variables:",
                          choices  = input$cols,
                          selected = input$cols[1])
     })

     output$choose_rhs <- renderUI({
       checkboxGroupInput("colsRHS", "Choose RHS variables:",
                          choices  = input$cols,
                          selected = input$cols[1])
     })

     ## Extracting and Defining arules
     rules <- reactive({
       tr <- as(dataset[,input$cols], 'transactions')
       arAll <- apriori(tr, parameter=list(support=input$supp, confidence=input$conf, minlen=input$minL, maxlen=input$maxL))

       if(input$rhsv=='Subset' & input$lhsv!='Subset'){
         varsR <- character()
         for(i in 1:length(input$colsRHS)){
           tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
           varsR <- c(varsR, tmp)
         }
         ar <- subset(arAll, subset=rhs %in% varsR)

       } else if(input$lhsv=='Subset' & input$rhsv!='Subset') {
         varsL <- character()
         for(i in 1:length(input$colsLHS)){
           tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
           varsL <- c(varsL, tmp)
         }
         ar <- subset(arAll, subset=lhs %in% varsL)

       } else if(input$lhsv=='Subset' & input$rhsv=='Subset') {
         varsL <- character()
         for(i in 1:length(input$colsLHS)){
           tmp <- with(dataset, paste(input$colsLHS[i], '=', levels(as.factor(get(input$colsLHS[i]))), sep=''))
           varsL <- c(varsL, tmp)
         }
         varsR <- character()
         for(i in 1:length(input$colsRHS)){
           tmp <- with(dataset, paste(input$colsRHS[i], '=', levels(as.factor(get(input$colsRHS[i]))), sep=''))
           varsR <- c(varsR, tmp)
         }
         ar <- subset(arAll, subset=lhs %in% varsL & rhs %in% varsR)

       } else {
         ar <- arAll
       }

       # Catch the case of too few variables selected
       tc <- tryCatch(
         {
           quality(ar)$conviction <- interestMeasure(ar, 'conviction', transactions=tr)
           quality(ar)$hyperConfidence <- interestMeasure(ar, 'hyperConfidence', transactions=tr)
           quality(ar)$cosine <- interestMeasure(ar, 'cosine', transactions=tr)
           quality(ar)$chiSquare <- interestMeasure(ar, 'chiSquare', transactions=tr)
           quality(ar)$coverage <- interestMeasure(ar, 'coverage', transactions=tr)
           quality(ar)$doc <- interestMeasure(ar, 'doc', transactions=tr)
           quality(ar)$gini <- interestMeasure(ar, 'gini', transactions=tr)
           quality(ar)$hyperLift <- interestMeasure(ar, 'hyperLift', transactions=tr)
         },
         error=function(cond) {
             validate(
               need(0>1,'Plase select more variables')
             )
         },
         warning=function(cond) {
         },
         finally={
         }
       )
       ar
     })

     # Rule length
     nR <- reactive({
       nRule <- ifelse(input$samp == 'All Rules', length(rules()), input$nrule)
     })

     handleErrors <- reactive({
       ar <- rules()
       validate(
                need(nR()>=2,'Please increase the number of rules')
       )
       validate(
                need(nR()<=length(ar),'Please decrease the number of rules')
       )
       validate(
                need(length(input$cols)>0,'Please increase the number of variables')
       )
     })

     ## Grouped Plot #########################
     output$groupedPlot <- renderPlot({
       ar <- rules()
       handleErrors()
       plot(sort(ar, by=input$sort)[1:nR()], method='grouped', control=list(k=input$k))
     }, height=800, width=800)

     ## Graph Plot ##########################
     output$graphPlot <- renderPlot({
       ar <- rules()
       handleErrors()
       plot(sort(ar, by=input$sort)[1:nR()], method='graph', control=list(type=input$graphType))
     }, height=800, width=800)

     ## Scatter Plot ##########################
     output$scatterPlot <- renderPlot({
       ar <- rules()
       handleErrors()
       plot(sort(ar, by=input$sort)[1:nR()], method='scatterplot')
     }, height=800, width=800)

     ## Parallel Coordinates Plot ###################
     output$paracoordPlot <- renderPlot({
       ar <- rules()
       handleErrors()
       plot(sort(ar, by=input$sort)[1:nR()], method='paracoord')
     }, height=800, width=800)

     ## Matrix Plot ###################
     output$matrixPlot <- renderPlot({
       ar <- rules()
       handleErrors()
       plot(sort(ar, by=input$sort)[1:nR()], method='matrix', control=list(reorder='similarity'))
     }, height=800, width=800)

     ## Item Frequency Plot ##########################
     output$itemFreqPlot <- renderPlot({
       trans <- as(dataset[,input$cols], 'transactions')
       itemFrequencyPlot(trans)
     }, height=800, width=800)

     ## Rules Data Table ##########################
     output$rulesDataTable <- renderDataTable({
       ar <- rules()
       handleErrors()
       rulesdt <- rules2df(ar)
       rulesdt
     })

     ## Rules Printed ########################
     output$rulesTable <- renderPrint({
       ar <- rules()
       handleErrors()
       inspect(sort(ar, by=input$sort))
     })

     ## Download data to csv ########################
     output$downloadData <- downloadHandler(
       filename = 'arules_data.csv',
       content = function(file) {
         write.csv(rules2df(rules()), file)
       }
     )


   }
  )
}
