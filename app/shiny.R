library(shiny)
library(DT)
library(dplyr)
require(tidyverse)

library(devtools)
 # install_github("esm-ispm-unibe-ch/reportingbias")
library(reportingbias)
 # install_github("esm-ispm-unibe-ch/flow_contribution")
library(contribution)
library(netmeta)
library(nmadb)
library(xlsx)
library(BUGSnet)

rm(list = ls())
print("removed everything")

source("./css.R")

source("./table1.R")

# Helper functions for comparisons
comparisonToTreatments = function(comparison){
  ctrs <- strsplit(comparison,":")[[1]]
  return(ctrs)
}
iscomparison = function(treat1, treat2, comparison) {
  ctrs <- comparisonToTreatments
  res <- (ctrs[1]==treat1 & ctrs[2]==treat2) |
         (ctrs[2]==treat1 & ctrs[1]==treat2)
  return(res)
}
iscompared = function(treat1, treat2, directs) {
  res = any(unlist(lapply(directs,function(x){iscomparison(treat1,treat2,x)})))
 return(res) 
}
##

server <- function(input, output, session) {
  state <- reactiveValues(allData = {}, table1 = tibble(), error="")
  
  myData <- reactive({state$allData$directs})
  
  observeEvent(input$file1,{
    state$allData <- allMyData(input$file1)
  },ignoreInit=T)
  
  observe({
     res <- unique(state$allData$directs$t) %>% sort()
     state$treatments <- res
    state$bdata <- bdata(state$allData$directs)
  })
  
  observeEvent(input$inputMod,{
    state$modelFixed <- input$inputMod=="fixed"
    state$modelRandom <- input$inputMod=="random"
  },ignoreInit=T)
  
  observeEvent(input$inputSM,{
    state$nma <- nma(state$allData$directs, input$inputSM, state$modelFixed, state$modelRandom)
    state$bnma <- bnma(input$inputSM, state$bdata, input$inputRef, input$inputMod)
  },ignoreInit=T)
   
  observe({
    if(!is.null(state$treatments)){
      state$table1 <- buildTable1(state$treatments, state$allData$directs, state$allData$otherOutcomes)
    }
  })
  
    output$table2 <- DT::renderDataTable({
      datatable(state$table1)
    })
      
    output$table1 <- DT::renderDataTable({
    
    groupALabel = "Group A: 
observed for this outcome" 
    groupBLabel = "Group B:
observed for other outcomes"
    groupCLabel = "Group C:
Unobserved"
    
    table1DropDown = function(column,treat1,treat2,comparison,groupLabel,level,proposed){
      choices = c()
      if(column == "known_unknowns"){
        if(groupLabel != groupCLabel){
          choices = c( ""
                     , "Undetected bias"
                     , paste("Suspected bias favouring ",treat1,sep="")
                     , paste("Suspected bias favouring ",treat2,sep="")
                     , "Unclear")
        }
      }else{
        choices = c( ""
                   , "Undetected bias"
                   , paste("Suspected bias favouring ",treat1,sep="")
                   , paste("Suspected bias favouring ",treat2,sep="")
        )
      }
      
      if(length(choices)>0){
        chs <- tibble::rowid_to_column(as.data.frame(choices), "n")
        
        chs$optiontag = mapply(function(l,choice){
            if(l==level+1){
              selectedstring = " selected"
            }else{
              selectedstring = " "
            }
            if(column=="overall_bias"){
              if(l==proposed+1){
                proposedstring = " style='color:grey;font-style:italic;font-weight:bold'"
              }else{
                proposedstring = " "
              }
            }else{
                proposedstring = " "
            }
            optag <- paste("<option "
                          ,"value="
                          ,l-1
                          ,selectedstring
                          ,proposedstring
                          ,">"
                          ,choice
                          ,"</option>", sep="")
            return(optag)
            },chs$n,chs$choices)
        
        res = paste0("<select onchange=table1select(id) class=table1_selector "
                   ,"id='"
                   ,paste(column,comparison,sep="-vs-"),"'>"
                   ,unite(chs,optiontag,sep="")
                   ,"</select>")
      }else{
        res=""
      }
      return(res)
    }
    
    labelGroup = function(compgroup) {
      out = ""
      if(compgroup == "groupA"){
       out <- groupALabel
      }
      if(compgroup == "groupB"){
        out <- groupBLabel
      }
      if(compgroup == "groupC"){
        out <- groupCLabel
      }
      return(out)
    }
    
  table1Web <- tibble()
  
  table1Header = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, '-'),
        th(rowspan = 2, 'Comparisons'),
        th(rowspan = 2, 'group'),
        th(colspan = 2, 'Number of studies in each comparison'),
        th(colspan = 1, 'known unknowns',
                         ),
        th(colspan = 1, 'unknowns unknowns'),
        th(colspan = 1, 'overall bias'),
      ),tr(
        th(colspan = 1, 'Reporting this outcome (sample size)'),
        th(colspan = 1, 'Total identified in the SR (total sample size)'),
        th(colspan = 1, 'Classification system (e.g. ORBIT)'),
        th(colspan = 1, 'Qualitative signals and quantitative considerations'
          ),
        th(colspan = 1, 'Algorithm for merging of previous assessments')
      )
    )
  ))
    table1Web <- state$table1 %>%
      mutate(groupLabel = mapply(labelGroup,compgroup)) %>%
      mutate(known_unknownsWeb = mapply(table1DropDown,"known_unknowns",treat1,treat2,comparison,groupLabel,known_unknowns,proposed)) %>%
      mutate(unknown_unknownsWeb = mapply(table1DropDown,"unknown_unknowns",treat1,treat2,comparison,groupLabel, unknown_unknowns,proposed)) %>%
      mutate(overall_biasWeb = mapply(table1DropDown,"overall_bias",treat1,treat2,comparison,groupLabel, overall_bias,proposed)) %>%
      mutate(column1= paste(numstudies," (",samplesize,")",sep="")) %>%
      mutate(column2= paste(total.numstudies," (",total.samplesize,")",sep="")) %>%
      select(comparison
             ,groupLabel
             ,column1
             ,column2
             ,known_unknownsWeb
             ,unknown_unknownsWeb
             ,overall_biasWeb
             )
   
    datatable(table1Web,
                  container = table1Header,
                  escape = F,
                  extensions = c('RowGroup'),
                  options = list(rowGroup = list(dataSrc = 2)
                                 , paging = F
                                 , columnDefs = list(list(visible=FALSE, targets=c(2)))
                                 , dom = 'Bfrtip'
                  ),
                  selection = 'none'
                  )

  }, server=F
  )
   
  bnma <- function(sm,bdata,ref,eff){
    tryCatch({
      if(sm=="OR" | sm=="RR")
          model <- nma.model(data=bdata,
                             outcome="r",
                             N="n",
                             reference=ref,
                             family="binomial",
                             link=ifelse(sm=="OR","logit", "log"),
                             effects= eff)
      else model <- nma.model(data=bdata,
                              outcome="mean",
                              sd="sd",
                              N="n",
                              reference=ref,
                              family="normal",
                              link="identity",
                              effects= eff)
      results <- nma.run(model,
                         n.iter=1000)
      return(results)
    }, warning=function(w){print(paste(w));results
      }
    , error=function(e){print(paste(e));return({})}
    )
  }
  
  nma <- function(data, sm, modelFixed, modelRandom){
    getNMA <- function() {
      if(sm == "OR" | sm == "RR"){
      pw <- pairwise(treat=t, event=r, n=n, data = myData(), studlab = id)
    }
    else {
      pw <- pairwise(treat=t, n=n, mean=mean, sd=sd, data = myData(), studlab = id)
    }
    return(netmeta(TE, seTE, treat1, treat2, studlab, data = pw, sm=sm, n1=n1, n2=n2,  
            comb.fixed = modelFixed, comb.random = modelRandom)  
           )
    }
    tryCatch({ res = getNMA(); res
    }, warning=function(w){print(paste(w));res
      }
    , error=function(e){print(paste(e));return({})}
    )
  }
  
  allMyData <- function(inFile){
    getData <- function(){
      dataraw <- read.csv(inFile$datapath, header = TRUE)
      # dataraw <- read.csv("../griselda.csv", header = TRUE)
      alldata <- as.data.frame(dataraw)
      binaryColumns = c("id","study","t","n","r")
      isBinary = all(lapply(binaryColumns, function(x){x %in% colnames(alldata)}))
      alldata <- alldata %>%
        mutate(n = as.integer(n) ) %>%
         mutate(r = as.integer(r) )
      directs <- alldata %>%
        filter(!is.na(r))
      otherOutcomes <- alldata %>%
        filter(is.na(r))
      return(list( isBinary=isBinary
                 , directs=directs
                 , otherOutcomes=otherOutcomes
                 ))
    }
    tryCatch({
      return(getData())
    }, warning=function(w){print(paste(w))
      return(getData())}
    , error=function(e){
      print(paste(e))
      state$error<-paste(e);
      return(NULL)
      })
  }
  
  output$contents <- DT::renderDataTable({
    DT::datatable(myData())       
  })
  
  output$ref <- renderUI({
    radioButtons(inputId = "inputRef", 
                 label = "Choose the reference treatment",
                 choices = state$treatments)
  }) 
  
  output$summary <- renderPrint({
    summary(state$nma)
  })
  
  getNMA <- reactive({
    validate(
      need(state$nma != "", "netmeta not ready")
    )
    state$nma
  })
  
  output$netgraph <- renderPlot({
    netgraph(getNMA(), cex = 0.7, col = "black", plastic=FALSE, 
             points=T, col.points = "darkgreen", cex.points =10*sqrt(n.trts/max(n.trts)),  
             thickness="number.of.studies", lwd.max = 12, lwd.min = 1, multiarm=F)
  })
  
  bdata <- function(mydata){
    tryCatch({
    res <- data.prep(arm.data = mydata,
                varname.t = "t",
                varname.s = "id")
      return(res)
    }, warning=function(w){print(paste(w));res
      }
    , error=function(e){print(paste(e));return({})}
    )
  }
  btab <- reactive({
    net.tab(data = state$bdata,
            outcome = ifelse(input$inputSM=="OR" | input$inputSM=="RR", "r", "mean"),
            N = "n",
            type.outcome = ifelse(input$inputSM=="OR" | input$inputSM=="RR", "binomial", "continuous"))
  })
  output$netinfo <- renderTable({
    btab()$network
  })
  output$intinfo <- renderTable({
    btab()$intervention
  })
  output$compinfo <- renderTable({
    btab()$comparison
  })
    

  bleague <- reactive({
    nma.league(state$bnma,
               central.tdcy="median",
               order = nma.rank(state$bnma, largerbetter=ifelse(input$inputBH=="good", F, T))$order,
               log.scale = FALSE,
               low.colour = "springgreen4",
               mid.colour = "white",
               high.colour = "red")
    })
  output$league <- renderTable({
    bleague()$table
  }, rownames = T)
  
  output$forest <- renderPlot({
    nma.forest(state$bnma, comparator = input$inputRef)
  })
  
  NMRdata <- reactive({
    newdata <- pooledVar(state$nma, myData())
    data.prep(arm.data = newdata,
              varname.t = "t",
              varname.s = "id")
  })
  
  output$minvar <- renderText({
    min(NMRdata()$arm.data$varStudies)
  }) 
  
  bnmr <- reactive({
    if(input$inputSM=="OR" | input$inputSM=="RR")
      model <- nma.model(data=NMRdata(),
                         outcome="r",
                         N="n",
                         reference=input$inputRef,
                         family="binomial",
                         link=ifelse(input$inputSM=="OR","logit", "log"),
                         effects= input$inputMod,
                         covariate = "varStudies",
                         prior.beta = "UNRELATED")
    else model <- nma.model(data=NMRdata(),
                            outcome="mean",
                            sd="sd",
                            N="n",
                            reference=input$inputRef,
                            family="normal",
                            link="identity",
                            effects= input$inputMod,
                            covariate = "varStudies",
                            prior.beta = "UNRELATED")
    results <- nma.run(model,
                       n.iter=1000)
  })
  
  NMRleague <- reactive({
    nma.league(bnmr(),
               central.tdcy="median",
               order = nma.rank(bnmr(), largerbetter=ifelse(input$inputBH=="good", F, T), cov.value = min(NMRdata()$arm.data$varStudies))$order,
               log.scale = FALSE,
               low.colour = "springgreen4",
               mid.colour = "white",
               high.colour = "red", 
               cov.value = min(NMRdata()$arm.data$varStudies))
  })
  output$nmr <- renderPlot({
    NMRleague()$heatplot
  })
  
  output$nmrplot <- renderPlot({
    nma.regplot(bnmr())
  })
  
  fp <- reactive({
    nmafunnel(state$nma, small.values = input$inputBH)
  })    
  
  output$fpprint <- renderPrint({
      fp()
  })

  output$table <- DT::renderDataTable(fp()$tests)
  
  fp2 <- function() { 
    nmafunnel(state$nma, small.values = input$inputBH)    
  }
  
  output$plot2 <- renderPlot({
    par(mfrow=c(2,3))
    fp2()
  })
  
  output$mydownload <- downloadHandler(                         
    filename = function() {paste("", ".pdf")},       # name for the downloaded file with extension
    content = function(file) { 
      pdf(file)
      fp2()
      dev.off()
    },
    contentType = 'pdf')

  reffp <- reactive({
        reffunnel(state$nma, small.values = input$inputBH, ref=input$inputRef)
  })
  output$fpref <- renderPlot(reffp())
  output$reftest <- renderPrint(reffp())
    
  contr <- reactive({getContributionMatrix(myData()
                                 , type = ifelse(input$inputSM=="OR" 
                                                 | input$inputSM=="RR"
                                                 ,"long_binary"
                                                 , "long_continuous")
                                 , model = input$inputMod
                                 , sm=input$inputSM)
  })
  
  output$contr <- renderTable(round(contr()$contributionMatrix, digits = 2), rownames = T)
  
  output$mydownload2 <- downloadHandler(                         
    filename = function() {paste("", ".xlsx")},       # name for the downloaded file with extension
    content = function(file) {
      write.xlsx(round(contribution()$contributionMatrix, digits = 2), file)
    }
  )
  output$smOptions <- renderUI({
    if(state$allData$isBinary==T){
     radioButtons(inputId = "inputSM", label = "Summary measure",
                  c("Odds Ratio" = "OR",
                    "Risk Ratio" = "RR"
                    ),
                    selected = character(0)
                  )
    }else{
     radioButtons(inputId = "inputSM", label = "Summary measure",
                  c("Mean difference" = "MD",
                    "Standardized mean difference" = "SMD"),
                     selected = character(0)
                  )
    }
  })
  
  proposeTable1Overall <- function(known, unknown) {
    res <- 0
    if(known %in% c(0,1,4)){
      res <- unknown
    }else{
      res <- known
    }
    return(res)
  }
  
  observeEvent(input$table1select, {
      t1sel <- input$table1select
      sel <-  unlist(strsplit(t1sel$id,"-vs-",fixed=T))
      icolumn <- sel[[1]]
      icomparison <- sel[[2]]
      print(t1sel$value)
      chr = filter(state$table1, icomparison == comparison) %>%
            mutate("{icolumn}" := as.integer(t1sel$value)) %>%
            mutate(proposed = 
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
      state$table1 <- rows_update(state$table1, chr)
      print(c("state1",chr))
  })
  
  observeEvent(input$applyProposedTable1, {
    print("Applying proposed to overall")
    state$table1 <- mutate(state$table1, overall_bias = proposed) %>%
            mutate(proposed = 
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
  })
  
  observeEvent(input$unsetKnowns, {
    print("unsetting")
    state$table1 <- mutate(state$table1, known_unknowns = 0) %>%
            mutate(proposed = 
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
  })
  
  observeEvent(input$unsetUnknowns, {
    print("usetting")
    state$table1 <- mutate(state$table1, unknown_unknowns = 0) %>%
            mutate(proposed = 
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
  })
  
  observeEvent(input$setKnownsUndetected, {
    print("setting to Undetected")
    state$table1 <- mutate(state$table1, known_unknowns = 1) %>%
            mutate(proposed = 
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
  })
  
  observeEvent(input$setUnknownsUndetected, {
    print("setting to Undetected")
    state$table1 <- mutate(state$table1, unknown_unknowns = 1) %>%
            mutate(proposed = 
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
  })
  
  output$table1Header <- renderUI({
    if(!is.null(state$table1)){
      tags$div(
        actionButton("setKnownsUndetected","set known unknowns Undetected"),
        actionButton("unsetKnowns","unset known unknowns"),
        actionButton("setUnknownsUndetected","set unknown unknowns Undetected"),
        actionButton("unsetUnknowns","unset unknown unknowns"),
        actionButton("applyProposedTable1", "Apply proposed values to overall bias column")
      )
    }
  })
  
 output$messages <- renderText({state$error})
 
  output$dataAnalysis <- renderUI({
    if(!is.null(myData())){
             sidebarLayout(
               sidebarPanel(
                 uiOutput("smOptions"),
                 radioButtons(inputId = "inputBH",
                              label = "Smaller outcome values are",
                              c("Desirable" = "good",
                                "Undesirable" = "bad")),
                 radioButtons(inputId = "inputMod",
                              label = "Model",
                              c("Random effects" = "random",
                                "Fixed effects" = "fixed")),
                 uiOutput("ref"),
                 width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data summary",
                            plotOutput("netgraph"),
                            h4("Network info", align = "left"),
                            tableOutput("netinfo"),
                            h4("Interventions characteristics", align = "left"),
                            tableOutput("intinfo"),
                            h4("Direct comparisons", align = "left"),
                            tableOutput("compinfo")),
                   tabPanel("Frequentist network meta-analysis", verbatimTextOutput("summary")),
                   tabPanel("Bayesian network meta-analysis",
                            fluidRow(
                              h4("Forest plot", align = "center"),
                              conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                               tags$div(id = "plot-container5", tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif", id = "loading-spinner1")),
                                               tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage5")),
                              div(plotOutput("forest", height = "500px", width = "800px"), align = "center")
                            ),
                            br(),
                            fluidRow(
                              h4("League table", align = "center"),
                              conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                               tags$div(id = "plot-container6", tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif", id = "loading-spinner2")),
                                               tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage6")),
                              div(tableOutput("league"), align = "center")
                            )),
                   tabPanel("Bayesian network meta-regression",
                            fluidRow(
                              h4("Network meta-regression plot", align = "center"),
                              conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                               tags$div(id = "plot-container1", tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif", id = "loading-spinner1")),
                                               tags$div("Please wait. The calculations of network meta-regression may take up to several minutes.",id="loadmessage1")),
                              div(plotOutput("nmrplot", height = "500px", width = "800px"), align = "center")
                            ),
                            br(),
                            fluidRow(
                              h4("League table", align = "center"),
                              conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                                               tags$div(id = "plot-container2", tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif", id = "loading-spinner2")),
                                               tags$div("Please wait. The calculations of network meta-regression may take up to several minutes.",id="loadmessage2")),
                              p("League table showing results for the minimum observed variance value of", textOutput("minvar", inline = T)),
                              div(plotOutput("nmr", height = "500px", width = "800px"), align = "center")
                            ))
                 ),
                 width = 9
               )
               )
  }})
}






ui <- fluidPage(
  titlePanel("Framework for reporting bias in network meta-analysis"),
  tags$script(src = "table1.js") ,
  tags$head(tags$style(HTML(mycss))),
  
  tags$div(uiOutput("messages")),
  tabsetPanel(
    tabPanel("Load data",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file1", "Choose CSV file", 
                           accept = c("text/csv", 
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 width = 3
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Instructions",      # section division
                            tags$h4("This tab provides instructions for", tags$b("long format"), "data, where each row contains one treatment arm. Please follow the steps below."), 
                            tags$br(),   # line break
                            tags$h5("The long format data file should contain five columns (for binary data) or six columns (for continuous data) labelled as follows:"),
                            tags$ul(
                              tags$li("Column", tags$i(tags$b("id")), "containing the study identifier, starting from 1, then 2, 3, 4... etc."),
                              tags$li("Column", tags$i(tags$b("study")), "containing the name (e.g., author,year) of the study."),
                              tags$li("Column", tags$i(tags$b("t")), "containing the treatment code used in each arm of the study."),
                              tags$li("Column", tags$i(tags$b("n")), "containing the number of participants in each arm of the study."),
                              tags$li(tags$i("(For binary data)"),"Column", tags$i(tags$b("r")), "containing the number of participants with the outcome of interest in each arm of the study."),
                              tags$li(tags$i("(For continuous data)"),"Column", tags$i(tags$b("mean")), "the mean value of the outcome in each arm of the study."),
                              tags$li(tags$i("(For continuous data)"),"Column", tags$i(tags$b("sd")), "the standard deviation of the outcome in each arm of the study.")
                            )
                            ),
                   tabPanel("View data", DT::dataTableOutput('contents'))
                 ),
                 width = 9
               )
             )),
    
           tabPanel("Data analysis",
                   uiOutput("dataAnalysis")
             ),
    
    tabPanel("Evaluation of pairwise comparisons",
               tabsetPanel(
                   tabPanel("Contour-enhanced funnel plots",
                            sidebarPanel(checkboxInput(inputId = "NatRef", label = "Check box if there is a natural reference treatment"),
                                         width = 3),
                            mainPanel(
                              fluidRow(
                                verbatimTextOutput("fpprint"),
                                dataTableOutput("table")),
                              hr(),
                              fluidRow(         # would like to make this conditional i.e. only showing if funnel plots available
                                tags$h4("Only the first six contour-enhanced funnel plots are shown here. To view all plots, press the button below to download them as PDF."),
                                plotOutput("plot2"),
                                downloadButton('mydownload', 'Download Plots as PDF')),
                              fluidRow(
                                conditionalPanel("input.NatRef",
                                                 plotOutput("fpref"),
                                                 verbatimTextOutput("reftest")))    # does not print out reftest (test result)
                            )
                 )
               )
            ),
    tabPanel("Evaluation of NMA effects",
             tabsetPanel(
               tabPanel("Contribution matrix", 
                        tableOutput("contr"),
                        downloadButton('mydownload2', 'Download Contribution Matrix'))
             )),
    
    tabPanel( "Table 1"
            , uiOutput("table1Header")
            , tabPanel("View data", DT::dataTableOutput('table1'))
            ),
    
    tabPanel("Table 2"
            , tabPanel("View data", DT::dataTableOutput('table2'))
    )
            
  )
)

shinyApp(ui = ui, server = server)
