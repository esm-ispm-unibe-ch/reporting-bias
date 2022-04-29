library(shiny)
library(data.table)
library(DT)
library(dplyr)
require(tidyverse)


library(devtools)
# install_github("esm-ispm-unibe-ch/reportingbias")
# install.packages(netmeta, repos = NULL, type="source")
# install.packages("reportingbias", repos = NULL, type="source")
load_all("helpers")
# library(reportingbias)
library(netmeta)
library(xlsx)

library(BUGSnet)

source("./css.R")

source("./netcontrib.R")

source("./table1.R")

source("./table2.R")
# renv::init()
# renv::save()
server <- function(input, output, session) {
  state <- reactiveValues( allData = {}
                         , table1 = tibble()
                         , error=""
                         , parametersSet=F
                         , analysisStarted=F
                         , nma=""
                         , nmaDone=F
                         , bdata=""
                         , bnma=""
                         , bnmaDone=F
                         , NMRdata=""
                         , bnmr=""
                         , bnmrDone=F
                         , contr=NULL
                         , table2 = tibble()
                         , numIter = 10000
                         , burnin = 1000
                         )

  myData <- reactive({state$allData$directs})
  dataset <- reactive({state$allData$alldata})

  observeEvent(input$file1,{
    state$allData <- allMyData(input$file1)
  },ignoreInit=T)

  observeEvent(state$allData$directs, {
     res <- unique(state$allData$directs$t) %>% sort()
     state$treatments <- res
     state$bdata <- bdata(state$allData$directs)
     print("bdata calculated")
  })

  observeEvent(state$inputMod,{
    state$modelFixed <- input$inputMod=="fixed"
    state$modelRandom <- input$inputMod=="random"
  },ignoreNULL=T)

  observeEvent(state$analysisStarted, {
    if(state$analysisStarted){
      print("starting NMA")
      state$nma <- nma(state$allData$directs, state$inputSM, state$modelFixed, state$modelRandom)
      state$nmaDone = T
      print("NMA done")
    }else{
      state$nma <- ""
      state$nmaDone = F
    }
  }, ignoreNULL=T)

  observeEvent(state$nmaDone, {
   validate(need(state$nmaDone == T, "netmeta not ready")
           )
      state$table1 <- buildTable1(state$treatments, state$allData$directs, state$allData$otherOutcomes, state$allData$isBinary)
  })

  observe({
    validate(need(state$analysisStarted == T, "Analysis in progress..."),
             need(nrow(state$table1) > "0", "table1 not ready"),
             need(state$contr != "", "contribution matrix not ready"))
    isolate({
      if(nrow(state$table2)==0){
        print(c("building not rebuilding",state$state2))
        state$table2 <- buildTable2(state$table1, state$contr, state$bleague$table, state$nmrleague$table)
      }else{
        print(c("rebuilding not building",state$state2))
        state$table2 <- rebuildTable2(state$table1, state$contr, state$bleague$table, state$nmrleague$table, state$table2)
      }
    })
  })

  output$table2 <- DT::renderDataTable({
    validate(need(state$analysisStarted == T, "analysis not started"),
             need(nrow(state$table1) != "0", "table1 not ready"),
             need(state$contr != "", "contribution matrix not ready"))

    table2col4 = function(comparison, treat1, treat2, level){
      choices = c( ""
                 , "No substantial contribution from bias"
                 , paste("Substantial contribution from bias favouring ",treat1,sep="")
                 , paste("Substantial contribution from bias favouring ",treat2,sep="")
                 , "Substantial contribution from bias balanced"
                 # , "Substantial but more or less equal contribution from comparisons favouring the opposite treatments"
      )

      chs <- tibble::rowid_to_column(as.data.frame(choices), "n")

      chs$optiontag = mapply(function(l,choice){
          if(l==level+1){
            selectedstring = " selected"
          }else{
            selectedstring = " "
          }
          optag <- paste("<option "
                        ,"value="
                        ,l-1
                        ,selectedstring
                        ,">"
                        ,choice
                        ,"</option>", sep="")
          return(optag)
          },chs$n,chs$choices)

      res = paste0("<select onchange=table2col4select(id) class=table2_col4 "
                   ,"id='"
                   ,paste("table2col4",comparison,sep="-vs-"),"'>"
                   ,unite(chs,optiontag,sep="")
                   ,"</select>")
      return(res)
    }

    table2col8 = function(comparison, treat1, treat2, level){
      choices = c( ""
                 ,"No evidence of small-study effects"
                 , paste("Small-study effects favouring ",treat1,sep="")
                 , paste("Small-study effects favouring ",treat2,sep="")
      )

      chs <- tibble::rowid_to_column(as.data.frame(choices), "n")

      chs$optiontag = mapply(function(l,choice){
          if(l==level+1){
            selectedstring = " selected"
          }else{
            selectedstring = " "
          }
          optag <- paste("<option "
                        ,"value="
                        ,l-1
                        ,selectedstring
                        ,">"
                        ,choice
                        ,"</option>", sep="")
          return(optag)
          },chs$n,chs$choices)

      res = paste0("<select onchange=table2col8select(id) class=table2_col8 "
                   ,"id='"
                   ,paste("table2col8",comparison,sep="-vs-"),"'>"
                   ,unite(chs,optiontag,sep="")
                   ,"</select>")
      return(res)
    }
    overallWeb <- function(comparison,treat1,treat2,
                           table1_overall_bias,mixed){
        choices = c( ""
                   , "No bias detected"
                   , paste("Suspected bias favouring ",treat1,sep="")
                   , paste("Suspected bias favouring ",treat2,sep="")
        )
      if(mixed == "indirect"){
        res <- choices[table1_overall_bias+1]
      }else{
        res <- paste("<span style='color:lightgrey'>", choices[table1_overall_bias+1],"</span>",sep="")
      }
      return(res)
    }
    table2Final <- function(comparison, treat1, treat2, proposed, level){
      choices = c( ""
                 , "Low risk"
                 , "Some concerns"
                 , "High risk"
      )
      chs <- tibble::rowid_to_column(as.data.frame(choices), "n")

      chs$optiontag = mapply(function(l,choice){
          if(l==level+1){
            selectedstring = " selected"
          }else{
            selectedstring = " "
          }
          if(l==proposed+1){
            proposedstring = " style='color:grey;font-style:italic;font-weight:bold'"
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

       if(proposed != level){
         changedstring = " style='background-color:lightyellow;'"
       }else{
         changedstring = " "
       }

      res = paste0("<select onchange=table2finalselect(id) class=table2_selector "
                 ,changedstring
                 ,"id='"
                 ,paste("table2Final",comparison,sep="-vs-"),"'>"
                 ,unite(chs,optiontag,sep="")
                 ,"</select>")
      return(res)
    }
    table2web <- state$table2 %>%
      mutate(table1_overall_bias_web = mapply(overallWeb,comparison,treat1,treat2,table1_overall_bias,mixed)) %>%
      mutate(contrTreat1Web = mapply(function(x){round(x, digits=0)},contrTreat1)) %>%
      mutate(contrTreat2Web = mapply(function(x){round(x, digits=0)},contrTreat2)) %>%
      mutate(contrTreat3Web = mapply(function(x){round(x, digits=0)},contrTreat3)) %>%
      mutate(contrEvaluationWeb = mapply(table2col4,comparison,treat1, treat2, contrEvaluation)) %>%
      mutate(effectsEvaluationWeb = mapply(table2col8,comparison,treat1, treat2, effectsEvaluation)) %>%
      mutate(finalWeb = mapply(table2Final,comparison,treat1, treat2, proposedFinal, final)) %>%
      select(mixed
            ,comparison
            ,contrTreat1Web
            ,contrTreat2Web
            ,contrEvaluationWeb
            ,table1_overall_bias_web
            ,nmaEffect
            ,nmrEffect
            ,effectsEvaluationWeb
            ,finalWeb
            )

  table2Header = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th(rowspan = 2, colspan=2, ' '),
        th(rowspan = 2, colspan = 1, 'NMA estimate'),
        th(rowspan = 1, colspan =2, '% contribution of evidence from pairwise comparisons with suspected bias'),
        th(rowspan = 2, colspan = 1, 'Evaluation of contribution from evidence with suspected bias'),
        th(rowspan = 2, colspan = 1, 'Bias assessment for indirect evidence'),
        th(rowspan = 2, colspan = 1, "NMA treatment effect"),
        th(rowspan = 2, colspan = 1, 'NMR treatment effect at the smallest observed variance'),
        th(rowspan = 2, colspan = 1, 'Evaluation of small-study effects'),
        th(rowspan = 2, colspan = 1, 'Overall risk of bias')
      ),
      tr(
        th(colspan = 1, 'Favouring first treatment'),
        th(colspan = 1, 'Favouring second treatment')
      )
    )
  ))

    datatable(table2web,
                  container = table2Header,
                  escape = F,
                  extensions = c('RowGroup'),
                  options = list(rowGroup = list(dataSrc = 1)
                                 , paging = F
                                 , columnDefs = list(list(visible=FALSE, targets=c(1)))
                                 , dom = 'Bfrtip'
                  ),
                  selection = 'none'
                  )
  }, server=F
  )

  observeEvent(input$updateTable2, {
    validate(need(state$analysisStarted == T, "analysis not started"),
             need(nrow(state$table1) > "0", "table1 not ready"),
             need(state$contr != "", "contribution matrix not ready"))
    state$table2 <- rebuildTable2(state$table1, state$contr, state$bleague$table, state$nmrleague$table, state$table2)
    print(c("rebuilded not building",state$table2))
  })

  observeEvent(input$table2col4, {
      t2col4 <- input$table2col4
      sel <-  unlist(strsplit(t2col4$id,"-vs-",fixed=T))
      icomparison <- sel[[2]]
      chr = filter(state$table2, icomparison == comparison) %>%
            mutate(contrEvaluation = as.integer(t2col4$value))
      state$table2 <- rows_update(state$table2, chr)
      state$table2$proposedFinal <- table2proposedFinal(state$table2)
  },ignoreNULL=T,ignoreInit=T)

  observeEvent(input$table2col8, {
      t2col8 <- input$table2col8
      sel <-  unlist(strsplit(t2col8$id,"-vs-",fixed=T))
      icomparison <- sel[[2]]
      chr = filter(state$table2, icomparison == comparison) %>%
            mutate(effectsEvaluation = as.integer(t2col8$value))
      state$table2 <- rows_update(state$table2, chr)
      state$table2$proposedFinal <- table2proposedFinal(state$table2)
  },ignoreNULL=T,ignoreInit=T)

  observeEvent(input$table2final, {
      t2final <- input$table2final
      sel <-  unlist(strsplit(t2final$id,"-vs-",fixed=T))
      icomparison <- sel[[2]]
      chr = filter(state$table2, icomparison == comparison) %>%
            mutate(final = as.integer(t2final$value))
      state$table2 <- rows_update(state$table2, chr)
  },ignoreNULL=T,ignoreInit=T)

  observeEvent(input$applyProposedTable2, {
    print("Applying proposed to Final")
    state$table2 <- mutate(state$table2, final = proposedFinal)
  })

  observeEvent(input$setSSEUndetected, {
    print("Setting SSE to No bias detected")
    state$table2 <- mutate(state$table2, effectsEvaluation = 1)
    state$table2$proposedFinal <- table2proposedFinal(state$table2)
  })

  observeEvent(input$resetTable2Finals, {
    print("Resetting final column")
    state$table2 <- mutate(state$table2, final = 0)
  })

  output$table1 <- DT::renderDataTable({
     validate(need(state$nmaDone == T, "netmeta not ready"))

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
                     , "No bias detected"
                     , paste("Suspected bias favouring ",treat1,sep="")
                     , paste("Suspected bias favouring ",treat2,sep="")
          )
        }
      }else{
        choices = c( ""
                   , "No bias detected"
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
       #check if changed from proposed
       changedstring = " "
       if(column=="overall_bias") {
         if(proposed != level){
           changedstring = " style='background-color:lightyellow;'"
         }
       }

        res = paste0("<select onchange=table1select(id) class=table1_selector "
                   ,changedstring
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
        th(rowspan = 2, ' '),
        th(rowspan = 2, 'Pairwise comparison'),
        th(rowspan = 2, 'group'),
        th(colspan = 2, 'Number of studies in each comparison'),
        th(colspan = 1, 'Within study assessment of bias'),
        th(colspan = 1, 'Across study assessment of bias'),
        th(colspan = 1, 'Overall bias')
      ),tr(
        th(colspan = 1, 'Reporting this outcome (sample size)'),
        th(colspan = 1, 'Total identified in the SR (total sample size)'),
        th(colspan = 1, 'Evaluation of selective reporting within studies using signalling questions'),
        th(colspan = 1, 'Qualitative and quantitative assessment of publication bias'),
        th(colspan = 1, 'Overall judgement')
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

  observeEvent(state$nmaDone, {
    validate(need(state$bdata !="", "bdata not ready")
            ,need(state$nmaDone == T, "nma not started")
            )
      print("starting bnma")
      state$bnma <- bnma(state$inputSM, state$bdata, state$inputRef, state$inputMod)
      state$bnmaDone = T
      print("bnma done")
  }, ignoreNULL=T, ignoreInit=T)

  bnma <- function(sm,bdata,ref,eff){
      if(sm=="OR" | sm=="RR"){
          model <- BUGSnet::nma.model(data=bdata,
                               outcome="r",
                               N="n",
                               reference=ref,
                               family="binomial",
                               link=ifelse(sm=="OR","logit", "log"),
                               effects= eff)
      }else{
       model <- BUGSnet::nma.model(data=bdata,
                                  outcome="mean",
                                  sd="sd",
                                  N="n",
                                  reference=ref,
                                  family="normal",
                                  link="identity",
                                  effects= eff)
      }
      results <- BUGSnet::nma.run(model,
                                  n.burnin=2000,
                                  n.iter=10000,
                                  n.chains = 2)
      return(results)
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
            fixed = modelFixed, random = modelRandom)
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
      tryCatch({datarawcomma <- read.csv(inFile$datapath, header = TRUE, sep=',', dec='.')},
               error = function(e){datarawcomma<-matrix()})
      tryCatch({datarawsemicolon1 <- read.csv(inFile$datapath, header = TRUE, sep=';', dec='.')},
               error = function(e){datarawsemicolon1<-matrix()})
      tryCatch({datarawsemicolon2 <- read.csv(inFile$datapath, header = TRUE, sep=';', dec=',')},
               error = function(e){datarawsemicolon2<-matrix()})
      nc <- tibble( name=c("datarawcomma","datarawsemicolon1","datarawsemicolon2")
                  , ncol=c(dim(datarawcomma)[2],dim(datarawsemicolon1)[2], dim(datarawsemicolon2)[2])
      )
      datarawname <- nc %>% arrange(desc(ncol)) %>% slice(1) %>% select("name")
      alldata <- as.data.frame(get(as.character(datarawname)))
      binaryColumns = c("id","study","t","n","r")
      continuousColumns = c("id","study","t","n","mean","sd")
      isBinary = all(lapply(binaryColumns, function(x){x %in% colnames(alldata)}))
      isContinuous = all(lapply(continuousColumns, function(x){x %in% colnames(alldata)}))
      if(isBinary){
        alldata <- alldata %>%
          mutate(n = as.integer(n) ) %>%
           mutate(r = as.integer(r) )
        directs <- alldata %>%
          filter(!is.na(r))
        otherOutcomes <- alldata %>%
          filter(is.na(r))
      }else{ # continuous
        if(!isContinuous){
          stop("missing columns or missmatching column names. Please refresh page")
        }
        alldata <- alldata %>%
          mutate(n = as.integer(n) ) %>%
           mutate(mean = as.numeric(mean) ) %>%
           mutate(sd = as.numeric(sd) )
        directs <- alldata %>%
          filter(!is.na(mean))
        otherOutcomes <- alldata %>%
          filter(is.na(mean))
      }
      return(list( isBinary=isBinary
                 , directs=directs
                 , otherOutcomes=otherOutcomes
                 , alldata=alldata
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
    DT::datatable(dataset())
  })

  getNMA <- reactive({
    validate(
      need(state$nma != "", "netmeta not ready")
    )
    state$nma
  })

  output$netgraph <- renderPlot({
    netgraph(getNMA(), col = "black", plastic=FALSE,
             points=T, col.points = "darkgreen", cex.points =10*sqrt(n.trts/max(n.trts)),
             thickness="number.of.studies", lwd.max = 12, lwd.min = 1, multiarm=F)
  })

  bdata <- function(mydata){
    res <- data.prep(arm.data = mydata,
                varname.t = "t",
                varname.s = "id")
    return(res)
  }

  btab <- reactive({
    validate(need(state$bdata != "", "bdata not ready")
             ,need(state$parametersSet == T, "parameters not set"))

    net.tab(data = state$bdata,
            outcome = ifelse(state$inputSM=="OR" | state$inputSM=="RR", "r", "mean"),
            N = "n",
            type.outcome = ifelse(state$inputSM=="OR" | state$inputSM=="RR", "binomial", "continuous"))
  })
  output$netinfo <- renderTable({
    btab()$network
  }, colnames = FALSE)
  output$intinfo <- renderTable({
    out <- btab()$intervention
    #Change here colnames
    if(state$allData$isBinary){
      colnames(out) <- c("Intervention","Total no. of studies", "Total no. of events", "Total no. of patients","Min observed event rate","Max observed event rate","Average event rate")
    }else{
      colnames(out) <- c("Intervention","Total no. of studies", "Total no. of patients","Min outcome value","Max outcome value","Average outcome value")
    }
    out
  })
  output$compinfo <- renderTable({
    #Change here colnames using the code from above
    if(state$allData$isBinary){
      out <- btab()$comparison[,-5]
      colnames(out) <- c("Comparison","Total no. of studies", "Total no. of patients","Total no. of events")
    }else{
      out <- btab()$comparison
      colnames(out) <- c("Comparison","Total no. of studies", "Total no. of patients")
    }
    out
  })


  observeEvent(state$bnmaDone, {
    validate(need(state$bnmaDone == T, "bnma not ready"))
    print("calculating bnma league")

    state$bleague <- BUGSnet::nma.league(state$bnma,
               central.tdcy="median",
               order = nma.rank(state$bnma, largerbetter=ifelse(input$inputBH=="good", F, T))$order,
               log.scale = FALSE
               )

      output$forest <- renderPlot({
        BUGSnet::nma.forest(state$bnma, comparator = state$inputRef) +
          ylab(paste(input$inputSM, "relative to", input$inputRef )) +
          theme(axis.text = element_text(size=15))
      })
      
      output$tau <- renderText({
        round(mean(c(mean(state$bnma$samples[1][[1]][,"sigma"]), mean(state$bnma$samples[2][[1]][,"sigma"]))),3)
      })

      output$league <- renderTable({
        state$bleague$table
      }, rownames = T)
    },ignoreNULL=T,ignoreInit=T)



  observeEvent(state$nmaDone, {
    validate(need(state$nmaDone == T, "netmeta not ready")
            )
    print("calculating NMRdata")
    newdata <- pool_variances(state$nma, myData())
    state$NMRdata <- data.prep(arm.data = newdata,
              varname.t = "t",
              varname.s = "id")
    print("calculated NMRdata")
  }, ignoreNULL=T, ignoreInit=T)


  observeEvent(state$bnmaDone,{
    validate(
      need(state$NMRdata != "", "NMRdata not ready")
      ,need(state$bnmaDone == T, "bnma not ready")
    )
    print("Start bnmr")
    if(state$inputSM=="OR" | state$inputSM=="RR")
      model <- BUGSnet::nma.model(data=state$NMRdata,
                         outcome="r",
                         N="n",
                         reference=input$inputRef,
                         family="binomial",
                         link=ifelse(state$inputSM=="OR","logit", "log"),
                         effects= input$inputMod,
                         covariate = "varStudies",
                         prior.beta = input$inputBeta)
    else model <- BUGSnet::nma.model(data=state$NMRdata,
                            outcome="mean",
                            sd="sd",
                            N="n",
                            reference=input$inputRef,
                            family="normal",
                            link="identity",
                            effects= input$inputMod,
                            covariate = "varStudies",
                            prior.beta = input$inputBeta)
  state$bnmr <- BUGSnet::nma.run(model,
                                 n.burnin=state$burnin,
                                 n.iter=state$numIter,
                                 n.chains = 2)
  state$bnmrDone <- T
  print("bnmr is Done")
  },ignoreInit=T)


  observeEvent(state$bnmrDone, {
    validate(need(state$bnmrDone == T, "bnmr not ready"))
    nmrleague <- nma.league(state$bnmr,
               central.tdcy="median"
               , order = nma.rank( state$bnmr
                                 , largerbetter=ifelse(state$inputBH=="good", F, T)
                                 , cov.value = min(state$NMRdata$arm.data$varStudies)
                                 )$order
               , log.scale = FALSE
               , cov.value = min(state$NMRdata$arm.data$varStudies)
               )
      state$nmrleague <- nmrleague
      output$nmr <- renderTable({
        nmrleague$table
      }, rownames = T)

      output$rhat <- renderTable({
        nma.diag(state$bnmr,plot_prompt = F)$gelman.rubin$psrf[,-2]
      }, rownames = T, colnames = F)

      
      output$coefficients <- renderTable({
          c1 <- state$bnmr$samples[1][[1]][,grep("beta",colnames(state$bnmr$samples[1][[1]]))]
          c2 <- state$bnmr$samples[2][[1]][,grep("beta",colnames(state$bnmr$samples[2][[1]]))]
        if(state$inputBeta=="UNRELATED"){
          coef <- colMeans(rbind(c1, c2))
        }else{
          coef <- mean(rbind(c1, c2))
        }
        coef
      }, rownames = ifelse(state$inputBeta=="UNRELATED", T, F), colnames = F)
      
      output$tracedownload <- downloadHandler(
        filename = "traceplots.pdf",       # name for the downloaded file with extension
        content = function(file) {
          pdf(file)
          nma.diag(state$bnmr,plot_prompt = F)
          dev.off()
        },
        contentType = 'pdf')

      output$nmrplot <- renderPlot({
        nma.regplot(state$bnmr) +
          xlab("Study variance of the (linear) treatment effect") +
          ylab(paste("Treatment effect (linear scale) versus", input$inputRef))
      })
      # TODO: fix pdf, download works but pdf won't open as it has no pages
      # output$nmrplotD <- downloadHandler(            
      #   filename = "nmrPlot.pdf",      
      #   content = function(file) {
      #     pdf(file)
      #     nma.regplot(state$bnmr) +
      #       xlab("Study variance of the (linear) treatment effect") +
      #       ylab(paste("Treatment effect (linear scale) versus", input$inputRef))
      #     dev.off()
      #   },
      #   contentType = 'pdf')
      
      output$minvar <- renderText({
        min(state$NMRdata$arm.data$varStudies)
      })
  }, ignoreNULL=T, ignoreInit=T)

  observeEvent(state$nmaDone,{
    validate(need(state$nmaDone == T, "netmeta not ready"))
    state$hasFunnels <- !is.null(fp())
  },ignoreNULL=T,ignoreInit=T)

  fp <- function() {
    nmafunnel(state$nma, small.values = state$inputBH)
  }

  output$fpprint <- renderPrint({
      fp()
  })

  output$fptable <- DT::renderDataTable(fp()$tests)

  output$plot2 <- renderPlot({
    validate(need(state$nmaDone == T, "netmeta not ready"))
    par(mfrow=c(2,3))
    fp()
  })

  output$mydownload <- downloadHandler(
    filename = "funnelPlots.pdf",       # name for the downloaded file with extension
    content = function(file) {
      pdf(file)
      fp()
      dev.off()
    },
    contentType = 'pdf')

  observeEvent(state$bnmrDone, {
    validate(need(state$analysisStarted == T, "Analysis not started"),
             need(state$bnmrDone == T, "bnmr not Done"))
     print("calculating contribution")
     cm <- netcontrib(state$nma, state$inputMod)
     state$contr <- round(cm, digits = 1) %>% mutate(comparison=rownames(cm)) %>% relocate(comparison)
     print("calculated contribution matrix")
     output$contr <- shiny::renderTable(state$contr, digits=1)
  }, ignoreInit=T, ignoreNULL = T)

  output$table1download <- downloadHandler(
    filename = "table1.csv",       # name for the downloaded file with extension
    content = function(file) {
      write.csv(state$table1, file, row.names=F)
    }
  )

  output$table2download <- downloadHandler(
    filename = "table2.csv",       # name for the downloaded file with extension
    content = function(file) {
      write.csv(state$table2, file, row.names=F)
    }
  )

  output$mydownload2 <- downloadHandler(
    filename = "contributionMatrix.xlsx",
    content = function(file) {
      write.xlsx(state$contr, file, row.names = F)
    }
  )
  output$smOptions <- renderUI({
    if(state$allData$isBinary){
      if(!state$analysisStarted ){
        chs = c("Odds Ratio" = "OR",
                "Risk Ratio" = "RR")
      }else{
        chs = state$inputSM
      }
     radioButtons(inputId = "inputSM", label = "Summary measure",
                  choices = chs,
                  selected = state$inputSM
                  )
    }else{
      if(!state$analysisStarted){
        chs = c("Mean difference" = "MD",
               "Standardized mean difference" = "SMD")
      }else{
        chs = state$inputSM
      }
     radioButtons(inputId = "inputSM", label = "Summary measure",
                  choices = chs,
                  selected = state$inputSM
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
      chr = filter(state$table1, icomparison == comparison) %>%
            mutate("{icolumn}" := as.integer(t1sel$value)) %>%
            mutate(proposed =
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
      state$table1 <- rows_update(state$table1, chr)
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
    print("setting to No bias detected")
    state$table1 <- mutate(state$table1, known_unknowns = 1) %>%
            mutate(proposed =
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
  })

  observeEvent(input$setUnknownsUndetected, {
    print("setting to No bias detected")
    state$table1 <- mutate(state$table1, unknown_unknowns = 1) %>%
            mutate(proposed =
                   mapply(proposeTable1Overall,known_unknowns, unknown_unknowns)
                   )
  })

  output$table2Header <- renderUI({
   validate(need(state$nma != "", "netmeta not ready")
           , need(nrow(state$table1)!="0","table1 empty"))
      tags$div(
        # actionButton("resetTable2Finals", "Delete all final overall entries"),
        actionButton("setSSEUndetected", "Set Evaluation of small-study effects to No evidence"),
        actionButton("applyProposedTable2", tags$b(style="color:blue","Use algorithm to calculate overall risk of bias judgements"))
      )
  })

  output$table1Header <- renderUI({
   validate(need(state$nma != "", "netmeta not ready")
           , need(nrow(state$table1)!="0","table1 empty"))
      tags$div(
        actionButton("setKnownsUndetected","Set Within-study assessment as No bias detected"),
        actionButton("unsetKnowns","Unset Within-study assessment of bias"),
        actionButton("setUnknownsUndetected","Set Across-study assessment as No bias detected"),
        actionButton("unsetUnknowns","Unset Across-study assessment of bias "),
        actionButton("applyProposedTable1", tags$b(style="color:blue","Use algorithm to calculate overall bias"))
      )
  })

 output$messages <- renderText({state$error})

 output$bhOptions <- renderUI({
    if(!state$analysisStarted){
      chs = c("Desirable" = "good",
              "Undesirable" = "bad")
    }else{
      if(state$inputBH == "good"){
        chs = c("Desirable" = "good")

      }else{
        chs = c("Undesirable" = "bad")
      }
    }
   radioButtons(inputId = "inputBH",
                label = "Smaller outcome values are",
                selected = state$inputBH,
                choices = chs
                )
 })
 output$ModelOptions <- renderUI({
    if(!state$analysisStarted ){
      chs = c("Random effects" = "random",
              "Fixed effects" = "fixed")
    }else{
      if(state$inputMod == "fixed") {
        chs = c("Fixed effects" = "fixed")
      }else{
        chs = c("Random effects" = "random")
      }
    }
   radioButtons(inputId = "inputMod",
                label = "Synthesis model",
                selected = state$inputMod,
                choices = chs
                )
  })

  output$ref <- renderUI({
    if(!state$analysisStarted ){
      chs = state$treatments
    }else{
      chs = state$inputRef
    }
    radioButtons(inputId = "inputRef",
                 label = "Choose the reference treatment",
                 selected = state$inputRef,
                 choices = chs)
  })

  output$bugsnetOptions <- renderUI({
    if(!state$analysisStarted ){
      bin = state$burnin
      iter = state$numIter
      tags$div(
        tags$h5(tags$b("Parameters for the Bayesian network meta-regression")),
        numericInput(
          inputId = "burnin",
          label = "Burn In",
          value = bin,
          min = 1000,
          max = NA,
          step = 100,
          width = NULL
        ),
        numericInput(
          inputId = "numIter",
          label = "Iterations",
          value = iter,
          min = 10000,
          max = NA,
          step = 1000,
          width = NULL
        )
      )
    }else{
      tags$div(
        tags$h5(tags$b("Parameters for the Bayesian network meta-regression")),
        tags$h5("Burn In"),
        tags$h6(state$burnin),
        tags$h5("Iterations"),
        tags$h6(state$numIter)
      )
    }
  })
  output$priorbeta <- renderUI({
    if(!state$analysisStarted ){
      chs = c("Unrelated treatment-specific interactions" = "UNRELATED",
              "Exchangeable/related treatment-specific interactions" = "EXCHANGEABLE")
    }else{
      if(state$inputBeta == "UNRELATED") {
        chs = c("Unrelated treatment-specific interactions" = "UNRELATED")
      }else{
        chs = c("Exchangeable/related treatment-specific interactions" = "EXCHANGEABLE")
      }
    }
    radioButtons(inputId = "inputBeta",
                 label = "Assumption for treatment-specific interactions",
                 selected = state$inputBeta,
                 choices = chs
    )
  })

 output$DataSummary <- renderUI({
    validate(
      need(state$analysisStarted == T, "Analysis parameters not set")
    )
    tags$div(
             h4("Network graph", align="center"),
             plotOutput("netgraph"),
             h4("Network characteristics", align = "left"),
             tableOutput("netinfo"),
             h4("Interventions characteristics", align = "left"),
             tableOutput("intinfo"),
             h4("Direct comparisons characteristics", align = "left"),
             tableOutput("compinfo")
             )
 })

 #set parameter
 observe({
    validate(
      need(state$inputSM != "", "SM not selected"),
      need(state$inputMod != "", "Model not selected"),
      need(state$inputRef != "", "Reference not selected"),
      need(state$inputBH != "", "Value direction not selected"),
      need(state$inputBeta != "", "Assumption coefficients not selected")
    )
   print("parameters set")
    state$parametersSet <- T
 })

 observeEvent(input$inputSM,{
     print("seting state$inputSM")
     state$inputSM <- input$inputSM
   }
 )

 observeEvent(input$numIter,
    state$numIter <- input$numIter
 )

 observeEvent(input$burnin,
    state$burnin <- input$burnin
 )

 observeEvent(input$inputMod,
    state$inputMod <- input$inputMod
 )
 observeEvent(input$inputRef,
    state$inputRef <- input$inputRef
 )
 observeEvent(input$inputBH,
    state$inputBH <- input$inputBH
 )
 observeEvent(input$inputBeta,
    state$inputBeta <- input$inputBeta)

 resetParameters <- reactive({
      state$parametersSet <- F
      state$inputSM <- ""
      state$inputMod <- ""
      state$inputRef <- ""
      state$inputBH <- ""
 })
   resetAnalysis <- reactive({
     state$analysisStarted <- F
     state$nma <- ""
     state$nmaDone <- F
     state$bdata <- ""
     state$bnma <- ""
     state$bnmaDone <- F
     state$NMRdata <- ""
     state$bnmr <- ""
     state$bnmrDone <- F
   })

 #start analysis button
 output$setButton <- reactive({
   if(state$parametersSet &
      !state$analysisStarted ){
     res = "<button onclick='startAnalysis()'>Start Analysis</button>"
   }else{
    res=""
   }
   res
 })

 output$unsetButton <- reactive({
   validate(need(state$analysisStarted == T, ""))
   res = "<button onclick='resetAnalysis()'>Reset Analysis</button>"
 })

 observeEvent(input$startAnalysis,{
   print("starting analysis")
   state$analysisStarted <- T
 })

 observeEvent(input$resetAnalysis,{
   print("reset analysis")
   resetParameters()
   resetAnalysis()
 })

 output$sidePanel <- renderUI({
   tags$div(
   uiOutput("smOptions"),
   uiOutput("bhOptions"),
   uiOutput("ModelOptions"),
   uiOutput("ref"),
   uiOutput("bugsnetOptions"),
   uiOutput("priorbeta")
   )
 })

 output$bayesianNMA <- renderUI({
   validate(need(state$analysisStarted==T,"analysis not started"),
            need(state$bnmaDone == T, "waiting for analysis"))

            tags$div(
              h4("Posterior medians and 95% Cr.I.", align = "center"),
              conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                               tags$div(id = "plot-container5", tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif", id = "loading-spinner1")),
                               tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage5")),
              div(plotOutput("forest", height = "500px", width = "800px"), align = "center"),
              br(),
              h5("The heterogeneity (tau) is estimated at ", textOutput("tau", inline = T)),
              br(),
              h4("League table", align = "center"),
              conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                               tags$div(id = "plot-container6", tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif", id = "loading-spinner2")),
                               tags$div("Please wait. The calculations of network meta-analysis may take up to several minutes.",id="loadmessage6")),
              div(tableOutput("league"), style = "font-size:80%", align = "center"),
              h6(paste(state$inputSM, "and 95% credible intervals of treatment in the column versus treatment in the row"))
            )
 })

 output$bayesianNMR <- renderUI({
 validate(need(state$bnmrDone == T, "waiting for analysis"))
   isolate({
    tags$div(
      h4("Checks for convergence of network meta-regression model", align = "center"),
      p("Check the trace plots (download) and the Gelman-Rubin diagnostic values (table below) being close to 1 for convergence. If needed, increase number of iterations and burn-in or change the assumption for treatment-specific interactions to 'Exchangeable' and rerun analysis"),
      div(tableOutput("rhat"), align= "center"),
      downloadButton('tracedownload', 'Download Trace Plots as PDF'),
      h4("Network meta-regression for variance of the (linear) treatment effect", align = "center"),
      p(ifelse(state$inputBeta=="UNRELATED", "Values of the coefficients (betas) in the regression model between relative treatment effects and study variance", "Average value of the coefficients (betas) in the regression model between relative treatment effects and study variance")),
      div(tableOutput("coefficients"), align= "center"),
      p("Each line shows how the linear effect of each treatment versus reference changes for different study variances.
        The value at variance 0 are the extrapolated linear effects of each treatment versus reference for an imaginary study with 0 variance."),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div(id = "plot-container1", tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif", id = "loading-spinner1")),
                       tags$div("Please wait. The calculations of network meta-regression may take up to several minutes.",id="loadmessage1")),
      div(plotOutput("nmrplot", height = "500px", width = "800px"), align = "center"),
      #downloadButton('nmrplotD', 'Download Regression Plot as PDF'),
      h4("League table", align = "center"),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div(id = "plot-container2", tags$img(src = "https://media.giphy.com/media/3oEjI6SIIHBdRxXI40/giphy.gif", id = "loading-spinner2")),
                       tags$div("Please wait. The calculations of network meta-regression may take up to several minutes.",id="loadmessage2")),
      p("League table showing results for the minimum observed variance of", textOutput("minvar", inline = T), align= "center"),
      div(tableOutput("nmr"), style = "font-size:80%", align = "center")
    )
   })
 })

 output$mainPanel <- renderUI({
   if(state$analysisStarted){
                 tabsetPanel(
                   tabPanel("Data Summary",uiOutput("DataSummary")),
                   tabPanel("Bayesian network meta-analysis", uiOutput("bayesianNMA")),
                   tabPanel("Bayesian network meta-regression", uiOutput("bayesianNMR")),
                   tabPanel("Funnel plots and test for small-study effects",
                            tabPanel("Contour-enhanced funnel plots",
                                     uiOutput("funnelplots")
                            )
                   ),
                   tabPanel("Contribution matrix",
                            tags$br(),   # line break
                            p("Each cell entry provides the percentage contribution that the direct comparison (column) makes to the calculation of the corresponding NMA relative treatment effect (row)."),
                            tags$br(),   # line break
                            tableOutput("contr"),
                            downloadButton('mydownload2', 'Download Contribution Matrix'))
                 )
   }else{
     tags$h4("analysis not started")
   }
})

  output$dataAnalysis <- renderUI({
    validate(need(myData() != '', "no dataset present"))
             sidebarLayout(
               sidebarPanel(
                 uiOutput("sidePanel"),
                 uiOutput("setButton"),
                 # uiOutput("unsetButton"),
                 width = 3
               ),
               mainPanel(
                 uiOutput("mainPanel"),
                 width = 9
               )
               )
  })

 output$funnelplots <- renderUI({
   validate(need(state$nmaDone == T, "netmeta not ready")
           )
    if(state$hasFunnels){
          fluidPage(
            fluidRow(
              verbatimTextOutput("fpprint"),
              dataTableOutput("fptable")),
            hr(),
            fluidRow(
              tags$h4("Only some funnel plots are shown here. To view all plots, press the button below to download them as PDF."),
              plotOutput("plot2"),
              downloadButton('mydownload', 'Download Plots as PDF')
            )
          )
    }else{
      tags$h3("All comparisons have fewer than 10 studies")
    }
   })
 
 output$loaddata <- renderUI({
   validate(need(state$analysisStarted==F,"Analysis already started. Please refresh page if you need to upload new data"))
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
                    tags$h4("This tab provides instructions for", tags$b("long format"), "data, where each row contains one treatment arm", tags$u("for all studies identified in the systematic review, including those not reporting the outcome of interest."), "Please follow the steps below."),
                    tags$br(),   # line break
                    tags$h5("The long format data file should contain five columns (for binary data) or six columns (for continuous data) labelled as follows:"),
                    tags$ul(
                      tags$li("Column", tags$i(tags$b("id")), "containing the study identifier, starting from 1, then 2, 3, 4... etc."),
                      tags$li("Column", tags$i(tags$b("study")), "containing the name (e.g., author,year) of the study."),
                      tags$li("Column", tags$i(tags$b("t")), "containing the treatment code used in each arm of the study."),
                      tags$li("Column", tags$i(tags$b("n")), "containing the number of participants in each arm of the study."),
                      tags$li(tags$i("(For binary data)"),"Column", tags$i(tags$b("r")), "containing the number of participants with the outcome of interest in each arm of the study.", tags$u("For studies not reporting the outcome of interest this should be empty or contain an asterisk")),
                      tags$li(tags$i("(For continuous data)"),"Column", tags$i(tags$b("mean")), "the mean value of the outcome in each arm of the study.", tags$u("For studies not reporting the outcome of interest this should be empty or contain an asterisk")),
                      tags$li(tags$i("(For continuous data)"),"Column", tags$i(tags$b("sd")), "the standard deviation of the outcome in each arm of the study.", tags$u("For studies not reporting the outcome of interest this should be empty or contain an asterisk"))
                    ),
                    tags$br(),
                    tags$h4("Demo dataset"),
                    tags$h5("Click", tags$a(href='cad_detection.csv', target='blank', 'here', download = 'cad_detection.csv'), "to download the demo dataset: a network of non-invasive diagnostic modalities for the detection of coronary artery disease in patients with low risk acute coronary syndrome. ",
                            tags$a(href="https://www.bmj.com/content/360/bmj.k504", "Siontis G C, et al. Outcomes of non-invasive diagnostic modalities for the detection of coronary artery disease: network meta-analysis of diagnostic randomised controlled trials. BMJ 2018; 360 :k504"))
                    ),
           tabPanel("View data", DT::dataTableOutput('contents'))
         ),
         width = 9
       )
     )
 })
}






ui <- tags$div(id="wrapper",
  fluidPage(
    tags$head(
      tags$style(HTML("#wrapper {
            min-height: 100vh;
            display: flex;
            flex-direction: column;
        }
        .container-fluid { flex-grow: 1; width: 100%; }"
        ))
    ),
    titlePanel("ROB-MEN: Risk Of Bias due to Missing Evidence in Network meta-analysis"),
    tags$script(src = "tables.js") ,
    tags$head(tags$style(HTML(mycss))),

    tags$div(uiOutput("messages")),
    tabsetPanel(
      tabPanel("Load data", uiOutput("loaddata")
      ),

      tabPanel("Data analysis",
                     uiOutput("dataAnalysis")
               ),

      tabPanel( "Pairwise Comparison Table"
              , uiOutput("table1Header")
              , tabPanel("View data", DT::dataTableOutput('table1'))
              , downloadButton('table1download', 'Download Pairwise Comparison Table')
              ),

      tabPanel("ROB-MEN Table"
              , uiOutput("table2Header")
              , tabPanel("View data", DT::dataTableOutput('table2'))
              , downloadButton('table2download', 'Download ROB-MEN Table')
      )
    )
  ), # end fluidPage
  tags$footer(
    tags$div(
      tags$div(
        tags$img(src = "snsf.png", width = "250px", height = "82px"),
        style="float: left; width: 50%; text-align: center; padding: 10px 0"
      ),
      tags$div(
        tags$img(src = "unibe.png", width = "250px", height = "82px"),
        style="float: left; width: 50%; text-align: center; padding: 10px 0"
      ),
      tags$br(),
      "Please cite ROB-MEN as", tags$i("Chiocchia V. et al. ROB-MEN: a tool to assess risk of bias due to missing evidence in network meta-analysis. BMC Med 19, 304 (2021)."), tags$a(href="https://doi.org/10.1186/s12916-021-02166-3", "DOI: 10.1186/s12916-021-02166-3. "),
      "ROB-MEN is distributed, in the hope that it will be useful but without any warranty, under the ", tags$a(href='LICENSE.txt', target='blank', 'GNU General Public License.', download = 'LICENSE.txt'),
      "By using ROB-MEN you accept the ", tags$a(href="DISCLAIMER.txt", target="_blank", "DISCLAIMER.", download = "DISCLAIMER.txt"),
      style="width:80%; margin: 0 auto;"
    ),
    style = "
      flex-shrink: 0;
      padding: 10px 0;
      margin-top: 10px;
      width:100%;
      background: #f8f9fa;"
  )
) # end wrapper


shinyApp(ui = ui, server = server)
