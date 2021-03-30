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
reversecomparison = function(comparison){
  ctrs <- comparisonToTreatments(comparison)
  return(paste(ctrs[2],ctrs[1],sep=":"))
}
##
buildTable2 <- function(table1, contribution.matrix, nmaleague, nmrleague){
  namedMixed <- function(compgroup){
    if(compgroup == "groupA"){
      res <- "mixed/only direct"
    }else{
      res <- "indirect"
    }
    res
  }
  getBiasContribution <- function(compr, treat){
    cmrow <- contribution.matrix[ contribution.matrix$comparison == reversecomparison(compr)
                                | contribution.matrix$comparison == compr,]
    biasedComparisons <- c()
    if (missing(treat)){
      biasedComparisons <- as.vector(unlist(table1 %>% 
        filter( compgroup == "groupA" ) %>%
        filter( (overall_bias == 2 | overall_bias == 3)
              ) %>%
        select(comparison)
      ))
    }else{
      biasedComparisons <- as.vector(unlist(table1 %>% 
        filter( compgroup == "groupA" ) %>%
        filter( (treat==treat1 & overall_bias == 2)
              | (treat==treat2 & overall_bias == 3)
              ) %>%
        select(comparison)
      ))
    }
    if(identical(biasedComparisons, character(0))){
      return(0)
    }else{
      revbiasedComparisons <- unlist(lapply(biasedComparisons, reversecomparison))
      biasedComparisons <- c(biasedComparisons,revbiasedComparisons)
      bias <- select(cmrow, -comparison) %>% 
        select(one_of(biasedComparisons))
      # print(c("comparison",compr,"biasedComparison",biasedComparisons,"bias",bias) ) 
      return(sum(bias))
    }
  }
  table2 <- table1 %>%
    rename(table1_overall_bias = overall_bias) %>%
    mutate(mixed = namedMixed(compgroup)) %>%
    mutate(contrTreat1 = suppressWarnings(getBiasContribution(comparison, treat1))) %>%
    mutate(contrTreat2 = suppressWarnings(getBiasContribution(comparison, treat2))) %>%
    mutate(contrTreat3 = suppressWarnings(getBiasContribution(comparison))) %>%
    mutate(contrEvaluation = 0) %>%
    mutate(nmaEffect = nmaleague[treat1,treat2]) %>%
    mutate(nmrEffect = nmrleague[treat1,treat2]) %>%
    mutate(effectsEvaluation = 0) %>%
    mutate(proposedFinal = 0) %>%
    mutate(final = 0) %>%
    select(comparison
          ,treat1
          ,treat2
          ,contrTreat1
          ,contrTreat2
          ,contrTreat3
          ,contrEvaluation
          ,table1_overall_bias
          ,nmaEffect
          ,nmrEffect
          ,effectsEvaluation
          ,proposedFinal
          ,final
          ,mixed) 
  return(table2)
}

table2proposedFinal <- function(table2){
  mixed <- table2$mixed
  cls <- as.numeric(table2$contrEvaluation)
  ols <- as.numeric(table2$table1_overall_bias)
  els <- as.numeric(table2$effectsEvaluation)
  
  propose <- function(cl, ol, el, mix){
    if((cl+el)==0){
      out <- 0
    }else{
      out <- 2 # default some concerns
    }
    if(((cl == 1) | (cl == 4))){
      if((el == 1) | (mix=="indirect" & ol == 1)){
        out <- 1
      }
    }else{
      if(((cl==2)|(cl==3))){
        if(((cl == el) | ((cl == ol) & (mix == "indirect")))){
          out <- 3
        }
      }
    }
    out
  }
  res <- mapply(propose, cls, ols, els, mixed)
}

rebuildTable2 <- function(table1, contribution.matrix, nmaleague, nmrleague, table2){
  res <- buildTable2(table1, contribution.matrix, nmaleague, nmrleague)
  res$contrEvaluation <- table2$contrEvaluation
  res$effectsEvaluation <- table2$effectsEvaluation
  res$proposedFinal <- table2proposedFinal(res)
  res$final <- table2$final
  return(res)
}