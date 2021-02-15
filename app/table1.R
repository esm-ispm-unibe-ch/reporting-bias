buildTable1 = function(treatments,directslong,otherOutcomeslong){
    alldata <- rbind(directslong, otherOutcomeslong)

    combinations <- t(combn(treatments,2))
    colnames(combinations) <- c("treat1","treat2")
    allcomparisons <- as.tibble(combinations) %>% 
      mutate(comparison =
          mapply(function(x,y){comp = sort(c(x,y)); return(paste(comp[1],comp[2],sep=":"))}, treat1, treat2)
      ) %>%
      select(comparison,treat1,treat2)

    getTable1Rows <- function(longdata){
      pw <- as.tibble(pairwise(treat=t, event=r, n=n, data = longdata , studlab = id)) %>% 
        mutate(comparison =
            mapply(function(x,y){comp = sort(c(x,y)); return(paste(comp[1],comp[2],sep=":"))}, treat1, treat2)
              )
      dirspw = pw %>% group_by(comparison) %>% summarise(ns=length(study),n=n1+n2) %>%
        group_by(comparison,ns) %>% summarise(n = sum(n))
      return(dirspw) 
    }
    
    groupAcolumn1 = getTable1Rows(directslong) 
    groupAcolumn1 = groupAcolumn1 %>% rename(numstudies=ns, samplesize=n)
    
    allDirectComparisons = getTable1Rows(alldata) %>% rename(total.numstudies=ns, total.samplesize=n)
    groupA = groupAcolumn1 %>% left_join(allDirectComparisons, by="comparison") %>% mutate(compgroup="groupA")
    
    groupB = allDirectComparisons %>% anti_join(groupAcolumn1, by="comparison") %>% 
      filter(comparison %in% allcomparisons$comparison) %>% mutate(compgroup="groupB")
    
    groupC <- allcomparisons %>% select(comparison) %>%
              anti_join(groupA, by="comparison") %>%
              anti_join(groupB, by="comparison") %>% mutate(compgroup="groupC")
    
    restable1 <- rbind(groupA,groupB,groupC) %>% left_join(allcomparisons, by="comparison") %>%
      mutate(known_unknowns = 0) %>%
      mutate(unknown_unknowns = 0) %>%
      mutate(proposed = 0) %>%
      mutate(overall_bias = 0)
    column_to_rownames(restable1, var = "comparison")
    print(restable1)
    return(restable1)
}