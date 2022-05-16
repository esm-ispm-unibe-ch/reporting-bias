calculate_pooled_variance <- function(nma, study_id) {
  x <- nma$data[nma$data$studlab == study_id, c("studlab","n1","n2","treat1","treat2","var", ".narms")]
  num <- (x$n1 + x$n2-1) * x$var
  den <- (x$.narms[1] - 1) * (sum(x$n1 + x$n2)) - (x$.narms[1] * (x$.narms[1] - 1))/2
  round(sum(num) / den, 3)
}

pool_variances <- function(nma, data) {
  
  nma$data$var <- round(nma$seTE.adj^2, 3)  # handle the case of only two arms
  to.replace <- which(nma$n.arms > 2)  # handle the case of multiple arm studies
  to.calculate <- which(nma$narms > 2) # note n.arms and narms are different variables
  pooled <- sapply(nma$studies[to.calculate], function(study_id){
                                              calculate_pooled_variance(nma, study_id)
                                        })
  if(any(nma$n.arms>2)){
    pooled <- data.frame(id_multi = as.numeric(names(pooled)), pooled_var = pooled)
    to.join <- nma$data %>%         # get id and var only for two-arm studies to merge with data
      select(studlab, var) %>%
      mutate(studlab = as.numeric(studlab)) %>%
      group_by(studlab) %>%
      filter(n() == 1)
    data <- data %>% dplyr::left_join(pooled, by = c("id" = "id_multi")) %>%
      left_join(to.join, by = c("id" = "studlab")) %>%
      mutate(varStudies = coalesce(pooled_var, var)) %>%  # TODO: give a meaningful name to new column
      select(-c(var, pooled_var))
  }else{
    to.join <- nma$data %>%         # get id and var only for two-arm studies to merge with data
      select(studlab, var) %>%
      mutate(studlab = as.numeric(studlab)) %>%
      group_by(studlab) %>%
      filter(n() == 1)
    data <- data %>% dplyr::left_join(to.join, by= c("id" = "studlab")) %>%
      mutate(varStudies = var) %>%  # TODO: give a meaningful name to new column
      select(-c(var))
  }
  
  return(data)
  
#  data <- cbind(data, varStudies=rep(nma$data[!duplicated(nma$data$studlab), "var"], times = nma$narms))
#  return(data)
}

