pooledVar <- function(nma, data) {

  nma$data$var <- round(nma$seTE.adj^2, 3)

  #narms <- count(data, id, wt=n())
  #multiarm <- which(nma$narms>2)
  nma$data$var[which(nma$n.arms>2)] <- unlist(rep(sapply(nma$studies[which(nma$narms>2)],
                                                  function(i){
                                                    x <- nma$data[nma$data$studlab==i, c("studlab","n1","n2","treat1","treat2","var", ".narms")]
                                                    num <- (x$n1+x$n2-1)*x$var
                                                    den <- (x$.narms[1]-1)*((sum(x$n1+x$n2)-x$.narms[1]))
                                                    round(sum(num)/den, 3)
                                                  }), times = nma$narms[which(nma$narms>2)]*(nma$narms[which(nma$narms>2)]-1)/2))
  data <- cbind(data, varStudies=rep(nma$data[!duplicated(nma$data$studlab), "var"], times = nma$narms))
  return(data)
}

