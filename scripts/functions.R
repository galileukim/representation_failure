# ideology estimation based on Bonica (2014)
get.contrib.means <- function(contrib_matrix,cand.ips,contrib.ips=NULL,weights=NULL,upper.limit=2000,
                              get.sds=FALSE,cores=1){
    numconts <- nrow(contrib_matrix)
    use.weights <- !is.null(weights)
    use <- !is.na(as.numeric(cand.ips))
    t1 <- contrib_matrix[,use] %*% (as.numeric(cand.ips[use]))
    t2 <- rowSums(contrib_matrix[,use])
    contrib.ips <- as.numeric(t1/t2)
    return(contrib.ips)
}

get.cand.means <- function(contrib_matrix,contrib.ips,weights=NULL,dynamic.cands=FALSE,
                           upper.limit=30,cores=1,get.sds=F){
  numconts <- ncol(contrib_matrix)
  contrib_matrix <- contrib_matrix[!is.na(contrib.ips),]
  contrib.ips <- contrib.ips[!is.na(contrib.ips)]
  count <- 1

  cand.ips <- rep(NA,numconts)
  t1 <- t(contrib_matrix) %*% (as.numeric(contrib.ips))
  t2 <- colSums(contrib_matrix)
  cand.ips <- as.numeric(t1/t2)
  
  return(cand.ips)
}

normalize.rows <- function(x,y){
  y <- y/ sum(y)
  ##CENTERING
  ctr <- as.numeric(t(y) %*% x)
  x <- x - as.numeric(ctr)
  ##SCALING VARIANCE = 1
  vtr <-  t(x) %*% Diagonal(length(y),y) %*% x
  x <- x * (1/sqrt(as.numeric(vtr)))
  return(x)
}

normalize.cols <- function(x,y){
  y <- y/ sum(y)
  ##CENTERING
  ctr <- as.numeric(t(y) %*% x)
  x <- x - as.numeric(ctr)
  ##SCALING VARIANCE = 1
  vtr <-  t(x) %*% Diagonal(length(y),y) %*% x
  scale.param <- (1/sqrt(as.numeric(vtr)))
  x <- x * scale.param
  return(list(x=x,scale.param=scale.param))
}

awm <- function(cands=NA, cm=NA,iters = 8, party_ideology){
    cat('\n Correspondence Analysis: \n')
    print(table(cands[,'party']))
    
    ##EXTRACTING CANDIDATE IDS
    cpf_candidate <- (cands[,'cpf_candidate'])
    mm <- !duplicated(cpf_candidate)
    cands <- cands[mm,]
    
    # create contributor ideal point repo
    contrib_matrix <- cm
    contrib.ips.rval <- rep(NA,nrow(contrib_matrix))

    ##SETTING INITIAL PARAMTERS AT -1 FOR LEFT AND 1 FOR RIGHT
    # anchor left and right based on Power and rodrigues silveira (2018)
    cands <- cands %>% 
      left_join(
        party_ideology, 
        by = "party"
      )

    ko <- which(is.na(cands[,'cfscore']))
    cands[ko,'cfscore'] <- mean(as.numeric(cands$cfscore[-ko], na.rm = T))
    
    # candidate ideal points
    cand.ips <- as.numeric(cands[['cfscore']])

    for(iter in 1:iters){
        cat('Iteration ',iter,'\n')
        contrib.ips <- get.contrib.means(contrib_matrix,weights=NULL,cand.ips,
                                         cores=cores,upper.limit=contrib.lim)
        contrib.ips[is.na(contrib.ips)] <- 0
        contrib.ips <- normalize.rows(contrib.ips,rowSums(contrib_matrix))
        cand.ips <- get.cand.means(contrib_matrix,contrib.ips,weights=NULL,upper.limit=NA,
                                   dynamic.cands=dynamic.cands,cores=cores)
        cand.ips[is.na(cand.ips)] <- 0
        cand.ips[cand.ips > 4] <- 4
        cand.ips[cand.ips < -4] <- -4
        out <- normalize.cols(cand.ips,colSums(contrib_matrix))
        cand.ips <- out$x
        scale.param <- out$scale.param
        tt <- match(cpf_candidate, unique(cpf_candidate))
        cands[,'cfscore'] <- cand.ips
    }
    
    contrib.ips.rval <- contrib.ips

    rval <- list(cands = cands,
                 contrib.ips=contrib.ips.rval,
                 scale.param = scale.param)
    return(rval)
}
