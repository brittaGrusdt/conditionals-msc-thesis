source("constants.r")
library(dplyr)
library(purrr)
library("varhandle")
####################

buildDF_from_samples <- function(listener, withQUD=TRUE){
  df <- data.frame(matrix(ncol = 3, nrow = length(listener$Parameter=='bn.table')))
  colnames(df) <- c("bn.table", "BN")
  df$bn.table = listener$value[listener$Parameter=='bn.table']
  df$bn.cn = listener$value[listener$Parameter=='bn.cn']
  if(withQUD){
    df$qud = listener$value[listener$Parameter=='qud']
  }
  df$prob = rep(1/length(df$bn.table), length(df$bn.table))
  return(df)
}

buildDF_from_enumerate <- function(listener, withQUD=TRUE){
  df <- data.frame(matrix(ncol = 4, nrow = length(listener$bn.table)))
  colnames(df) <- c("bn.table", "bn.cn", "qud", "prob")
  df$bn.table <- listener$bn.table
  df$bn.cn <- listener$bn.cn
  if(withQUD){
    df$qud <- listener$qud
  }else{
    df$qud <- rep("-", length(listener$bn.table))
  }
  df$prob = listener$prob
  return(df)
}

computeProbs <- function(tables){
  probs <- data.frame(matrix(ncol = 9, nrow = length(tables)))
  colnames(probs) <- c("pca", "pcna", "pnca", "pncna", "pc",
                       "pa", "pCgivenA", "pAgivenC", "pCgivenNA")

  probs$pca <- as.numeric(map(tables, 1))
  probs$pcna <- as.numeric(map(tables, 2))
  probs$pnca <- as.numeric(map(tables, 3))
  probs$pncna <- as.numeric(map(tables, 4))
  
  probs$pc <- as.numeric(Map("+", probs$pca, probs$pcna))
  probs$pa <- as.numeric(Map("+", probs$pca, probs$pnca))
  probs$pCgivenA <- as.numeric(Map("/", probs$pca, probs$pa))
  probs$pAgivenC <- as.numeric(Map("/", probs$pca, probs$pc))
  probs$pAgivenNC <- as.numeric(Map("/", probs$pnca, 1-probs$pc))
  probs$pCgivenNA <- as.numeric(Map("/", probs$pcna, 1-probs$pa))
  probs$pNCgivenNA <- as.numeric(Map("/", probs$pncna, 1-probs$pa))
  probs$pNAgivenNC <- as.numeric(Map("/", probs$pncna, 1-probs$pc))
  return(probs)
}

##### expected values ###### 

getEVs <- function(listener_df, bias, withQud){
  
  EV_probs <- marginalEVTables(listener_df, bias)
  EV_cns <- marginalEVCNs(listener_df)
  if(withQud){
    EV_quds <- marginalEVQuds(listener_df)
    EVs <- cbind(EV_quds,EV_probs, EV_cns)
  }else{
    EVs <- cbind(EV_probs, EV_cns)
  }
  return(EVs)
}

marginalEVTables <- function(df_listener, bias){
  probVals <- computeProbs(df_listener$bn.table)
  ps <- df_listener$prob
  EVs <- data.frame(matrix(ncol = 10, nrow = 1))
  colnames(EVs) <- c("p_c_given_a", "p_c_given_na", "p_c",
                     "p_a_given_c", "p_a_given_nc", "p_a", 
                     "p_CA", "p_CNA", "p_NCA", "p_NCNA")
  rownames(EVs) <- c(bias)

  EVs$p_c_given_a <- round(x=sum(probVals$pCgivenA * ps), digits=3)
  EVs$p_a_given_c <- round(x=sum(probVals$pAgivenC * ps), digits=3)
  EVs$p_c_given_na <- round(x=sum(probVals$pCgivenNA * ps), digits=3)
  EVs$p_a_given_nc <- round(x=sum(probVals$pAgivenNC * ps), digits=3)
  EVs$p_a <- round(x=sum(probVals$pa * ps), digits=3)
  EVs$p_c <- round(x=sum(probVals$pc * ps), digits=3)

  EVs$p_CA <- round(x=sum(probVals$pca * ps), digits=3)
  EVs$p_CNA <- round(x=sum(probVals$pcna * ps), digits=3)
  EVs$p_NCA <- round(x=sum(probVals$pnca * ps), digits=3)
  EVs$p_NCNA <- round(x=sum(probVals$pncna * ps), digits=3)
  
  return(EVs)
}

marginalEVQuds <- function(listener_df){
  df <- data.frame(matrix(ncol=2, nrow=1))
  colnames(df) <- c("qud_bn", "qud_table")
  iter <- 0
  for(qud in QUDs){
    iter <- iter + 1
    quds <- listener_df$qud
    quds[quds!=qud] <- 0
    quds[quds==qud] <- 1
    quds <- as.numeric(quds)  
    df[,iter] <- round(x=sum(quds * listener_df$prob), digits=3)    
  }
  return(df)
}

marginalEVCNs <- function(listener_df){
  cns <- listener_df$bn.cn
  df <- data.frame(matrix(ncol=9, nrow=1))

  iter <- 0
  for(cn in CNs){
    iter <- iter + 1
    cns <- listener_df$bn.cn
    cns[cns!=cn] <- 0
    cns[cns==cn] <- 1
    cns <- as.numeric(cns)
    df[,iter] <- round(x=sum(cns * listener_df$prob), digits=3)    
  }
  colnames(df) <- CNs
  return(df)
}

conditionedEVs <- function(listener_df){
  probVals <- computeProbs(listener_df$bn.table)
  ps <- listener_df$prob
  
  all_results <- data.frame(matrix(ncol=4, nrow=9))
  colnames(all_results) <- c("p_CA", "p_CNA", "p_NCA", "p_NCNA")
  rownames(all_results) <- CNs
  iter <- 0
  for(cn in CNs){
    iter <- iter + 1
    EVs <- data.frame(matrix(ncol = 4, nrow = 1))
    colnames(EVs) <- c("p_CA", "p_CNA", "p_NCA", "p_NCNA")
    
    cns <- listener_df$bn.cn
    indices <- cns == cn
    pcn <- sum(ps[indices])
    EVs$p_CA <- round(x=sum(probVals$pca[indices] * (ps[indices]/pcn)),
                      digits=3)
    EVs$p_CNA <- round(x=sum(probVals$pcna[indices]* (ps[indices]/pcn)),
                       digits=3)
    EVs$p_NCA <- round(x=sum(probVals$pnca[indices]* (ps[indices]/pcn)),
                       digits=3)
    EVs$p_NCNA <- round(x=sum(probVals$pncna[indices]* (ps[indices]/pcn)),
                        digits=3)
    all_results[iter,] <- EVs
  }
  return(all_results)
}

##### save, read data ###### 
saveResults <- function(results, bias, n_runs, target_dir){
  pdf(paste(target_dir, bias, "-", n_runs, "-runs", sep=""))
  grid.newpage()
  grid.table(t(results))
  dev.off()
}

getSamples <- function(lt, posterior){
  samples <- get_samples(posterior,10000)
  if(lt=='LL'){
    tables <- samples$table
    cns <- samples$cn
  }else{
    tables <- samples$bn.table
    cns <- samples$bn.cn
  }
  probs <- computeProbs(tables)
  probs$lt <- rep(lt,length(probs$pc))
  probs$cn <- cns
  return(probs)
}

getData <- function(ll_posterior, pl_posterior){
  df_ll <- getSamples('LL', ll_posterior)
  df_pl <- getSamples('PL', pl_posterior)
  df <- rbind(df_ll,df_pl)
  return(df)
}

readData <- function(lt, bias, folderName, nrun=1){
  target_dir <- paste("./", folderName, "/", lt, "/",sep="")
  fn <- paste(target_dir, bias, "-run-", nrun, ".rds", sep="")
  print(paste('read data from: ', fn, sep=""))
  data <- readRDS(fn)
  return(data)
}





