source("constants.r")
library(purrr)
library(dplyr)
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

computeProbs <- function(df){
  probs <- data.frame(matrix(ncol = 9, nrow = nrow(df)))
  colnames(probs) <- c("pca", "pcna", "pnca", "pncna", "pc",
                       "pa", "pCgivenA", "pAgivenC", "pCgivenNA")
  tables <- df$bn.table
  
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
  return(probs)
}

marginalEVTables <- function(df_listener, type){
  probVals <- computeProbs(df_listener)
  ps <- df_listener$prob
  EVs <- data.frame(matrix(ncol = 10, nrow = 1))
  colnames(EVs) <- c("p_c_given_a", "p_c_given_na", "p_c",
                     "p_a_given_c", "p_a_given_nc", "p_a", 
                     "p_CA", "p_CNA", "p_NCA", "p_NCNA")
  rownames(EVs) <- c(type)

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
  df <- data.frame(matrix(ncol=3, nrow=1))
  colnames(df) <- c("qud_bn", "qud_cn", "qud_table")
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

jointEVs <- function(listener_df){
  probVals <- computeProbs(listener_df)
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
    EVs$p_CA <- round(x=sum(probVals$pca[indices] * ps[indices]),
                      digits=3)
    EVs$p_CNA <- round(x=sum(probVals$pcna[indices]* ps[indices]),
                       digits=3)
    EVs$p_NCA <- round(x=sum(probVals$pnca[indices]* ps[indices]),
                       digits=3)
    EVs$p_NCNA <- round(x=sum(probVals$pncna[indices]* ps[indices]),
                        digits=3)
    all_results[iter,] <- EVs
  }
  return(all_results)
}









