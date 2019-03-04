source("constants.r")
library(dplyr)
library(purrr)
library(varhandle)
library(rwebppl)
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

getTableEVs <- function(df){
  m <- aggregate(df[, "val"], list(df$entry), mean)
  df2 <- data.frame(entry=levels(df$entry), val=m$x)
  return(df2)
}

getTableProbs <- function(distr){
  probs <- data.frame(matrix(ncol = 4, nrow = nrow(distr)))
  colnames(probs) <- c("pca", "pcna", "pnca", "pncna")
  tables <- distr$value
  probs$pca <- as.numeric(map(tables, 1))
  probs$pcna <- as.numeric(map(tables, 2))
  probs$pnca <- as.numeric(map(tables, 3))
  probs$pncna <- as.numeric(map(tables, 4))
  
  t0 <- probs$pca; t1 <- probs$pcna
  t2 <- probs$pnca; t3 <- probs$pncna
  df <- melt(data.frame(t0=t0,t1,t2,t3))
  colnames(df) <- c("entry", "val")
  
  labs <- c("P(-C,-A)","P(-C,A)","P(C,-A)","P(C,A)")
  levels(df$entry) <- rev(labs)
  return(df)
}
##### probabilities #####
getSampleProbs <- function(lt, posterior){
  n <- NB_WEBPPL_SAMPLES
  print(paste('get samples for ', lt, "...", sep=""))
  samples <- get_samples(posterior,n)
  if(lt=='LL' || lt=='prior'){
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

##### Expected Values ###### 
getEVs <- function(listener_df, bias, withQud){
  
  EV_cns <- computeEVs(listener_df, "cns")
  EV_probs <- computeEVs(listener_df, "probs")
  rownames(EV_probs) <- c(bias)
  
  if(withQud){
    EV_quds <- computeEVs(listener_df, "qud")
    EVs <- cbind(EV_quds,EV_probs, EV_cns)
  }else{
    EVs <- cbind(EV_probs, EV_cns)
  }
  return(EVs)
}

getTableEVsPerCN <- function(listener_df){
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

computeEVProbs <- function(df_listener){
  probVals <- computeProbs(df_listener$bn.table)
  ps <- df_listener$prob
  EVs <- data.frame(matrix(ncol = 10, nrow = 1))
  colnames(EVs) <- c("p_c_given_a", "p_c_given_na", "p_c",
                     "p_a_given_c", "p_a_given_nc", "p_a", 
                     "p_CA", "p_CNA", "p_NCA", "p_NCNA")

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

computeEVs <- function(listener_df, marginal){
  if(marginal=="probs"){
    marginals <- computeEVProbs(listener_df)
    return(marginals)
  }
  
  if(marginal=='qud'){
    df <- data.frame(matrix(ncol=2, nrow=1))
    colnames(df) <- c("qud_bn", "qud_table")
    all_vars <- QUDs
    var_marginal <- "qud"
  }else if(marginal=='cns'){
    df <- data.frame(matrix(ncol=9, nrow=1))
    colnames(df) <- CNs
    all_vars <- CNs
    var_marginal <- "bn.cn"
  }else{
    stop(paste('unknown marginal:', marginal))
  }
  iter <- 0
  for(token in all_vars){
    iter <- iter + 1
    tokens <- listener_df[[var_marginal]]
    tokens[tokens!=token] <- 0
    tokens[tokens==token] <- 1
    tokens <- as.numeric(tokens)  
    df[,iter] <- round(x=sum(tokens * listener_df$prob), digits=3)    
  }
  return(df)
}
  
##### save, read data ###### 
checkAndCreateDir <- function(target_dir){
  if(!dir.exists(target_dir)){
    dir.create(target_dir, recursive = TRUE)
    print(paste('created directory: ', target_dir))
  }
}

savePDF <- function(results, bias, n_runs, target_dir){
  checkAndCreateDir(target_dir)
  target <- paste(target_dir, bias, "-", n_runs, "-runs.pdf", sep="")
  print(paste('save results to', target))
  pdf(target)
  grid.newpage()
  grid.table(t(results))
  dev.off()
}

saveGGPlot <- function(lt, bias, fn, alpha, filename, width, height){
  target_dir <- getPlotDir(lt, bias, fn, alpha)
  checkAndCreateDir(target_dir)
  target <- paste(target_dir, filename, sep="")
  print(paste('save plot to:', target))
  ggsave(target, width=width, height=height)
}

readData <- function(lt, bias, base_dir, subFolder='', nrun=1, alpha=5, samples=TRUE){
  base <- getDataDir(lt, bias, base_dir, alpha)
  if(subFolder!=''){
    base <- paste(".", base_dir, lt, bias, subFolder, sep="/")
  }
  if(samples){
    fn <- paste(base, bias, "-run-", nrun, "-samples.rds", sep="")
  }else{
    fn <- paste(base, bias, "-run-", nrun, "-distr.rds", sep="")
  }
  print(paste('read data from:', fn))
  data <- readRDS(fn)
  return(data)
}

getPlotDir <- function(lt, bias, base_dir, alpha=5, joint=FALSE){
  if(joint){
    target_dir <- paste(base_dir, "/alpha-", alpha, "/joint-plots-LL-PL/", sep="")
  }else if(lt=='PL'){
    target_dir <- paste(base_dir, "/alpha-", alpha, "/", lt, "/", bias, "/plots/", sep="")
  }else{
    target_dir <- paste(base_dir, "/", lt, "/", bias, "/plots/", sep="")
  }
  return(target_dir)
}

getDataDir <- function(lt, bias, base_dir, alpha=5){
  if(lt=='PL'){
    target_dir <- paste(base_dir, "/alpha-", alpha, "/", lt, "/", bias, "/", sep="")
  }else{
    target_dir <- paste(base_dir, "/", lt, "/", bias, "/", sep="")
  }
  return(target_dir)
}

getData <- function(ll_posterior, pl_posterior){
  df_ll <- getSampleProbs('LL', ll_posterior)
  df_pl <- getSampleProbs('PL', pl_posterior)
  df <- rbind(df_ll,df_pl)
  return(df)
}

getLLPLdata <- function(bias, base, alpha, lawnThetaFolder='', nrun=1){
  df_ll <- readData('LL', bias, base, subFolder=lawnThetaFolder, nrun=nrun, samples=TRUE)
  df_pl <- readData('PL', bias, base, subFolder=lawnThetaFolder, nrun=nrun, alpha=alpha, samples=TRUE)
  df <- rbind(df_ll,df_pl)
  return(df)
}


##### prepare data #####
getDataCPStrengths <- function(biases, all_dfs, scorePs, scorePsLabels){
  data <- data.frame()
  for(bias in biases){
    df <- get(bias, all_dfs)
    evs <- aggregate(df[, scorePs], list(df$lt), mean)
    if(length(scorePs)==1){
      colnames(evs) <- c("Group.1", scorePs[1])
    }
    evs <- format(evs, digits = 2)
    evs$bias <- c(bias, bias)
    data <- rbind(data,evs)
  }
  data2 <- melt(data, id.vars = c("Group.1", "bias"), measure.vars = scorePs, variable.name = "p")
  data2$bias <- mapvalues(data$bias, from=biases, to=seq(1,length(biases)))
  data2$biasLabel=data2$bias
  data2$bias=as.numeric(data2$bias)
  data2$value=as.numeric(data2$value)
  data2$Group.1=as.factor(data2$Group.1)
  data2$p <- mapvalues(data2$p, from=scorePs, to=scorePsLabels)
  return(data2)
}
