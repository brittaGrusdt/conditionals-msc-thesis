require("grid")
require("gridExtra")
require("ggplot2")
require("plyr")
library(purrr)
source("data-processing-functions.r")
source("constants.r")
require(reshape2)

#####
readData <- function(lt, bias){
  fn <- paste("/home/britta/UNI/Masterarbeit/conditionals/rwebppl/results-", lt, "-3-runs-each-bias/",
              lt, "-", bias, ".rds", sep="")
  data <- readRDS(fn)
  return(data)
}

getDistrData <- function(lt, bias){
  posterior <- readData(lt, bias)
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

#### Visualizations with R
plotDistributions <- function(bias, utterance){
  # Probability Plots P(C) and P(A)
  df_ll <- getDistrData('LL', bias)
  df_pl <- getDistrData('PL', bias)
  df <- rbind(df_ll,df_pl)
  
  mu_pc <- ddply(df, "lt", summarise, grp.mean=mean(pc))
  mu_pa <- ddply(df, "lt", summarise, grp.mean=mean(pa))
  
  ggplot(df, aes(x=pc,color=lt)) +
    geom_density() +
    geom_vline(data=mu_pc, aes(xintercept=grp.mean, color=lt), linetype="dashed") +
    labs(x = "P(C)")
  ggsave("./plots/pc.png", width = 3, height=2.5)

  ggplot(df, aes(x=pa,color=lt)) +
    geom_density() +
    geom_vline(data=mu_pa, aes(xintercept=grp.mean, color=lt), linetype="dashed") +
    labs(x = "P(A)")
  ggsave("./plots/pa.png", width = 3, height=2.5)
  
  # Barplot of expected values for perfection probs
  collist <- c('pCgivenA', 'pNAgivenNC', 'pAgivenC', 'pNCgivenNA')
  labels <- c("E[P(C|A)]", "E[P(-A|-C)]", "E[P(A|C)]", "E[P(-C|-A)]")
  
  # with pragmatic and literal listener
  evs <- aggregate(df[, collist], list(df$lt), mean)
  evs <- format(evs, digits = 2)
  names(evs)[names(evs) == 'Group.1'] <- 'lt'
  evs.long<-melt(evs,id.vars="lt")
  evs.long$value <- as.numeric(evs.long$value)
  evs.long$variable <- mapvalues(evs.long$variable, from = collist, to = labels)
  
  ggplot(evs.long, aes(x=variable, y=value, fill=factor(lt))) +
    geom_bar(stat="identity", position="dodge") +
    # scale_x_continuous(labels=labels,breaks=1:4) +
    # scale_y_continuous(labels=c('0','0.25','0.5','0.75','1',''), breaks=c(0,0.25,0.5,0.75,1,1.2), limits=c(0,1.1)) +
    # geom_text(aes(label=evs), vjust = -0.4, size=2.5) +
    xlab('') + ylab('') + 
    theme(text = element_text(size=10), axis.text.x = element_text(angle = 60, hjust = 1),
          legend.title=element_blank()) + 
    facet_wrap(~lt)
  ggsave("./plots/expValsCP-LL-PL.png", width = 2.5, height=3)
  
  # barplot for causal networks
  ll_cn <- as.data.frame(table(filter(df, lt=='LL')$cn))
  ll_cn$lt <- rep("LL", nrow(ll_cn))
  ll_cn$Freq <- round(x=ll_cn$Freq / sum(ll_cn$Freq), digits=2)
  
  pl_cn <- as.data.frame(table(filter(df, lt=='PL')$cn))
  pl_cn$lt <- rep("PL", nrow(pl_cn))
  pl_cn$Freq <- round(x=pl_cn$Freq / sum(pl_cn$Freq),digits=2)
  data <- rbind(ll_cn,pl_cn)
  
  ggplot(data, aes(x=Var1, y=Freq, fill=factor(lt))) +
    geom_bar(stat="identity", position="dodge", show.legend = FALSE) +
    geom_text(aes(label=Freq), vjust = -0.4, size=2.5) +
    xlab('') + ylab('') + 
    theme(text = element_text(size=10), axis.text.x = element_text(angle = 60, hjust = 1),
          legend.title=element_blank()) + 
    facet_wrap(~lt)
  ggsave("./plots/cns-LL-PL.png", width = 5, height=3.5)

}


visualizeAsBarPlot <- function(evs, bias, kind){
  # One barplot command to get histogram of x
  jpeg(paste(bias, "-", kind, ".jpeg", sep=""), width = 480, height=480)
  indices <- as.logical(evs)
  par(oma = c( 5, 0, 3, 0))
  y <- as.numeric(round(x=evs[indices], digits=2))
  graph <- barplot(height = y,
          names.arg = names(evs)[indices],
          las = 2,
          width = rep(0.1, sum(indices)),
          xlim = c(0, 1),
          ylim = c(0, 1.1),
          col=c("darkblue"),
          main = bias)
  text(graph, y=y+0.05, labels=y)
  dev.off()
}

saveResults <- function(results, bias, n_runs){
  pdf(paste(bias, "-", n_runs, "-runs", sep=""))
  grid.newpage()
  grid.table(t(results))
  dev.off()
}


visualizeSpeaker <- function(speaker, bias, qud){
  par(oma = c( 5, 0, 3, 0))
  y <- as.numeric(round(x=speaker$prob, digits=2))
  graph <- barplot(height = y,
                   names.arg = speaker$support,
                   las = 2,
                   width = rep(0.1, length(speaker$support)),
                   xlim = c(0, 1),
                   ylim = c(0, 1.1),
                   col=c("darkblue"),
                   main = paste("bias:", bias, "qud:", qud, sep=" "))
  text(graph, y=y+0.05, labels=y)
}



plotTablesPrior <- function(){
  prior <- webppl(program_file = paste(BASEDIR, "vanilla-rsa-with-quds-bn-prior.wppl", sep=""),
                  random_seed=SEEDS[1])
  samples <- get_samples(prior, 10000)
  probs <- computeProbs(samples$table)
  t0 <- probs$pca 
  t1 <- probs$pcna
  t2 <- probs$pnca
  t3 <- probs$pncna
  df <- melt(data.frame(t0=t0,t1,t2,t3))
  colnames(df) <- c("entry", "val")
  
  labs <- c("P(-C,-A)","P(-C,A)","P(C,-A)","P(C,A)")
  levels(df$entry) <- rev(labs)
  m <- aggregate(df[, "val"], list(df$entry), mean)
  df2 <- data.frame(entry=levels(df$entry), val=m$x)
  
  ggplot(df, aes(x = val, color=entry)) +
    geom_density(show.legend = FALSE) +
    theme(text = element_text(size=6)) +
    scale_x_continuous(breaks=c(0,0.5,1)) +
    ylab('') + xlab('') +
    geom_vline(aes(xintercept=val), data=df2, linetype="dashed", color="blue") +
    facet_wrap(~entry,scales="free_y")
  
  ggsave("./plots/tablesPrior.png", width = 2, height=2)

}
