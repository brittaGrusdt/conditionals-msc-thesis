source("data-processing-functions.r")
source("constants.r")
require("grid")
require("gridExtra")
require("ggplot2")
require("plyr")
require(reshape2)
library(purrr)

###### PLOTs for LL + PL #####
plotPC_PA <- function(bias, fn, ll_posterior, pl_posterior){
  df <- getData(ll_posterior, pl_posterior)
  mu_pc <- ddply(df, "lt", summarise, grp.mean=mean(pc))
  mu_pa <- ddply(df, "lt", summarise, grp.mean=mean(pa))
  
  ggplot(df, aes(x=pc,color=lt)) +
    geom_density() +
    geom_vline(data=mu_pc, aes(xintercept=grp.mean, color=lt), linetype="dashed") +
    labs(x = "P(C)")
  ggsave(paste("./", fn, "/plots/", bias, "-pc.png", sep=""), width = 3, height=2.5)
  
  ggplot(df, aes(x=pa,color=lt)) +
    geom_density() +
    geom_vline(data=mu_pa, aes(xintercept=grp.mean, color=lt), linetype="dashed") +
    labs(x = "P(A)")
  ggsave(paste("./", fn, "/joint-plots-LL-PL/", bias, "-pa.png", sep=""), width = 3, height=2.5)
}

plotPerfectionProbs <- function(bias, fn, ll_posterior, pl_posterior){
  df <- getData(ll_posterior, pl_posterior)
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
    xlab('') + ylab('') +
    theme(text = element_text(size=10), axis.text.x = element_text(angle = 60, hjust = 1),
          legend.title=element_blank()) +
    facet_wrap(~lt)
  ggsave(paste(fn, "/plots/", bias, "-expValsCP-LL-PL.png", sep=""), width = 3, height=3)
  # Density plots of the 4 relevant conditional probabilities for PL and LL
  df2 <- df[,c(collist,"lt")]
  df2.long <- melt(df2,id.vars="lt")
  df2.long.ll <- df2.long[which(df2.long$lt=='LL'),]
  df2.long.pl <- df2.long[which(df2.long$lt=='PL'),]
  
  labels <- c("P(C|A)", "P(-A|-C)",  "P(A|C)", "P(-C|-A)")
  df2.long.ll$variable <- mapvalues(df2.long.ll$variable, from=collist, to=labels)
  df2.long.pl$variable <- mapvalues(df2.long.pl$variable, from=collist, to=labels)
  
  ggplot(data = df2.long.ll, aes(x=value)) +
    geom_density(aes(color=variable),show.legend = FALSE, alpha = 0.4) +
    xlab('')  + theme(text = element_text(size=10)) +
    facet_wrap( ~ variable, scales="free_y")
  ggsave(paste(fn, "/joint-plots-LL-PL/", bias, "-cp-densities-LL.png", sep=""), width = 3, height=3)
  
  ggplot(data = df2.long.pl, aes(x=value)) +
    geom_density(aes(color=variable),show.legend = FALSE, alpha = 0.4) +
    xlab('') + ylab('') + theme(text = element_text(size=10)) +
    facet_wrap( ~ variable, scales="free_y")
  ggsave(paste(fn, "/joint-plots-LL-PL/", bias, "-cp-densities-PL.png", sep=""), width = 3, height=3)
}

plotCNs_LLPL <- function(bias, fn, ll_posterior, pl_posterior){
  df <- getData(ll_posterior, pl_posterior)
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
  ggsave(paste(fn, "/joint-plots-LL-PL/", bias, "-cns-LL-PL.png", sep=""), width = 5, height=3.5)
}

###### Plots for single distribuion #####
plotTables <- function(probs, fn, bias, lt){
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
  
  ggsave(paste(fn, "/", lt, "/plots/", bias, "-tables.png", sep=""), width = 2, height=2)

}

plotCNs <- function(samples, fn, bias, lt){
  df <- as.data.frame(samples$cn)
  df <- count(df)
  df$freq <- round(x=df$freq / sum(df$freq),digits=2)
  
  ggplot(df, aes(x=samples.cn, y=freq)) +
    geom_bar(stat="identity", show.legend = FALSE, fill="blue") +
    geom_text(aes(label=freq), vjust = -0.4, size=2.5) +
    xlab('') + ylab('') + 
    theme(text = element_text(size=10), axis.text.x = element_text(angle = 60, hjust = 1),
          legend.title=element_blank())
  target <- paste(fn, "/", lt, "/plots/", bias, "-cns.png", sep="")
  print(target)
  ggsave(target, width = 5, height=3.5)
  
}

plotQUDs <- function(bias, fn, ll_posterior, pl_posterior){
  for(bias in BIASES){
    posterior <- readData('PL', bias, fn)
    samples <- get_samples(posterior,10000)
    x <- levels(factor(samples$qud))
    y <- table(samples$qud)
    y <- y/sum(y)
    
    ggplot(data.frame(x=x,y=y), aes(x=x, y=y)) +
      geom_bar(stat="identity", show.legend = FALSE, fill='steelblue') +
      # geom_text(aes(label=Freq), vjust = -0.4, size=2.5) +
      xlab('') + ylab('') + 
      scale_y_continuous(labels = percent) +
      theme(text = element_text(size=10)) 
    ggsave(paste(fn, "/plots/", bias, "-quds-PL.png", sep=""), width = 2, height=3)
  }
}

plotTableDensityPerCN <- function(cn, bias, lt, fn){
  data <- readData(lt, bias, fn)
  samples <- getSamples('PL', data)
  keep <- c("pca","pcna","pnca","pncna", "cn")
  data <- samples[keep]
  df <- melt(data,id.vars="cn")
  df2 <- df[which(df$cn==cn),]

  if(dim(df2)[1]==0){
    print(paste('cn', cn, 'has 0 probability - no output plot'))
  }else{
    evs <- aggregate(df2[,3], list(df2$variable), mean)
    colnames(evs) <- c("variable","value")
    ggplot(df2, aes(x=value, color=variable)) +
      geom_density() +
      geom_vline(data=evs, aes(xintercept=value), linetype="dashed") +
      xlab("") + ylab("") +  ggtitle(cn) +
      theme(legend.position='none', plot.title = element_text(size=8, hjust = 0.5),
            text = element_text(size=7))+
      
      facet_wrap(~variable, ncol = 2, scales="free_y") 
    
    target <- paste(fn, "/", lt, "/plots/", bias, "-table-densities-", cn, ".png", sep="")
    print(paste('target dir:', target))
    ggsave(target, width = 2.75, height=2.5)
  }
}

plotTableDensitiesAllCNs <- function(bias,lt,fn){
  for(cn in CNs){
    plotTableDensityPerCN(cn, bias, lt, fn)
  }
}




