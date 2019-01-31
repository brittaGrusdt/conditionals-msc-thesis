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
  ggsave(paste("./", fn, "/joint-plots-LL-PL/", bias, "-pc.png", sep=""), width = 3, height=2.5)
  
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
  ggsave(paste(fn, "/joint-plots-LL-PL/", bias, "-expValsCP-LL-PL.png", sep=""), width = 3, height=3)
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
    theme(text = element_text(size=8)) +
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
    samples <- get_samples(posterior,100000)
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
  samples <- getSampleProbs(lt, data)
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

#### Speaker Plots  #### 
visualizeSpeakerFull <- function(bias){
    target_dir <- "results-all-biases/speaker/"
    data <- list(bias=bias, lt='speakerAll',utterance="dummy", qud=qud)
    model <- paste(BASEDIR, "model.wppl", sep="")
    likelihoods <- posterior_with_data_input(model, data, viz=FALSE, seed=SEEDS[1])
  
}
visualizeSpeakerQimp <- function(bias){
  target_dir <- "results-all-biases/speaker/"
  qud <- "bn"
  data <- list(bias=bias, lt='Q',utterance="dummy", qud=qud)
  model <- paste(BASEDIR, "model.wppl", sep="")
  likelihoods <- posterior_with_data_input(model, data, viz=FALSE, seed=SEEDS[1])
  
  best.first <- data.frame()
  best.second <- data.frame()
  best.third <- data.frame()
  p1 <- 0; p2 <- 0; p3 <- 0
  for(i in seq(1,length(likelihoods))){
    df <- as.data.frame(likelihoods[i])
    df2 <- df[order(df$p,decreasing=T),]
    best.first <- rbind(best.first, df2[1,])
    p1 <- p1 + df2[1,"p"]
    best.second <- rbind(best.second, df2[2,])
    p2 <- p2 + df2[2,"p"] + df2[1,"p"]
    best.third <- rbind(best.third, df2[3,])
    p3 <- p3 + df2[3,"p"] + df2[2,"p"] + df2[1,"p"]
  }
  p1 <- p1/length(likelihoods)
  p2 <- p2/length(likelihoods)
  p3 <- p3/length(likelihoods)
  
  best.first.summary <- aggregate(p ~ utt, best.first, mean )
  best.first.summary$utt <- factor(best.first.summary$utt, levels = 
                                     best.first.summary$utt[order(-best.first.summary$p)])
  best.second.summary <- aggregate(p ~ utt, best.second, mean )
  best.second.summary$utt <- factor(best.second.summary$utt, levels = 
                                      best.second.summary$utt[order(-best.second.summary$p)])
  best.third.summary <- aggregate(p ~ utt, best.third, mean )
  best.third.summary$utt <- factor(best.third.summary$utt, levels = 
                                     best.third.summary$utt[order(-best.third.summary$p)])
  
  ggplot(best.first.summary, aes(x=utt, y=p)) +
    geom_bar(stat="identity", fill="magenta") +
    theme(text = element_text(size=8)) +
    ylab('') + xlab('') 
  ggsave(paste(target_dir, "first-best-utts-", bias, "-qud-", qud,".png", sep=""), width = 3, height=2)
  
  ggplot(best.second.summary, aes(x=utt,y=p)) +
    geom_bar(stat="identity",fill="orange") +
    theme(text = element_text(size=8)) +
    ylab('') + xlab('')
  ggsave(paste(target_dir, "second-best-utts-", bias, "-qud-", qud,".png", sep=""), width = 3, height=2)
  
  ggplot(best.third.summary, aes(x=utt,y=p)) +
    geom_bar(stat="identity",fill="red") +
    theme(text = element_text(size=8)) +
    ylab('') + xlab('')
  ggsave(paste(target_dir, "third-best-utts-", bias, "-qud-", qud,".png", sep=""), width = 3, height=2)
}

visualizeSpeakerExpectations <- function(bias, qud){
  model <- paste(BASEDIR, "model.wppl", sep="")
  target_dir <- "results-all-biases/speaker/"
  
  data <- list(bias=bias, lt='SE',utterance='dummy', qud=qud)
  likelihoods <- posterior_with_data_input(model, data, viz=FALSE, seed=SEEDS[1])
  likelihoods.df <- as.data.frame(likelihoods)
  colnames(likelihoods.df) <- c("Group.1", "x")
  likelihoods.df$x <- unfactor(likelihoods.df$x)
  saveRDS(likelihoods.df, paste(target_dir, "SE-", bias, "-qud-", qud,".rds", sep=""))
  
  ggplot(likelihoods.df, aes(x=reorder(Group.1,-x), y=x)) +
    geom_bar(stat="identity",fill='steelblue') + 
    scale_y_continuous(limits=c(0,0.3)) +
    ylab('') +   xlab('') +
    theme(text = element_text(size=10),
          axis.text.x = element_text(angle = 60, hjust = 1))
  ggsave(paste(target_dir, "SE-", bias, "-qud-", qud,".png", sep=""), width = 3, height=2)
  return(likelihoods.df)
}


visualizeUtilities <- function(bias, alpha, fn){
  prior <- readData("prior", bias, "results-all-biases")

  # find specific bayes net
  v1 <- lapply(prior$table, `[[`, 1); v2 <- lapply(prior$table, `[[`, 2)
  v3 <- lapply(prior$table, `[[`, 3); v4 <- lapply(prior$table, `[[`, 4)
  prior$t0 <- v1; prior$t1 <- v2
  prior$t2 <- v3; prior$t3 <- v4
  
  # look_for <- c(0.024, 0.819,0.004, 0.153)
  # look_for <- c(0.041, 0.001, 0.935, 0.023)
  look_for <- c(0.001, 0.5, 0.479, 0.021)
  idx1 <- which(prior$cn=="-C implies -A")
  
  idx2 <- which(prior$t0 == look_for[1])
  idx3 <- which(prior$t1== look_for[2])
  idx4 <- which(prior$t2== look_for[3])
  idx5 <- which(prior$t3== look_for[4])
  
  idx <- intersect(intersect(intersect(idx2,idx3),idx4), idx5)
  # idx <- intersect(intersect(intersect(intersect(idx1,idx2),idx3), idx4), idx5)
  bn <- prior[idx,]
  bn <- select(bn, "cn", "table")
  
  # Get speaker distribution
  model_path <- paste(BASEDIR, "model.wppl", sep="")
  data <- list(bias=bias, utterance="dummy", lt="speaker", bn=bn, alpha=alpha)
  s1 <- posterior_with_data_input(model_path, data, FALSE, SEEDS[1])
  
  # replace factor(globalStore.alpha * (utility-costs(utterance))) by 
  #         factor(globalStore.alpha * utility) in speaker code 
  s2 <- posterior_with_data_input(model_path, data, FALSE, SEEDS[1])
  
  s1$support <- factor(s1$support, levels = s1$support[order(-s1$prob)])
  s2$support <- factor(s2$support, levels = s2$support[order(-s2$prob)])
  
  ggplot(s1, aes(x=support, y=prob)) +
    geom_bar(stat="identity", show.legend = FALSE, fill='indianred') +
    xlab(paste("<", bn$table, ", ", bn$cn, ">", sep="")) + ylab('') +
    theme(text = element_text(size=8), plot.title = element_text(size=8, hjust = 0.5)) 
  ggsave(paste(fn, "/", bias, "-with-costs.png", sep=""), width=3.5, height=2)
  
  ggplot(s2, aes(x=support, y=prob)) +
  geom_bar(stat="identity", show.legend = FALSE, fill='indianred') +
    xlab(paste("<", bn$table, ", ", bn$cn, ">", sep="")) + ylab('')  +
    theme(text = element_text(size=8), plot.title = element_text(size=8, hjust = 0.5)) 
  ggsave(paste(fn, "/", bias, "-without-costs.png", sep=""),width=3.5, height=2)
}


