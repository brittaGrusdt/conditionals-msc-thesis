require("rwebppl")
source("data-processing-functions.r")
##########################################

getSeed <- function(){
  time <- as.numeric(Sys.time())
  return(time)
}

webppl_posterior <- function(model_path, seed=FALSE){
  if(!seed){
    seed <- getSeed()
  }
  print(paste("seed: ", seed))
  listener <- webppl(program_file = model_path, random_seed=seed)
  return(listener)
}

posterior_with_data_input <- function(model_path, data, viz, seed=FALSE){
  if(!seed){
    seed <- getSeed()
  }
  if(viz){
    webppl(program_file=model_path, data=data, data_var='myDF',
                       packages=c("webppl-viz"), random_seed = seed)
    listener <- ''
  }else{
    listener <- webppl(program_file = model_path, data=data, data_var='myDF', random_seed=seed)
  }
  return(listener)
}

run_visualize_PL_LL <- function(bias, fn_ll, fn_pl){
  ll_posterior <- readData('LL', bias, fn_ll)
  pl_posterior <- readData("PL", bias, fn_pl)
  
  plotPC_PA(bias, fn_pl, ll_posterior, pl_posterior)
  plotPerfectionProbs(bias, fn_pl, ll_posterior, pl_posterior)
  plotCNs_LLPL(bias, fn_pl, ll_posterior, pl_posterior)
}

run_visualize_all <- function(fn_ll, alphas, biases){
  for(alpha in alphas){
    for(bias in biases){
      fn_pl <- paste(base, "/alpha-", alpha, sep="")
      run_visualize_PL_LL(bias, fn_ll, fn_pl)
    }
  }
}

run_visualize_distr <- function(bias, fn, lt){
  prior <- readData(lt, bias, fn)
  samples <- get_samples(prior, 10000)
  probs <- computeProbs(samples$table)
  plotTables(probs, fn, bias, lt)
  plotCNs(samples, fn, bias, lt)
}

runSpeakerExpectations <- function(bias, qud){
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

runSpeakerQimp <- function(bias){
  target_dir <- "results-all-biases/speaker/"
  qud <- "bn"
  data <- list(bias=bias, lt='Q',utterance="dummy", qud=qud)
  model <- paste(BASEDIR, "model.wppl", sep="")
  likelihoods <- posterior_with_data_input(model, data, viz=FALSE, seed=SEEDS[1])
  
  best.first <- data.frame()
  best.second <- data.frame()
  best.third <- data.frame()
  p1 <- 0
  p2 <- 0
  p3 <- 0
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
  
  ggplot(best.first, aes(x=utt)) +
    geom_bar(aes(y =..count..),fill="magenta") +
    theme(text = element_text(size=6)) +
    ylab('') + xlab('')
  ggsave(paste(target_dir, "first-best-utts-", bias, "-qud-", qud,".png", sep=""), width = 3, height=2)
  
  ggplot(best.second, aes(x=utt)) +
    geom_bar(aes(y =..count..),fill="orange") +
    theme(text = element_text(size=6)) +
    ylab('') + xlab('')
  ggsave(paste(target_dir, "second-best-utts-", bias, "-qud-", qud,".png", sep=""), width = 3, height=2)
  
  ggplot(best.third, aes(x=utt)) +
    geom_bar(aes(y =..count..),fill="red") +
    theme(text = element_text(size=6)) +
    ylab('') + xlab('')
  ggsave(paste(target_dir, "third-best-utts-", bias, "-qud-", qud,".png", sep=""), width = 3, height=2)
}
