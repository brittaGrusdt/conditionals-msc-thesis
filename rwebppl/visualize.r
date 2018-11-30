require("grid")
require("gridExtra")
require("ggplot2")
source("data-processing-functions.r")

#### Visualizations with R

visualizeR <- function(df, utterance){
  # Probability Plots
  probs <- computeProbs(df)
  p1 <- ggplot(probs, aes(x=pc)) + geom_histogram(aes(y= ..density..))
  p2 <- ggplot(probs, aes(x=pa)) + geom_histogram(aes(y= ..density..))
  p3 <- ggplot(probs, aes(x=pCgivenA)) + geom_histogram(aes(y= ..density..))
  p4 <- ggplot(probs, aes(x=pAgivenC)) + geom_histogram(aes(y= ..density..))
  grid.arrange(p1,p2,p3,p4, ncol=2, top=paste("pragmatic listener hears: ", utterance))

  # Expected Values for all probabilities
  d <- data.frame(map(probs,mean))
  rownames(d) <- "EV"
  d <- d[,order(d[1,], decreasing=TRUE),drop=F];
  grid.newpage()
  grid.table(t(d))

  # Bayes Nets
  qplot(x = factor(unlist(df$causalNet)), data = df, xlab="Bayes Nets", ylab="frequency",
        main=paste("Pragmatic Listener hears: ",utterance, sep=""))
}

visualizeCNs <- function(evs, bias){
  # One barplot command to get histogram of x
  jpeg(paste(bias,".jpeg", sep=""), width = 480, height=480)
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

#### Visualization with webppl
disp <- "viz(myDF)"

webpplviz <- function(marginal){
  d <- data.frame(matrix(ncol = 1, nrow = length(marginal)))
  d$marginal = marginal
  webppl(disp, packages=c("webppl-viz"), data=d, data_var='myDF')
}



