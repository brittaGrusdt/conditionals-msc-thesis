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


#### Visualization with webppl
disp <- "viz(myDF)"

webpplviz <- function(marginal){
  d <- data.frame(matrix(ncol = 1, nrow = length(marginal)))
  d$marginal = marginal
  webppl(disp, packages=c("webppl-viz"), data=d, data_var='myDF')
}



