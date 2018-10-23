require("grid")
require("gridExtra")
require("rwebppl")
source("data-processing-functions.r")
##########################################

posterior <- function(model_path){
  listener <- webppl(program_file = model_path)
  return(listener)
}

eval <- function(listener, listenerType, inferType){
  if(inferType == "samples"){
    df <- buildDF_from_samples(listener)
    ps <- rep(1/length(df$jointP), length(df$jointP))
  }else{
    df <- buildDF_from_enumerate(listener)
    ps <- listener$prob
  }
  probs <- computeProbs(df)
  EVs <- computeEVs(probs, ps, listenerType)
  return(EVs)
}
########################
baseDir <- "/home/britta/UNI/Masterarbeit/conditionals/model/listener/"
#model_path <- paste(baseDir, "listener-networkPriors-biscuits-cp.wppl", sep="")
# model_path <- paste(baseDir, "hpotheses-approach-ll-Not-conditioned-on-cn.wppl", sep="")
model_path <- paste(baseDir, "hypotheses-approach-ll-conditioned-on-cn.wppl", sep="")

listenerType <- "simple"
listenerType <- "biscuits"
listenerType <- "perfection"
listener <- posterior(model_path)
df <- buildDF_from_samples(listener)
result <- eval(listener, listenerType, "samples")

#####################
grid.newpage()
grid.table(t(result))

# result <- rbind(simple,biscuits, perfection)
