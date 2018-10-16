require("grid")
require("gridExtra")
require("rwebppl")
source("data-processing-functions.r")
##########################################

posterior <- function(model_path){
  listener <- webppl(program_file = model_path)
  return(listener)
}

run <- function(model_path, listenerType, inferType){
  listener <- posterior(model_path)
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
model_path <- paste(baseDir, "listener-networkPriors-biscuits-cp.wppl", sep="")

listenerType <- "simple"
result <- run(model_path, listenerType, "samples")

#####################
grid.newpage()
grid.table(t(result))
