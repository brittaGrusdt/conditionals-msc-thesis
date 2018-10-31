require("rwebppl")
source("data-processing-functions.r")
##########################################

posterior <- function(model_path){
  listener <- webppl(program_file = model_path)
  return(listener)
}
posterior_with_data_input <- function(model_path, data){
  listener <- webppl(program_file = model_path, data=data, data_var='myDF')
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