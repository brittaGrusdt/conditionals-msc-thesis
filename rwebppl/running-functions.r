require("rwebppl")
source("data-processing-functions.r")
##########################################

getSeed <- function(){
  time <- as.numeric(Sys.time())
  return(time)
}

posterior <- function(model_path, seed=FALSE){
  if(!seed){
    seed <- getSeed()
  }
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
  }
  listener <- webppl(program_file = model_path, data=data, data_var='myDF', random_seed=seed)
  return(listener)
}

getEVs <- function(listener_df, listenerType, qud){

  EV_probs <- marginalEVTables(listener_df, listenerType)
  EV_cns <- marginalEVCNs(listener_df)
  if(qud){
    EV_quds <- marginalEVQuds(listener_df)
    EVs <- cbind(EV_quds,EV_probs, EV_cns)
  }else{
    EVs <- cbind(EV_probs, EV_cns)
  }
  return(EVs)
}


