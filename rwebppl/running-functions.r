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

getEVs <- function(listener_df, bias, withQud){

  EV_probs <- marginalEVTables(listener_df, bias)
  EV_cns <- marginalEVCNs(listener_df)
  if(withQud){
    EV_quds <- marginalEVQuds(listener_df)
    EVs <- cbind(EV_quds,EV_probs, EV_cns)
  }else{
    EVs <- cbind(EV_probs, EV_cns)
  }
  return(EVs)
}


