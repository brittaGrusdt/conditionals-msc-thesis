source("constants.r")
source("running-functions.r")
source("data-processing-functions.r")
source("visualize.r")
require("grid")
require("gridExtra")
# ----------------- #
# Parameters #### 
model_path <- paste(BASEDIR, "model.wppl", sep="")
listenerType <- "LL" # LL / PL
n_runs <- 3
biases <- c("none", "lawn", "pizza", "douven1")
# biases <- c("lawn")
alphas = c(1,3,5,10,20)

base <- "results-all-biases"
# base <- "results-across-alpha"
# base <- "results-tests"
# base <- "results-wason-abstract/unconditioned"
# base <- "results-wason-abstract/conditioned_pa"
# base <- "results-none-conditioned/AC_nCnA"
# base <- "results-none-conditioned/nAnC_CA"

inferenceType <- "enumerate"
viz <- FALSE

withQUD <- FALSE 
# withQUD <- TRUE

##### webppl loop ####
if(withQUD){
  all_results <- data.frame(matrix(ncol=21))
}else{
  all_results <- data.frame(matrix(ncol=19))
}
for(alpha in alphas){
  if(listenerType=='prior' || listenerType=='LL'){
    target_dir <- paste(base, "/", listenerType, "/", sep="")
  }else{
    target_dir <- paste(base, "/alpha-", alpha, "/", listenerType, "/", sep="")
  }
  print(target_dir)
  all_ev_joints <- array(dim=c(9,4,length(biases)))
  n_iter <- 0
  for (bias in biases) {
    n_iter <- n_iter + 1
    results <- data.frame()
    summed_ev_joint <- matrix(0, ncol=4,nrow=9)
    for(i in seq(1,n_runs,1)){
      if(bias=="douven1"){
        utt <- "If A, -C"
      } else if(bias=="wason-abstract"){
        utt <- "A and C" 
      }else{
        utt <- "If A, C"
      }
      # parameters for webppl program:
      data <- list(bias=bias, utterance=utt, lt=listenerType, alpha=alpha)
      listener <- posterior_with_data_input(model_path, data, viz, SEEDS[i])
      saveRDS(listener, paste(target_dir, bias,"-run-", i, ".rds", sep=""))
      if(!withQUD){
        c <- colnames(listener)
        c[c=="cn"] <- "bn.cn"
        c[c=="table"] <- "bn.table"
        colnames(listener) <- c
      }
      if(inferenceType == "samples"){
        listener_df <- buildDF_from_samples(listener, withQUD)
      }else{
        listener_df <- buildDF_from_enumerate(listener, withQUD)
      }
      res <- getEVs(listener_df, bias, withQUD)
      results <- rbind(results, res)
      
      EV_probs_cns <- conditionedEVs(listener_df)
      summed_ev_joint <- summed_ev_joint + as.matrix(EV_probs_cns)
    }
    if(n_runs!=1){
      saveResults(results, bias, n_runs, target_dir)
    }
    avgs <- data.frame(colMeans(results))
    all_results[n_iter,] <- t(round(x=avgs, digits=3))
  
    # average of expected vals for tables based on cns
    summed_ev_joint <- summed_ev_joint/n_runs
    all_ev_joints[,,n_iter] <- summed_ev_joint
  }

  if(n_runs!=1){
    colnames(all_results) <- rownames(avgs)
    rownames(all_results) <- biases
    colnames(all_ev_joints) <- colnames(summed_ev_joint)
    rownames(all_ev_joints) <- rownames(summed_ev_joint)
    
    write.csv(all_results, file=paste(target_dir, 'evs-marginal-n_runs-', n_runs, '.csv', sep=""))
    write.csv(all_ev_joints, file=paste(target_dir, 'evs-conditioned-n_runs-', n_runs, '.csv', sep=""))
    
    saveRDS(all_results, paste(target_dir, 'evs-marginal-n_runs-', n_runs, '.rds', sep=""))
    saveRDS(all_ev_joints, paste(target_dir, 'evs-conditioned-n_runs-', n_runs, '.rds', sep=""))
  }
  if(listenerType=='prior' || listenerType=='LL'){
    break;
  }
}
