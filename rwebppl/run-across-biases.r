source("constants.r")
source("running-functions.r")
source("data-processing-functions.r")
source("visualize.r")
require("grid")
require("gridExtra")
# ----------------- #
# Parameters #### 
model_path <- paste(BASEDIR, "vanilla-rsa-with-quds.wppl", sep="")

# parameters for processing in R:
# make sure to change output in webppl file!!!!
listenerType <- "LL"
biases <- c("none", "lawn", "pizza", "douven1")
n_runs <- 3

if(listenerType=='PL'){
  withQUD <- TRUE
}else{
  withQUD <- FALSE 
}
inferenceType <- "enumerate"
viz <- FALSE
savePlots <- TRUE
##### webppl loop ####
if(withQUD){
  all_results <- data.frame(matrix(ncol=21))
}else{
  all_results <- data.frame(matrix(ncol=19))
}
all_ev_joints <- array(dim=c(9,4,length(biases)))
n_iter <- 0
for (bias in biases) {
  n_iter <- n_iter + 1
  results <- data.frame()
  summed_ev_joint <- matrix(0, ncol=4,nrow=9)
  for(i in seq(1,n_runs,1)){
    if(bias=="douven1"){
      utt <- "If A, -C"
    }else{
      utt <- "If A, C"
    }
    # parameters for webppl program:
    data <- list(bias=bias, utterance=utt)
    listener <- posterior_with_data_input(model_path, data, viz, SEEDS[i])
    saveRDS(listener, paste(listenerType, "-", bias,"-run-", i, ".rds", sep=""))
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
    
    EV_probs_cns <- jointEVs(listener_df)
    summed_ev_joint <- summed_ev_joint + as.matrix(EV_probs_cns)
  }
  saveResults(results, bias, n_runs)
  
  avgs <- data.frame(colMeans(results))
  all_results[n_iter,] <- t(round(x=avgs, digits=3))

  # average of expected vals for tables based on cns
  summed_ev_joint <- summed_ev_joint/n_runs
  all_ev_joints[,,n_iter] <- summed_ev_joint
}

colnames(all_results) <- rownames(avgs)
rownames(all_results) <- biases
colnames(all_ev_joints) <- colnames(summed_ev_joint)
rownames(all_ev_joints) <- rownames(summed_ev_joint)


write.csv(all_results, file='evs-marginal.csv')
write.csv(all_ev_joints, file='evs-joint.csv')

saveRDS(all_results, paste("evs-marginal.rds", sep=""))
saveRDS(all_ev_joints, paste("evs-joint.rds", sep=""))


