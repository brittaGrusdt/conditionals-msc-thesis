source("running-functions.r")
source("visualize.r")
require("grid")
require("gridExtra")
# ----------------- #
# Parameters #### 
baseDir <- "/home/britta/UNI/Masterarbeit/conditionals/model/listener/"
model_path <- paste(baseDir, "vanilla-rsa-with-quds.wppl", sep="")

# parameters for processing in R:
inferenceType <- "enumerate"
viz <- FALSE
savePlots <- TRUE

listenerTypes <- c("none", "lawn-nn", "pizza", "douven1")
n_runs <- 3
seeds <- c(8546340, 6538185, 5230322)

##### webppl loop ####

all_results <- data.frame(matrix(ncol=22))
all_ev_joints <- array(dim=c(9,4,length(listenerTypes)))
n_iter <- 0
for (lt in listenerTypes) {
  n_iter <- n_iter + 1
  results <- data.frame()
  summed_ev_joint <- matrix(0, ncol=4,nrow=9)
  for(i in seq(1,n_runs,1)){
    if(lt=="douven1"){
      utt <- "If A, -C"
    }else{
      utt <- "If A, C"
    }
    # parameters for webppl program:
    data <- list(bias=lt, utterance=utt)
    listener <- posterior_with_data_input(model_path, data, viz, seeds[i])
    
    if(inferenceType == "samples"){
      listener_df <- buildDF_from_samples(listener)
    }else{
      listener_df <- buildDF_from_enumerate(listener)
    }
    res <- getEVs(listener_df, lt)
    results <- rbind(results, res)
    
    EV_probs_cns <- jointEVs(listener_df)
    summed_ev_joint <- summed_ev_joint + as.matrix(EV_probs_cns)
  }
  saveResults(results, lt, n_runs)
  
  avgs <- data.frame(colMeans(results))
  all_results[n_iter,] <- t(round(x=avgs, digits=3))

  if(savePlots){
    cnData <- sapply(CNs, function(x){return(avgs[[x,1]])})
    visualizeCNs(cnData, lt)
  }

  # average of expected vals for tables based on cns
  summed_ev_joint <- summed_ev_joint/n_runs
  all_ev_joints[,,n_iter] <- summed_ev_joint
}

colnames(all_results) <- rownames(avgs)
rownames(all_results) <- listenerTypes
colnames(all_ev_joints) <- colnames(summed_ev_joint)
rownames(all_ev_joints) <- rownames(summed_ev_joint)


write.csv(all_results, file='evs-marginal.csv')
write.csv(all_ev_joints, file='evs-joint.csv')