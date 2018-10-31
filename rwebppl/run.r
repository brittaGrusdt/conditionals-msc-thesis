source("running-functions.r")
source("visualize.r")
require("grid")
require("gridExtra")
# ----------------- #
# Parameters #### 
baseDir <- "/home/britta/UNI/Masterarbeit/conditionals/model/listener/"
model_path <- paste(baseDir, "concrete-hypotheses-different-causal-nets.wppl",sep="")

# parameters for processing in R:
inferenceType <- "enumerate"

listenerTypes <- c("", "lawn-negotiable", "lawn-non-negotiable", "pizza")
# listenerTypes <- c("", "douven1", "douven2", "douven3")
all_results <- data.frame()
for (lt in listenerTypes) {
  if(lt=="douven1"){
    utt <- "If A, not C"
  }else{
    utt <- "If A, C"
  }
  # parameters for webppl program:
  data <- list(bias=lt, utterance=utt)
  listener <- posterior_with_data_input(model_path, data)
  if(lt==""){lt<-"simple"}
  result <- eval(listener, lt, inferenceType)
  all_results <- rbind(all_results, result)
}

# PLOTTING  ####
grid.newpage()
grid.table(t(all_results))



