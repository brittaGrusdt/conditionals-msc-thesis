source("running-functions.r")
require("grid")
require("gridExtra")
# ----------------- #
# Parameters #### 
baseDir <- "/home/britta/UNI/Masterarbeit/conditionals/model/listener/"
model_path <- paste(baseDir, "listener-concrete-hypotheses.wppl", sep="")

# parameters for processing in R:
inferenceType <- "enumerate"

listenerTypes <- c("", "lawn-negotiable", "lawn-non-negotiable", "pizza")
# ,
#                    "wason-ind", "wason-dep")
listenerTypes <- c("wason-a", "wason-na", "wason-c", "wason-nc")
all_results <- data.frame()
for (lt in listenerTypes) {
  # parameters for webppl program:
  data <- list(bias=lt)
  listener <- posterior_with_data_input(model_path, data)
  if(lt==""){lt<-"simple"}
  result <- eval(listener, lt, inferenceType)
  all_results <- rbind(all_results, result)
}

# PLOTTING  ####
grid.newpage()
grid.table(t(all_results))
# grid.table(t(all_results[1:3,]))
# grid.newpage()
# grid.table(t(all_results[c(1,4:6),]))


