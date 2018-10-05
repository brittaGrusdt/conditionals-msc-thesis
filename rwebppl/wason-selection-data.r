#### RUN once
EV_listener <- data.frame(matrix(ncol = 4, nrow = 12))
colnames(EV_listener) <- c("ac", "nac", "anc", "nanc")
rownames(EV_listener) <- c("a", "c", "a-c", "c-a",
                     "na", "nc",
                     "na-c", "a-nc", "na-nc",
                     "nc-a", "c-na", "nc-na")

#model_path <- "/home/britta/UNI/Masterarbeit/RwebPPL/webppl/literalListener.wppl"
model_path <- "/home/britta/UNI/Masterarbeit/RwebPPL/webppl/listener.wppl"

posterior <- function(){
  listener <- webppl(program_file = model_path)
  return(listener)
}

####
getTableEntries <- function(jointPs){
  df <- data.frame(matrix(ncol=4, nrow=length(jointPs)))
  colnames(df) <- c("ac", "nac", "anc", "nanc")

  df$ac <- as.numeric(lapply(jointPs, function(l) l[[1]]))
  df$nac <- as.numeric(lapply(jointPs, function(l) l[[2]]))
  df$anc <- as.numeric(lapply(jointPs, function(l) l[[3]]))
  df$nanc <- as.numeric(lapply(jointPs, function(l) l[[4]]))

  return(df)
}

####
computeEVs <- function(entries, probs){
  df <- data.frame(matrix(ncol=4, nrow=1))
  colnames(df) <- c("ac", "nac", "anc", "nanc")
  df$ac <- sum(entries$ac * probs)
  df$nac <- sum(entries$nac * probs)
  df$anc <- sum(entries$anc * probs)
  df$nanc <- sum(entries$nanc * probs)
  return(df)
}

getEVs <- function(filename, mcmc){
  listener <- posterior()
  saveRDS(listener, filename)
  if(mcmc==TRUE){
    tables <- listener$value[listener$Parameter=="jointP"]
    tableEntries <- getTableEntries(tables)
    EVs <- computeEVs(tableEntries, rep(1/length(tables), length(tables)))
  }else{
    tableEntries <- getTableEntries(listener$jointP)
    EVs <- computeEVs(tableEntries, listener$prob)
  }
  return(EVs)
}

#### RUN for each utterance
mcmc <- TRUE
utterance <- "a"
filename <- paste("PL-mcmc-g20-utt-", utterance , ".rds", sep="")
EVs <- getEVs(filename, mcmc)

EV_listener[utterance, "ac"] = EVs$ac
EV_listener[utterance, "nac"] = EVs$nac
EV_listener[utterance, "anc"] = EVs$anc
EV_listener[utterance, "nanc"] = EVs$nanc

####
saveRDS(EV_listener, "expectedValsPL.rds")
