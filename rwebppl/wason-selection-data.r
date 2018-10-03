#### RUN once
EV_LL <- data.frame(matrix(ncol = 4, nrow = 12))
colnames(EV_LL) <- c("ac", "nac", "anc", "nanc")
rownames(EV_LL) <- c("a", "c", "a-c", "c-a",
                     "na", "nc",
                     "na-c", "a-nc", "na-nc",
                     "nc-a", "c-na", "nc-na")

model_path <- "/home/britta/UNI/Masterarbeit/RwebPPL/webppl/literalListener.wppl"

posterior <- function(){
  LL <- webppl(program_file = model_path)
  return(LL)
}

####
getTableEntries <- function(LL){
  df <- data.frame(matrix(ncol=4, nrow=nrow(LL)))
  colnames(df) <- c("ac", "nac", "anc", "nanc")

  df$ac <- as.numeric(lapply(LL$jointP, function(l) l[[1]]))
  df$nac <- as.numeric(lapply(LL$jointP, function(l) l[[2]]))
  df$anc <- as.numeric(lapply(LL$jointP, function(l) l[[3]]))
  df$nanc <- as.numeric(lapply(LL$jointP, function(l) l[[4]]))

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

getEVs <- function(){
  LL <- posterior()
  tableEntries <- getTableEntries(LL)
  EVs <- computeEVs(tableEntries, LL$prob)
  return(EVs)
}

#### RUN for each utterance
utterance <- "nc-na"
EVs <- getEVs()

EV_LL[utterance, "ac"] = EVs$ac
EV_LL[utterance, "nac"] = EVs$nac
EV_LL[utterance, "anc"] = EVs$anc
EV_LL[utterance, "nanc"] = EVs$nanc
