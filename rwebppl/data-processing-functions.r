library(purrr)
library(dplyr)
####################

buildDF_from_samples <- function(listener){
  df <- data.frame(matrix(ncol = 2, nrow = length(listener$Parameter=='jointP')))
  colnames(df) <- c("jointP", "BN")
  df$jointP = listener$value[listener$Parameter=='jointP']
  df$causalNet = listener$value[listener$Parameter=='cn']
  return(df)
}

buildDF_from_enumerate <- function(listener){
  df <- data.frame(matrix(ncol = 3, nrow = length(listener$jointP)))
  colnames(df) <- c("jointP", "cn", "prob")
  df$jointP = listener$jointP
  df$cn = listener$cn
  df$prob = listener$prob
  return(df)
}

computeProbs <- function(df){
  probs <- data.frame(matrix(ncol = 9, nrow = nrow(df)))
  colnames(probs) <- c("pca", "pcna", "pnca", "pncna", "pc",
                       "pa", "pCgivenA", "pAgivenC", "pCgivenNA")
  tables <- df$jointP
  probs$pca <- as.numeric(map(tables, 1))
  probs$pcna <- as.numeric(map(tables, 2))
  probs$pnca <- as.numeric(map(tables, 3))
  probs$pncna <- as.numeric(map(tables, 4))
  probs$pc <- as.numeric(Map("+", probs$pca, probs$pcna))
  probs$pa <- as.numeric(Map("+", probs$pca, probs$pnca))
  probs$pCgivenA <- as.numeric(Map("/", probs$pca, probs$pa))
  probs$pAgivenC <- as.numeric(Map("/", probs$pca, probs$pc))
  probs$pAgivenNC <- as.numeric(Map("/", probs$pnca, 1-probs$pc))
  probs$pCgivenNA <- as.numeric(Map("/", probs$pcna, 1-probs$pa))
  probs$pNCgivenNA <- as.numeric(Map("/", probs$pncna, 1-probs$pa))
  return(probs)
}

computeEVs <- function(probVals, ps, type){
  EVs <- data.frame(matrix(ncol = 6, nrow = 1))
  colnames(EVs) <- c("p_c_given_a", "p_c_given_na", "p_c",
                     "p_a_given_c", "p_a_given_nc", "p_a")
  rownames(EVs) <- c(type)

  EVs$p_c_given_a <- sum(probVals$pCgivenA * ps)
  EVs$p_a_given_c <- sum(probVals$pAgivenC * ps)
  EVs$p_c_given_na <- sum(probVals$pCgivenNA * ps)
  EVs$p_a_given_nc <- sum(probVals$pAgivenNC * ps)
  EVs$p_a <- sum(probVals$pa * ps)
  EVs$p_c <- sum(probVals$pc * ps)

  return(EVs)
}
