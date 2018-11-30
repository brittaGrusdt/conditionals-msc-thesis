source("running-functions.r")
library(dplyr)
library("ggpubr")
library("gplots")

##### constants #####
model <- paste(BASEDIR, "vanilla-rsa-with-quds.wppl", sep="")
bias <- "douven1"
qud <- "bn"

simpleUtts <- c("null", "A", "C", "-A", "-C")
probUtts <- c("likely A", "likely C", "likely -A", "likely -C")
conjUtts <- c("A and C", "C but -A", "A but -C", "neither A nor C")
ifacUtts <- c("If A, C", "If A, -C", "If -A, C", "If -A, -C")
ifcaUtts <- c("If C, A", "If C, -A", "If -C, A", "If -C, -A")
utterances <- c(simpleUtts, probUtts, conjUtts, ifacUtts, ifcaUtts)
##### functions #####
buildDF <- function(samples, utterance){
  df <- data.frame(matrix(ncol = 2, nrow = length(samples)))
  colnames(df) <- c("utterance", "pspeaker")
  
  df$pspeaker <- samples
  df$utterance <- rep(utterance, length(samples))

  return(df)
}

##### get samples from webppl program #####
all_results <- data.frame(utterance= numeric(0), pspeaker= numeric(0))
for (utt in utterances){
  data <- list(bias=bias, utterance=utt, qud=qud)
  likelihoods <- posterior_with_data_input(model, data, viz=FALSE, seed=SEEDS[1])
  
  result <- buildDF(likelihoods, utt)
  all_results <- rbind(all_results, result)
}

##### statistics #####
data <- group_by(all_results, utterance) %>%
  summarise(
    count = n(),
    mean = mean(pspeaker),
    sd = sd(pspeaker)
  )

# Compute analysis of variance
res.aov <- aov(pspeaker ~ utterance, data = all_results)
summary(res.aov)
tuk<- TukeyHSD(res.aov)

##### visualizations ####
ylabel <- paste("P_s(utt | prior=", bias, ")", sep="")
ggboxplot(all_results, x = "utterance", y = "pspeaker", 
          color = "utterance", 
          ylab = ylabel, xlab = "utterances")

simples <- all_results[which(all_results$utterance %in% simpleUtts), ]
conjs <- all_results[which(all_results$utterance %in% conjUtts), ]
probs <- all_results[which(all_results$utterance %in% probUtts), ]
ifacs <- all_results[which(all_results$utterance %in% ifacUtts), ]
ifcas <- all_results[which(all_results$utterance %in% ifcaUtts), ]

dfList <- list(simples) #, conjs, probs, ifacs, ifcas)
plotM <- function(df){
  jpeg(paste(bias, "-qud-", qud, ".jpeg", sep=""), width = 480, height=480)
  plotmeans(pspeaker ~ utterance, data = df,
          xlab = "utterances", ylab = ylabel,
          n.label=FALSE,
          main="Expectation Speaker (95% CI)") 
  dev.off()
}

lapply(dfList, plotM)






