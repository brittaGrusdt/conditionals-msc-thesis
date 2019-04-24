CNs <- c('A implies C', 'A implies -C', '-A implies C', '-A implies -C',
         'C implies A', 'C implies -A', '-C implies A','-C implies -A', 
         'A ind. C')
QUDs <- c("bn", "table")
BASEDIR <- "/home/britta/UNI/Masterarbeit/conditionals/model/src/"
# MODEL_PATH <- paste(BASEDIR, "model.wppl", sep="")
MODEL_PATH <- "/home/britta/UNI/Masterarbeit/conditionals/model/src/model.wppl"

BIASES <- c("none", "lawn", "pizza", "douven1")
SEEDS <- c(8546340, 6538185, 5230322)

UTT_ORDER_BY_COST <- c("A", "C", "likely A", "likely C", "-A", "-C",
                       "likely -A", "likely -C", "A and C", "C but -A",
                       "A but -C", "neither A nor C", "If A, C",
                       "If C, A", "If -A, C", "If A, -C", "If -C, A",
                       "If C, -A", "If -A, -C", "If -C, -A")

CP_THETAS <- c(0.1, 0.2, 0.3, 0.4, 0.5)

NB_WEBPPL_SAMPLES <- 1000000

THETA_TRUE <- 0.9
THETA_FALSE <- 1-THETA_TRUE

LABELS <- data.frame(pca=c("P(A,C)"), pcna="P(-A,C)", pnca="P(A,-C)", pncna="P(-A,-C)",
                    pc="P(C)",  pa="P(A)",
                    pCgivenA="P(C|A)", pAgivenC= "P(A|C)", pCgivenNA="P(C|-A)",  pAgivenNC="P(A|-C)",
                    pNCgivenNA="P(-C|-A)", pNAgivenNC="P(-A|-C)", stringsAsFactors = FALSE)



