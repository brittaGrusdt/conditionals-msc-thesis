CNs <- c('A implies C', 'A implies -C', '-A implies C', '-A implies -C',
         'C implies A', 'C implies -A', '-C implies A','-C implies -A', 
         'A ind. C')
QUDs <- c("bn", "table")
BASEDIR <- "/home/britta/UNI/Masterarbeit/conditionals/model/src/"
BIASES <- c("none", "lawn", "pizza", "douven1")
SEEDS <- c(8546340, 6538185, 5230322)

UTT_ORDER_BY_COST <- c("A", "C", "likely A", "likely C", "-A", "-C",
                       "likely -A", "likely -C", "A and C", "C but -A",
                       "A but -C", "neither A nor C", "If A, C",
                       "If C, A", "If -A, C", "If A, -C", "If -C, A",
                       "If C, -A", "If -A, -C", "If -C, -A")