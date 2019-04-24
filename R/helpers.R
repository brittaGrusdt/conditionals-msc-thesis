source("constants.r")

create_target_dirs <- function(which_data, folderName='', runs=seq(1,3)){
  # which_data: plots, data-model, data-samples
  for(i in runs){
    target <- get_target_dir(which_data, folderName, i)
    checkAndCreateDir(target)
  }
}

get_target_dir <- function(which_data, folderName='', n_run=1){
  if(folderName==''){folderName <- '/'}
  else{folderName <- paste('/', folderName, '/',sep="")}
  
  if(which_data=="data-model"){
    dir <- paste("../data/seed-", SEEDS[n_run], "/model", folderName, sep="")
  }else if(which_data=="data-samples"){
    dir <- paste("../data/seed-", SEEDS[n_run], "/samples", folderName, sep="")
  }else if(which_data=="data-SE"){
    dir <- paste("../data/seed-", SEEDS[n_run], "/speakerExpectation", folderName, sep="")
  }
  else{
    dir <- paste("../plots/seed-", SEEDS[n_run], folderName, sep="")
  }
  return(dir)
}
  

get_filename <- function(bias, lt, alpha, n_run, lawn_theta=0.05){
  fn <- paste(bias, lt, sep="-")
  if(bias=='lawn'){
    theta_lawn_str <- sub(".", "", as.character(lawn_theta), fixed = TRUE)
    fn <- paste(bias, theta_lawn_str, "-", lt, sep="")
  }
  if(lt=='PL'){
    fn <- paste(fn, "alpha", alpha, sep="-")
  }
  fn <- paste(fn, "run", n_run, sep="-")
  return(fn)
}

read_data <- function(bias, lt, n_run, alpha=5, lawn_theta=0.05, folderName="", samples=TRUE){
  type <- "samples"
  if(!samples){type <- "model"}
  
  # target_dir <- paste("../data/seed-", SEEDS[n_run], "/", type, "/", sep="")  
  target_dir <- get_target_dir(paste("data-", type, sep=""), folderName, n_run)
  fn <- get_filename(bias, lt, alpha, n_run, lawn_theta)
  data <- readRDS(paste(target_dir, fn, "-", type, ".rds", sep=""))
  return(data)
}

getSamples <- function(bias, alpha, n_run, lawn_theta=0.05, folderName="", which_lt=c("LL", "PL", "prior")){
  
  df <- data.frame()
  if('prior' %in% which_lt){ 
    samples <- read_data(bias, "prior", n_run, alpha, lawn_theta, folderName)
    df <- rbind(df, samples)
  }
  if('LL' %in% which_lt){
    samples <- read_data(bias, "LL", n_run, alpha, lawn_theta, folderName)
    df <- rbind(df, samples)
  }
  if('PL' %in% which_lt){ 
    samples <- read_data(bias, "PL", n_run, alpha, lawn_theta, folderName)
    df <- rbind(df, samples)
  }

  return(df)
}


getInterval <- function(samples, from, to){
  low <- which(samples > from)
  high <- which(samples < to)
  indices <- intersect(low, high)
  return(indices)
}


