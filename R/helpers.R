source("constants.r")

create_target_dirs <- function(which_data, runs=seq(1,3)){
  # which_data: plots, data-model, data-samples
  for(i in runs){
    target_plots <- get_target_dir(which_data, i)
    checkAndCreateDir(target_plots)
  }
}

get_target_dir <- function(which_data, n_run=1){
  if(which_data=="data-model"){
    dir <- paste("../data/seed-", SEEDS[n_run], "model/", sep="")
  }else if(which_data=="data-samples"){
    dir <- paste("../data/seed-", SEEDS[n_run], "samples/", sep="")
  }else{
    dir <- paste("../plots/seed-", SEEDS[n_run], "/", sep="")
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

read_data <- function(bias, lt, n_run, alpha=5, lawn_theta=0.05, samples=TRUE){
  type <- "samples"
  if(!samples){type <- "model"}
  
  target_dir <- paste("../data/seed-", SEEDS[n_run], "/", type, "/", sep="")  
  fn <- get_filename(bias, lt, alpha, n_run, lawn_theta)
  data <- readRDS(paste(target_dir, fn, "-", type, ".rds", sep=""))
  return(data)
}

get_samples_LL_PL <- function(bias, alpha, n_run, lawn_theta=0.05){
  samples_ll <- read_data(bias, "LL", n_run, alpha, lawn_theta)
  samples_pl <- read_data(bias, "PL", n_run, alpha, lawn_theta)
  
  df <- rbind(samples_ll, samples_pl)
  return(df)
}


getInterval <- function(samples, from, to){
  low <- which(samples > from)
  high <- which(samples < to)
  indices <- intersect(low, high)
  return(indices)
}


