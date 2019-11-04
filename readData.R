### READ DATA

# INPUT: filename, desired fs time horizon
# OUTPUT: matrix with crude scale observations in first column and fine scale observations in corresponding row
readData <- function(controls){
  
  fileName = controls[["fileName"]]
  t_min    = controls[["t_min"]]
  t_max    = controls[["t_max"]]
  fs       = controls[["T_star"]]
  

	data <- read.csv(file=fileName,head=TRUE,sep=",",na.strings="null")
	data$Date <- as.Date(data$Date, format="%Y-%m-%d")
	min <- which(data$Date==as.Date("2000-1-3"))
	max <- which(data$Date==as.Date("2018-12-28"))
	data <- data[min:max,]
	data <- data[which(!is.na(data$Close)),]
	
	T <- length(data[,1])
	log_returns <- numeric(T)
	for(t in 2:T){
		log_returns[t] <- log(data$Close[t]/data$Close[t-1])
	}
	data <- cbind(data,log_returns)

	## Split into crude scale and fine scale

	T <- length(data[,1])
	cs <- floor(T/fs)
	T <- fs*cs
	data <- data[1:T,]

	fs_observations <- matrix(data$log_returns,ncol=fs,nrow=cs,byrow=TRUE)
	cs_observations <- numeric(cs)
	for(i in 1:cs){
		cs_observations[i] <- mean(fs_observations[i,])
	}
	observations <- cbind(cs_observations,fs_observations,deparse.level=0)

	return(observations)

}

