# Process financial data from csv file

readData = function(controls){
  fileName = controls[["fileName"]]
  t_min    = controls[["t_min"]]
  t_max    = controls[["t_max"]]
  fs       = controls[["T_star"]]
  data      = read.csv(file=fileName,head=TRUE,sep=",",na.strings="null")
	data$Date = as.Date(data$Date, format="%Y-%m-%d")
	min       = which(data$Date==as.Date(t_min))
	if(length(min)==0){ stop("non-valid start time for time series") }
	max       = which(data$Date==as.Date(t_max))
	if(length(max)==0){ stop("non-valid end time for time series") }
	data      = data[min:max,]
	data      = data[which(!is.na(data$Close)),]
	T  = length(data[,1])
	log_returns = numeric(T)
	for(t in 2:T){
		log_returns[t] = log(data$Close[t]/data$Close[t-1])
	}
	data = cbind(data,log_returns)
	## Split into coarse scale and fine scale
	cs   = floor(T/fs)
	T    = fs*cs        
	data = data[1:T,]
	fs_obs = matrix(data$log_returns,ncol=fs,nrow=cs,byrow=TRUE)
	cs_obs = numeric(cs)
	for(i in 1:cs){
		cs_obs[i] = mean(fs_obs[i,])
	}
	observations = cbind(cs_obs,fs_obs,deparse.level=0)
	return(list(
	  "observations" = observations,
	  "cs_obs" = cs_obs,
	  "fs_obs" = data$log_returns,
	  "close" = data$Close,
	  "date" = data$Date
	 )
	)
}

