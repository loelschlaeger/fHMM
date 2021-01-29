#' Download data from www.finance.yahoo.com
#'
#' @param name
#' @param symbol
#' @param from
#' @param to
#' @param show_symbols
#'
#' @return ...
#'
#' @examples
#' download_data("dax","^GDAXI")
#' download_data(show_symbols=TRUE)
download_data = function(name=NULL,symbol=NULL,from=as.Date("1902-01-01"),to=Sys.Date(),show_symbols=FALSE){
  
  ### load and sort or create 'stock_symbols'
  if(file.exists("data/stock_symbols.rds")){
    stock_symbols = readRDS("data/stock_symbols.rds")
    stock_symbols = stock_symbols[order(stock_symbols["name"]),]
  } else {
    stock_symbols = data.frame("name"=character(),"symbol"=character())
    saveRDS(stock_symbols,file="data/stock_symbols.rds")
  }
  
  ### print 'stock_symbols'
  if(show_symbols){
    if(dim(stock_symbols)[1]!=0){
      print(stock_symbols,row.names = FALSE)
    } else {
      message("No saved stock symbols.")
    }
  } 
  if(!is.null(name)){  
    ### convert 'from' and 'to' to dates
    from = as.Date(from)
    to = as.Date(to)
    
    ### define minimum date 'from'
    min_date = as.Date("1902-01-01")
    if(from < min_date) from = min_date
    
    ### function to create finance.yahoo.com-URL
    create_url = function(symbol,from,to){
      t1 = as.integer(ISOdate(as.numeric(format(from,format="%Y")),as.numeric(format(from,format="%m")),as.numeric(format(from,format="%d")),hour=0))
      t2 = as.integer(ISOdate(as.numeric(format(to,format="%Y")),as.numeric(format(to,format="%m")),as.numeric(format(to,format="%d")),hour=24))
      url = paste("https://query1.finance.yahoo.com/v7/finance/download/",symbol,"?period1=",t1,"&period2=",t2,"&interval=1d&events=history",sep="")
      return(url)
    }
    
    ### covert 'name' to lowercase
    name = tolower(name)
    
    ### search 'name' in 'stock_symbols' and get corresponding 'symbol'
    if(is.null(symbol)){
      if(name %in% stock_symbols[["name"]]){
        symbol = stock_symbols[which(stock_symbols["name"]==name),"symbol"]
      } else {
        stop(paste0("Symbol for the stock '",name,"' is unknown."),call.=FALSE)
      }
    } else {
      if(name %in% stock_symbols["name"]){
        symbol = stock_symbols[which(stock_symbols["name"]==name),"symbol"]
      } else {
        read_try = suppressWarnings(try(read.csv(create_url(symbol,from,to)),silent=TRUE))
        if(inherits(read_try, "try-error")){
          stop(paste0("Symbol '",symbol,"' is invalid."),call.=FALSE)
        } else {
          ### save new symbol
          stock_symbols[nrow(stock_symbols)+1,] = c(name,symbol)
          saveRDS(stock_symbols,file="data/stock_symbols.rds")
        }
      }
    }
    
    ### download and save data
    filename = paste0("data/",name,".csv")
    download.file(create_url(symbol,from,to),destfile=filename,quiet=TRUE)
    
    ### print summary of new data
    data = read.csv(file=filename,head=TRUE,sep=",",na.strings="null") 
    message("Data download successful.")
    message(paste("Source:",paste0(name,".csv")))
    message(paste("Symbol:",symbol))
    message(paste("From:",head(data$Date,n=1)))
    message(paste("To:",tail(data$Date,n=1)))
  }
  
}