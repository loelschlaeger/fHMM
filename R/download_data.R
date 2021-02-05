#' Download data from www.finance.yahoo.com
#'
#' @param name A personal identifier for a stock, default \code{NULL}
#' @param symbol The stock's symbol, default \code{NULL}
#' @param from A date setting the lower data bound, default is \code{"1902-01-01"}
#' @param to A date setting the upper data bound, default is the current date \code{Sys.date()}
#' @param show_symbols A boolean determining whether all saved symbols should be printed, default \code{FALSE}
#'
#' @examples
#' ### download 21st century DAX data
#' download_data(name="dax",symbol="^GDAXI",from=as.Date("2000-01-03"))
#' 
#' ### print saved symbols
#' download_data(show_symbols=TRUE)

download_data = function(name=NULL,symbol=NULL,from="1902-01-01",to=Sys.Date(),show_symbols=FALSE){
  
  ### load and sort or create 'stock_symbols'
  if(!dir.exists("data")){
    dir.create("data")
  }
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
    if(from < min_date){
      warning(sprintf("%s (%s)",exception("D.1")[2],exception("D.1")[1]),call.=FALSE)
      from = min_date
    }
    
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
        stop(sprintf("%s (%s)",exception("D.2")[2],exception("D.2")[1]),call.=FALSE)
      }
    } else {
      if(name %in% stock_symbols["name"]){
        symbol = stock_symbols[which(stock_symbols["name"]==name),"symbol"]
      } else {
        read_try = suppressWarnings(try(read.csv(create_url(symbol,from,to)),silent=TRUE))
        if(inherits(read_try, "try-error")){
          stop(sprintf("%s (%s)",exception("D.3")[2],exception("D.3")[1]),call.=FALSE)
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
    message("data download successful")
    message(paste("source:",paste0(name,".csv")))
    message(paste("symbol:",symbol))
    message(paste("from:",head(data$Date,n=1)))
    message(paste("to:",tail(data$Date,n=1)))
  }
  
}