#' Data download
#' @description 
#' Download financial data from <https://finance.yahoo.com>.
#' @param name A character, personal identifier for a stock, default \code{NA}.
#' @param symbol A character, the stock's symbol, default \code{NA}.
#' @param from A date, setting the lower data bound, default is \code{"1902-01-01"}.
#' @param to A date, setting the upper data bound, default is the current date \code{Sys.date()}.
#' @param show_symbols A boolean, determining whether all saved symbols should be printed, default \code{FALSE}.
#' @param path A character, setting the data saving path.
#' @return 
#' No return value. Downloaded data is saved as "\code{name}.csv" in the folder "\code{path}/data".
#' @details 
#' \code{symbol} has to match the official symbol on <https://finance.yahoo.com>. 
#' Once used stock symbols are saved in "stock_symbols.rds" in the folder "\code{path}/data".
#' Values for \code{from} earlier than its default value are set to the default value.
#' @examples
#' ### download 21st century DAX data
#' download_data(name="dax",symbol="^GDAXI",from=as.Date("2000-01-03"),path=tempdir())
#' @export

download_data = function(name=NA, symbol=NA, from="1902-01-01", to=Sys.Date(), show_symbols=FALSE, path){
  
  ### load and sort or create 'stock_symbols'
  save_path = paste0(path,"/data")
  if(!dir.exists(save_path)){
    dir.create(save_path)
  }
  if(file.exists(paste0(save_path,"/stock_symbols.rds"))){
    stock_symbols = readRDS(paste0(save_path,"/stock_symbols.rds"))
    stock_symbols = unique(stock_symbols)
  } else {
    stock_symbols = data.frame("name"=character(),"symbol"=character())
    saveRDS(stock_symbols,file=paste0(save_path,"/stock_symbols.rds"))
  }
  
  ### print 'stock_symbols'
  if(show_symbols){
    if(dim(stock_symbols)[1]!=0){
      print(stock_symbols,row.names = FALSE)
    } else {
      message("No saved stock symbols.")
    }
  } 
  if(!is.na(name)){  
    ### convert 'from' and 'to' to dates
    from = as.Date(from)
    to = as.Date(to)
    ### define minimum date 'from'
    min_date = as.Date("1902-01-01")
    if(from < min_date){
      warning("D.1")
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
    if(is.na(symbol)){
      if(name %in% stock_symbols[["name"]]){
        symbol = stock_symbols[which(stock_symbols["name"]==name),"symbol"]
      } else {
        stop("D.2")
      }
    } else {
      if(name %in% stock_symbols["name"]){
        symbol = stock_symbols[which(stock_symbols["name"]==name),"symbol"]
      } else {
        read_try = suppressWarnings(try(read.csv(create_url(symbol,from,to)),silent=TRUE))
        if(inherits(read_try, "try-error")){
          stop("D.3")
        } else {
          ### save new symbol
          stock_symbols[nrow(stock_symbols)+1,] = c(name,symbol)
          saveRDS(stock_symbols,file=paste0(save_path,"/stock_symbols.rds"))
        }
      }
    }
    
    ### download and save data
    filename = paste0(save_path,"/",name,".csv")
    download.file(create_url(symbol,from,to),destfile=filename,quiet=TRUE)
    
    ### print summary of new data
    data = read.csv(file=filename,header=TRUE,sep=",",na.strings="null") 
    message(paste0("Downloaded data ",symbol," from ",head(data$Date,n=1)," to " ,tail(data$Date,n=1)),".")
  }
}