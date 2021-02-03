### install package 'hexSticker'
#remotes::install_github("GuangchuangYu/hexSticker")
library(hexSticker)

### load data
model = "HMM_3_DAX"
path = paste0("models/",model,"/")
for(object in c("controls","data","decoding","fit")){
  assign(object,readRDS(paste0(path,object,".rds")))
}

### produce sticker
s = sticker(subplot = ~{
  dates = data[["dates"]]
  logReturns = data[["logReturns"]]
  dates = dates[logReturns < sort(logReturns, decreasing=TRUE)[1:6] & logReturns > sort(logReturns, decreasing=FALSE)[1:5]]
  logReturns = logReturns[logReturns < sort(logReturns, decreasing=TRUE)[1:6] & logReturns > sort(logReturns, decreasing=FALSE)[1:5]]
  base_col = function(n) colorRampPalette(c("darkgreen","green","yellow","orange","red","darkred"))(n)
  col_alpha = function(col,alpha=0.6) adjustcolor(col,alpha)
  colors = list()
  colors[["HMM"]] = col_alpha(base_col(controls[["states"]][1]))
  T = length(logReturns)
  xmin = as.Date(format(as.Date(head(dates,n=1)),"%Y-01-01")) 
  xmax = as.Date(paste0(as.numeric(format(tail(dates,n=1),"%Y"))+1,"-01-01")) 
  ymin = min(logReturns)
  ymax = max(logReturns) 
  par(mar=c(0,0,0,0)) 
  plot(dates,logReturns,type="h",col="grey",xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(xmin,xmax),ylim=c(ymin,ymax),bty="n")
  for(s in seq_len(controls[["states"]][1])) points(dates[decoding==s],logReturns[decoding==s],col=colors[["HMM"]][s],pch=20,cex=0.25)
  },
s_x=.8, 
s_y=.65, 
s_width=2.2, 
s_height=1.3,
package="fHMM", 
p_color="darkblue",
p_size=10,
p_y = 1.45,
h_fill="cornflowerblue",
h_color="darkblue",
url="fitting HMMs to financial data",
u_color="darkblue",
u_size=1.6,
filename="sticker/sticker.pdf")

### restart R session (necessary for some unknown reason)
.rs.restartR()
