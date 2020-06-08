## required packages
install.packages("ggplot2")
install.packages("grid")
install.packages("gridExtra")
install.packages("RColorBrewer")
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)

## color scheme
# coarse-scale colors
pal = brewer.pal(6, "YlOrRd")
col1 = pal[2]
col2 = pal[4]
col3 = pal[6]
# fine-scale colors
pal1 = brewer.pal(9, "Greens")
pal2 = brewer.pal(9, "Blues")
pal3 = brewer.pal(9, "Blues")
col4 = pal1[5]
col5 = pal1[8]
col6 = pal2[4]
col7 = pal2[6]
col8 = pal3[7]
col9 = pal3[9]

# unlist fine-scale log-returns
foo = NULL
for(i in 1:192){
  foo = c(foo, length(GSlogReturns[[i]]))
}

## fig4-7.eps (estimated state-dependent distributions and decoded time series)
mod = GSMod
x = seq(0.1,7e+08,length=500)
df3 = data.frame(x=x,y1=dgamma(x,shape=mod$pn$vol.mu[1]^2/mod$pn$vol.sigma[1]^2,scale=mod$pn$vol.sigma[1]^2/mod$pn$vol.mu[1]),y2=dgamma(x,shape=mod$pn$vol.mu[2]^2/mod$pn$vol.sigma[2]^2,scale=mod$pn$vol.sigma[2]^2/mod$pn$vol.mu[2]),y3=dgamma(x,shape=mod$pn$vol.mu[3]^2/mod$pn$vol.sigma[3]^2,scale=mod$pn$vol.sigma[3]^2/mod$pn$vol.mu[3]))
p3 = ggplot(data=df3)+geom_line(aes(x=x,y=y3),color=col3,size=1/2)+geom_area(aes(x=x,y=y3),fill=col3,alpha=1/20)+
  geom_line(aes(x=x,y=y2),color=col2,size=1/2)+geom_area(aes(x=x,y=y2),fill=col2,alpha=1/20)+
  geom_line(aes(x=x,y=y1),color=col1,size=1/2)+geom_area(aes(x=x,y=y1),fill=col1,alpha=1/20)+
  xlab(expression(paste(Volume[t]," (USD",{}%.%{10}^{-8},")",sep="")))+ylab(expression(paste("Density",{}%.%{10}^{8})))+ggtitle("Trade volumes")+
  theme(plot.title=element_text(hjust=0.5))+scale_x_continuous(breaks=c(0, 2e+08, 4e+08, 6e+08, 8e+08), labels=c("0.0", "2.0", "4.0", "6.0", "8.0"))+
  annotate("text", x=x[which.max(df3$y1)]+1.5e+08, y=max(df3$y1), label="coarse-scale", col=col1)+
  annotate("text", x=x[which.max(df3$y1)]+1.5e+08, y=max(df3$y1)-2.5e-09, label="state 1", col=col1)+
  annotate("text", x=x[which.max(df3$y2)]+1.2e+08, y=max(df3$y2)*1.75, label="coarse-scale", col=col2)+
  annotate("text", x=x[which.max(df3$y2)]+1.2e+08, y=max(df3$y2)*1.75-2.5e-09, label="state 2", col=col2)+
  annotate("text", x=x[which.max(df3$y3)]+0.75e+08, y=max(df3$y3)*2.25, label="coarse-scale", col=col3)+
  annotate("text", x=x[which.max(df3$y3)]+0.75e+08, y=max(df3$y3)*2.25-2.5e-09, label="state 3", col=col3)+
  scale_y_continuous(breaks=c(0, 0.5e-08, 1e-08, 1.5e-08, 2e-08), labels=c("0.0", "0.5", "1.0", "1.5", "2.0"))
  
x = seq(-0.15,0.15,length=500)
df4 = data.frame(x=x,y1=1/mod$pn$ret.sigma[[1]][1]*dt(x/mod$pn$ret.sigma[[1]][1],df=mod$pn$ret.mu[[1]][1]),y2=1/mod$pn$ret.sigma[[1]][2]*dt(x/mod$pn$ret.sigma[[1]][2],df=mod$pn$ret.mu[[1]][2]))
p7 = ggplot(data=df4)+
  geom_line(aes(x=x,y=y2),color=col5,size=1/2)+geom_area(aes(x=x,y=y2),fill=col5,alpha=1/20)+
  geom_line(aes(x=x,y=y1),color=col4,size=1/2)+geom_area(aes(x=x,y=y1),fill=col4,alpha=1/20)+
  xlab(expression(paste(LogReturn["t,"],{}["t'"],sep="")))+ylab(expression(paste("Density",{}%.%{10}^{-1},sep="")))+ggtitle("Coarse-scale state 1")+
  annotate("text", x=x[which.max(df4$y1)]-0.08, y=max(df4$y1), label="fine-scale", col=col4)+
  annotate("text", x=x[which.max(df4$y1)]-0.08, y=max(df4$y1)-4, label="state 1", col=col4)+
  annotate("text", x=x[which.max(df4$y2)]+0.09, y=max(df4$y2), label="fine-scale", col=col5)+
  annotate("text", x=x[which.max(df4$y2)]+0.09, y=max(df4$y2)-4, label="state 2", col=col5)+
  theme(plot.title=element_text(hjust=0.5))+scale_y_continuous(limits=c(0, max(df4)), breaks=c(0,10,20,30), labels=c("0.0", "1.0", "2.0", "3.0"))
  
df5 = data.frame(x=x,y1=1/mod$pn$ret.sigma[[2]][1]*dt(x/mod$pn$ret.sigma[[2]][1],df=mod$pn$ret.mu[[2]][1]),y2=1/mod$pn$ret.sigma[[2]][2]*dt(x/mod$pn$ret.sigma[[2]][2],df=mod$pn$ret.mu[[2]][2]))
p8 = ggplot(data=df5)+
  geom_line(aes(x=x,y=y2),color=col7,size=1/2)+geom_area(aes(x=x,y=y2),fill=col7,alpha=1/20)+
  geom_line(aes(x=x,y=y1),color=col6,size=1/2)+geom_area(aes(x=x,y=y1),fill=col6,alpha=1/20)+
  xlab(expression(paste(LogReturn["t,"],{}["t'"],sep="")))+ylab(expression(paste("Density",{}%.%{10}^{-1},sep="")))+ggtitle("Coarse-scale state 2")+
  annotate("text", x=x[which.max(df5$y1)]-0.08, y=max(df5$y1), label="fine-scale", col=col6)+
  annotate("text", x=x[which.max(df5$y1)]-0.08, y=max(df5$y1)-4, label="state 1", col=col6)+
  annotate("text", x=x[which.max(df5$y2)]+0.09, y=max(df5$y2), label="fine-scale", col=col7)+
  annotate("text", x=x[which.max(df5$y2)]+0.09, y=max(df5$y2)-4, label="state 2", col=col7)+
  theme(plot.title=element_text(hjust=0.5))+scale_y_continuous(limits=c(0, max(df4)), breaks=c(0,10,20,30), labels=c("0.0", "1.0", "2.0", "3.0"))
  
df6 = data.frame(x=x,y1=1/mod$pn$ret.sigma[[3]][1]*dt(x/mod$pn$ret.sigma[[3]][1],df=mod$pn$ret.mu[[3]][1]),y2=1/mod$pn$ret.sigma[[3]][2]*dt(x/mod$pn$ret.sigma[[3]][2],df=mod$pn$ret.mu[[3]][2]))
p9 = ggplot(data=df6)+
  geom_line(aes(x=x,y=y2),color=col9,size=1/2)+geom_area(aes(x=x,y=y2),fill=col9,alpha=1/20)+
  geom_line(aes(x=x,y=y1),color=col8,size=1/2)+geom_area(aes(x=x,y=y1),fill=col8,alpha=1/20)+
  xlab(expression(paste(LogReturn["t,"],{}["t'"],sep="")))+ylab(expression(paste("Density",{}%.%{10}^{-1},sep="")))+ggtitle("Coarse-scale state 3")+
  annotate("text", x=x[which.max(df6$y1)]-0.08, y=max(df6$y1)*1.25, label="fine-scale", col=col8)+
  annotate("text", x=x[which.max(df6$y1)]-0.08, y=max(df6$y1)*1.25-4, label="state 1", col=col8)+
  annotate("text", x=x[which.max(df6$y2)]+0.09, y=max(df6$y2)*2, label="fine-scale", col=col9)+
  annotate("text", x=x[which.max(df6$y2)]+0.09, y=max(df6$y2)*2-4, label="state 2", col=col9)+
  theme(plot.title=element_text(hjust=0.5))+scale_y_continuous(limits=c(0, max(df4)), breaks=c(0,10,20,30), labels=c("0.0", "1.0", "2.0", "3.0"))
  
cols = rep(col1, 192)
cols[GSStates$ll.states==2] = col2
cols[GSStates$ll.states==3] = col3
df2 = data.frame(x=1:192,y=GSVolumes)
p12 = ggplot(data=df2)+
  geom_line(aes(x=x,y=y),color=cols,size=1/2)+
  xlab("t")+ylab(expression(paste(Volume[t]," (USD",{}%.%{10}^{-8},")",sep="")))+ggtitle("Decoded time series")+
  geom_vline(aes(xintercept=57), linetype="dashed")+
  geom_vline(aes(xintercept=44), linetype="dashed")+
  theme(plot.title=element_text(hjust=0.5))+scale_y_continuous(limits=c(0, max(df2)), breaks=c(0, 2e+08, 4e+08, 6e+08), labels=c("0.0", "2.0", "4.0", "6.0"))+scale_x_continuous(limits=c(1, 192), breaks=c(1, 85, 169), labels=c("Jan. 2004", "Jan. 2011", "Jan. 2018"))

cols = NULL
for(i in 1:192){
  for(j in 1:length(GSStates$ul.states[[i]])){
    if(GSStates$ll.states[i]==1&GSStates$ul.states[[i]][j]==1){
      cols = c(cols, col4)
    }
    if(GSStates$ll.states[i]==1&GSStates$ul.states[[i]][j]==2){
      cols = c(cols, col5)
    }
    if(GSStates$ll.states[i]==2&GSStates$ul.states[[i]][j]==1){
      cols = c(cols, col6)
    }
    if(GSStates$ll.states[i]==2&GSStates$ul.states[[i]][j]==2){
      cols = c(cols, col7)
    }
    if(GSStates$ll.states[i]==3&GSStates$ul.states[[i]][j]==1){
      cols = c(cols, col8)
    }
    if(GSStates$ll.states[i]==3&GSStates$ul.states[[i]][j]==2){
      cols = c(cols, col9)
    }
  }
}
  
df4 = data.frame(x=1:4026, y=unlist(GSlogReturns))
p13 = ggplot(data=df4)+
  geom_line(aes(x=x,y=y),color=cols,size=1/2)+
  xlab(expression(paste("t,","t'",sep="")))+ylab(expression(paste(LogReturn["t,"],{}["t'"],sep="")))+ggtitle("Decoded time series")+
  geom_vline(aes(xintercept=1184), linetype="dashed")+
  geom_vline(aes(xintercept=907), linetype="dashed")+
  theme(plot.title=element_text(hjust=0.5))+scale_y_continuous(labels=scaleFUN1)+
  scale_x_continuous(limits=c(1,4026), breaks=c(1, 1764, 3525), labels=c("Jan. 2, 2004", "Jan. 3, 2011", "Jan. 2, 2018"))
  
df5 = data.frame(x=1:4026, y=unlist(GSClose))
p14 = ggplot(data=df5)+
  geom_line(aes(x=x,y=y),color=cols,size=1/2)+
  xlab(expression(paste("t,","t'",sep="")))+ylab(expression(paste(ClosingPrice["t,"],{}["t'"]," (USD)",sep="")))+ggtitle("Decoded time series")+
  geom_vline(aes(xintercept=1184), linetype="dashed")+
  geom_vline(aes(xintercept=907), linetype="dashed")+
  theme(plot.title=element_text(hjust=0.5))+scale_y_continuous(breaks=c(50, 100, 150, 200, 250),labels=scaleFUN0)+
  scale_x_continuous(limits=c(1,4026), breaks=c(1, 1764, 3525), labels=c("Jan. 2, 2004", "Jan. 3, 2011", "Jan. 2, 2018"))
  
p17 = grid.arrange(grid.arrange(p3, p12, nrow=1), grid.arrange(p7, p8, p9, nrow=1), grid.arrange(p13, p14, nrow=1), nrow=3)
ggsave(file="/.../fig4-7.eps", device=cairo_ps, plot=p17, width=7.5, height=6.6)

## fig4-8.eps (qq-plots and sample ACFs)
q1 = qqnorm(GSPr$volRes)
q2 = qqnorm(unlist(GSPr$retRes))
a1 = acf(GSPr$volRes, na.action=na.pass,lag=40)[[1]][,,1]
a2 = acf(unlist(GSPr$retRes),na.action=na.pass,lag=40)[[1]][,,1]
ci1 = rep(qnorm((1+0.95)/2)/sqrt(acf(GSPr$volRes,na.action=na.pass,lag=40)$n.used),41)
ci2 = rep(qnorm((1+0.95)/2)/sqrt(acf(unlist(GSPr$retRes),na.action=na.pass,lag=40)$n.used),41)
  
df10 = data.frame(x=q1$x,y=q1$y)
cols = rep(col1, 192)
cols[GSStates$ll.states==2] = col2
cols[GSStates$ll.states==3] = col3
p11 = ggplot(data=df10)+geom_point(aes(x=x,y=y),col=cols,shape=21,size=1.5,alpha=1/2,fill=cols)+geom_line(aes(x=seq(min(df10$x,na.rm=TRUE),max(df10$x,na.rm=TRUE),length=192),y=seq(min(df10$x,na.rm=TRUE),max(df10$x,na.rm=TRUE),length=192)),size=1/2,linetype="dashed")+xlab("Theoretical quantiles")+ylab("Sample quantiles")+
  scale_x_continuous(labels=scaleFUN1,breaks=seq(-3,3,by=1.5))+
  scale_y_continuous(labels=scaleFUN1,breaks=seq(-3,3,by=1.5))+
  ggtitle("Trade volumes")+
  theme(plot.title=element_text(hjust=0.5))
  
df11 = data.frame(x=0:40,y=a1,z=ci1)
p12 = ggplot(data=df11)+geom_segment(aes(x=x,y=rep(0,41),xend=x,yend=y),size=0.5)+geom_line(aes(x=x,y=z),color="black",linetype="dashed")+geom_line(aes(x=x,y=-z),color="black",linetype="dashed")+xlab("Lag")+ylab("Sample ACF")+scale_x_continuous(labels=scaleFUN0)+scale_y_continuous(labels=scaleFUN1, breaks=seq(-0.2, 1, by=0.3),limits=c(-0.2,1))+
  ggtitle("Trade volumes")+
  theme(plot.title=element_text(hjust=0.5))
  
df12 = data.frame(x=q2$x,y=q2$y)
cols = NULL
for(i in 1:192){
  for(j in 1:length(GSStates$ul.states[[i]])){
    if(GSStates$ll.states[i]==1&GSStates$ul.states[[i]][j]==1){
      cols = c(cols, col4)
    }
    if(GSStates$ll.states[i]==1&GSStates$ul.states[[i]][j]==2){
      cols = c(cols, col5)
    }
    if(GSStates$ll.states[i]==2&GSStates$ul.states[[i]][j]==1){
      cols = c(cols, col6)
    }
    if(GSStates$ll.states[i]==2&GSStates$ul.states[[i]][j]==2){
      cols = c(cols, col7)
    }
    if(GSStates$ll.states[i]==3&GSStates$ul.states[[i]][j]==1){
      cols = c(cols, col8)
    }
    if(GSStates$ll.states[i]==3&GSStates$ul.states[[i]][j]==2){
      cols = c(cols, col9)
    }
  }
}
p13 = ggplot(data=df12)+geom_point(aes(x=x,y=y),col=cols,shape=21,size=1.5,alpha=1/2,fill=cols)+geom_line(aes(x=seq(min(df12$x,na.rm=TRUE),max(df12$x,na.rm=TRUE),length=4026),y=seq(min(df12$x,na.rm=TRUE),max(df12$x,na.rm=TRUE),length=4026)),linetype="dashed")+xlab("Theoretical quantiles")+ylab("Sample quantiles")+
  scale_x_continuous(labels=scaleFUN1, breaks=seq(-3,3,by=1.5))+
  scale_y_continuous(labels=scaleFUN1, breaks=seq(-3,3,by=1.5))+
  ggtitle("Log-returns")+
  theme(plot.title=element_text(hjust=0.5))
  
df13 = data.frame(x=0:40,y=a2,z=ci2)
p14 = ggplot(data=df13)+geom_segment(aes(x=x,y=rep(0,41),xend=x,yend=y),size=0.5)+geom_line(aes(x=x,y=z),color="black",linetype="dashed")+geom_line(aes(x=x,y=-z),color="black",linetype="dashed")+xlab("Lag")+ylab("Sample ACF")+scale_x_continuous(labels=scaleFUN0)+scale_y_continuous(labels=scaleFUN1, breaks=seq(-0.2, 1, by=0.3),limits=c(-0.2,1))+
  ggtitle("Log-returns")+
  theme(plot.title=element_text(hjust=0.5))
  
p23 = grid.arrange(p11, p12, p13, p14, nrow=2)
ggsave(file="/.../fig4-8.eps", plot=p23, device=cairo_ps, width=7.5, height=6.1)
