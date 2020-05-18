# viterbi.R for decoded states, results.R 

cs_obs <- cs_observations
fs_obs <- fs_observations

cs_s_s <- cs_s_single
cs_s <- cs_s
fs_s <- fs_s
pseudos_cs <- numeric(cs)
pseudos_fs <- numeric(fs*cs)

# cs
mus <- mus
sigmas <- sigmas
dfs <- dfs

# fs
mus_star <- list()
sigmas_star <- list()
dfs_star <- list()
mus_star[[1]] <- mus_star1
sigmas_star[[1]] <- sigmas_star1
dfs_star[[1]] <- dfs_star1
mus_star[[2]] <- mus_star2
sigmas_star[[2]] <- sigmas_star2
dfs_star[[2]] <- dfs_star2
mus_star[[3]] <- mus_star3
sigmas_star[[3]] <- sigmas_star3
dfs_star[[3]] <- dfs_star3

for(i in 1:cs){
	pseudos_cs[i] <- qnorm(pt((cs_obs[i]-mus[cs_s_s[i]])/sigmas[cs_s_s[i]],dfs[cs_s_s[i]]))
}
for(i in 1:(cs*fs)){
	pseudos_fs[i] <- qnorm(pt((fs_obs[i]-mus_star[[cs_s[i]]][fs_s[i]])/sigmas_star[[cs_s[i]]][fs_s[i]],dfs_star[[cs_s[i]]][fs_s[i]]))
}

pseudos <- pseudos_fs

pdf("latex/img/pseudos_tdistr.pdf", width=15, height=6)
par(mfrow = c(2,4), mar=c(5, 5, 3, 3) + 0.1, las=1,cex.lab=1.5, cex.main=1.5)#it goes c(bottom, left, top, right) 

plot(pseudos_cs,ylim=c(-3,3),xlim=c(0,160),main="Index plot",ylab="PR CS")
hist(pseudos_cs,freq=FALSE,breaks=15,col="lightgrey",ylim=c(0,0.5),xlim=c(-3,3),main="Histogram w/ N(0;1)-density",xlab="PR CS")
x <- seq(-4,4,0.01)
curve(dnorm(x),add=TRUE,lwd=2)
qqnorm(pseudos_cs[is.finite(pseudos_cs)],ylim=c(-3,3),xlim=c(-3,3),main="Normal Q-Q plot")
abline(a=0,b=1)
acf(pseudos_cs,lag.max = 10,main="")
title("Autocorrelation plot")

plot(pseudos,ylim=c(-4,4),xlim=c(0,5000),main="",ylab="PR FS")
hist(pseudos,freq=FALSE,breaks=20,col="lightgrey",ylim=c(0,0.5),xlim=c(-4,4),main="",xlab="PR FS")
x <- seq(-4,4,0.01)
curve(dnorm(x),add=TRUE,lwd=2)
qqnorm(pseudos[is.finite(pseudos)],ylim=c(-4,4),xlim=c(-4,4),main="")
abline(a=0,b=1)
acf(pseudos[is.finite(pseudos)],lag.max = 30,main="")
dev.off()

