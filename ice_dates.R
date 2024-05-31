#!/bin/Rscript
library(stringr)

fig_out <- '../65e0d491f698c7b0fdfee2b7/figures/on_off_dates.pdf'
w <- 15; h <- 7
fgcol <- 'dodgerblue';  # model color
bgcol='white'; 
oncol <- 'gold3'  
offcol <- 'forestgreen'
lwd <- 5 # width for obs lines

meta <- read.table('txt/lake_ids.txt', head=T)
lks <- meta$name
dat <- read.csv('PhenologyData.csv')

# subset for my lakes
ice_obs <- dat[dat$lake %in% lks,c('lake','start_year','iceOn','iceOff')]
# subset for 2018-2019 year
ice_obs <- ice_obs[ice_obs$start_year==2018,]



ctl <- read.table('csv/ctl_ice.csv', sep=',', row.names=1)
glo <- read.table('csv/glo_ice.csv', sep=',', row.names=1)

ctl <- ctl[,c(ice_obs$lake)]
glo <- glo[,c(ice_obs$lake)]

# trim dates to not include 2020 season
dts <- as.Date(rownames(ctl), format='%Y%m%d')
seldts <- dts < as.Date('2019-09-01')
ctl <- ctl[seldts,]
glo <- glo[seldts,]


#debug
ctl_ice <- ctl
glo_ice <- glo 



onval <- .9
ofval <- .1

on_idx <- apply(ctl, 2, function(x) min(which(x>onval)))
off_idx <- apply(ctl, 2, function(x) max(which(x>=ofval)))
ctl[] <- F
for (i in 1:4){ 
	if(!is.finite(on_idx[i])) next
	ctl[on_idx[i]:off_idx[i],i] <- T
}

rm('on_idx','off_idx')

on_idx <- apply(glo, 2, function(x) min(which(x>onval)))
off_idx <- apply(glo, 2, function(x) max(which(x>=ofval)))
glo[] <- F
for (i in 1:4){
	if(!is.finite(on_idx[i])) next
	glo[on_idx[i]:off_idx[i],i] <- T
}




onoff <- interleave(t(ctl), t(glo))
lknames <- unique(rownames(onoff))
lknames <- gsub('aukee','.',lknames)
lknames <- str_to_title(lknames)
dts <- as.Date(colnames(onoff), format='%Y%m%d')



# ====================== PLOT ====================================================
pdf(file=fig_out, width=w, height=h)
par(cex.axis=1.5, cex.lab=1.5)
# plot both models
image(x=dts, y=(1:nrow(onoff))-.5, z=t(onoff), yaxt='n', ylab=NA,
	  xlim=c(as.Date('2018-12-01'), as.Date('2019-05-01')), col=c(bgcol,fgcol))
abline(h=(1:8), col='white')
axis(2, at=seq(1,7,by=2), lab=lknames, tcl=0, line=1, lwd=0)
axis(2, at=(0:7)+.5, lab=rep(c('FB','Glo'),4), tcl=0, line=-.5, lwd=0, cex.axis=1.125)
axis(2, at=c(0,2,4,6,8), lab=NA, tcl=-3)
abline(h=c(2,4,6), col='black')


# add obs on/offdates
row.names(ice_obs) <- lknames
ice_obs <- ice_obs[,-1]

y0 <- 0
for (lk in lknames){
	segments(x0=as.Date(ice_obs[lk,'iceOn']), y0=y0, y1=y0+2, col=oncol, lwd=lwd, lend=1)
	segments(x0=as.Date(ice_obs[lk,'iceOff']), y0=y0, y1=y0+2, col=offcol, lwd=lwd, lend=1)
	y0 <- y0+2
}

legend('top', legend=c('CLM-Lake','Observed On','Observed Off'), 
	   text.col=c(fgcol,oncol,offcol), horiz=T, xpd=T, inset=-.15, adj=.075, cex=1.5)

dev.off()



