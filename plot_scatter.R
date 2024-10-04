#!/bin/Rscript
source('utils.R')
library(fields)

dat <- read.csv('table.csv', strip=T, row.names=1)
dat <- dat[-24,]

dep_red <- (dat$dep_flt-dat$dep_glo)/dat$dep_flt # dep reduction (fraction)
sa <- dat$area_km/mean(dat$area_km)
dat$dep_red <- dep_red
dat$sa <- sa


# handle latitude
dat$lat <- meta$lat
levs <- pretty(c(26.0,dat$lat), n=60)
nlevs <- length(levs)
cidx <- findInterval(dat$lat, levs)
cmap <- viridis(nlevs)






scale_by <- 3 




pdf(file=sprintf('%s/scatter.pdf', outdir), w=16, h=8)
#par(pty='s')
#layout(rbind(1:2))
layout(matrix(c(3,3,1,2), 2,2, byrow=T), heights=c(.1,.9))
par(mar=c(1,1,1,1), oma=c(5,4,3,4), cex.axis=1.5)

lab_pt <- function(lkname) text(x=dat[lkname,'dep_red'], y=dat[lkname,'rmsd'], lab=lkname, adj=c(0.5,-1.0*dat[lkname, 'sa']))
#lab_pt <- function(lkname) text(x=dat[lkname,'dep_red'], y=dat[lkname,'rmsd'], lab=lkname, adj=c(0.5,-0.7*dat[lkname, 'sa']))
#lab_pt <- function(lkname) text(x=dat[lkname,'dep_red'], y=dat[lkname,'rmse_diff'], lab=lkname, adj=c(0.5,-0.5*dat[lkname, 'sa']))

lks_lab <- c('Nipigon','Great Salt','Pontchartrain','Mono', 'Lower Red', 'Sakakawea', 'Flathead', 'Okeechobee', 'Manitoba')
plot(dep_red, dat$rmsd, cex=sa*scale_by, col=cmap[cidx], pch=20, ylab=NA, xlab=NA, xlim=c(-.4,1), lwd=3)

abline(h=0, lty=2)
for (lk in lks_lab) lab_pt(lk)


#plot(dep_red, dat$rmse_diff, cex=sa*scale_by, col='black', bg=cmap[cidx], yaxt='n', pch=21, ylab=NA, xlab=NA, xlim=c(-.4,1))
plot(dep_red, dat$rmse_diff, cex=sa*scale_by, col=cmap[cidx], yaxt='n', pch=20, ylab=NA, xlab=NA, xlim=c(-.4,1))
abline(h=0, lty=2)
axis(4)


## range plots
#plot(dep_red, dat$mindif, cex=sa*scale_by, col='blue', pch=20, 
#	 ylim=range(c(dat$mindif,dat$maxdif)), ylab=NA, xlab=NA, yaxt='n')
#points(dep_red, dat$maxdif, cex=sa*scale_by, col='red', pch=20)
#abline(h=0, lty=2)
#axis(4)

mtext(side=1, cex=1.5, line=2.5, outer=T, 'proportional reduction in lake depth')
mtext(side=2, cex=1.5, line=2, outer=T, 'root-mean-square deviation (deg C)')
mtext(side=4, cex=1.5, line=2, outer=T, 'Satellite RMSE change (deg C)')
#mtext(side=4, cex=1.5, line=2, outer=T, 'min/max departures: GLOBathy-flatbottom (deg C)')


par(mar=c(2,20,0,20))  #pdf
#par(mar=c(3,10,4,10))  #pdf
plot.new()
plot.window(ylim=c(0,1), xlim=c(0,nlevs))
rect(0:(nlevs-2), 0, 1:(nlevs-1),1, col=cmap, border=NA)
#lablevs <- levs - (levs%%0.5) # round labels to nearest 0.5
#axis(1, at=(1:nlevs)[c(F,T,F,F,F,F)]-0.5, lab=levs[c(F,T,F,F,F,F)], lwd=0, line=-.5)
axis(1, at=1:nlevs, lab=levs, lwd=0, line=-.5, gap.axis=8)
mtext('latitude (°N)', 3, cex=1.5, line=1)



graphics.off()
