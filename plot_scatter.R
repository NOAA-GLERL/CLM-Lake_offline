#!/bin/Rscript

outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/figures/'
dat <- read.csv('../65e0d491f698c7b0fdfee2b7/table.csv', strip=T, row.names=1)

dep_red <- (dat$dep_flt-dat$dep_glo)/dat$dep_flt # dep reduction (fraction)
sa <- dat$area_km/mean(dat$area_km)
dat$dep_red <- dep_red
dat$sa <- sa

scale_by <- 2 

pdf(file=sprintf('%s/scatter.pdf', outdir), w=16, h=8)
#par(pty='s')
layout(rbind(1:2))
par(mar=c(1,1,1,1), oma=c(5,4,3,4), cex.axis=1.5)

lab_pt <- function(lkname) text(x=dat[lkname,'dep_red'], y=dat[lkname,'rmsd'], lab=lkname, adj=c(0.5,-0.7*dat[lkname, 'sa']))
#lab_pt <- function(lkname) text(x=dat[lkname,'dep_red'], y=dat[lkname,'rmse_diff'], lab=lkname, adj=c(0.5,-0.5*dat[lkname, 'sa']))

lks_lab <- c('Nipigon','Great Salt','Pontchartrain','Mono', 'Lower Red', 'Sakakawea', 'Flathead', 'Champlain', 'Manitoba')
plot(dep_red, dat$rmsd, cex=sa*scale_by, col='black', pch=20, ylab=NA, xlab=NA, xlim=c(-.4,1))

abline(h=0, lty=2)
for (lk in lks_lab) lab_pt(lk)


plot(dep_red, dat$rmse_diff, cex=sa*scale_by, col=ifelse(dat$rmse_diff>0,'red','blue'), 
	 yaxt='n', pch=20, ylab=NA, xlab=NA, xlim=c(-.4,1))
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
mtext(side=4, cex=1.5, line=2, outer=T, 'Satellite RMSE reduction (deg C)')
#mtext(side=4, cex=1.5, line=2, outer=T, 'min/max departures: GLOBathy-flatbottom (deg C)')



graphics.off()
