#!/bin/Rscript
qlibrary(viridis)
qlibrary(stringr)
qlibrary(fields)
graphics.off()


outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/figures/'
rng <- list(min=-40, max=50)

lkmeta <- read.table('lake_wide_csv/lake_ids.txt', head=T)

min_frac <- .3
ctlcol <- 'brown'
glocol <- 'orange'

filter_rng <- function(data){
	sellow <- which(data < rng$min, arr.ind=T)
	selhig <- which(data > rng$max, arr.ind=T)
	data[sellow] <- NA
	data[selhig] <- NA
	if(length(sellow) > 0) cat(sprintf('threw out %i low vals\n', length(sellow)))
	if(length(selhig) > 0) cat(sprintf('threw out %i high vals\n', length(selhig)))
	return(data)
}

obs <- read.table('lake_wide_csv/temp_out_mq3.txt', row.names=1)
frac <- read.table('lake_wide_csv/frac_out_mq3.txt', row.names=1)
obs[frac < min_frac] <- NA

dts_obs <- as.Date(row.names(obs), format='%Y%m%d')
row.names(obs) <- dts_obs
ctl <- read.table('lake_wide_csv/ctl_T3D.csv', row.names=1, sep=',')
glo <- read.table('lake_wide_csv/bi0m_T3D.csv', row.names=1, sep=',')


dts_mod <- as.POSIXct(row.names(ctl), 'z')
ymd <- format(dts_mod, '%y%m%d')

# filter for wild values
ctl <- filter_rng(ctl)
glo <- filter_rng(glo)
obs <- filter_rng(obs)

# save hourly data for later
ctl_all <- ctl
glo_all <- glo

ctl_avg <- aggregate(ctl, by=list(ymd), mean, na.rm=T, finite=T)[,-1]
glo_avg <- aggregate(glo, by=list(ymd), mean, na.rm=T, finite=T)[,-1]
seldts <- grep('201.-..-.. 12:00:00', dts_mod)
ctl <- ctl[seldts,]
glo <- glo[seldts,]
dts_mod <- unique(as.Date(dts_mod))
row.names(ctl) <- dts_mod
row.names(glo) <- dts_mod
row.names(ctl_avg) <- dts_mod
row.names(glo_avg) <- dts_mod



# rename and re-order lakes
renamorder <- function(dat){ 
	dat <- dat[,order(names(dat), decreasing=T)]
	names(dat) <- str_to_title(gsub('_',' ',names(dat)))
	names(dat)[names(dat)=='Mcconaugh'] <- 'McConaughy'
	names(dat) <- gsub(' Lake','', names(dat)
	)
	return(dat)
}


ctl <- renamorder(ctl)
glo <- renamorder(glo)
ctl_avg <- renamorder(ctl_avg)
glo_avg <- renamorder(glo_avg)
#glo2 <- renamorder(glo2)
obs <- renamorder(obs)
lks <- names(ctl)
max_diff <- abs(max(range(glo-ctl, na.rm=T)))

#dts_mod <- dts_mod[1:420]
#ctl <- ctl[1:420,]
#glo <- glo[1:420,]

ctl_ice <- read.table('lake_wide_csv/ctl_ice.csv', row.names=1, sep=',')
glo_ice <- read.table('lake_wide_csv/glo_ice.csv', row.names=1, sep=',')
dts_ice <- as.Date(row.names(ctl_ice), format='%Y%m%d')
ice_mask <- matrix(NA, length(dts_ice), ncol(ctl_ice))

thresh <- .9
ice_mask[ glo_ice > thresh & ctl_ice > thresh] <- 1

nlks <- length(lks)

#dbg
#x11()
#plot(dts_ice, glo_ice$sebago, 'l')
#lines(dts_ice, ctl_ice$sebago, 'l', col='blue')
#abline(h=.9, lty=2)
#points(dts_ice,ice_mask[19,], pch=20, col='red')


# temp diff plot
if(F){
	pdf(file=sprintf('%s/lakewide_tempdiff.pdf', outdir), w=20)
	#x11(w=20)
	par(mar=c(4,9.5,4,2), cex.axis=1.25, cex.main=2)
	image.plot(x=dts_mod, y=1:ncol(ctl), z=as.matrix(glo_avg-ctl_avg), col=hcl.colors(256,'blue-Red 3'), xaxt='n', yaxt='n', 
			   main='Temperature Difference (°C): GLOBathy - Flatbottom ', ylab=NA, xlab=NA, zlim=c(-7,7))
	axis(2, at=1:ncol(ctl), lab=names(ctl), las=2)
	#axis.Date(side=1, x=dts_mod, at=seq(dts_mod[1], rev(dts_mod)[1], by='month', format='%b'), format='%b \'%y')
	axis.Date(side=1, x=dts_mod, at=seq(dts_mod[1], rev(dts_mod)[1], by='month'), format='%b')

	for (i in 1:nlks) points(x=dts_ice, y=rep(nlks-i+1, length(dts_ice)), col=ice_mask[,i], pch=3, lwd=.5, cex=.75)
	#for (i in 1:nlks) points(x=dts_ice, y=rep(nlks-i+1, length(dts_ice)), col=ice_mask[,i], pch='-', lwd=.5, cex=.75)
	dev.off()
}


# INTERSECT DATES for OBS and MODEL 
comdts <- format(as.Date(intersect(dts_mod, dts_obs)), '%Y-%m-%d')
comdts <- comdts[comdts<'2019-12-20']  # ditch bad data post xmas (some ctl lakes blowup)


ctl <- ctl[comdts,]
glo <- glo[comdts,]
obs <- obs[comdts,]
dts <- as.Date(comdts)


# loop to draw individual plots
if(F){
	graphics.off()
	for (lk in rev(lks)){
		if(all(is.na(obs[,lk]))) next
		x11(w=14)
		plot(dts, obs[,lk], main=lk, pch=20, ylab='surface temp (deg C)', xlab=NA)
		lines(dts, ctl[,lk], col='blue', lwd=2)
		lines(dts, glo[,lk], col='red', lwd=2)
	}
}


# loop to panel plot
# PART 1:  PLOT AND SKILL ASSESS FULL TIME SERIES
lwd <- 1.25
if(F){
	lab_dts <- seq(dts[1], rev(dts)[1], by='month')
pdf(file=sprintf('%s/remote_val.pdf', outdir), w=17, h=10)
layout(matrix(1:20, 5,4)); par(mar=c(0,0,0,0), oma=c(3,6,3,3), cex.axis=1.5)
for (lk in rev(lks)){
	if(all(is.na(obs[,lk]))) next
		lkname <- gsub(' Lake','',lk)
		lkname <- paste('  ',lkname, sep='')
		plot(dts, obs[,lk], pch=NA, ylab=NA, xlab=NA, xaxt='n', yaxt='n', ylim=c(-10,35))
		lines(dts, ctl[,lk], col=ctlcol, lwd=lwd)
		lines(dts, glo[,lk], col=glocol, lwd=lwd)
		#points(dts, obs[,lk], pch=5, cex=1, col='black', lwd=.5)
		points(dts, obs[,lk], pch=20, cex=.75, col='black')
		mtext(side=3, adj=0, line=-2, text=lkname, cex=1.25)

		yi <- par('mfg')[1]
		xi <- par('mfg')[2]
		if(xi==1 & yi%%2==1) axis(2)
		if(xi==4 & yi%%2==0) axis(4)
		if(xi==1 & yi%%2==0) axis(2, lab=NA)
		if(xi==4 & yi%%2==1) axis(4, lab=NA)
		if(yi==1) axis.Date(3, x=dts, at=lab_dts, lab=NA)
		if(yi==5) axis.Date(1, x=dts, at=lab_dts, lab=substr(month.abb, 1, 1))
	}

axis.Date(1, x=dts, at=lab_dts, lab=substr(month.abb, 1, 1))
plot.new()
legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'), 
	   col=c(ctlcol, glocol, 'black'), lwd=c(lwd, lwd, NA), pch=c(NA,NA,20), cex=1.75)
mtext('Lake Surface Temperature (°C)', side=2, cex=1.75, line=4, outer=T)
}

# PART 2:  PLOT MONTHLY AVERAGES
if(T){ # MANUAL SWITCH TO PLOT
lab_dts <- seq(dts[1], rev(dts)[1], by='month')
mons <- format(dts,'%b')
reorder <- match(month.abb, sort(month.abb))

obs_mon <- aggregate(obs, by=list(mons), mean, na.rm=T)[reorder,-1]
ctl_mon <- aggregate(ctl, by=list(mons), mean, na.rm=T)[reorder,-1]
glo_mon <- aggregate(glo, by=list(mons), mean, na.rm=T)[reorder,-1]
mons <- as.Date(sprintf('2019-%02i-01', 1:12)) 
pdf(file=sprintf('%s/../supporting_info/mon_validate.pdf', outdir), w=20, h=14)
layout(matrix(1:20, 5,4)); par(mar=c(0,0,0,0), oma=c(3,5,3,3), cex.axis=1.5)
for (lk in rev(lks)){
	if(all(is.na(obs[,lk]))) next
		plot(mons, obs_mon[,lk], pch=NA, ylab=NA, xlab=NA, xaxt='n', yaxt='n', ylim=c(-25,35))
		points(mons, obs_mon[,lk], pch=15, cex=2.5, col='black')
		points(mons, ctl_mon[,lk], col=ctlcol, pch=19, cex=2)
		points(mons, glo_mon[,lk], col=glocol, pch=17, cex=2)
		mtext(side=3, adj=0, line=-2.5, text=sprintf(' %s', lk), cex=1.5)

#		par(new=T)
#		plot(mons, ctl_mon[,lk]-obs_mon[,lk], 'h', col=ctlcol, lwd=5, ylim=c(-10,40), lend=1, yaxt='n', xaxt='n')
#		lines(mons+5, glo_mon[,lk]-obs_mon[,lk], 'h', col=glocol, lwd=5, ylim=c(-10,40), lend=1)
#		abline(h=0, lwd=1)

		yi <- par('mfg')[1]
		xi <- par('mfg')[2]

		# yaxes
		if(xi==1 && yi%%2==0) axis(2, at=seq(-10,30,by=10), lwd=0)
		if(xi==1) axis(2, at=seq(0,30,by=10), lab=NA)
		if(xi==4 && yi%%2==1) axis(4, at=seq(-10,30,by=10), lwd=0)
		if(xi==4) axis(4, at=seq(0,30,by=10), lab=NA)

		# xaxes 
		if(yi==1) axis.Date(3,at=mons, lab=NA)
		if(yi==5 || lk=='nipigon') axis.Date(1,at=mons, lab=substr(month.abb,1,1), line=0, lwd=0)
	}
#plot.new()
#legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'))
frame()
legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'), col=c(ctlcol, glocol, 'black'), pch=c(19,17,15), cex=3)
#legend('center', legend=c('Flatbottom', 'GLOBathy','Satellite Data'), col=c(ctlcol, glocol, 'black'), lty=NA, pch=c(19,17,15))#, cex=2.5, pt.cex=c(3,3,3))
mtext(side=2, outer=T, text='Lake-wide surface temperature (deg C)', cex=1.25, line=3)
}

if(F){
pdf(file=sprintf('%s/supplemental/mon_diff.pdf', outdir), w=12, h=10)
par(mar=c(4,10,4,1))
lk_names <- names(ctl_mon)
#lk_names <- str_to_title(gsub('_',' ',names(ctl_mon)))

image.plot(x=mons, y=1:ncol(ctl_mon), z=as.matrix(glo_mon-ctl_mon), col=hcl.colors(100,'Blue-Red 3') , xaxt='n', yaxt='n',
		   main='Lake-wide LST Difference (GLOBathy - Flatbottom)', ylab=NA, xlab=NA, legend.lab='degrees C')
axis(2, at=1:ncol(ctl), lab=lk_names, las=2, cex.axis=1.5)
axis.Date(1,at=mons, lab=substr(month.abb,1,1), line=0, lwd=1, cex.axis=1.25)
dev.off()
}










ctl_ice <- read.table('csv/ctl_ice.csv', row.names=1, sep=',')
glo_ice <- read.table('csv/glo_ice.csv', row.names=1, sep=',')
dts_ice <- as.Date(row.names(ctl_ice), format='%Y%m%d')

# clip no ice lakes (based on ctl runs)
has_ice <- apply(ctl_ice, 2, max, na.rm=T)>0
ctl_ice <- ctl_ice[,has_ice]
glo_ice <- glo_ice[,has_ice]

ctl_ice <- renamorder(ctl_ice)
glo_ice <- renamorder(glo_ice)

#
dts_ice <- dts_ice[which(!is.na(glo_ice[,1]))]
ctl_ice <- na.exclude(ctl_ice)
glo_ice <- na.exclude(glo_ice)


# plot suspect ice lakes
if(F){
	x11()
	matplot(dts_ice, ctl_ice[,c('Sebago','Goose','Oneida')], 'l', ylim=c(0,1), lty=1, ylab='FLATBOTTOM ice cover', lwd=2)
	legend('topright', legend=c('Sebago','Goose','Oneida'), text.col=1:3, cex=2)
	x11(w=10)
	matplot(dts_ice, glo_ice[,c('Sebago','Goose','Oneida')], 'l', ylim=c(0,1), lty=1, ylab='GLOBATHY ice cover', lwd=2)
	legend('topright', legend=c('Sebago','Goose','Oneida'), text.col=1:3, cex=2)
}

# clean up sticky ice pixel (persists 12/13% in globathy run)
glo_ice[glo_ice[,'Goose']==.05,'Goose'] <- 0
ctl_ice[ctl_ice[,'Goose']==.12,'Goose'] <- 0






# ICE DIFF PLOT
if(F){
	pdf(file=sprintf('%s/lakewide_icediff.pdf', outdir), w=20)
	par(mar=c(4,9.5,4,2), cex.axis=1.25, cex.main=2)
	image.plot(x=dts_ice, y=1:ncol(ctl_ice), z=as.matrix(glo_ice-ctl_ice), col=rev(hcl.colors(100, 'Purple-Brown')), zlim=c(-1,1), 
			   xaxt='n', yaxt='n', main='Fractional Ice Cover Difference: GLOBathy - Flatbottom', 
			ylab=NA, xlab=NA)
	axis(2, at=1:ncol(ctl_ice), lab=names(ctl_ice), las=2)
	axis.Date(side=1, x=dts_ice, at=seq(dts_ice[1], rev(dts_ice)[1], by='month'), format='%b')
}
dev.off()


graphics.off()


