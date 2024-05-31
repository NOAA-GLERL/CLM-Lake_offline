source('utils.R')

ctl <- read.table('csv/ctl_EVAP2D.csv', row.names=1, sep=',', strip=T)
glo <- read.table('csv/bi0m_EVAP2D.csv', row.names=1, sep=',', strip=T)

sglo <- apply(glo, 2, sum, na.rm=T)
sctl <- apply(ctl, 2, sum, na.rm=T)

dts_mod <- as.POSIXct(row.names(ctl), 'z')

# exclude leading Nov and trailing Dec (otherwise there are doubled months in some seasons)
t0 <- as.POSIXct('2018-12-01', 'z')
tf <- as.POSIXct('2019-11-30', 'z')
seldts <- dts_mod >= t0 & dts_mod <= tf
ctl <- ctl[seldts,]
glo <- glo[seldts,]
dts_mod <- dts_mod[seldts]

# diurnal ranges
ctl <- renamorder(ctl)
glo <- renamorder(glo)
temp <- round(apply(ctl, 2, mean, na.rm=T))
ymd <- format(dts_mod, '%b%d')

ctl <- rev(filter_rng(ctl, low=-1, high=100))
glo <- rev(filter_rng(glo, low=-1, high=100))


ctl_dly <- as.matrix(aggregate(ctl, by=list(ymd), sum, na.rm=T)[,-1])
glo_dly <- as.matrix(aggregate(glo, by=list(ymd), sum, na.rm=T)[,-1])
print(apply(ctl, 2, range, na.rm=T))
print(apply(glo, 2, range, na.rm=T))
#row.names(ctl) <- ymd
#row.names(glo) <- ymd

#lks <- names(ctl)
lks <- row.names(meta)

#sel1 <- lks[temp<7]
#sel2 <- lks[temp>=7 & temp <10]
#sel3 <- lks[temp>=10]

mon <- tolower(substr(ymd,1,3))

sel_djf <- grep('dec|jan|feb', mon)
sel_mam <- grep('mar|apr|may', mon)
sel_jja <- grep('jun|jul|aug', mon)
sel_son <- grep('sep|oct|nov', mon)

seas <- vector('character', nrow(ctl))
seas[sel_djf] <- 'DJF'
seas[sel_mam] <- 'MAM'
seas[sel_jja] <- 'JJA'
seas[sel_son] <- 'SON'



seas <- factor(seas,  levels=c('DJF','MAM','JJA','SON'))
ctl_seas <- as.matrix(aggregate(ctl, by=list(seas), sum, na.rm=T)[,-1])
glo_seas <- as.matrix(aggregate(glo, by=list(seas), sum, na.rm=T)[,-1])

row.names(ctl_seas) <- levels(seas)
row.names(glo_seas) <- levels(seas)


# not used 
ctl_seas_all <- apply(ctl_seas, 1, sum)
glo_seas_all <- apply(glo_seas, 1, sum)



pdf(width=14, h=8, sprintf('%s/evap_bar.pdf', outdir))
par(cex.axis=1.25, cex.lab=1.5)
dat <- t(interleave(t(ctl_seas),t(glo_seas)))
ats <- barplot(dat, axes=T, axisnames=F, space=c(1,.1), 
			   legend.text=F, ylab='total evaporation (mm)')
legend('topright', legend=levels(seas), cex=1.75, col=grey.colors(4), pch=15)
text(ats[c(T,F)]+.5, -150, lab=lks, srt=45, xpd=T)
dev.off()






pdf(width=24, h=16, sprintf('%s/supplemental/evap_scatter.pdf', outdir))
layout(matrix(1:24,4,6)); par(mar=c(1,1,1,1), oma=c(6,6,6,0), cex.axis=1.75)
for (lk in lks){ 
	plot(ctl[,lk], jitter(glo[,lk]), asp=1, xlim=c(-.2,1), ylim=c(-.2,1), 
		 xaxt='n', yaxt='n', xlab='FB', ylab='GLO', lwd=.1, cex=.5); 
	mtext(sprintf('  %s',lk), side=3, adj=0, line=-2)
	axis(1, lab=NA); axis(2, lab=NA)
	abline(a=0, b=1, col='red')

	xi=par()$mfg[2]; yi=par()$mfg[1]
	print(xi); print(yi)
	if (xi == 1) axis(2)
	if (yi == 4) axis(1)

}
mtext('Hourly Evaporation (mm)', side=3, outer=T, cex=3, line=2)
mtext('GLOBathy', side=2, outer=T, cex=2, line=3)
mtext('Flatbottom', side=1, outer=T, cex=2, line=3)
dev.off()


# no use
#plot(ctl_dly[,1], glo_dly[,1], xlim=c(0,1), asp=1, ylim=c(0,1), cex=3, lwd=.25)
#for (lk in lks) points(ctl[,lk], jitter(glo[,lk]), col=match(lk,lks), cex=3, lwd=.25)

