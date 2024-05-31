source('utils.R')

ctl <- filter_rng(read.table('csv/ctl_T3D.csv', row.names=1, sep=','))
glo <- filter_rng(read.table('csv/bi0m_T3D.csv', row.names=1, sep=','))

# diurnal ranges
ctl <- renamorder(ctl)
glo <- renamorder(glo)
temp <- round(apply(ctl, 2, mean, na.rm=T))
dts_mod <- as.POSIXct(row.names(ctl), 'z')

# exclude leading Nov and trailing Dec (otherwise there are doubled months in some seasons)
t0 <- as.POSIXct('2018-12-01', 'z')
tf <- as.POSIXct('2019-11-30', 'z')
seldts <- dts_mod >= t0 & dts_mod <= tf
ctl <- ctl[seldts,]
glo <- glo[seldts,]
dts_mod <- dts_mod[seldts]

print(head(glo, n=1))
print(tail(glo, n=1))

#stop()
ymd <- format(dts_mod, '%b%d')
ctl <- aggregate(ctl, by=list(ymd), function(x) diff(range(x)))[,-1]
glo <- aggregate(glo, by=list(ymd), function(x) diff(range(x)))[,-1]
print(apply(ctl, 2, range, na.rm=T))
print(apply(glo, 2, range, na.rm=T))
row.names(ctl) <- unique(ymd)
row.names(glo) <- unique(ymd)

#lks <- names(ctl)
lks <- row.names(meta)

#sel1 <- lks[temp<7]
#sel2 <- lks[temp>=7 & temp <10]
#sel3 <- lks[temp>=10]

mon <- tolower(substr(row.names(ctl),1,3))

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
ctl$seas <- seas
glo$seas <- seas
ctl$case <- 'flt'
glo$case <- 'glob'
dat <- rbind(ctl, glo)



ats <-  1:8 - c(-.125,.125)
at_labs <- seq(1.5,7.5, length=4)
labs <- levels(seas)

pdf(file=sprintf('%s/box_diurnal.pdf', outdir), w=17, h=11)
#x11()
layout(matrix(1:25, 5,5)); 
par(mar=c(0,0,0,0), oma=c(3,7,3,3), cex.axis=1.5)
for (lk in lks){
		boxplot(dat[,lk]~dat$case+dat$seas, col=c(ctlcol, glocol), at=1:8-c(0,.125),
			xaxt='n', xlab=NA, ylab=NA, outline=F, ylim=c(0,14), yaxt='n')

		ti_str <- sprintf('%s \n%i°N ',lk, round(meta[lk,'lat'], digits=0))
		mtext(side=3, adj=1, line=-4, text=ti_str, cex=1.125)
        yi <- par('mfg')[1]
        xi <- par('mfg')[2]
        if(xi==1 & yi%%2==0) axis(2, at=seq(2,12,by=2), cex.axis=1.125)
        if((xi==5 | (yi==4 & xi==4)) & yi%%2==1) axis(4, at=seq(2,12,by=2), cex.axis=1.125)
        if(yi==5 | (yi==3 & xi==5)) axis(1, at=at_labs, lab=labs)
		axis(4, lab=NA, tcl=.15); 
		axis(2, lab=NA, tcl=.15)
}
mtext('Diurnal Temperature Range (°C) N=days', side=2, outer=T, cex=1.5, line=4)

plot.new()
legend('center', legend=c('flatbottom', 'GLOBathy'), text.col=c(ctlcol, glocol), cex=2.25, adj=+.125)


boxplot(as.matrix(dat[lks])~dat$case+dat$seas, col=c(ctlcol, glocol), at=1:8-c(0,.125),
	xaxt='n', xlab=NA, ylab=NA, outline=F, ylim=c(0,14), yaxt='n')
axis(4, at=seq(2,12,by=2), cex.axis=1.125)
mtext(side=3, adj=1, line=-2, text='All Lakes  ', cex=1.25)
axis(1, at=at_labs, lab=labs)
graphics.off()



#cold <- dat[c(sel1,'case','seas')]


#boxplot(Manitoba~case+seas, dat, col=c(ctlcol,glocol))








