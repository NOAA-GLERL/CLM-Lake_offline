#!/bin/Rscript
qlibrary(viridis)
qlibrary(stringr)
qlibrary(fields)
graphics.off()


outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/figures/'
lkmeta <- read.table('txt/lake_ids.txt', head=T)

hrrr <- read.table('csv/T2D.csv', row.names=1, sep=',')
dts <- as.POSIXct(row.names(hrrr), 'z')


# rename and re-order lakes
renamorder <- function(dat){ 
	dat <- dat[,order(names(dat), decreasing=T)]
	names(dat) <- str_to_title(gsub('_',' ',names(dat)))
	names(dat)[names(dat)=='Mcconaugh'] <- 'McConaugh'
	names(dat) <- gsub(' Lake', '', names(dat))
	return(dat)
}


hrrr <- renamorder(hrrr)


# temp diff plot
if(T){
	pdf(file=sprintf('%s/hrrr_temp.pdf', outdir), w=20)
#	graphics.off(); x11(w=20)
	par(mar=c(4,9.5,4,2), cex.axis=1.25, cex.main=2)
	image.plot(x=dts, y=1:ncol(hrrr), z=as.matrix(hrrr), col=inferno(100), xaxt='n', yaxt='n', 
			   main='HRRR 2M Air Temperature (°C)', ylab=NA, xlab=NA)
	axis(2, at=1:ncol(hrrr), lab=names(hrrr), las=2)
	#axis.Date(side=1, x=dts_mod, at=seq(dts_mod[1], rev(dts_mod)[1], by='month', format='%b'), format='%b \'%y')
	axis.POSIXct(side=1, x=dts, at=seq(dts[1], rev(dts)[1], by='month', format='%b'), format='%b')
}

# could repeat the above block for more HRRR vars?
dev.off()
