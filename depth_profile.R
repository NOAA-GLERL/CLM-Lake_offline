################################################################################
############################ User Controls #####################################
################################################################################

# load array z and dz, dims are sensor, column layer, model version
source('get_z_depths.R')

# read in obs and obs metadata
meta    = read.csv('csv/all_metadata_adjusted.csv')
obs     = read.csv('csv/all_data_hourly.csv')[-8761,]
column_temps_dir = 'csv/Column_temps'


outfile <- '../65e0d491f698c7b0fdfee2b7/depth_profile.pdf'
# many locations have very low temperatures (around -20), which throws off the color scale
# change all values below lower_lim to equal lower_lim?

# select locations of interest (multiple depths or no)
locs = names(table(substr(names(obs)[-1],1,5)))
dts <- seq.POSIXt(as.POSIXct('2019-01-01 00:00'), by='hour', length=8760)

mypal = function(n) hcl.colors(n, "YlOrRd", rev = TRUE)

locs <- c('RED01','UPK03','GSL05','MAR02')
longs <- c('Lower Red', 'Upper Klamath', 'Great Salt', 'Marion')
#          DC      SC      DNC     SNC
# also see                         SAK01



#x11(width=22, height=10)
# plot 4 select locs for main graphic
A <- matrix(c(1,3,2,3), 2,2) # basic template for two heatmaps with single cbar below
pdf(outfile, width=22, h=10)
B <- cbind(rbind(A,A+3),rbind(A+6,A+9))
layout(B, heights=c(.75,.25,.75,.25))
par(cex.axis=1.5, cex.lab=1.5)
par(oma=c(0,5,4,2))

for (loc in locs){

	idx <- which(loc==locs)
	print(idx)

	# 0 READ 
	ctl = as.matrix(read.csv(sprintf('%s/%s/%s_00.csv', column_temps_dir, 'ctl', loc))[,-1])
	z_ctl = z[sprintf('%s_00',loc),,'ctl']
	glo = as.matrix(read.csv(sprintf('%s/%s/%s_00.csv', column_temps_dir, 'bi0m', loc))[,-1])
	z_glo = z[sprintf('%s_00',loc),,'bi0m']
	if(all(is.na(ctl)) | all(is.na(glo))) next

	zmax <- max(z_ctl, z_glo)
	tmax <- max(ctl, glo)
	if(tmax > 100){ print(tmax); next }
	tmin <- min(ctl, glo) 
	levs <- pretty(0:tmax, n=20)
	ctl[ctl < 0] <- 0
	glo[glo < 0] <- 0
	nlevs <- length(levs)
	cmap <- mypal(nlevs-1)
	


# 1 LEFT HEAT PANEL
	par(mar=c(0,2,3,.5), cex.axis=2.0)
	frame(); 
	plot.window(xlim=range(dts), y=c(zmax,0), xaxs='i', yaxs='i')
	.filled.contour(x=dts, y=z_ctl, z=ctl, levels=levs, col=cmap); box()
	axis.POSIXct(1,at=seq.POSIXt(dts[1], rev(dts)[1], by='month'), lab=NA)
	axis.POSIXct(1,at=seq.POSIXt(dts[1], rev(dts)[1], by='month'), lab=substr(month.abb,1,1), line=.5, lwd=0)

	mtext(' FB', 3, cex=1.5, line=-2, adj=0)
	if(idx %in% c(1,2)) axis(2); 
	if(idx %in% c(3,4)) axis(4, lab=NA); 
	#axis(4, lab=NA, tcl=-.25); 

# 2 RIGHT HEAT PANEL
	par(mar=c(0,.5,3,2), cex.axis=2.0)
	frame(); 
	plot.window(xlim=range(dts), y=c(zmax,0), xaxs='i', yaxs='i')
	.filled.contour(x=dts, y=z_glo, z=glo, levels=levs, col=cmap); box()
	axis.POSIXct(1,at=seq.POSIXt(dts[1], rev(dts)[1], by='month'), lab=NA)
	axis.POSIXct(1,at=seq.POSIXt(dts[1], rev(dts)[1], by='month'), lab=substr(month.abb,1,1), line=.5, lwd=0)

	mtext(' GLOB', 3, cex=1.5, line=-2, adj=0)
	#if(idx %in% c(1,3)) mtext('globathy', 3, cex=1.75, line=.5)
	if(idx %in% c(1,2)) axis(2, lab=NA)
	if(idx %in% c(3,4)) axis(4)

	#mtext(side=3, outer=T, loc, cex=2,line=2)
	#mtext(side=3, outer=T, loc, cex=2,line=2)

#3. ADD COLOR BAR
	par(mar=c(3,25,4,25))  #pdf
	plot.new()
	plot.window(ylim=c(0,1), xlim=c(0,nlevs))
	rect(0:(nlevs-2), 0, 1:(nlevs-1),1, col=cmap, border='white')
	axis(1, at=(1:nlevs)[c(T,F,F,F)]-0.5, lab=levs[c(T,F,F,F)], lwd=0, line=-.5)
	mtext('temperature (°C)', 3, cex=1)
	mtext(longs[idx], side=3, line=29, cex=2.0)
}

mtext('depth (m)', 2, line=3, cex=1.75, outer=T);
graphics.off()

