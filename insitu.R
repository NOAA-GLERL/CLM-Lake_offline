######## Surface Temperature Plotting ###########
#library(RColorBrewer)      

outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/'
ctlcol <- 'brown'
glocol <- 'orange'
lwd <- .5


if(T){ # do slow stuff
## Set directories and read in observational data (see Sorenson et al 2024 for data)
column_temps_dir <- 'csv/Column_temps'
meta    <- read.csv('csv/all_metadata_adjusted.csv')
obs     <- read.csv('csv/all_data_hourly.csv')[-8761,]
depths = read.csv('csv/model_depths_at_obs_locations.csv')

## Filter to only select top sensors
meta_top <- meta[grepl("_00$",meta$name),]
obs_top  <- obs[,grepl("_00$",colnames(obs))]

## Filter to only select surface (<0.5m) sensors
surf_sensors <- meta_top[which(meta_top[,2]<=0.5),1]
# AGU Edit
# surf_sensors <- c("UTA06_00", "UTA02_00", "UPK02_00", "UPL06_00", "TAW02_00", "TAW04_00", "TAH02_00","TAH01_00","SAK04_00","SAK01_00","RED08_00",
#                   "RED01_00","PON03_00","PON02_00",
#                   "ONE03_00","ONE01_00","OKE01_00","OKE15_00","MON08_00","MON07_00","MEA01_00","MCC02_00","MAR07_00","MAR05_00","GSL02_00",
#                   "GSL01_00","DEV02_00","DEV01_00","CLE05_00","CLE02_00")
meta_surf <- meta[meta$name %in% surf_sensors,]
depths_surf <- depths[depths$name %in% surf_sensors,]
obs_surf <- obs_top[,colnames(obs_top) %in% surf_sensors]




## Create master lists of the top sensor data for each model run
locs <- meta_top$name
ctl_top <- matrix(nrow=length(obs_top[,1]), ncol = length(colnames(obs_top)))
colnames(ctl_top) <- colnames(obs_top)
for (i in 1:length(colnames(ctl_top))){
  ctl_top[,i] <- read.csv(sprintf('%s/%s/%s.csv', column_temps_dir, 'ctl', locs[i]))[,2]
}
ctl_top <- as.data.frame(ctl_top)

bi0m_top <- matrix(nrow=length(obs_top[,1]), ncol = length(colnames(obs_top)))
colnames(bi0m_top) <- colnames(obs_top)
for (i in 1:length(colnames(bi0m_top))){
  bi0m_top[,i] <- read.csv(sprintf('%s/%s/%s.csv', column_temps_dir, 'bi0m', locs[i]))[,2]
}
bi0m_top <- as.data.frame(bi0m_top)

bi2m_top <- matrix(nrow=length(obs_top[,1]), ncol = length(colnames(obs_top)))
colnames(bi2m_top) <- colnames(obs_top)
for (i in 1:length(colnames(bi2m_top))){
  bi2m_top[,i] <- read.csv(sprintf('%s/%s/%s.csv', column_temps_dir, 'bi2m', locs[i]))[,2]
}
bi2m_top <- as.data.frame(bi2m_top)

## Filter to only select locations which have sensors < 0.5m depth
obs_surf   <- obs_top[,colnames(obs_top) %in% surf_sensors]
ctl_surf   <- ctl_top[,colnames(ctl_top) %in% surf_sensors]
bi0m_surf  <- bi0m_top[,colnames(bi0m_top) %in% surf_sensors]
bi2m_surf  <- bi2m_top[,colnames(bi2m_top) %in% surf_sensors]

## Filter to remove locations which do not have any model data
active_locs <- which(!is.na(ctl_surf[1,]) & depths_surf[,1] != -10)
obs_surf  <- obs_surf[,active_locs]
ctl_surf  <- ctl_surf[,active_locs]
bi0m_surf <- bi0m_surf[,active_locs]
bi2m_surf <- bi2m_surf[,active_locs]


dts <- read.csv(sprintf('%s/%s/%s.csv', column_temps_dir, 'ctl', locs[i]))[,1]
dts <- as.POSIXct(dts, format='%Y-%m-%d %H', 'z')
#Plot surface data for sensors < 0.5m and > 15 observations

        names(ctl_surf) <- gsub('_00', '', names(ctl_surf))


}
lab_dts <- seq(dts[1], rev(dts)[1], by='month')


pdf(file=sprintf('%s/insitu_val.pdf', outdir), w=17, h=15)
#layout(matrix(1:20, 5,4)); par(mar=c(0,0,0,0), oma=c(3,5,3,3))
layout(matrix(1:36, 9,4)); 
par(mar=c(0,0,0,0), oma=c(3,7,5,3), cex.axis=1.75)
for (i in 1:length(active_locs)){
  obs_count <- sum(!is.na(obs_surf[,i]))
  if (obs_count < 12) next
     lkname <- paste('  ',names(ctl_surf)[i], sep='')
  	 print(lkname)
	 if (grepl('CLE|MEA', lkname)) next
     plot(dts, ctl_surf[,i], type='l', col=ctlcol, lwd=lwd, 
		 ylab=NA, xlab=NA, xaxt='n', yaxt='n', ylim=c(0,35))
     lines(dts, bi0m_surf[,i], col=glocol, lwd=lwd)
     points(dts, obs_surf[,i], pch=5, cex=1.5, col='black', lwd=1.25)
	 if (grepl('GSL01', lkname)){
		print('overplotting lines...')
     	lines(dts, ctl_surf[,i], col=ctlcol, lwd=lwd)
     	lines(dts, bi0m_surf[,i], col=glocol, lwd=lwd)
	 }
     #points(dts_obs, obs[,lk], pch=18, cex=1, col='black')
     mtext(side=3, adj=0, line=-2, text=lkname, cex=1.25)

     yi <- par('mfg')[1]
     xi <- par('mfg')[2]
	 print(yi)
	 if(xi==1 && yi%%2==0) axis(2)
	 if(xi==4 && yi%%2!=0) axis(4)
     if(yi==1) axis.POSIXct(3, x=dts, at=lab_dts, lab=NA)
     if(yi==9) axis.POSIXct(1, x=dts, at=lab_dts, lab=NA)
     if(yi==9) axis.POSIXct(1, x=dts, at=lab_dts, lab=substr(month.abb, 1, 1), gap.axis=.5, line=.5, lwd=0)
     axis.POSIXct(1, x=dts, at=lab_dts, lab=NA, tcl=.2)
	 axis(4, lab=NA, tcl=-.2); axis(2, lab=NA, tcl=-.2)
    }

axis.Date(1, x=dts, at=seq(dts[1], rev(dts)[1], by='month'), format='%b')

#par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), mar=c(0, 0, 0, 0), new=TRUE)
#legend('top', legend=c('Flatbottom', 'GLOBathy',expression(italic('in-situ')*' obs.')), horiz=T,
#      col=c(ctlcol, glocol, 'black'), lwd=c(2, 2, NA), pch=c(NA,NA,5), cex=1.75, inset=c(0,.01))

frame(); #frame()
legend('center', legend=c('Flatbottom', 'GLOBathy',expression(italic('in-situ')*' obs.')), horiz=F,
       col=c(ctlcol, glocol, 'black'), lwd=c(2, 2, NA), pch=c(NA,NA,5), cex=2.25, inset=c(0,.01))

mtext('Lake Surface Temperature (°C)', side=2, outer=T, cex=1.75, line=4.5)
#plot.new()

dev.off()


