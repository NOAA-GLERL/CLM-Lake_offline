#!/bin/Rscript
source('export/utils.R')
library(raster)
library(viridisLite)
library(stringr)
source('utils.R')

# mono lake
mono_bathy <- raster('hires_bathy/mono/hdr.adf') # see citation for data source
mono_bathy$hdr <- (cellStats(mono_bathy$hdr, max) - (mono_bathy$hdr))/3.28094

# lake tahoe
tahoe_bathy <- raster('hires_bathy/lt_bathy.tif') # see citation for data source
values(tahoe_bathy)[values(tahoe_bathy)==0] <- NA
values(tahoe_bathy) <- cellStats(tahoe_bathy, max) - values(tahoe_bathy)

varstr <- 'LAKEDEPTH2D'

# read vars
options(warn=-1)
lks <- read_lks()
grd <- read_grid()
grdctl <- grd
options(warn=1)

# assign glo depths
fn <- 'netcdf/bi0m_depth.nc'
my_var <- ncvar_get(nc_open(fn), varstr)#[,1,]
values(grd) <- t(my_var)[1059:1,]

fn <- 'netcdf/ctl_depth.nc'
my_var <- ncvar_get(nc_open(fn), varstr)#[,1,]
values(grdctl) <- t(my_var)[1059:1,]


# open/draw graphics
#x11(w=10,h=10)


lks <- lks[order(lks$NAME),]
lks$NAME <- str_to_title(gsub('_',' ',lks$NAME))
lks$NAME <- gsub(' Lake','',lks$NAME)
lks$NAME <- gsub('Mcconaugh','McConaughy',lks$NAME)



try_nlevs <- 25

mcex <- 2.0

draw_three <- function(lkname, realbathy){
# loop thru all lakes and plot stuff
		i <- which(lks$NAME == lkname)
		sub_grd <- intersect(grd, lks[i,])
		max_glo <- max(values(sub_grd))
		sub_grd_ctl <- intersect(grdctl, lks[i,])
		max_ctl <- max(values(sub_grd_ctl))
		zmax <- max(max(values(realbathy), na.rm=T), max_glo, max_ctl)
		print(zmax)
		levs <- pretty(c(0, zmax), n=try_nlevs)
		nlevs <- length(levs)
		cmap <- rev(viridis(nlevs-1))

		#plot(lks[i,], border='black', ax=F, lwd=3)
		image(sub_grd_ctl, maxpixels=2e6, col=cmap,
			  zlim=c(0,zmax), axes=F, ylab=NA, xlab=NA, asp=1)
		mtext(lkname, side=2, cex=mcex, line=3)
		if(lkname == 'Tahoe') mtext('flatbottom', side=1, cex=mcex, line=6)
		image(sub_grd, maxpixels=2e6, col=cmap,
			  zlim=c(0,zmax), axes=F, ylab=NA, xlab=NA, asp=1)
		if(lkname == 'Tahoe') mtext('GLOBathy', side=1, cex=mcex, line=6)
		image(realbathy, maxpixels=2e6, col=cmap,
			  zlim=c(0,zmax), axes=F, ylab=NA, xlab=NA, asp=1)
		if(lkname == 'Tahoe') mtext('high-res bathymetry', side=1, cex=mcex, line=6)

		plot.new()
		plot.window(xlim=c(0,1), ylim=c(0,zmax))
		#rect(0.4,0:(nlevs-2), 0.6, 1:(nlevs-1), col=cmap, border=NA)
		rect(0.4,rev(rev(levs)[-1]), 0.6, levs[-1], col=cmap, border=NA)
		axis(4, line=-4, lwd=0, las=2, at=levs[c(T,F,F)], cex.axis=1.5)
		mtext('depth (m)', side=2, cex=1.5, line=-2)
		#dev.off()
}

pdf(file=sprintf('%s/real_depth.pdf', outdir), w=16, h=10)
layout(matrix(1:8,2,4, byrow=T), widths=c(.3,.3,.3,.1))
par(mar=c(1.0,0.5,1.0,0.5), oma=c(8,8,0,0))
draw_three('Mono', mono_bathy)
draw_three('Tahoe', tahoe_bathy)


		#ttl <- sprintf('%s: %.0fm (%.0fm)', lkname, max_glo, max_ctl)
	#	scalebar(3e3, lwd=3, lab='3 km')
		#mtext(ttl, adj=0, side=3, cex=2)
