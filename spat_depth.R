#!/usr/bin/Rscript
source('export/utils.R')
library(viridisLite)
library(stringr)

outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/figures/'

#varstr <- commandArgs(trail=T)
#varstr <- 'LAKE_DEPTH'
varstr <- 'LAKEDEPTH2D'
#fn <- 'wrfinput_d01.nc'
#fn <- '201801010000.LDASOUT_DOMAIN1'

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
#png(sprintf('%s_OBS.png',varstr), w=1920, h=1080)
#png('depth.png', w=1920, h=1080)
pdf(file=sprintf('%s/glo_depth.pdf', outdir), w=24, h=12)
#x11(w=10,h=10)


lks <- lks[order(lks$NAME),]
lks$NAME <- str_to_title(gsub('_',' ',lks$NAME))
lks$NAME <- gsub(' Lake','',lks$NAME)
lks$NAME <- gsub('Mcconaugh','McConaughy',lks$NAME)


# loop thru all lakes and plot stuff
layout(matrix(1:24,4,6))
par(mar=c(0.5,0.5,2.5,0.5))
for (i in 1:length(lks)){
	lkname <- lks$NAME[i]
	sub_grd <- intersect(grd, lks[i,])
	max_glo <- max(values(sub_grd))
	sub_grd_ctl <- intersect(grdctl, lks[i,])
	max_ctl <- max(values(sub_grd_ctl))
	plot(lks[i,], border='black', ax=F, lwd=3)
	image(sub_grd, add=T, maxpixels=2e6, col=rev(viridis(25)), zlim=c(0,max_glo))
	ttl <- sprintf('%s: %.0fm (%.0fm)', lkname, max_glo, max_ctl)
#	scalebar(3e3, lwd=3, lab='3 km')
	mtext(ttl, adj=0, side=3, cex=2)


	lk_area <- sum(values(sub_grd) != -10) * 9
	cat(sprintf('%s, %i\n', lkname, lk_area))
}


nlevs <- 25

plot.new()
plot.window(ylim=c(0,1), xlim=c(0,nlevs))
rect(0:(nlevs-2), 0.425, 1:(nlevs-1),.575, col=rev(viridis(25)), border=NA)
mtext('relative depth', side=3, cex=2, line=-7)
mtext('0m', side=1, adj=0, line=-7, cex=2)
mtext('max', side=1, adj=1, line=-7, cex=2)
dev.off()






## also plot an individual img for each lake?
#if (T){
#for (i in 1:length(lks)){
#	lkname <- lks$NAME[i]
#	plot(lks[i,], border='lightblue', ax=F)
#	maxdepth_i <- max(unlist(extract(grd, lks[i,])))
#	plot(grd, add=T, maxpixels=2e6, col=viridis(25), legend.width=5, zlim=c(0,maxdepth_i))
#	sp::plot(pts, add=T, col='darkgreen', cex=4, lwd=2)
##	text(pts, lab=pts$name, pos=1, cex=.5)
#	scalebar(3e3, lwd=2, lab='3 km')
#	mtext(lkname, side=3, cex=1.5)
#}
#graphics.off()
