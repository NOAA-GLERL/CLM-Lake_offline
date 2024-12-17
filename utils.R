#!/bin/Rscript
library(stringr)
library(sf)
library(terra)
library(ncdf4)


# rename and re-order lakes
renamorder <- function(dat){
    dat <- dat[,order(names(dat), decreasing=T)]
    names(dat) <- str_to_title(gsub('_',' ',names(dat)))
    names(dat)[names(dat)=='Mcconaugh'] <- 'McConaughy'
    names(dat) <- gsub(' Lake','', names(dat)
    )
    return(dat)
}


#outdir <- '/home/kessler/work/jtti/65e0d491f698c7b0fdfee2b7/'
outdir <- '/home/j4mes/work/clm/'
basedir <- '/home/j4mes/work/clm/CLM-Lake-offline/'

meta <- read.table('lake_wide_csv/lks_meta.txt', head=T, row.names=1)
row.names(meta) <- gsub('_',' ',row.names(meta))
lks <- row.names(meta)

min_frac <- .3
ctlcol <- 'brown'
glocol <- 'orange'

rng <- list(min=-40, max=50)
filter_rng <- function(data, low=rng$min, high=rng$max){
    sellow <- which(data < low, arr.ind=T)
    selhig <- which(data > high, arr.ind=T)
    data[sellow] <- NA
    data[selhig] <- NA
    #if(length(sellow) > 0) cat(sprintf('threw out %i low vals\n', length(sellow)))
    #if(length(selhig) > 0) cat(sprintf('threw out %i high vals\n', length(selhig)))
    return(data)
}

read_T <- function(fn, selx, sely){
	ncid <- nc_open(fn)
	T3D <- ncvar_get(ncid,'LAKE_T3D')-273.15  # native dims (y, z, x)  WHY??? lol
	T2D <- matrix(NA, 10, length(selx)) # saved output for this timestep
	for (z in 1:10) T2D[z,] <- T3D[,z,][cbind(selx,sely)] 
	nc_close(ncid)
	return(T2D)
}

#read_meta <- function(meta_fn='/nfs/turbo/seas-drewgron/in_situ/final_data/sensor_metadata.csv'){
#	dat <- read.csv(meta_fn)
#	df <- data.frame(name=dat$name, depth=dat$depth, lake=dat$lake)
#	pts_ll <- SpatialPointsDataFrame(dat[c('lon','lat')], proj4string=CRS('+proj=longlat +datum=WGS84'), data=df)
#	pts <- spTransform(pts_ll, '+proj=lcc +lat_0=38.5 +lon_0=-97.5 +lat_1=38.5 +lat_2=38.5 +x_0=0 +y_0=0 +R=6370000 +units=m +no_defs +type=crs')
#}


read_lks <- function()	return(st_read(sprintf('%s/grid/my_lks.shp', basedir)))
read_grid <- function() return(rast(sprintf('%s/grid/wrf_grid.tif', basedir)))

rastT <- function(fn, varstr){
	grd <- read_grid()
	if (grepl('2D', varstr)) T <- ncvar_get(nc_open(fn), varstr)
	if (grepl('3D', varstr)) T <- ncvar_get(nc_open(fn), varstr)[,1,] # only get sfc
	values(grd) <- t(T)[1059:1,]
	return(grd)
}






