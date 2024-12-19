#!/usr/bin/Rscripts
library(terra)
library(sf)
library(ncdf4)
source('utils.R')
#
## =====================================================================================
## ==================     USER  CONTROLS   =============================================
## =====================================================================================
#casename <- commandArgs(trail=T)[1]
#t0 <- '20181101'
#tf <- '20190101'
#var_str <- 'LAKE_T3D'
var_str <- 'LAKE_ICE3D'
#msk <- T 
#casename <- 'ctl'
casename <- 'ctl'
#print('USING LATEST RUNS')
#dir_in <- sprintf('/nfs/turbo/seas-drewgron/wrf_hydro/run/OUTPUT/%s', casename)

#dir_in <- sprintf('/%s', casename)
dir_in <- sprintf('/nfs/turbo/seas-hydro/um-jtti-ufs/wrfout/%s/', casename)
dir_out <- sprintf('/nfs/turbo/seas-hydro/um-jtti-ufs/CLM-Lake-offline/extracted_lakes_netCDF/%s', casename)

#dir_out <- sprintf('lw_out/%s', var_str)
if (!dir.exists(dir_out)) dir.create(dir_out)

# =====================================================================================
# =====================================================================================
# =====================================================================================
write_rast <- function(){
	print('writing rasters out to netcdf')
	for (l in 1:nrow(lks)) writeCDF(get(lks[l,]$NAME), filename=sprintf('%s/%s_%s.nc', dir_out, lks[l,]$NAME, var_str), overwrite=T, varname=var_str)
}

options(warn=-1)
lks <- read_lks()
options(warn=0)


print('finding files')
#flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018|2019).*.(01|08|15|22|29)0000.LDASOUT_DOMAIN1',recursive=T)) # weekly starting Nov '18
flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018(11|12)|2019).*.0000.LDASOUT_DOMAIN1$',recursive=T)) # daily starting in Nov '18
#flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018(11|12)|2019).*.LDASOUT_DOMAIN1$',recursive=T)) # hourly starting in Nov '18
#flist <- flist[1:10] # debug first 10 time steps

# define dates based on flist
dts <- as.POSIXct(gsub('.LDASOUT_DOMAIN1','',basename(flist)), format='%Y%m%d%H', tz='z')

# initialize lakes 
for (lkname in lks$NAME) assign(lkname, rast())


# 
for (d in 1:length(dts)){
	print(dts[d])
	var2D <- rastT(flist[d], var_str)
	time(var2D) <- dts[d]
	for (l in 1:nrow(lks))	assign(lks[l,]$NAME, c(get(lks[l,]$NAME), crop(var2D, lks[l,]), warn=F))
	if (!d%%60)  write_rast() # write every 60 timesteps
}
	

write_rast()




