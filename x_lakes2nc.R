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
var_str <- 'LAKE_ICE3D'
msk <- T
casename <- 'ctl'
#print('USING LATEST RUNS')
#dir_in <- sprintf('/nfs/turbo/seas-drewgron/wrf_hydro/run/OUTPUT/%s', casename)

dir_in <- sprintf('/%s', casename)
dir_out <- sprintf('/nfs/turbo/seas-drewgron/wrf_hydro/extracted_data/%s/daily_files/', casename)

#dir_out <- sprintf('lw_out/%s', var_str)
#if (!dir.exists(dir_out)) dir.create(dir_out)

# =====================================================================================
# =====================================================================================
# =====================================================================================

options(warn=-1)
lks <- read_lks()
options(warn=0)


print('finding files')
#flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018|2019).*.(01|08|15|22|29)0000.LDASOUT_DOMAIN1',recursive=T)) # weekly starting Nov '18
#flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018(11|12)|2019).*.0000.LDASOUT_DOMAIN1$',recursive=T)) # daily starting in Nov '18
#flist <- sort(list.files(path=dir_in, full.names=T, pattern='(2018(11|12)|2019).*.LDASOUT_DOMAIN1$',recursive=T)) # hourly starting in Nov '18


# subset dates
dts <- as.POSIXct(gsub('.LDASOUT_DOMAIN1','',basename(flist)), format='%Y%m%d%H', tz='z')
t0 <- as.POSIXct(t0, format='%Y%m%d', tz='z')
tf <- as.POSIXct(tf, format='%Y%m%d', tz='z')

sel_dts <- dts >= t0  & dts <= tf
dts <- dts[sel_dts]
flist <- flist[sel_dts]
dtstr <- format(dts, '%Y%m%d%H00')
print(range(dts))

process_file <- function(fn, dtstr){
		var2D <- rastT(fn, var_str)
        # ================== LAKE LOOP =======================
        for (i in 1:length(lks)){
            lk <- lks[i,]
            cat(sprintf('%s...',lk$NAME))
            lk_var <- intersect(var2D, lk)    
            names(lk_var) <- dtstr
            if (msk) lk_var <- mask(lk_var, lk)
            fout <- sprintf('%s/%s_%s_%s.nc', dir_out, tolower(lk$NAME), dtstr, var_str)
            writeRaster(t(lk_var), file=fout, varname=var_str, zname='time', overwrite=T)

        }
        cat(sprintf('\n'))
    }

for (i in 1:length(flist)){
	fn <- flist[i]
	print(fn)
	process_file(fn, dtstr[i])
}


